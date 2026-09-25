import LeanPlot.Core.Data
import LeanPlot.Recipes.Algo.F32

/-!
# Streamplot (exact port of Makie `streamplot_impl`)

Makie 0.24 `basic_recipes/streamplot.jl`. The bounding box is cut into a
`gridsize` mask of cells. Seed cells are drawn from an additive quasi-random
(Kronecker) sequence with coefficients `φ_N^{-i}` (`φ_2 = 1.3247…` the plastic
number, `φ_3 = 1.2207…`); for every unvisited seed cell an arrow is placed at
the cell centre and a streamline is traced in both directions with fixed-length
normalised Euler steps `x ← x + d·dt·f(x)/‖f(x)‖` (`dt = Float32(stepsize)`).
A trace stops when it leaves the box, reaches `maxsteps` points, or enters an
already visited cell. Seeding continues until `density · ∏ gridsize` cells are
visited.

Precision follows Makie exactly:
* positions are binary64 during tracing and stored as binary32
  (`Point{N, Float32}`); colours are `Float32(norm(f(x)))`;
* `dt` is binary32; `d·dt·f/‖f‖` is binary64 for a binary64 field and binary32
  for a field returning `Point2f` (`fieldF32`);
* the cell ranges are `LinRange`s in the box's element type (`limitsF32` for a
  `Rect2f`/`Rect3f` box; `Int` and `Float64` boxes behave identically), and cell
  lookup is Julia's `searchsortedlast` on those ranges.

The only deviation: a point exactly on the upper box face makes Julia index the
mask out of bounds (`BoundsError`); here it is counted in the last cell.

Loops are tail-recursive with `Float` accumulators; outputs are SoA buffers.
-/

namespace LeanPlot.Recipes.Algo.Stream

open LeanPlot.Num
open LeanPlot.Recipes.Algo.F32

/-- Streamplot parameters (Makie attribute defaults). -/
structure Options where
  /-- Cells per dimension; padded with its last entry to the plot dimension (Makie `to_ndim`). -/
  gridsize : Array Nat := #[32, 32, 32]
  /-- Euler step length (converted to `Float32`). -/
  stepsize : Float := 0.01
  /-- Maximum number of points per half streamline (including the seed). -/
  maxsteps : Nat := 500
  /-- Fraction of cells to visit (capped at 1). -/
  density : Float := 1.0
  /-- The box is a `Rect{N, Float32}` (ranges in binary32). -/
  limitsF32 : Bool := false
  /-- The field returns binary32 vectors (`Point2f`/`Point3f`). -/
  fieldF32 : Bool := false
  /-- Colour function of the field value; `none` is Makie's default `norm`
  (evaluated in the field's precision). -/
  colorFn : Option (Vec3 → Float) := none
  deriving Inhabited

/-- Output of `streamplot_impl`: arrow anchors and unit directions, the traced
line points (`NaN`-separated, two separators per seed), and per-arrow and
per-line-point colour values. All coordinates and colours are binary32 values. -/
structure Result where
  /-- Plot dimension (2 or 3). -/
  dim : Nat
  /-- Arrow positions (seed cell centres). -/
  arrowPos : Pts3
  /-- Unit field directions at the arrows. -/
  arrowDir : Pts3
  /-- Streamline points, each half-line preceded by a `NaN` point. -/
  linePoints : Pts3
  /-- Colour value per arrow. -/
  arrowColors : FloatArray
  /-- Colour value per line point (separators get their seed's colour). -/
  lineColors : FloatArray
  deriving Inhabited

namespace Result

/-- The 2D view of a 2D result (drops the zero z coordinates). -/
def arrowPos2 (r : Result) : Pts2 := r.arrowPos.xy
/-- Arrow directions as 2D vectors. -/
def arrowDir2 (r : Result) : Pts2 := r.arrowDir.xy
/-- Line points as 2D points. -/
def linePoints2 (r : Result) : Pts2 := r.linePoints.xy

/-- Push line points and arrow anchors through a transform (the Makie
`transform_func` hook Cartan uses to draw parameter-space streamlines on an
embedded surface); `NaN` separators stay `NaN`. Directions and colours are kept. -/
def mapPoints (r : Result) (f : Vec3 → Vec3) : Result :=
  let tr (p : Pts3) : Pts3 := Id.run do
    let mut xs : FloatArray := .empty
    let mut ys : FloatArray := .empty
    let mut zs : FloatArray := .empty
    for i in [0:p.size] do
      let v := p.get! i
      let w := if v.x.isNaN || v.y.isNaN || v.z.isNaN then (⟨nan, nan, nan⟩ : Vec3) else f v
      xs := xs.push w.x; ys := ys.push w.y; zs := zs.push w.z
    return Pts3.ofArrays xs ys zs
  { r with dim := 3, arrowPos := tr r.arrowPos, linePoints := tr r.linePoints }

end Result

/-- Julia `norm` of a `StaticVector` (StaticArrays `_norm`): `√(Σ xᵢ²)`, with
the overflow/underflow-safe rescaled evaluation when that is `0` or not finite.
Arithmetic in binary32 when `f32`. -/
def jnorm (f32 : Bool) (dim : Nat) (x y z : Float) : Float :=
  let sq (a : Float) : Float := rnd f32 (a * a)
  let s2 := rnd f32 (sq x + sq y)
  let s := if dim == 3 then rnd f32 (s2 + sq z) else s2
  let l := rnd f32 (Float.sqrt s)
  if 0 < l && l.isFinite then l else
  let m := jmax x.abs y.abs
  let m := if dim == 3 then jmax m z.abs else m
  if !m.isFinite then m else
  if m == 0 then 0 else
  let q (a : Float) : Float := sq (rnd f32 (a / m))
  let t2 := rnd f32 (q x + q y)
  let t := if dim == 3 then rnd f32 (t2 + q z) else t2
  rnd f32 (m * rnd f32 (Float.sqrt t))

/-- Quasi-random sequence coefficient base `φ_N` (Makie's tuple indexed by `N`). -/
def phi (dim : Nat) : Float := if dim == 3 then 1.2207440846057596 else 1.324717957244746

/-- Per-dimension cell range: Julia `LinRange(lo, hi, res + 1)` in the box's
element type. -/
structure Axis where
  /-- Lower bound (`minimum(limits)[i]`). -/
  lo : Float
  /-- Upper bound (`maximum(limits)[i] = origin + width`). -/
  hi : Float
  /-- Number of cells. -/
  res : Nat
  /-- `step(r) = (hi - lo) / res`. -/
  step : Float
  /-- Seed coefficient `φ^{-i}`. -/
  a : Float
  /-- `res` as a float. -/
  resF : Float
  deriving Inhabited

/-- Build the range for one dimension. -/
def mkAxis (f32 : Bool) (origin width : Float) (res : Nat) (a : Float) : Axis :=
  let lo := rnd f32 origin
  let hi := rnd f32 (lo + rnd f32 width)
  let res := max res 1
  ⟨lo, hi, res, rnd f32 (rnd f32 (hi - lo) / Num.ofInt res), a, Num.ofInt res⟩

/-- Julia `searchsortedlast(r::LinRange, x)` (1-based; `0` below, `res + 1` at or
above the top). The index arithmetic is exact in binary64 (indices are small
integers), so no `Int` is materialised. -/
def Axis.searchLast (ax : Axis) (f32 : Bool) (x : Float) : Nat :=
  if x < ax.lo then 0
  else if ax.step == 0 || !(x < ax.hi) then ax.res + 1
  else
    let n := roundEven ((x - ax.lo) / ax.step + fOne)
    let t := (n - fOne) / ax.resF
    let an := rnd f32 ((fOne - t) * ax.lo + t * ax.hi)
    (if x < an then n - fOne else n).toUInt64.toNat

/-- Cell index along one dimension for a traced point (clamped into `1..res`,
see the module note). -/
@[inline] def Axis.cell (ax : Axis) (f32 : Bool) (x : Float) : Nat :=
  min (max (ax.searchLast f32 x) 1) ax.res

/-- Seed cell along one dimension for sequence index `ind`:
`clamp(ceil(Int, ((0.5 + a·ind) % 1)·res), 1, res)`. -/
@[inline] def Axis.seed (ax : Axis) (ind : Nat) : Nat :=
  let v := fHalf + ax.a * ind.toUInt64.toFloat
  let frac := v - v.floor
  let j := (frac * ax.resF).ceil
  if j < fOne then 1 else if j > ax.resF then ax.res else j.toUInt64.toNat

/-- Cell centre `first(r) + (c - 0.5)·step(r)` (binary64). -/
@[inline] def Axis.center (ax : Axis) (c : Nat) : Float := ax.lo + (Num.ofInt c - fHalf) * ax.step

/-- `lo ≤ x ≤ hi` (GeometryBasics `in(::Point, ::Rect)` per coordinate). -/
@[inline] def Axis.contains (ax : Axis) (x : Float) : Bool := x ≤ ax.hi && x ≥ ax.lo

/-- Line-point buffers (coordinates and colours), threaded linearly. -/
structure Lines where
  lx : FloatArray
  ly : FloatArray
  lz : FloatArray
  lcol : FloatArray

/-- Output buffers, threaded linearly. -/
structure Buf where
  apx : FloatArray
  apy : FloatArray
  apz : FloatArray
  adx : FloatArray
  ady : FloatArray
  adz : FloatArray
  acol : FloatArray
  lines : Lines

/-- Empty buffers. -/
def Buf.empty : Buf := ⟨.empty, .empty, .empty, .empty, .empty, .empty, .empty, ⟨.empty, .empty, .empty, .empty⟩⟩

/-- Append a line point with its colour. -/
@[inline] def Lines.push (b : Lines) (x y z c : Float) : Lines :=
  ⟨b.lx.push (r32 x), b.ly.push (r32 y), b.lz.push (r32 z), b.lcol.push c⟩

/-- Static data of one streamplot run. -/
structure Ctx where
  dim : Nat
  ax : Axis
  ay : Axis
  az : Axis
  limitsF32 : Bool
  fieldF32 : Bool
  dt : Float
  maxsteps : Nat
  colorFn : Option (Vec3 → Float)

/-- Evaluate the field at a binary64 point, rounding to binary32 for a `Point2f`
field (and dropping z in 2D). -/
@[inline] def Ctx.eval (c : Ctx) (field : Vec3 → Vec3) (x y z : Float) : Vec3 :=
  let q := field ⟨x, y, z⟩
  let w := if c.dim == 3 then q.z else 0
  if c.fieldF32 then ⟨r32 q.x, r32 q.y, r32 w⟩ else if c.dim == 3 then q else ⟨q.x, q.y, 0⟩

/-- Colour value of a field vector (Makie `to_color(color(f(x)))`, binary32).
Takes components so the field value need not be materialised. -/
@[inline] def Ctx.color (c : Ctx) (u v w : Float) : Float :=
  match c.colorFn with
  | some g => r32 (g ⟨u, v, w⟩)
  | none => r32 (jnorm c.fieldF32 c.dim u v w)

/-- The point lies in the (closed) box. -/
@[inline] def Ctx.inBox (c : Ctx) (x y z : Float) : Bool :=
  c.ax.contains x && c.ay.contains y && (c.dim != 3 || c.az.contains z)

/-- Linear mask index of 1-based cell coordinates (column-major like Julia's `trues(res...)`). -/
@[inline] def Ctx.maskIdx (c : Ctx) (i j k : Nat) : Nat :=
  (i - 1) + c.ax.res * ((j - 1) + c.ay.res * (if c.dim == 3 then k - 1 else 0))

/-- Number of cells. -/
def Ctx.cells (c : Ctx) : Nat := c.ax.res * c.ay.res * (if c.dim == 3 then c.az.res else 1)

/-- One Euler coordinate update `x + ((d·dt)·p)/‖p‖` in the field's precision. -/
@[inline] def Ctx.stepCoord (c : Ctx) (dd x p pn : Float) : Float :=
  x + rnd c.fieldF32 (rnd c.fieldF32 (dd * p) / pn)

/-- Result of one half-line trace. -/
structure TraceOut where
  mask : ByteArray
  np : Nat
  lines : Lines

/-- Trace one half streamline from the current point (tail-recursive, with the
line buffers as separate arguments so a step allocates only the field value).
Specialised on the field, so a known field function is inlined. -/
@[specialize] def trace (c : Ctx) (field : Vec3 → Vec3) (dd : Float) (fuel : Nat) (x y z : Float) (ci cj ck nlp : Nat)
    (mask : ByteArray) (np : Nat) (lx ly lz lc : FloatArray) : TraceOut :=
  match fuel with
  | 0 => ⟨mask, np, ⟨lx, ly, lz, lc⟩⟩
  | fuel + 1 =>
    if !(c.inBox x y z && nlp < c.maxsteps) then ⟨mask, np, ⟨lx, ly, lz, lc⟩⟩ else
    let q := c.eval field x y z
    let pn := jnorm c.fieldF32 c.dim q.x q.y q.z
    let x' := c.stepCoord dd x q.x pn
    let y' := c.stepCoord dd y q.y pn
    let z' := if c.dim == 3 then c.stepCoord dd z q.z pn else z
    if !c.inBox x' y' z' then ⟨mask, np, ⟨lx, ly, lz, lc⟩⟩ else
    let i := c.ax.cell c.limitsF32 x'
    let j := c.ay.cell c.limitsF32 y'
    let k := if c.dim == 3 then c.az.cell c.limitsF32 z' else 1
    let col := c.color q.x q.y q.z
    if i != ci || j != cj || k != ck then
      let idx := c.maskIdx i j k
      if mask.get! idx == 0 then ⟨mask, np, ⟨lx, ly, lz, lc⟩⟩ else
      trace c field dd fuel x' y' z' i j k (nlp + 1) (mask.set! idx 0) (np + 1)
        (lx.push (r32 x')) (ly.push (r32 y')) (lz.push (r32 z')) (lc.push col)
    else
      trace c field dd fuel x' y' z' ci cj ck (nlp + 1) mask np
        (lx.push (r32 x')) (ly.push (r32 y')) (lz.push (r32 z')) (lc.push col)

/-- The seeding loop of `streamplot_impl`. -/
@[specialize] def seedLoop (c : Ctx) (field : Vec3 → Vec3) (target : Float) (fuel : Nat) (ind np : Nat) (mask : ByteArray) (b : Buf) : Buf :=
  match fuel with
  | 0 => b
  | fuel + 1 =>
    if !(np.toUInt64.toFloat < target) then b else
    let i := c.ax.seed ind
    let j := c.ay.seed ind
    let k := if c.dim == 3 then c.az.seed ind else 1
    let idx := c.maskIdx i j k
    if mask.get! idx == 0 then seedLoop c field target fuel (ind + 1) np mask b else
    let x0 := c.ax.center i
    let y0 := c.ay.center j
    let z0 := if c.dim == 3 then c.az.center k else 0
    let q := c.eval field x0 y0 z0
    let pn := jnorm c.fieldF32 c.dim q.x q.y q.z
    let col := c.color q.x q.y q.z
    let ⟨apx, apy, apz, adx, ady, adz, acol, lines⟩ := b
    let apx := apx.push (r32 x0)
    let apy := apy.push (r32 y0)
    let apz := apz.push (r32 z0)
    let adx := adx.push (r32 (rnd c.fieldF32 (q.x / pn)))
    let ady := ady.push (r32 (rnd c.fieldF32 (q.y / pn)))
    let adz := adz.push (r32 (rnd c.fieldF32 (q.z / pn)))
    let acol := acol.push col
    let mask := mask.set! idx 0
    let np := np + 1
    let dt := c.dt
    let lines := (lines.push nan nan nan col).push x0 y0 z0 col
    let ⟨mask, np, ⟨lx, ly, lz, lc⟩⟩ :=
      trace c field (-dt) (c.maxsteps + 1) x0 y0 z0 i j k 1 mask np lines.lx lines.ly lines.lz lines.lcol
    let lines : Lines := (Lines.push ⟨lx, ly, lz, lc⟩ nan nan nan col).push x0 y0 z0 col
    let ⟨mask, np, lines⟩ :=
      trace c field dt (c.maxsteps + 1) x0 y0 z0 i j k 1 mask np lines.lx lines.ly lines.lz lines.lcol
    seedLoop c field target fuel (ind + 1) np mask ⟨apx, apy, apz, adx, ady, adz, acol, lines⟩

/-- Makie `to_ndim(Vec{N, Int}, gridsize, last(gridsize))`. -/
def resolution (gs : Array Nat) (n : Nat) : Array Nat :=
  let last := gs.back?.getD 1
  (Array.range n).map fun i => gs.getD i last

/-- Run `streamplot_impl` for a field `f` on the box `origin + [0, widths]`
(dimension `dim ∈ {2, 3}`; for `dim = 2` the z components are ignored). -/
@[specialize] def run (dim : Nat) (f : Vec3 → Vec3) (origin widths : Vec3) (o : Options) : Result :=
  let res := resolution o.gridsize dim
  let a := phi dim
  let ax := mkAxis o.limitsF32 origin.x widths.x (res.getD 0 1) (powInt a (-1))
  let ay := mkAxis o.limitsF32 origin.y widths.y (res.getD 1 1) (powInt a (-2))
  let az := mkAxis o.limitsF32 origin.z widths.z (res.getD 2 1) (powInt a (-3))
  let c : Ctx := ⟨dim, ax, ay, az, o.limitsF32, o.fieldF32, r32 o.stepsize, o.maxsteps, o.colorFn⟩
  let ncells := c.cells
  let target := Num.ofInt ncells * jmin 1 o.density
  let mask := ByteArray.mk (Array.replicate ncells 1)
  -- the Kronecker sequence visits every cell; the fuel only guards pathological inputs
  let b := seedLoop c f target (1024 * ncells + 1000000) 0 0 mask Buf.empty
  { dim := dim
    arrowPos := Pts3.ofArrays b.apx b.apy b.apz
    arrowDir := Pts3.ofArrays b.adx b.ady b.adz
    linePoints := Pts3.ofArrays b.lines.lx b.lines.ly b.lines.lz
    arrowColors := b.acol
    lineColors := b.lines.lcol }

/-- 2D streamplot of `f` over the box `[x0, x0 + w] × [y0, y0 + h]` (Makie
`streamplot(f, x0..x0+w, y0..y0+h)`; for a `Rect2f` box set `limitsF32`). -/
@[specialize] def streamplot2 (f : Vec2 → Vec2) (x0 y0 w h : Float) (o : Options := {}) : Result :=
  run 2 (fun p => let q := f ⟨p.x, p.y⟩; ⟨q.x, q.y, 0⟩) ⟨x0, y0, 0⟩ ⟨w, h, 0⟩ o

/-- 3D streamplot of `f` over the box `origin + [0, widths]`. -/
@[specialize] def streamplot3 (f : Vec3 → Vec3) (origin widths : Vec3) (o : Options := {}) : Result :=
  run 3 f origin widths o

/-- Makie's automatic 3D arrow (cone) size `0.2·min(widths)/min(gridsize)`. -/
def arrowSize3 (widths : Vec3) (gridsize : Array Nat) : Float :=
  let mw := jmin widths.x (jmin widths.y widths.z)
  let mg := gridsize.foldl (fun m g => min m g) (gridsize.getD 0 1)
  0.2 * mw / Num.ofInt mg

/-- Screen-space rotation of the 2D arrow markers (`:utriangle`): for each unit
direction `(dx, dy)` and the data-to-pixel scale `(sx, sy)` of a linear axis,
`atan(sy·dy, sx·dx) - π/2` (radians, counter-clockwise, y up). -/
def arrowRotations2 (dirs : Pts2) (sx sy : Float) : FloatArray :=
  let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
    if h : i < dirs.size then
      let d := dirs.get i h
      go (i + 1) (acc.push (Float.atan2 (sy * d.y) (sx * d.x) - fHalf * Num.pi))
    else acc
  termination_by dirs.size - i
  go 0 (FloatArray.emptyWithCapacity dirs.size)

end LeanPlot.Recipes.Algo.Stream
