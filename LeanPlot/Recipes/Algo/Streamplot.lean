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
  deriving Inhabited

/-- Build the range for one dimension. -/
def mkAxis (f32 : Bool) (origin width : Float) (res : Nat) (a : Float) : Axis :=
  let lo := rnd f32 origin
  let hi := rnd f32 (lo + rnd f32 width)
  let res := max res 1
  ⟨lo, hi, res, rnd f32 (rnd f32 (hi - lo) / Num.ofInt res), a⟩

/-- Julia `searchsortedlast(r::LinRange, x)` (1-based; `0` below, `res + 1` at or
above the top). -/
def Axis.searchLast (ax : Axis) (f32 : Bool) (x : Float) : Nat :=
  if x < ax.lo then 0
  else if ax.step == 0 || !(x < ax.hi) then ax.res + 1
  else
    let n := roundInt ((x - ax.lo) / ax.step + 1)
    let t := Num.ofInt (n - 1) / Num.ofInt ax.res
    let an := rnd f32 ((1 - t) * ax.lo + t * ax.hi)
    (if x < an then n - 1 else n).toNat

/-- Cell index along one dimension for a traced point (clamped into `1..res`,
see the module note). -/
@[inline] def Axis.cell (ax : Axis) (f32 : Bool) (x : Float) : Nat :=
  min (max (ax.searchLast f32 x) 1) ax.res

/-- Seed cell along one dimension for sequence index `ind`:
`clamp(ceil(Int, ((0.5 + a·ind) % 1)·res), 1, res)`. -/
@[inline] def Axis.seed (ax : Axis) (ind : Nat) : Nat :=
  let v := fHalf + ax.a * Num.ofInt ind
  let frac := v - v.floor
  let j := ceilInt (frac * Num.ofInt ax.res)
  (max 1 (min j ax.res)).toNat

/-- Cell centre `first(r) + (c - 0.5)·step(r)` (binary64). -/
@[inline] def Axis.center (ax : Axis) (c : Nat) : Float := ax.lo + (Num.ofInt c - fHalf) * ax.step

/-- `lo ≤ x ≤ hi` (GeometryBasics `in(::Point, ::Rect)` per coordinate). -/
@[inline] def Axis.contains (ax : Axis) (x : Float) : Bool := x ≤ ax.hi && x ≥ ax.lo

/-- Output buffers, threaded linearly. -/
structure Buf where
  apx : FloatArray
  apy : FloatArray
  apz : FloatArray
  adx : FloatArray
  ady : FloatArray
  adz : FloatArray
  lx : FloatArray
  ly : FloatArray
  lz : FloatArray
  acol : FloatArray
  lcol : FloatArray

/-- Empty buffers. -/
def Buf.empty : Buf := ⟨.empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty⟩

/-- Append a line point with its colour. -/
@[inline] def Buf.pushLine (b : Buf) (x y z c : Float) : Buf :=
  { b with lx := b.lx.push (r32 x), ly := b.ly.push (r32 y), lz := b.lz.push (r32 z), lcol := b.lcol.push c }

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
  field : Vec3 → Float × Float × Float
  color : Float → Float → Float → Float

/-- Evaluate the field at a binary64 point, rounding to binary32 for a `Point2f` field. -/
@[inline] def Ctx.eval (c : Ctx) (x y z : Float) : Float × Float × Float :=
  let (u, v, w) := c.field ⟨x, y, z⟩
  let w := if c.dim == 3 then w else 0
  if c.fieldF32 then (r32 u, r32 v, r32 w) else (u, v, w)

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

/-- Trace one half streamline from the current point. Returns the updated mask,
visited-cell count and buffers. -/
def trace (c : Ctx) (dd : Float) (fuel : Nat) (x y z : Float) (ci cj ck nlp : Nat)
    (mask : ByteArray) (np : Nat) (b : Buf) : ByteArray × Nat × Buf :=
  match fuel with
  | 0 => (mask, np, b)
  | fuel + 1 =>
    if !(c.inBox x y z && nlp < c.maxsteps) then (mask, np, b) else
    let (u, v, w) := c.eval x y z
    let pn := jnorm c.fieldF32 c.dim u v w
    let x' := c.stepCoord dd x u pn
    let y' := c.stepCoord dd y v pn
    let z' := if c.dim == 3 then c.stepCoord dd z w pn else z
    if !c.inBox x' y' z' then (mask, np, b) else
    let i := c.ax.cell c.limitsF32 x'
    let j := c.ay.cell c.limitsF32 y'
    let k := if c.dim == 3 then c.az.cell c.limitsF32 z' else 1
    let col := r32 (c.color u v w)
    if i != ci || j != cj || k != ck then
      let idx := c.maskIdx i j k
      if mask.get! idx == 0 then (mask, np, b) else
      let mask := mask.set! idx 0
      trace c dd fuel x' y' z' i j k (nlp + 1) mask (np + 1) (b.pushLine x' y' z' col)
    else
      trace c dd fuel x' y' z' ci cj ck (nlp + 1) mask np (b.pushLine x' y' z' col)

/-- The seeding loop of `streamplot_impl`. -/
def seedLoop (c : Ctx) (target : Float) (fuel : Nat) (ind np : Nat) (mask : ByteArray) (b : Buf) : Buf :=
  match fuel with
  | 0 => b
  | fuel + 1 =>
    if !(Num.ofInt np < target) then b else
    let i := c.ax.seed ind
    let j := c.ay.seed ind
    let k := if c.dim == 3 then c.az.seed ind else 1
    let idx := c.maskIdx i j k
    if mask.get! idx == 0 then seedLoop c target fuel (ind + 1) np mask b else
    let x0 := c.ax.center i
    let y0 := c.ay.center j
    let z0 := if c.dim == 3 then c.az.center k else 0
    let (u, v, w) := c.eval x0 y0 z0
    let pn := jnorm c.fieldF32 c.dim u v w
    let col := r32 (c.color u v w)
    let b := { b with
      apx := b.apx.push (r32 x0), apy := b.apy.push (r32 y0), apz := b.apz.push (r32 z0),
      adx := b.adx.push (r32 (rnd c.fieldF32 (u / pn))), ady := b.ady.push (r32 (rnd c.fieldF32 (v / pn))),
      adz := b.adz.push (r32 (rnd c.fieldF32 (w / pn))), acol := b.acol.push col }
    let mask := mask.set! idx 0
    let np := np + 1
    let dt := c.dt
    let b := (b.pushLine nan nan nan col).pushLine x0 y0 z0 col
    let (mask, np, b) := trace c (-dt) (c.maxsteps + 1) x0 y0 z0 i j k 1 mask np b
    let b := (b.pushLine nan nan nan col).pushLine x0 y0 z0 col
    let (mask, np, b) := trace c dt (c.maxsteps + 1) x0 y0 z0 i j k 1 mask np b
    seedLoop c target fuel (ind + 1) np mask b

/-- Makie `to_ndim(Vec{N, Int}, gridsize, last(gridsize))`. -/
def resolution (gs : Array Nat) (n : Nat) : Array Nat :=
  let last := gs.back?.getD 1
  (Array.range n).map fun i => gs.getD i last

/-- Run `streamplot_impl` for a field `f` on the box `origin + [0, widths]`
(dimension `dim ∈ {2, 3}`; for `dim = 2` the z components are ignored). -/
def run (dim : Nat) (f : Vec3 → Float × Float × Float) (origin widths : Vec3) (o : Options) : Result :=
  let res := resolution o.gridsize dim
  let a := phi dim
  let ax := mkAxis o.limitsF32 origin.x widths.x (res.getD 0 1) (powInt a (-1))
  let ay := mkAxis o.limitsF32 origin.y widths.y (res.getD 1 1) (powInt a (-2))
  let az := mkAxis o.limitsF32 origin.z widths.z (res.getD 2 1) (powInt a (-3))
  let colorF : Float → Float → Float → Float := match o.colorFn with
    | some g => fun u v w => g ⟨u, v, w⟩
    | none => fun u v w => jnorm o.fieldF32 dim u v w
  let c : Ctx := ⟨dim, ax, ay, az, o.limitsF32, o.fieldF32, r32 o.stepsize, o.maxsteps, f, colorF⟩
  let ncells := c.cells
  let target := Num.ofInt ncells * jmin 1 o.density
  let mask := ByteArray.mk (Array.replicate ncells 1)
  -- the Kronecker sequence visits every cell; the fuel only guards pathological inputs
  let b := seedLoop c target (1024 * ncells + 1000000) 0 0 mask Buf.empty
  { dim := dim
    arrowPos := Pts3.ofArrays b.apx b.apy b.apz
    arrowDir := Pts3.ofArrays b.adx b.ady b.adz
    linePoints := Pts3.ofArrays b.lx b.ly b.lz
    arrowColors := b.acol
    lineColors := b.lcol }

/-- 2D streamplot of `f` over the box `[x0, x0 + w] × [y0, y0 + h]` (Makie
`streamplot(f, x0..x0+w, y0..y0+h)`; for a `Rect2f` box set `limitsF32`). -/
def streamplot2 (f : Vec2 → Vec2) (x0 y0 w h : Float) (o : Options := {}) : Result :=
  run 2 (fun p => let q := f ⟨p.x, p.y⟩; (q.x, q.y, 0)) ⟨x0, y0, 0⟩ ⟨w, h, 0⟩ o

/-- 3D streamplot of `f` over the box `origin + [0, widths]`. -/
def streamplot3 (f : Vec3 → Vec3) (origin widths : Vec3) (o : Options := {}) : Result :=
  run 3 (fun p => let q := f p; (q.x, q.y, q.z)) origin widths o

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
