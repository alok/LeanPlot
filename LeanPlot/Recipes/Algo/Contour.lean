import LeanPlot.Core.Data
import LeanPlot.Core.Colormap
import LeanPlot.Recipes.Algo.Levels

/-!
# Contour lines (port of Contour.jl marching squares + Makie `contourlines`)

`Contour.jl` 0.6 (`src/Contour.jl`, `src/interpolate.jl`):

* every cell `(i, j)` of the grid gets a marching-squares case from its corners
  `z(i,j), z(i+1,j), z(i+1,j+1), z(i,j+1)` (bit set when `z > h`);
* the two saddle cases are disambiguated by the cell mean `0.25·Σz ≥ h`;
* lines are chased cell to cell (entry edge → exit edge), first forward from a
  start cell and then backward from the start cell's entry edge; a line that
  returns to its start edge is closed (its first point is repeated at the end);
* edge crossings are linear interpolations
  `y₀ + ((y₁ - y₀)·(h - z₀))/(z₁ - z₀)`.

Contour.jl picks start cells in `Dict` order, so Makie brings every traced line
into `canonical_line_order` (closed cycles rotated/reversed to their smallest
vertex sequence, open lines reversed if their last vertex is smaller). This port
scans start cells in column-major order and applies the same canonicalisation,
so each line is identical to Makie's; only the order of lines within a level
can differ from Julia's hash order.

Makie converts contour data to binary32 (`el32convert`) and traces in binary32
(`Contour.jl` with `Float32` element type); `f32 = true` reproduces that bit for
bit. Curvilinear grids (matrix `x`, `y`) are supported like Contour.jl.
-/

namespace LeanPlot.Recipes.Algo.Contour

open LeanPlot.Num
open LeanPlot.Recipes.Algo
open LeanPlot.Recipes.Algo.F32

/-- Grid coordinates: rectilinear axes, or curvilinear coordinate grids
(`x(i,j)`, `y(i,j)`, both `nx × ny`, column-major). -/
inductive Coords where
  /-- `x[i]` for `i < nx` and `y[j]` for `j < ny`. -/
  | rect (xs ys : FloatArray)
  /-- Point `(i, j)` is `(xg[i + nx*j], yg[i + nx*j])`. -/
  | curvilinear (xg yg : FloatArray)
  deriving Inhabited

namespace Coords
/-- Round every coordinate to binary32. -/
def round32 : Coords → Coords
  | .rect xs ys => .rect (roundArray xs) (roundArray ys)
  | .curvilinear xg yg => .curvilinear (roundArray xg) (roundArray yg)
end Coords

/-! ## Cell types (Contour.jl encoding) -/

/-- Edge bits `N, S, E, W`. -/
def eN : UInt8 := 1
/-- South edge. -/
def eS : UInt8 := 2
/-- East edge. -/
def eE : UInt8 := 4
/-- West edge. -/
def eW : UInt8 := 8
/-- Saddle cell with crossings `NW` and `SE`. -/
def cNWSE : UInt8 := 0x19
/-- Saddle cell with crossings `NE` and `SW`. -/
def cNESW : UInt8 := 0x15

/-- Contour.jl `edge_LUT` for the non-ambiguous cases `1…14`. -/
def edgeLUT (case : UInt8) : UInt8 :=
  match case.toNat with
  | 1 => 10 | 2 => 6 | 3 => 12 | 4 => 5 | 6 => 3 | 7 => 9 | 8 => 9
  | 9 => 3 | 11 => 5 | 12 => 12 | 13 => 6 | 14 => 10 | _ => 0

/-- `get_next_edge!`: exit edge for an entry edge, and the cell value left behind
(the other crossing of a saddle, otherwise `0`). -/
@[inline] def nextEdge (cell entry : UInt8) : UInt8 × UInt8 :=
  if cell == cNWSE then
    if entry == eN || entry == eW then ((9 : UInt8) ^^^ entry, 6) else ((6 : UInt8) ^^^ entry, 9)
  else if cell == cNESW then
    if entry == eN || entry == eE then ((5 : UInt8) ^^^ entry, 10) else ((10 : UInt8) ^^^ entry, 5)
  else (cell ^^^ entry, 0)

/-- `get_first_crossing` followed by the lowest set bit: the starting edge of a
trace beginning in a cell. -/
@[inline] def startEdge (cell : UInt8) : UInt8 :=
  let c := if cell == cNWSE then (9 : UInt8) else if cell == cNESW then 5 else cell
  if c &&& 1 != 0 then 1 else if c &&& 2 != 0 then 2 else if c &&& 4 != 0 then 4 else 8

/-- `advance_edge`: the neighbouring cell across an edge and the entry edge there. -/
@[inline] def advance (i j : Int) (edge : UInt8) : Int × Int × UInt8 :=
  if edge == eN then (i, j + 1, eS)
  else if edge == eS then (i, j - 1, eN)
  else if edge == eE then (i + 1, j, eW)
  else (i - 1, j, eE)

/-! ## Tracing -/

/-- Static data of one level. -/
structure Ctx where
  nx : Nat
  ny : Nat
  coords : Coords
  z : FloatArray
  h : Float
  f32 : Bool

namespace Ctx

/-- `z(i, j)`. -/
@[inline] def zAt (c : Ctx) (i j : Nat) : Float := c.z.get! (i + c.nx * j)

/-- Linear interpolation along the edge from vertex `(i0, j0)` to `(i1, j1)`. -/
@[inline] def lerpEdge (c : Ctx) (i0 j0 i1 j1 : Nat) : Float × Float :=
  let z0 := c.zAt i0 j0
  let frac (a b : Float) : Float := rnd c.f32 (a + rnd c.f32 (rnd c.f32 (rnd c.f32 (b - a) * rnd c.f32 (c.h - z0)) / rnd c.f32 (c.zAt i1 j1 - z0)))
  match c.coords with
  | .rect xs ys =>
    if i0 == i1 then (xs.get! i0, frac (ys.get! j0) (ys.get! j1))
    else (frac (xs.get! i0) (xs.get! i1), ys.get! j0)
  | .curvilinear xg yg =>
    let k0 := i0 + c.nx * j0
    let k1 := i1 + c.nx * j1
    (frac (xg.get! k0) (xg.get! k1), frac (yg.get! k0) (yg.get! k1))

/-- Contour.jl `interpolate`: the crossing on edge `edge` of cell `(i, j)`. -/
@[inline] def interp (c : Ctx) (i j : Nat) (edge : UInt8) : Float × Float :=
  if edge == eW then c.lerpEdge i j i (j + 1)
  else if edge == eE then c.lerpEdge (i + 1) j (i + 1) (j + 1)
  else if edge == eN then c.lerpEdge i (j + 1) (i + 1) (j + 1)
  else c.lerpEdge i j (i + 1) j

/-- Marching-squares cell type of the cell with corner values `z1 … z4`
(counter-clockwise from the lower left), Contour.jl `_get_case` + saddle rule. -/
@[inline] def cellType (f32 : Bool) (h z1 z2 z3 z4 : Float) : UInt8 :=
  let case : UInt8 := (if z1 > h then 1 else 0) ||| (if z2 > h then 2 else 0) |||
    (if z3 > h then 4 else 0) ||| (if z4 > h then 8 else 0)
  if case == 0 || case == 15 then 0
  else if case == 5 || case == 10 then
      let up := 0.25 * rnd f32 (rnd f32 (rnd f32 (z1 + z2) + z3) + z4) ≥ h
    if case == 5 then (if up then cNWSE else cNESW) else (if up then cNESW else cNWSE)
  else edgeLUT case

/-- `get_level_cells`: the marching-squares cell types (`0` = no crossing), in
column-major cell order. -/
def levelCells (c : Ctx) : ByteArray :=
  let cx := c.nx - 1
  let cy := c.ny - 1
  let z := c.z
  -- cell (i, j) with running vertex index `v = i + nx*j`
  let rec row (i v : Nat) (acc : ByteArray) : ByteArray :=
    if i < cx then
      let t := cellType c.f32 c.h (z.get! v) (z.get! (v + 1)) (z.get! (v + 1 + c.nx)) (z.get! (v + c.nx))
      row (i + 1) (v + 1) (acc.push t)
    else acc
  termination_by cx - i
  let rec rows (j : Nat) (acc : ByteArray) : ByteArray :=
    if j < cy then rows (j + 1) (row 0 (c.nx * j) acc) else acc
  termination_by cy - j
  rows 0 (ByteArray.emptyWithCapacity (cx * cy))

/-- Cell `(i, j)` lies in the cell range. -/
@[inline] def inRange (c : Ctx) (i j : Int) : Bool :=
  0 ≤ i && i < (c.nx : Int) - 1 && 0 ≤ j && j < (c.ny : Int) - 1

end Ctx

/-- A polyline under construction (two coordinate buffers). -/
structure Line where
  xs : FloatArray
  ys : FloatArray

namespace Line
/-- Append a point. -/
@[inline] def push (l : Line) (p : Float × Float) : Line := ⟨l.xs.push p.1, l.ys.push p.2⟩
/-- Number of points. -/
@[inline] def size (l : Line) : Nat := l.xs.size
/-- Reverse the point order. -/
def reverse (l : Line) : Line :=
  let n := l.size
  let rec go (k : Nat) (xs ys : FloatArray) : Line :=
    if k < n then go (k + 1) (xs.push (l.xs.get! (n - 1 - k))) (ys.push (l.ys.get! (n - 1 - k))) else ⟨xs, ys⟩
  termination_by n - k
  go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)
end Line

/-- `chase!`: follow a line from cell `(i, j)` entered through `entry` until it
leaves the grid or returns to its start edge. Returns the cells, the line and
the final cell index. -/
def chase (c : Ctx) (i0 j0 : Int) (loopback : UInt8) (fuel : Nat) (cells : ByteArray) (l : Line)
    (i j : Int) (entry : UInt8) : ByteArray × Line × Int × Int :=
  match fuel with
  | 0 => (cells, l, i, j)
  | fuel + 1 =>
    let k := i.toNat + (c.nx - 1) * j.toNat
    let (exit, rest) := nextEdge (cells.get! k) entry
    let cells := cells.set! k rest
    let l := l.push (c.interp i.toNat j.toNat exit)
    let (i', j', entry') := advance i j exit
    if (i' == i0 && j' == j0 && entry' == loopback) || !c.inRange i' j' then (cells, l, i', j')
    else chase c i0 j0 loopback fuel cells l i' j' entry'

/-- `trace_contour` for the line starting in cell `k`. -/
def traceOne (c : Ctx) (cells : ByteArray) (k : Nat) : ByteArray × Line :=
  let cx := c.nx - 1
  let i : Int := k % cx
  let j : Int := k / cx
  let e := startEdge (cells.get! k)
  let fuel := 2 * cells.size + 4
  let l : Line := (⟨.empty, .empty⟩ : Line).push (c.interp i.toNat j.toNat e)
  let (cells, l, ie, je) := chase c i j e fuel cells l i j e
  if ie == i && je == j then (cells, l) else
  let (ni, nj, ne) := advance i j e
  if c.inRange ni nj then
    let (cells, l, _, _) := chase c ni nj ne fuel cells l.reverse ni nj ne
    (cells, l)
  else (cells, l)

/-! ## Canonical line order (Makie `canonical_line_order`) -/

/-- Julia `isless` on floats (`-0.0 < 0.0`, NaN largest). -/
@[inline] def islessF (x y : Float) : Bool :=
  if x.isNaN || y.isNaN then !x.isNaN && y.isNaN
  else x < y || (x == y && signBit x && !signBit y)

/-- Julia `isequal` on floats. -/
@[inline] def isequalF (x y : Float) : Bool :=
  (x.isNaN && y.isNaN) || (x == y && signBit x == signBit y)

/-- Julia `isless` on 2-tuples (lexicographic). -/
@[inline] def islessP (ax ay bx by' : Float) : Bool :=
  islessF ax bx || (isequalF ax bx && islessF ay by')

/-- Julia `isequal` on 2-tuples. -/
@[inline] def isequalP (ax ay bx by' : Float) : Bool := isequalF ax bx && isequalF ay by'

/-- Julia `<` on 2-tuples (`==`/`<` per element). -/
@[inline] def ltP (ax ay bx by' : Float) : Bool := if ax == bx then ay < by' else ax < bx

/-- Bring a traced line into Makie's canonical order. -/
def canonical (l : Line) : Line :=
  let n := l.size
  let fx := l.xs.get! 0
  let fy := l.ys.get! 0
  let lx := l.xs.get! (n - 1)
  let ly := l.ys.get! (n - 1)
  if n > 2 && fx == lx && fy == ly then
    let m := n - 1
    -- element `t` of rotation `s` of the cycle (`rev`: of the reversed cycle)
    let idx (rev : Bool) (s t : Nat) : Nat := if rev then m - 1 - (s + t) % m else (s + t) % m
    -- smallest vertex under `isless`
    let rec smallest (k best : Nat) : Nat :=
      if k < m then
        smallest (k + 1) (if islessP (l.xs.get! k) (l.ys.get! k) (l.xs.get! best) (l.ys.get! best) then k else best)
      else best
    termination_by m - k
    let b := smallest 1 0
    let bx := l.xs.get! b
    let by' := l.ys.get! b
    -- `cmp(A, B) < 0` for two rotations
    let rec lessRot (ra : Bool) (sa : Nat) (rb : Bool) (sb : Nat) (t : Nat) : Bool :=
      if t < m then
        let ia := idx ra sa t
        let ib := idx rb sb t
        let ax := l.xs.get! ia
        let ay := l.ys.get! ia
        let bx := l.xs.get! ib
        let by' := l.ys.get! ib
        if isequalP ax ay bx by' then lessRot ra sa rb sb (t + 1) else islessP ax ay bx by'
      else false
    termination_by m - t
    -- candidates in Julia's generator order: forward rotations, then reversed ones
    let rec pick (q : Nat) (found : Bool) (br : Bool) (bs : Nat) : Bool × Nat :=
      if q < 2 * m then
        let rev := q ≥ m
        let s := if rev then q - m else q
        let v := idx rev s 0
        if l.xs.get! v == bx && l.ys.get! v == by' then
          if !found || lessRot rev s br bs 0 then pick (q + 1) true rev s else pick (q + 1) found br bs
        else pick (q + 1) found br bs
      else (br, bs)
    termination_by 2 * m - q
    let (rev, s) := pick 0 false false 0
    let rec build (t : Nat) (xs ys : FloatArray) : Line :=
      if t < m then
        let k := idx rev s t
        build (t + 1) (xs.push (l.xs.get! k)) (ys.push (l.ys.get! k))
      else
        let k := idx rev s 0
        ⟨xs.push (l.xs.get! k), ys.push (l.ys.get! k)⟩
    termination_by m - t
    build 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)
  else if n ≥ 1 && ltP lx ly fx fy then l.reverse else l

/-! ## Public API -/

/-- Contour.jl `contour(x, y, z, h)` with every line in canonical order. Lines are
listed in column-major order of their start cell. With `f32` the inputs must
already be binary32 values (see `contourLines`) and all arithmetic is binary32. -/
def traceLevel {nx ny : Nat} (coords : Coords) (g : Grid2 nx ny) (h : Float) (f32 : Bool := true) : Array Pts2 :=
  if nx < 2 || ny < 2 then #[] else
  let c : Ctx := ⟨nx, ny, coords, g.z, h, f32⟩
  let cells := c.levelCells
  -- every step either skips an empty cell or consumes at least one of the at
  -- most two crossings of a cell, so `3 · #cells` steps suffice
  let rec scan (fuel k : Nat) (cells : ByteArray) (acc : Array Pts2) : Array Pts2 :=
    match fuel with
    | 0 => acc
    | fuel + 1 =>
      if k < cells.size then
        if cells.get! k == 0 then scan fuel (k + 1) cells acc
        else
          let (cells, l) := traceOne c cells k
          let l := canonical l
          -- a saddle start cell may still hold its second crossing: rescan it
          scan fuel k cells (acc.push (Pts2.ofArrays l.xs l.ys))
      else acc
  scan (3 * cells.size + 1) 0 cells #[]

/-- Traced isolines of several levels (Makie `contourlines`). -/
structure Lines where
  /-- The level values. -/
  levels : FloatArray
  /-- Polylines with their level index (into `levels`). -/
  lines : Array (Nat × Pts2)
  deriving Inhabited

namespace Lines

/-- Makie's flat layout: all lines concatenated, each followed by a `NaN` point,
and per line `(level index, number of points + 1)` (`elements_per_segment`). -/
def flatten (ls : Lines) : Pts2 × Array (Nat × Nat) :=
  let n := ls.lines.foldl (fun a (_, p) => a + p.size + 1) 0
  let rec go (k : Nat) (xs ys : FloatArray) (segs : Array (Nat × Nat)) : Pts2 × Array (Nat × Nat) :=
    if h : k < ls.lines.size then
      let (lvl, p) := ls.lines[k]
      go (k + 1) ((appendFloats xs p.xs).push nan) ((appendFloats ys p.ys).push nan) (segs.push (lvl, p.size + 1))
    else (Pts2.ofArrays xs ys, segs)
  termination_by ls.lines.size - k
  go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n) #[]

/-- The level value of every line. -/
def lineLevels (ls : Lines) : FloatArray :=
  ⟨ls.lines.map fun (l, _) => ls.levels.get! l⟩

end Lines

/-- Trace every level. `f32` selects binary32 arithmetic (inputs must then be
binary32 values). -/
def contourLines {nx ny : Nat} (coords : Coords) (g : Grid2 nx ny) (levels : FloatArray) (f32 : Bool := true) : Lines :=
  let lines := (Array.range levels.size).foldl (init := #[]) fun acc k =>
    (traceLevel coords g (levels.get! k) f32).foldl (init := acc) fun acc p => acc.push (k, p)
  ⟨levels, lines⟩

/-- Round a grid's values to binary32 (Makie `el32convert`). -/
def roundGrid {nx ny : Nat} (g : Grid2 nx ny) : Grid2 nx ny := g.map r32

/-- Round every line point to binary32 (Makie's `Point2f` conversion). -/
def roundLines (ls : Lines) : Lines :=
  { ls with lines := ls.lines.map fun (k, p) => (k, Pts2.ofArrays (roundArray p.xs) (roundArray p.ys)) }

/-- Makie's `contour(x, y, z; levels)` recipe data: the traced lines exactly as
`contourlines` produces them, with `levels` the binary32 level values actually
traced (`convert(Vector{Float32}, zlevels)`). Rectilinear coordinates
are converted to binary32 and traced in binary32; curvilinear coordinate
matrices stay binary64 (only `z` and the levels are binary32), and the points
are rounded to `Point2f` at the end. -/
def makieContour {nx ny : Nat} (coords : Coords) (g : Grid2 nx ny) (spec : Levels.LevelSpec := .count 5) : Lines :=
  let g32 := roundGrid g
  let levels := roundArray (Levels.contourZLevels spec g32.z)
  match coords with
  | .rect .. => contourLines coords.round32 g32 levels true
  | .curvilinear .. => roundLines (contourLines coords g32 levels false)

/-- Makie's contour `computed_colorrange` for data range `(zmin, zmax)`
(binary32): the range itself, widened by `max(1, |zmin|)` on both sides when
degenerate (`isapprox`). -/
def colorRange (zmin zmax : Float) : Float × Float :=
  if !F32.isApprox32 zmin zmax then (zmin, zmax) else
  let delta := jmax 1 zmin.abs
  (F32.r32 (zmin - delta), F32.r32 (zmax + delta))

/-- Makie `interpolated_getindex(cmap, v, (lo, hi))` for binary32 `v`, `lo`, `hi`:
the normalisation, index and blend are all binary32 (Julia keeps `Float32`
arithmetic for a `Float32` value). -/
def lookup32 (cm : Colormap) (v lo hi : Float) : RGBA :=
  let i01 := clamp (r32 (r32 (v - lo) / r32 (hi - lo))) 0 1
  let n := cm.size
  let i1len := r32 (r32 (i01 * Num.ofInt ((n : Int) - 1)) + 1)
  let down := i1len.floor
  let up := i1len.ceil
  let dn := down.toUInt64.toNat - 1
  if down == up then cm.get dn else
  let t := r32 (i1len - down)
  let d := cm.get dn
  let u := cm.get (up.toUInt64.toNat - 1)
  let mix (a b : Float) : Float := r32 (r32 (a * r32 (1 - t)) + r32 (b * t))
  ⟨mix d.r u.r, mix d.g u.g, mix d.b u.b, mix d.a u.a⟩

/-- Makie `color_per_level(nothing, colormap, identity, colorrange, alpha, zlevels)`:
each level's colour, `interpolated_getindex(cmap, level, colorrange)`, with the
alpha multiplied by `alpha`. Automatic levels are binary32 (`f32Levels`, looked
up in binary32); explicit levels stay binary64 (looked up in binary64). -/
def levelColors (cm : Colormap) (levels : FloatArray) (lo hi : Float) (f32Levels : Bool := true)
    (alpha : Float := 1) : Array RGBA :=
  levels.toList.toArray.map fun l =>
    let c := if f32Levels then lookup32 cm l lo hi else cm.lookup l lo hi
    { c with a := r32 (c.a * alpha) }

/-- Makie `label_info`: the three points around the middle vertex of a line
(`mid = ceil(0.5·n)`, 1-based, clamped), used to place and orient a contour label. -/
def labelAnchors (p : Pts2) : Vec2 × Vec2 × Vec2 :=
  let n := p.size
  let mid := (n + 1) / 2
  let at1 (k : Nat) : Vec2 := p.get! (k - 1)
  (at1 (max 1 (mid - 1)), at1 mid, at1 (min (mid + 1) n))

end LeanPlot.Recipes.Algo.Contour
