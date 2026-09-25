import Std.Data.HashMap
import LeanPlot.Core.Data
import LeanPlot.Core.Colormap
import LeanPlot.Recipes.Algo.Levels

/-!
# Filled contours: isobands (port of the `isoband` C++ library) and Makie `contourf`

`Isoband.jl` wraps Claus Wilke's `isoband` library (`src/isoband.cpp`, the
revision built into `isoband_jll`). `isobands` below ports its `isobander`:

* every grid value is ternarised against the band `[lo, hi)`
  (`0` below, `1` inside, `2` at or above `hi`); cells with a non-finite corner
  are skipped;
* each of the 81 cell cases emits one or two *elementary polygons* (clockwise)
  over abstract grid points: grid vertices and low/high crossings on horizontal
  and vertical edges; the eight saddle cases are resolved by the cell's central
  value;
* elementary polygons are merged through shared points (`poly_merge`, with
  "alternative" connections where two polygons touch at one grid point), and
  the resulting rings are collected by following `next` links.

The case table was extracted mechanically from `isoband.cpp`. Crossing
coordinates use the library's `x0 + ((v - z0)/(z1 - z0))·(x1 - x0)` (the
arm64 build does not fuse it). The C++ collects rings in `unordered_map` order,
so this port collects them in grid-point order: ring vertex sequences agree up
to the starting vertex, and the order of rings within a band can differ.

`groupPolys` ports Makie's `_group_polys` (rings → polygons with holes, by
PolygonOps' Hao–Sun point-in-polygon test in binary32) and `makieContourf` the
data path of Makie's `contourf` recipe (binary32 data and levels, binary64
isobands, `Point2f` output, band-centre colours).
-/

namespace LeanPlot.Recipes.Algo.Isoband

open LeanPlot.Num
open LeanPlot.Recipes.Algo
open LeanPlot.Recipes.Algo.F32

/-- One cell case of the isoband table: `kind` 0 (empty), 1 (plain), 2 (saddle
split by `vc < lo`), 3 (saddle split by `vc ≥ hi`), 4 (8-sided saddle). The
branches hold elementary polygons; each point is coded
`type·4 + dr·2 + dc` with `type` ∈ {grid, h-lo, h-hi, v-lo, v-hi}. -/
structure CaseEntry where
  /-- Branch structure of the case. -/
  kind : Nat
  /-- Plain case, or the `vc < lo` / `vc ≥ hi` branch. -/
  b0 : Array (Array Nat)
  /-- `else` branch (kinds 2, 3) or the `vc ≥ hi` branch (kind 4). -/
  b1 : Array (Array Nat)
  /-- `else` branch of kind 4. -/
  b2 : Array (Array Nat)
  deriving Inhabited

/-- The 81 cases, indexed by `27·t(r,c) + 9·t(r,c+1) + 3·t(r+1,c+1) + t(r+1,c)`. -/
def caseTable : Array CaseEntry := #[
  ⟨0, #[], #[], #[]⟩,
  ⟨1, #[#[12, 6, 2]], #[], #[]⟩,
  ⟨1, #[#[12, 6, 10, 16]], #[], #[]⟩,
  ⟨1, #[#[13, 3, 6]], #[], #[]⟩,
  ⟨1, #[#[12, 13, 3, 2]], #[], #[]⟩,
  ⟨1, #[#[12, 13, 3, 10, 16]], #[], #[]⟩,
  ⟨1, #[#[6, 13, 17, 10]], #[], #[]⟩,
  ⟨1, #[#[2, 12, 13, 17, 10]], #[], #[]⟩,
  ⟨1, #[#[12, 13, 17, 16]], #[], #[]⟩,
  ⟨1, #[#[4, 1, 13]], #[], #[]⟩,
  ⟨2, #[#[2, 12, 6], #[1, 13, 4]], #[#[2, 12, 4, 1, 13, 6]], #[]⟩,
  ⟨2, #[#[1, 13, 4], #[12, 6, 10, 16]], #[#[1, 13, 6, 10, 16, 12, 4]], #[]⟩,
  ⟨1, #[#[4, 1, 3, 6]], #[], #[]⟩,
  ⟨1, #[#[2, 12, 4, 1, 3]], #[], #[]⟩,
  ⟨1, #[#[1, 3, 10, 16, 12, 4]], #[], #[]⟩,
  ⟨1, #[#[1, 17, 10, 6, 4]], #[], #[]⟩,
  ⟨1, #[#[1, 17, 10, 2, 12, 4]], #[], #[]⟩,
  ⟨1, #[#[1, 17, 16, 12, 4]], #[], #[]⟩,
  ⟨1, #[#[13, 4, 8, 17]], #[], #[]⟩,
  ⟨2, #[#[2, 12, 6], #[13, 4, 8, 17]], #[#[2, 12, 4, 8, 17, 13, 6]], #[]⟩,
  ⟨4, #[#[12, 6, 10, 16], #[13, 4, 8, 17]], #[#[12, 4, 8, 16], #[13, 6, 10, 17]], #[#[12, 4, 8, 17, 13, 6, 10, 16]]⟩,
  ⟨1, #[#[3, 6, 4, 8, 17]], #[], #[]⟩,
  ⟨1, #[#[2, 12, 4, 8, 17, 3]], #[], #[]⟩,
  ⟨3, #[#[3, 10, 17], #[8, 16, 12, 4]], #[#[3, 10, 16, 12, 4, 8, 17]], #[]⟩,
  ⟨1, #[#[4, 8, 10, 6]], #[], #[]⟩,
  ⟨1, #[#[2, 12, 4, 8, 10]], #[], #[]⟩,
  ⟨1, #[#[8, 16, 12, 4]], #[], #[]⟩,
  ⟨1, #[#[12, 0, 4]], #[], #[]⟩,
  ⟨1, #[#[4, 6, 2, 0]], #[], #[]⟩,
  ⟨1, #[#[0, 4, 6, 10, 16]], #[], #[]⟩,
  ⟨2, #[#[0, 4, 12], #[3, 6, 13]], #[#[0, 4, 13, 3, 6, 12]], #[]⟩,
  ⟨1, #[#[0, 4, 13, 3, 2]], #[], #[]⟩,
  ⟨1, #[#[0, 4, 13, 3, 10, 16]], #[], #[]⟩,
  ⟨2, #[#[0, 4, 12], #[6, 13, 17, 10]], #[#[0, 4, 13, 17, 10, 6, 12]], #[]⟩,
  ⟨1, #[#[0, 4, 13, 17, 10, 2]], #[], #[]⟩,
  ⟨1, #[#[0, 4, 13, 17, 16]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 13, 12]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 13, 6, 2]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 13, 6, 10, 16]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 3, 6, 12]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 3, 2]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 3, 10, 16]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 17, 10, 6, 12]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 17, 10, 2]], #[], #[]⟩,
  ⟨1, #[#[0, 1, 17, 16]], #[], #[]⟩,
  ⟨1, #[#[0, 8, 17, 13, 12]], #[], #[]⟩,
  ⟨1, #[#[0, 8, 17, 13, 6, 2]], #[], #[]⟩,
  ⟨3, #[#[0, 8, 16], #[10, 17, 13, 6]], #[#[0, 8, 17, 13, 6, 10, 16]], #[]⟩,
  ⟨1, #[#[0, 8, 17, 3, 6, 12]], #[], #[]⟩,
  ⟨1, #[#[0, 8, 17, 3, 2]], #[], #[]⟩,
  ⟨3, #[#[0, 8, 16], #[3, 10, 17]], #[#[0, 8, 17, 3, 10, 16]], #[]⟩,
  ⟨1, #[#[0, 8, 10, 6, 12]], #[], #[]⟩,
  ⟨1, #[#[8, 10, 2, 0]], #[], #[]⟩,
  ⟨1, #[#[16, 0, 8]], #[], #[]⟩,
  ⟨1, #[#[4, 12, 16, 8]], #[], #[]⟩,
  ⟨1, #[#[2, 16, 8, 4, 6]], #[], #[]⟩,
  ⟨1, #[#[8, 4, 6, 10]], #[], #[]⟩,
  ⟨2, #[#[3, 6, 13], #[4, 12, 16, 8]], #[#[3, 6, 12, 16, 8, 4, 13]], #[]⟩,
  ⟨1, #[#[2, 16, 8, 4, 13, 3]], #[], #[]⟩,
  ⟨1, #[#[3, 10, 8, 4, 13]], #[], #[]⟩,
  ⟨4, #[#[16, 8, 4, 12], #[17, 10, 6, 13]], #[#[16, 10, 6, 12], #[17, 8, 4, 13]], #[#[16, 8, 4, 13, 17, 10, 6, 12]]⟩,
  ⟨3, #[#[2, 16, 10], #[17, 8, 4, 13]], #[#[2, 16, 8, 4, 13, 17, 10]], #[]⟩,
  ⟨1, #[#[17, 8, 4, 13]], #[], #[]⟩,
  ⟨1, #[#[1, 13, 12, 16, 8]], #[], #[]⟩,
  ⟨1, #[#[2, 16, 8, 1, 13, 6]], #[], #[]⟩,
  ⟨1, #[#[1, 13, 6, 10, 8]], #[], #[]⟩,
  ⟨1, #[#[1, 3, 6, 12, 16, 8]], #[], #[]⟩,
  ⟨1, #[#[2, 16, 8, 1, 3]], #[], #[]⟩,
  ⟨1, #[#[8, 1, 3, 10]], #[], #[]⟩,
  ⟨3, #[#[1, 17, 8], #[16, 10, 6, 12]], #[#[1, 17, 10, 6, 12, 16, 8]], #[]⟩,
  ⟨3, #[#[2, 16, 10], #[1, 17, 8]], #[#[2, 16, 8, 1, 17, 10]], #[]⟩,
  ⟨1, #[#[8, 1, 17]], #[], #[]⟩,
  ⟨1, #[#[16, 17, 13, 12]], #[], #[]⟩,
  ⟨1, #[#[2, 16, 17, 13, 6]], #[], #[]⟩,
  ⟨1, #[#[10, 17, 13, 6]], #[], #[]⟩,
  ⟨1, #[#[16, 17, 3, 6, 12]], #[], #[]⟩,
  ⟨1, #[#[16, 17, 3, 2]], #[], #[]⟩,
  ⟨1, #[#[17, 3, 10]], #[], #[]⟩,
  ⟨1, #[#[16, 10, 6, 12]], #[], #[]⟩,
  ⟨1, #[#[16, 10, 2]], #[], #[]⟩,
  ⟨0, #[], #[], #[]⟩
]

/-! ## Merging elementary polygons -/

/-- Missing point (C++ default `grid_point(-1, -1)`). -/
def noPoint : UInt64 := 0xFFFFFFFFFFFFFFFF

/-- `point_connect`: neighbours of a grid point in its polygon, plus the
alternative pair when two polygons share the point. -/
structure PC where
  prev : UInt64 := noPoint
  next : UInt64 := noPoint
  prev2 : UInt64 := noPoint
  next2 : UInt64 := noPoint
  alt : Bool := false
  collected : Bool := false
  collected2 : Bool := false
  deriving Inhabited

/-- The polygon grid (`unordered_map<grid_point, point_connect>`). -/
abbrev PGrid := Std.HashMap UInt64 PC

/-- `poly_merge`: merge one elementary polygon (grid-point keys, clockwise). -/
def polyMerge (grid : PGrid) (poly : Array UInt64) : PGrid :=
  let n := poly.size
  let conns : Array (PC × Bool) := (Array.range n).map fun i =>
    let p := poly[i]!
    let pc0 : PC := { next := poly[(i + 1) % n]!, prev := poly[(i + n - 1) % n]! }
    match grid.get? p with
    | none => (pc0, false)
    | some e =>
      if !e.alt then
        let score := 2 * (if pc0.next == e.prev then 1 else 0) + (if pc0.prev == e.next then 1 else 0)
        match score with
        | 3 => (pc0, true)
        | 2 => ({ pc0 with next := e.next }, false)
        | 1 => ({ pc0 with prev := e.prev }, false)
        | _ => ({ pc0 with prev2 := e.prev, next2 := e.next, alt := true }, false)
      else
        let score := 8 * (if pc0.next == e.prev2 then 1 else 0) + 4 * (if pc0.prev == e.next2 then 1 else 0) +
          2 * (if pc0.next == e.prev then 1 else 0) + (if pc0.prev == e.next then 1 else 0)
        match score with
        | 9 => ({ pc0 with next := e.next2, prev := e.prev }, false)
        | 6 => ({ pc0 with next := e.next, prev := e.prev2 }, false)
        | 8 => ({ pc0 with next2 := e.next2, prev2 := pc0.prev, prev := e.prev, next := e.next, alt := true }, false)
        | 4 => ({ pc0 with prev2 := e.prev2, next2 := pc0.next, prev := e.prev, next := e.next, alt := true }, false)
        | 2 => ({ pc0 with next := e.next, prev2 := e.prev2, next2 := e.next2, alt := true }, false)
        | 1 => ({ pc0 with prev := e.prev, prev2 := e.prev2, next2 := e.next2, alt := true }, false)
        -- the C++ throws "undefined merging configuration"; keep the new polygon's links
        | _ => (pc0, false)
  (Array.range n).foldl (init := grid) fun g i =>
    let (pc, del) := conns[i]!
    if del then g.erase poly[i]! else g.insert poly[i]! pc

/-- Static data of one band. -/
structure Ctx where
  nrow : Nat
  ncol : Nat
  xs : FloatArray
  ys : FloatArray
  z : FloatArray
  lo : Float
  hi : Float

namespace Ctx

/-- `grid_z(r, c)`: row `r` ↔ `y`, column `c` ↔ `x` (`z[c + ncol*r]`). -/
@[inline] def zAt (c : Ctx) (r col : Nat) : Float := c.z.get! (col + c.ncol * r)

/-- Grid-point key of `(r, c, type)`. -/
@[inline] def key (c : Ctx) (r col type : Nat) : UInt64 := ((r * c.ncol + col) * 8 + type).toUInt64

/-- Key of an elementary-polygon point code relative to cell `(r, c)`. -/
@[inline] def keyOfCode (c : Ctx) (r col code : Nat) : UInt64 :=
  c.key (r + (code / 2) % 2) (col + code % 2) (code / 4)

/-- `interpolate(x0, x1, z0, z1, v)`. -/
@[inline] def interp (x0 x1 z0 z1 v : Float) : Float := x0 + ((v - z0) / (z1 - z0)) * (x1 - x0)

/-- `calc_point_coords`. -/
def coords (c : Ctx) (k : UInt64) : Float × Float :=
  let k := k.toNat
  let type := k % 8
  let rc := k / 8
  let r := rc / c.ncol
  let col := rc % c.ncol
  let x := c.xs.get! col
  let y := c.ys.get! r
  match type with
  | 0 => (x, y)
  | 1 => (interp x (c.xs.get! (col + 1)) (c.zAt r col) (c.zAt r (col + 1)) c.lo, y)
  | 2 => (interp x (c.xs.get! (col + 1)) (c.zAt r col) (c.zAt r (col + 1)) c.hi, y)
  | 3 => (x, interp y (c.ys.get! (r + 1)) (c.zAt r col) (c.zAt (r + 1) col) c.lo)
  | _ => (x, interp y (c.ys.get! (r + 1)) (c.zAt r col) (c.zAt (r + 1) col) c.hi)

/-- Ternarised value: `0` below `lo`, `1` in `[lo, hi)`, `2` at or above `hi`. -/
@[inline] def tern (c : Ctx) (v : Float) : Nat :=
  (if v ≥ c.lo && v < c.hi then 1 else 0) + (if v ≥ c.hi then 2 else 0)

/-- `central_value(r, c)`. -/
@[inline] def central (c : Ctx) (r col : Nat) : Float :=
  (c.zAt r col + c.zAt r (col + 1) + c.zAt (r + 1) col + c.zAt (r + 1) (col + 1)) / 4

/-- Merge the elementary polygons of cell `(r, c)`. -/
def cellPolys (c : Ctx) (grid : PGrid) (r col : Nat) : PGrid :=
  let z1 := c.zAt r col
  let z2 := c.zAt r (col + 1)
  let z3 := c.zAt (r + 1) (col + 1)
  let z4 := c.zAt (r + 1) col
  if !(z1.isFinite && z2.isFinite && z3.isFinite && z4.isFinite) then grid else
  let idx := 27 * c.tern z1 + 9 * c.tern z2 + 3 * c.tern z3 + c.tern z4
  let e := caseTable[idx]!
  let polys : Array (Array Nat) :=
    match e.kind with
    | 0 => #[]
    | 1 => e.b0
    | 2 => if c.central r col < c.lo then e.b0 else e.b1
    | 3 => if c.central r col ≥ c.hi then e.b0 else e.b1
    | _ =>
      let vc := c.central r col
      if vc < c.lo then e.b0 else if vc ≥ c.hi then e.b1 else e.b2
  polys.foldl (init := grid) fun g p => polyMerge g (p.map (c.keyOfCode r col))

/-- `calculate_contour`: merge all cells, row by row. -/
def build (c : Ctx) : PGrid :=
  let nr := c.nrow - 1
  let nc := c.ncol - 1
  let n := nr * nc
  let rec go (k : Nat) (g : PGrid) : PGrid :=
    if k < n then go (k + 1) (c.cellPolys g (k / nc) (k % nc)) else g
  termination_by n - k
  go 0 {}

end Ctx

/-- `collect`: follow the links into rings, visiting start points in key order. -/
def collectRings (c : Ctx) (grid : PGrid) : Array Pts2 :=
  let keys := grid.keysArray.qsort (· < ·)
  let total := keys.size
  let rec ring (fuel : Nat) (g : PGrid) (start cur prev : UInt64) (xs ys : FloatArray) : PGrid × FloatArray × FloatArray :=
    match fuel with
    | 0 => (g, xs, ys)
    | fuel + 1 =>
      let (x, y) := c.coords cur
      let xs := xs.push x
      let ys := ys.push y
      let e := (g.get? cur).getD {}
      let (g, next) :=
        if e.alt && e.prev2 == prev then (g.insert cur { e with collected2 := true }, e.next2)
        else (g.insert cur { e with collected := true }, e.next)
      if next == start then (g, xs, ys) else ring fuel g start next cur xs ys
  let rec go (k : Nat) (g : PGrid) (acc : Array Pts2) : Array Pts2 :=
    if h : k < keys.size then
      let key := keys[k]
      let e := (g.get? key).getD {}
      if (e.collected && !e.alt) || (e.collected && e.collected2 && e.alt) then go (k + 1) g acc
      else
        let prev := if e.alt && !e.collected2 then e.prev2 else e.prev
        let (g, xs, ys) := ring (2 * total + 2) g key key prev .empty .empty
        go (k + 1) g (acc.push (Pts2.ofArrays xs ys))
    else acc
  termination_by keys.size - k
  go 0 grid #[]

/-- The rings of one isoband `[lo, hi)` of `z` on the grid `xs × ys`, as Makie
calls Isoband.jl (`isobands(xs, ys, zᵀ, lo, hi)`: grid rows ↔ `y`, columns ↔
`x`). Rings are open (the first vertex is not repeated). -/
def isobandRings {nx ny : Nat} (xs ys : FloatArray) (g : Grid2 nx ny) (lo hi : Float) : Array Pts2 :=
  if nx < 2 || ny < 2 then #[] else
  let c : Ctx := ⟨ny, nx, xs, ys, g.z, lo, hi⟩
  collectRings c c.build

/-! ## Rings to polygons with holes (Makie `_group_polys`) -/

/-- A polygon with holes; every ring is closed (first point repeated). -/
structure Polygon where
  /-- Outer boundary. -/
  outer : Pts2
  /-- Hole boundaries. -/
  holes : Array Pts2
  deriving Inhabited

/-- PolygonOps `inpolygon(p, poly)` (Hao–Sun), evaluated in binary32 like Makie's
`Point2f` rings: `1` inside, `-1` on the boundary, `0` outside. `ring` must be
closed. -/
def inPolygon (xp yp : Float) (ring : Pts2) : Int :=
  let n := ring.size
  let rec go (i k : Nat) : Int :=
    if i + 1 < n then
      let ay := ring.ys.get! i
      let by' := ring.ys.get! (i + 1)
      let v1 := r32 (ay - yp)
      let v2 := r32 (by' - yp)
      if (v1 < 0 && v2 < 0) || (v1 > 0 && v2 > 0) then go (i + 1) k else
      let u1 := r32 (ring.xs.get! i - xp)
      let u2 := r32 (ring.xs.get! (i + 1) - xp)
      let f := r32 (r32 (u1 * v2) - r32 (u2 * v1))
      if v2 > 0 && v1 ≤ 0 then
        if f > 0 then go (i + 1) (k + 1) else if f == 0 then -1 else go (i + 1) k
      else if v1 > 0 && v2 ≤ 0 then
        if f < 0 then go (i + 1) (k + 1) else if f == 0 then -1 else go (i + 1) k
      else if v2 == 0 && v1 < 0 then (if f == 0 then -1 else go (i + 1) k)
      else if v1 == 0 && v2 < 0 then (if f == 0 then -1 else go (i + 1) k)
      else if v1 == 0 && v2 == 0 then
        if (u2 ≤ 0 && u1 ≥ 0) || (u1 ≤ 0 && u2 ≥ 0) then -1 else go (i + 1) k
      else go (i + 1) k
    else if k % 2 == 0 then 0 else 1
  termination_by n - i
  go 0 0

/-- Bounding box `(xmin, xmax, ymin, ymax)` of a ring. -/
def ringBox (p : Pts2) : Float × Float × Float × Float :=
  match extremaNaN p.xs, extremaNaN p.ys with
  | some (x0, x1), some (y0, y1) => (x0, x1, y0, y1)
  | _, _ => (nan, nan, nan, nan)

/-- `inPolygon … == 1`, skipping points outside the ring's bounding box. Outside
the box every Hao–Sun edge test is decided by exact signs (binary32 subtraction
never flips a sign), so such points are always "out": the shortcut changes no
result (short of coordinate differences whose products underflow binary32). -/
@[inline] def strictlyInside (xp yp : Float) (ring : Pts2) (box : Float × Float × Float × Float) : Bool :=
  let (x0, x1, y0, y1) := box
  if xp < x0 || xp > x1 || yp < y0 || yp > y1 then false else inPolygon xp yp ring == 1

/-- Makie `_is_ring_contained(inner, outer)`: some vertex of `inner`, or some
midpoint of an `inner` edge, lies strictly inside `outer` (closed binary32 rings;
`box` is `outer`'s bounding box). -/
def ringContained (inner outer : Pts2) (box : Float × Float × Float × Float := ringBox outer) : Bool :=
  let n := inner.size
  let rec anyVertex (i : Nat) : Bool :=
    if i < n then strictlyInside (inner.xs.get! i) (inner.ys.get! i) outer box || anyVertex (i + 1) else false
  termination_by n - i
  let rec anyMid (i : Nat) : Bool :=
    if i + 1 < n then
      let mx := r32 (r32 (inner.xs.get! i + inner.xs.get! (i + 1)) / 2)
      let my := r32 (r32 (inner.ys.get! i + inner.ys.get! (i + 1)) / 2)
      strictlyInside mx my outer box || anyMid (i + 1)
    else false
  termination_by n - i
  anyVertex 0 || anyMid 0

/-- Close a ring (repeat its first point). -/
def closeRing (p : Pts2) : Pts2 := if p.size == 0 then p else Pts2.ofArrays (p.xs.push (p.xs.get! 0)) (p.ys.push (p.ys.get! 0))

/-- Makie `_group_polys`: classify rings (binary32, open) into outer boundaries
and holes by nesting depth, repeatedly peeling rings contained in no other
remaining ring (outer) and rings contained in exactly one (its hole). -/
def groupPolys (rings : Array Pts2) : Array Polygon := Id.run do
  let polys := rings.map closeRing
  let n := polys.size
  let boxes := polys.map ringBox
  -- containment[i][j]: ring i lies inside ring j
  let cont : Array (Array Bool) := (Array.range n).map fun i =>
    (Array.range n).map fun j => i != j && ringContained polys[i]! polys[j]! boxes[j]!
  let mut unclassified : Array Nat := Array.range n
  let mut groups : Array Polygon := #[]
  let mut groupOf : Array (Option Nat) := Array.replicate n none
  for _ in [0:n + 1] do
    if unclassified.isEmpty then break
    let rowSum (i : Nat) : Nat := unclassified.foldl (fun s j => if cont[i]![j]! then s + 1 else s) 0
    let mut keep : Array Bool := #[]
    for i in unclassified do
      if rowSum i == 0 then
        groups := groups.push ⟨polys[i]!, #[]⟩
        groupOf := groupOf.set! i (some (groups.size - 1))
        keep := keep.push false
      else keep := keep.push true
    let mut ii := 0
    for i in unclassified do
      if rowSum i == 1 then
        let outer := (unclassified.find? fun j => cont[i]![j]!).getD i
        match groupOf[outer]! with
        | some gi =>
          groups := groups.modify gi fun g => { g with holes := g.holes.push polys[i]! }
          keep := keep.set! ii false
        | none => pure ()
      ii := ii + 1
    let mut next : Array Nat := #[]
    for k in [0:unclassified.size] do
      if keep[k]! then next := next.push unclassified[k]!
    unclassified := next
  return groups

/-! ## Makie `contourf` -/

/-- Data of Makie's `contourf` recipe: band edges, one polygon (with holes) per
connected region of every band, and each polygon's colour value (its band
centre) and band index. Coordinates are binary32 values. -/
structure Contourf where
  /-- `computed_levels` (band edges, binary32). -/
  levels : FloatArray
  /-- Low edge per band (`-Inf` for an `extendlow` band). -/
  lows : FloatArray
  /-- High edge per band (`Inf` for an `extendhigh` band). -/
  highs : FloatArray
  /-- Polygons with holes. -/
  polys : Array Polygon
  /-- Colour value per polygon (`(low + high)/2` in binary32). -/
  colors : FloatArray
  /-- Band index per polygon. -/
  bandOf : Array Nat
  deriving Inhabited

/-- Round a ring to binary32 (`Point2f`). -/
def roundRing (p : Pts2) : Pts2 := Pts2.ofArrays (roundArray p.xs) (roundArray p.ys)

/-- Makie's `contourf(xs, ys, z; levels, mode, extendlow, extendhigh)` for a
rectilinear grid (`z[i + nx*j]`, `i ↔ x`). -/
def makieContourf {nx ny : Nat} (xs ys : FloatArray) (g : Grid2 nx ny) (spec : Levels.LevelSpec := .count 10)
    (mode : Levels.LevelMode := .normal) (extendLow extendHigh : Bool := false) : Contourf :=
  let z32 := g.map r32
  let levels := Levels.contourfLevels spec z32.z mode
  let (lows, highs) := Levels.bandEdges levels extendLow extendHigh
  let centers := Levels.bandCenters lows highs
  let (polys, colors, bands) := (Array.range lows.size).foldl (init := (#[], FloatArray.empty, #[]))
    fun (ps, cs, bs) b =>
      let rings := (isobandRings xs ys z32 (lows.get! b) (highs.get! b)).map roundRing
      (groupPolys rings).foldl (init := (ps, cs, bs)) fun (ps, cs, bs) p =>
        (ps.push p, cs.push (centers.get! b), bs.push b)
  ⟨levels, lows, highs, polys, colors, bands⟩

/-! ## Contourf colours (Makie's banded colormap) -/

/-- ColorSchemes `get(cs, x)` on a colormap's entries: `x ∈ [0, 1]` is mapped to
`1 … n` and the two neighbouring entries are blended (`w·c₁ + (1-w)·c₂`,
binary64). -/
def schemeGet (cm : Colormap) (x : Float) : RGBA :=
  let n := cm.size
  let xc := clamp x 0 1
  let bfp := xc * Num.ofInt ((n : Int) - 1) + 1
  let before := bfp.floor
  let bi := before.toUInt64.toNat
  let ai := min (bi + 1) n
  let cpt := bfp - before
  let w := 1 - cpt
  let c1 := cm.get (bi - 1)
  let c2 := cm.get (ai - 1)
  ⟨w * c1.r + (1 - w) * c2.r, w * c1.g + (1 - w) * c2.g, w * c1.b + (1 - w) * c2.b, w * c1.a + (1 - w) * c2.a⟩

/-- PlotUtils `cgrad(colors, n; categorical = true)` colours: `n` samples of the
scheme at `range(0, 1; length = n)`. -/
def schemeSamples (cm : Colormap) (n : Nat) : Array RGBA :=
  (Num.range 0 1 n).toList.toArray.map (schemeGet cm)

/-- Makie's contourf colouring: the banded colormap (`cgrad(base, edges_scaled;
categorical = true)`, sampled at 256 points like `to_colormap(::ColorGradient)`),
the colour range `extrema(levels)`, and the low/high clip colours (transparent
unless the band is extended with `:auto`, then the colormap's first/last
colour). `edges` are the binary32 `computed_levels`. -/
structure BandColoring where
  /-- 256-entry banded colormap. -/
  colormap : Colormap
  /-- Colour range. -/
  lo : Float
  hi : Float
  /-- Colour of values below `lo` (extended low band). -/
  lowclip : RGBA
  /-- Colour of values above `hi` (extended high band). -/
  highclip : RGBA

/-- Build Makie's contourf colouring from a base colormap. -/
def bandColoring (cm : Colormap) (edges : FloatArray) (extendLow extendHigh : Bool := false) : BandColoring :=
  let nb := edges.size - 1
  -- base colours (`base_colormap`): the colormap itself, or `n + 1`/`n + 2`
  -- categorical samples with the extension colours cut off
  let base : Colormap :=
    if extendLow && !extendHigh then Colormap.ofColors ((schemeSamples cm (nb + 1)).extract 1 (nb + 1))
    else if extendHigh && !extendLow then Colormap.ofColors ((schemeSamples cm (nb + 1)).extract 0 nb)
    else if extendHigh && extendLow then Colormap.ofColors ((schemeSamples cm (nb + 2)).extract 1 (nb + 1))
    else cm
  -- `edges_scaled` (binary32), then `prepare_categorical_cgrad_colors`
  let (mn, mx) := (extremaNaN edges).getD (0, 1)
  let den := F32.r32 (mx - mn)
  let scaled := (edges.toList.map fun e => F32.r32 (F32.r32 (e - mn) / den)).map (clamp · 0 1)
  let vals := scaled.toArray.qsort (· < ·)
  let vals := vals.foldl (fun acc v => if acc.back? == some v then acc else acc.push v) #[]
  let vals := if vals.contains 0 then vals else #[0] ++ vals
  let vals := if vals.contains 1 then vals else vals.push 1
  let colors := schemeSamples base (vals.size - 1)
  -- `to_colormap(cg)`: 256 samples `cg[x]`, `x ∈ LinRange(0, 1, 256)`
  let samples := (Num.linRange 0 1 256).toList.toArray.map fun x =>
    -- `findlast(<(x), values)` (1-based) is the colour index
    let k := if x == 0 then 0 else (vals.foldl (fun (acc : Nat × Nat) v => (acc.1 + 1, if v < x then acc.1 else acc.2)) (0, 0)).2
    RGBA.toF32 (colors.getD k (colors.getD 0 RGBA.black))
  let first := RGBA.toF32 cm.first
  let last := RGBA.toF32 cm.last
  { colormap := Colormap.ofColors samples
    lo := mn, hi := mx
    lowclip := if extendLow then first else RGBA.transparent
    highclip := if extendHigh then last else RGBA.transparent }

/-- Makie `numbers_to_colors` for one value with a binary32 colour range
(`Vec2f`): `NaN` → transparent, below/above the range → the clip colours, else
`interpolated_getindex` with the range width evaluated in binary32. -/
def BandColoring.colorOf (bc : BandColoring) (v : Float) : RGBA :=
  if v.isNaN then RGBA.transparent
  else if v < bc.lo then bc.lowclip
  else if v > bc.hi then bc.highclip
  else bc.colormap.interpolatedGetIndex (clamp ((v - bc.lo) / F32.r32 (bc.hi - bc.lo)) 0 1)

/-- The colour of each contourf polygon (Makie `numbers_to_colors` of the band
centres through the banded colormap, with low/high clipping). -/
def polygonColors (bc : BandColoring) (cf : Contourf) : Array RGBA :=
  cf.colors.toList.toArray.map bc.colorOf

end LeanPlot.Recipes.Algo.Isoband
