import LeanPlot.Recipes.Algo.F32

/-!
# Contour and contourf level selection (Makie semantics)

* `contourLevels`: Makie `to_levels(n, (zmin, zmax))`, i.e.
  `range(zmin + dz; step = dz, length = n)` with `dz = (zmax - zmin)/(n + 1)`.
  Makie converts contour data to `Float32` (`el32convert`), so the levels are a
  `Float32` range; the `Float64` variant is kept for callers that trace in
  double precision.
* `contourZLevels`: the `zlevels` node of Makie's `Contour` recipe (empty when
  the data range is degenerate by `isapprox`).
* `isobandLevels`: Makie `_get_isoband_levels(n, lo, hi)`,
  `collect(range(Float32(lo), nextfloat(Float32(hi)), length = n + 1))`.
* `contourfLevels`: the `computed_levels` node of Makie's `Contourf` recipe,
  including the degenerate-range widening and `mode = :relative`.
* `bandEdges`, `bandCenters`: low/high edges per band (with `-Inf`/`Inf`
  extension bands) and the `Float32` band centres used as polygon colours.
-/

namespace LeanPlot.Recipes.Algo.Levels

open LeanPlot.Num
open LeanPlot.Recipes.Algo.F32

/-- How levels are requested: a count of automatic levels or explicit values. -/
inductive LevelSpec where
  /-- `n` automatic levels (contour) or bands (contourf). -/
  | count (n : Nat)
  /-- Explicit level values (contour) or band edges (contourf), ascending. -/
  | values (vs : FloatArray)
  deriving Inhabited

/-- Makie `contourf(...; mode)`: levels are absolute values or fractions of the data range. -/
inductive LevelMode where
  /-- Levels are data values. -/
  | normal
  /-- Levels are fractions between the data minimum and maximum. -/
  | relative
  deriving Inhabited, BEq, Repr

/-- Makie `to_levels(n, (zmin, zmax))`. With `f32` (Makie's default data path) the
bounds must be binary32 values and the arithmetic is binary32. -/
def contourLevels (n : Nat) (zmin zmax : Float) (f32 : Bool := true) : FloatArray :=
  if f32 then
    let dz := r32 (r32 (zmax - zmin) / Num.ofInt (n + 1 : Nat))
    F32.rangeStep (r32 (zmin + dz)) dz n
  else
    let dz := (zmax - zmin) / Num.ofInt (n + 1 : Nat)
    Num.rangeStep (zmin + dz) dz n

/-- Makie `nan_extrema` of the data (`(NaN, NaN)` if all NaN). -/
def dataRange (z : FloatArray) : Float × Float := (extremaNaN z).getD (nan, nan)

/-- The `zlevels` node of Makie's `Contour` recipe for data `z` (binary32 values
when `f32`). Degenerate ranges (`isapprox(zmin, zmax)`) give no levels; explicit
levels are passed through unchanged (tracing converts them to the data's element
type). -/
def contourZLevels (spec : LevelSpec) (z : FloatArray) (f32 : Bool := true) : FloatArray :=
  let (lo, hi) := dataRange z
  let degenerate := if f32 then isApprox32 lo hi else isApprox lo hi (Float.sqrt eps64)
  if degenerate then .empty else
  match spec with
  | .values vs => vs
  | .count n => contourLevels n lo hi f32

/-- Makie `_get_isoband_levels(n, lo, hi)`: `n + 1` binary32 band edges from `lo`
to just above `hi`. -/
def isobandLevels (n : Nat) (lo hi : Float) : FloatArray :=
  F32.range (r32 lo) (nextFloat32 (r32 hi)) (n + 1)

/-- The `computed_levels` node of Makie's `Contourf` recipe for binary32 data
`z` (band edges, binary32). -/
def contourfLevels (spec : LevelSpec) (z : FloatArray) (mode : LevelMode := .normal) : FloatArray :=
  let (mi, ma) := dataRange z
  match spec with
  | .count n =>
    if isApprox32 mi ma then
      let delta := jmax 1 mi.abs
      let nbands := if n % 2 == 1 then n else n + 1
      F32.range (r32 (mi - delta)) (r32 (ma + delta)) (nbands + 1)
    else
      -- `_get_isoband_levels(Val(:relative), ::Int, …)` has no method in Makie;
      -- an integer level count always means evenly spaced absolute edges.
      isobandLevels n mi ma
  | .values vs =>
    match mode with
    | .normal => roundArray vs
    | .relative =>
      let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
        if h : i < vs.size then go (i + 1) (acc.push (r32 (vs[i] * r32 (ma - mi) + mi))) else acc
      termination_by vs.size - i
      go 0 (FloatArray.emptyWithCapacity vs.size)

/-- Low and high edges of each band (`_calculate_polys!`): consecutive pairs of
`edges`, with an extra `(-Inf, edges[0])` band when `extendLow` and
`(edges[last], Inf)` when `extendHigh`. -/
def bandEdges (edges : FloatArray) (extendLow extendHigh : Bool := false) : FloatArray × FloatArray :=
  let es : FloatArray := if extendLow then ⟨#[-inf] ++ edges.data⟩ else edges
  let es : FloatArray := if extendHigh then es.push inf else es
  let n := es.size - 1
  let rec go (i : Nat) (lows highs : FloatArray) : FloatArray × FloatArray :=
    if i < n then go (i + 1) (lows.push (es.get! i)) (highs.push (es.get! (i + 1))) else (lows, highs)
  termination_by n - i
  go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)

/-- Band centres `(high + low) / 2` in binary32 (Makie's per-polygon colour value). -/
def bandCenters (lows highs : FloatArray) : FloatArray :=
  let n := min lows.size highs.size
  let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
    if i < n then go (i + 1) (acc.push (r32 (r32 (highs.get! i + lows.get! i) / 2))) else acc
  termination_by n - i
  go 0 (FloatArray.emptyWithCapacity n)

end LeanPlot.Recipes.Algo.Levels
