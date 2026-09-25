import LeanPlot.Core.Range
import LeanPlot.Recipes.Algo.F32

/-!
# Heatmap cell edges (Makie `CellGrid` conversions, CairoMakie grid test)

* `edges`: Makie `edges(v)`: centres to edges (midpoints between neighbours,
  extended by half a step at both ends; `v ± 0.5` for a single centre).
* `cellEdges`: `adjust_axes(::CellGrid, …)`: an axis with one value per cell
  holds centres and is converted to edges; an axis with `n + 1` values already
  holds edges.
* `endpointEdges`: `heatmap((a, b), (c, d), z)`: the given end points are the
  outer cell *centres*, so the edges extend half a step
  (`step(range(a, b; length = n))/2`) beyond them.
* `regularGrid?`: CairoMakie `regularly_spaced_array_to_range`: edges whose
  distinct differences all agree (`≈`) are drawn as one image, otherwise every
  cell is a separate quad.
-/

namespace LeanPlot.Recipes.Algo.Heatmap

open LeanPlot.Num
open LeanPlot.Recipes.Algo.F32

/-- Makie `edges(v)` for binary64 values. -/
def edges (v : FloatArray) : FloatArray :=
  let l := v.size
  if l == 0 then .empty
  else if l == 1 then ⟨#[v.get! 0 - 0.5, v.get! 0 + 0.5]⟩
  else
    let mid (i : Nat) : Float := 0.5 * (v.get! (if i == 0 then 0 else i - 1) + v.get! (min (l - 1) i))
    let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
      if i ≤ l then go (i + 1) (acc.push (mid i)) else acc
    termination_by l + 1 - i
    let b := go 0 (FloatArray.emptyWithCapacity (l + 1))
    let b := b.set! 0 (2 * b.get! 0 - b.get! 1)
    b.set! l (2 * b.get! l - b.get! (l - 1))

/-- `adjust_axes(::CellGrid, v, n)`: edges for an axis with `n` cells, or `none`
when `v` has neither `n` nor `n + 1` entries. -/
def cellEdges (v : FloatArray) (n : Nat) : Option FloatArray :=
  if v.size == n then some (edges v)
  else if v.size == n + 1 then some v
  else none

/-- Makie `get_step(EndPoints(a, b), n)`: `step(range(a, b; length = n))`, or `1`
for a single cell with `a == b`. -/
def endpointStep (a b : Float) (n : Nat) : Float :=
  if a == b && n == 1 then 1 else
  let r := TwicePrecision.startStopLength a b n
  r.step.hi + r.step.lo

/-- `convert_arguments(::CellGrid, (a, b), …)`: the outer edges of `n` cells whose
first and last centres are `a` and `b`. -/
def endpointEdges (a b : Float) (n : Nat) : Float × Float :=
  let h := endpointStep a b n / 2.0
  (a - h, b + h)

/-- CairoMakie `regularly_spaced_array_to_range(arr)`: `some (start, step)` when
the distinct differences of `arr` all agree with their mean (`isapprox`, binary32
tolerance when `f32`), so the axis is the range `start, start + step, …`
(`range(m; step, length)`); `none` for an irregular axis. -/
def regularGrid? (arr : FloatArray) (f32 : Bool := true) : Option (Float × Float) :=
  let n := arr.size
  if n < 2 then none else
  let diffs := ((List.range (n - 1)).map fun i => rnd f32 (arr.get! (i + 1) - arr.get! i)).toArray.qsort (· < ·)
  let uniq := diffs.foldl (fun acc d => if acc.back? == some d then acc else acc.push d) #[]
  let step := rnd f32 (uniq.foldl (fun s d => rnd f32 (s + d)) 0 / Num.ofInt uniq.size)
  let approx (x : Float) : Bool :=
    if f32 then isApprox32 x step else isApprox x step (Float.sqrt eps64)
  if uniq.all approx then
    match extremaNaN arr with
    | some (m, mm) => some (if step < 0 then mm else m, step)
    | none => none
  else none

end LeanPlot.Recipes.Algo.Heatmap
