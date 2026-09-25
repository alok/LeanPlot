import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Heatmap

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Heatmap` against `heatmap.json`: Makie
`edges`, the heatmap `x`/`y` conversions for centres, edges, ranges, tuples and
intervals (bit-exact), and CairoMakie's regular-grid detection.
-/

namespace LeanPlotTest.Recipes.HeatmapTest

open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Check one converted axis. -/
def checkAxis (name : String) (kind : String) (inp : FloatArray) (n : Nat) (want : J) : TestM Unit := do
  if let some ep := want.get? "endpoints" then
    let (a, b) := Heatmap.endpointEdges (inp.get! 0) (inp.get! 1) n
    let got : FloatArray := ⟨#[a, b]⟩
    check s!"heatmap {name} {kind} endpoints" (faBitEq got (floatArr ep)) (fun _ => s!"got {showFA got} want {showFA (floatArr ep)}")
  else
    match Heatmap.cellEdges inp n with
    | none => check s!"heatmap {name} {kind} edges" false (fun _ => "size mismatch")
    | some e =>
      let w := floatArr (want.get "values")
      check s!"heatmap {name} {kind} edges" (faBitEq e w) (fun _ => s!"got {showFA e} want {showFA w}")

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "heatmap.json" | return
  for c in (j.get "edges").arrD do
    let got := Heatmap.edges (floatArr (c.get "v"))
    let want := floatArr (c.get "e")
    check s!"edges {showFA (floatArr (c.get "v"))}" (faBitEq got want) (fun _ => s!"got {showFA got} want {showFA want}")
  for c in (j.get "plots").arrD do
    let name := (c.get "name").string
    checkAxis name "x" (floatArr (c.get "x")) (c.get "nx").nat (c.get "hx")
    checkAxis name "y" (floatArr (c.get "y")) (c.get "ny").nat (c.get "hy")
  for c in (j.get "regular").arrD do
    let arr := floatArr (c.get "arr")
    let r := Heatmap.regularGrid? arr
    let want := (c.get "regular").boolean
    check s!"regular {showFA arr}" (r.isSome == want) (fun _ => s!"got {r.isSome}")
    if let some (start, step) := r then
      let ws := (c.get "start").float
      let wt := (c.get "step").float
      check s!"regular start {showFA arr}" (bitEq start ws) (fun _ => s!"got {start} want {ws}")
      check s!"regular step {showFA arr}" ((step - wt).abs ≤ 1e-6 * wt.abs) (fun _ => s!"got {step} want {wt}")

end LeanPlotTest.Recipes.HeatmapTest
