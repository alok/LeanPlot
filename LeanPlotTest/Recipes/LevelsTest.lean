import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Levels

/-!
Oracle tests for `LeanPlot.Recipes.Algo.F32` (Julia `Float32` ranges) and
`LeanPlot.Recipes.Algo.Levels` (Makie contour/contourf levels), against
`levels.json`. All comparisons are bit-exact.
-/

namespace LeanPlotTest.Recipes.LevelsTest

open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "levels.json" | return
  for c in (j.get "range32").arrD do
    let a := (c.get "a").float
    let b := (c.get "b").float
    let n := (c.get "n").nat
    let got := F32.range a b n
    let want := floatArr (c.get "v")
    check s!"range32 {a} {b} {n}" (faBitEq got want) (fun _ => s!"got {showFA got} want {showFA want}")
  for c in (j.get "rangestep32").arrD do
    let a := (c.get "a").float
    let s := (c.get "s").float
    let n := (c.get "n").nat
    let got := F32.rangeStep a s n
    let want := floatArr (c.get "v")
    check s!"rangestep32 {a} {s} {n}" (faBitEq got want) (fun _ => s!"got {showFA got} want {showFA want}")
  for c in (j.get "levels").arrD do
    let lo := (c.get "lo").float
    let hi := (c.get "hi").float
    let n := (c.get "n").nat
    let got := Levels.contourLevels n lo hi
    let want := floatArr (c.get "levels")
    check s!"to_levels {n} {lo} {hi}" (faBitEq got want) (fun _ => s!"got {showFA got} want {showFA want}")
    let got2 := Levels.isobandLevels n lo hi
    let want2 := floatArr (c.get "isoband")
    check s!"isoband_levels {n} {lo} {hi}" (faBitEq got2 want2) (fun _ => s!"got {showFA got2} want {showFA want2}")
  for c in (j.get "nodes").arrD do
    let name := (c.get "name").string
    let z := floatArr (c.get "z")
    let spec : Levels.LevelSpec := match c.get "spec" with
      | .arr _ => .values (floatArr (c.get "spec"))
      | s => .count s.nat
    let zl := Levels.contourZLevels spec z
    let wantZ := floatArr (c.get "zlevels")
    check s!"zlevels {name}" (faBitEq zl wantZ) (fun _ => s!"got {showFA zl} want {showFA wantZ}")
    let cl := Levels.contourfLevels spec z
    let wantC := floatArr (c.get "cflevels")
    check s!"cflevels {name}" (faBitEq cl wantC) (fun _ => s!"got {showFA cl} want {showFA wantC}")
    if let some r := c.get? "relative" then
      let rl := Levels.contourfLevels (.values ⟨#[0.1, 0.5, 0.9]⟩) z .relative
      check s!"relative {name}" (faBitEq rl (floatArr r)) (fun _ => s!"got {showFA rl} want {showFA (floatArr r)}")

end LeanPlotTest.Recipes.LevelsTest
