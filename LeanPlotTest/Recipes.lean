import LeanPlotTest.Recipes.Harness
import LeanPlotTest.Recipes.LevelsTest
import LeanPlotTest.Recipes.StreamTest
import LeanPlotTest.Recipes.ContourTest

/-!
Recipe-algorithm test suites (`LeanPlot.Recipes.Algo`). `LeanPlotTest.Recipes.run`
runs every suite and returns `(passed, failed)`. Oracle goldens are read from
`LeanPlotTest/oracle/recipes/` relative to the working directory (the package
root under `lake test`).
-/

namespace LeanPlotTest.Recipes

/-- Run all recipe-algorithm suites; returns `(passed, failed)`. -/
def run : IO (Nat × Nat) := do
  let suites : List (String × TestM Unit) :=
    [("recipes/levels", LevelsTest.suite),
     ("recipes/streamplot", StreamTest.suite),
     ("recipes/contour", ContourTest.suite)]
  let mut p := 0
  let mut f := 0
  for (name, s) in suites do
    let (a, b) ← runSuite name s
    p := p + a
    f := f + b
  return (p, f)

end LeanPlotTest.Recipes
