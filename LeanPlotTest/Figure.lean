import LeanPlotTest.Figure.LayoutTest
import LeanPlotTest.Figure.RenderTest
import LeanPlotTest.Figure.UnitTest

/-!
Figure/layout test suites. `LeanPlotTest.Figure.run` runs every suite and returns
`(passed, failed)`. Oracle goldens are read from `LeanPlotTest/oracle/figure/` and SVG
goldens from `LeanPlotTest/Figure/golden/`, relative to the working directory (the package
root under `lake test`).
-/

namespace LeanPlotTest.Figure

open LeanPlotTest.Core

/-- Run all figure suites; returns `(passed, failed)`. -/
def run : IO (Nat × Nat) := do
  let suites : List (String × TestM Unit) :=
    [("figure/unit", unitSuite), ("figure/layout", layoutSuite), ("figure/render", renderSuite)]
  let mut p := 0
  let mut f := 0
  for (name, s) in suites do
    let (a, b) ← runSuite name s
    p := p + a
    f := f + b
  return (p, f)

end LeanPlotTest.Figure
