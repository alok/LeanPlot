import LeanPlotTest.Core.Harness
import LeanPlotTest.Core.NumTest
import LeanPlotTest.Core.TicksTest
import LeanPlotTest.Core.ColorTest

/-!
Core test suites. `LeanPlotTest.Core.run` runs every suite and returns
`(passed, failed)`. Oracle goldens are read from `LeanPlotTest/oracle/core/`
relative to the working directory (the package root under `lake test`).
-/

namespace LeanPlotTest.Core

/-- Run all core suites; returns `(passed, failed)`. -/
def run : IO (Nat × Nat) := do
  let suites : List (String × TestM Unit) :=
    [("core/num", NumTest.suite),
     ("core/ticks", TicksTest.suite),
     ("core/color", ColorTest.suite)]
  let mut p := 0
  let mut f := 0
  for (name, s) in suites do
    let (a, b) ← runSuite name s
    p := p + a
    f := f + b
  return (p, f)

end LeanPlotTest.Core
