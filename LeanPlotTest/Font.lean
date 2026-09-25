import LeanPlotTest.Font.Metrics
import LeanPlotTest.Font.Paths
import LeanPlotTest.Font.Layout
import LeanPlotTest.Font.Golden
import LeanPlotTest.Font.Perf

/-! Font test suite: metrics vs fontTools, outline sanity, layout vs the Makie oracle,
golden SVG, performance budget. Run from the package root (reads `LeanPlotTest/Font/data`). -/

namespace LeanPlotTest.Font

/-- Run every font test; returns `(passed, failed)`. A suite that throws counts as one failure. -/
def run : IO (Nat × Nat) := do
  let suites : List (String × IO (Nat × Nat)) :=
    [("metrics", Metrics.run), ("paths", Paths.run), ("layout", Layout.run),
     ("golden", Golden.run), ("perf", Perf.run)]
  let mut passed := 0
  let mut failed := 0
  for (name, suite) in suites do
    try
      let (p, f) ← suite
      passed := passed + p
      failed := failed + f
    catch e =>
      IO.println s!"  font/{name}: ERROR {e}"
      failed := failed + 1
  return (passed, failed)

end LeanPlotTest.Font
