import LeanPlot
import LeanPlotTest

/-- Golden test driver: `lake test` / `lake exe leanplot-golden`. -/
def main (_args : List String) : IO UInt32 := do
  IO.println "leanplot-golden: no suites registered yet"
  return 0
