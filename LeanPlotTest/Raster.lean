import LeanPlotTest.Raster.Util
import LeanPlotTest.Raster.PNG
import LeanPlotTest.Raster.Fill
import LeanPlotTest.Raster.Stroke
import LeanPlotTest.Raster.Ops
import LeanPlotTest.Raster.Perf
import LeanPlotTest.Raster.Cairo

/-!
Raster backend + PNG codec test aggregator.

`LeanPlotTest.Raster.run` runs every suite and returns `(passed, failed)`.
The PNG suite shells out to `python3` (standard library only) for an
external zlib round trip and skips that part when Python is missing. The
Cairo parity suite reads reference PNGs from `LeanPlotTest/Raster/cairo/`
relative to the working directory (the repository root).
-/

namespace LeanPlotTest.Raster

/-- Run all raster/PNG suites; returns `(passed, failed)`. -/
def run : IO (Nat × Nat) := do
  let suites : List (IO (Nat × Nat)) :=
    [PNGTests.run, FillTests.run, StrokeTests.run, OpsTests.run, CairoTests.run, PerfTests.run]
  let mut p := 0
  let mut f := 0
  for s in suites do
    let (a, b) ← s
    p := p + a; f := f + b
  return (p, f)

end LeanPlotTest.Raster
