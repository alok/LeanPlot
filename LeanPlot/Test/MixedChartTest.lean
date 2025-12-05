import LeanPlot.Plot
import LeanPlot.Specification

open LeanPlot
open LeanPlot.PlotSpec (line lines scatter bar area)

namespace LeanPlot.Test

/-! Test mixed chart functionality -/

-- Simple test: bar chart with a line overlay
def mixedTest : PlotSpec :=
  let data := #[(1, 10), (2, 20), (3, 15), (4, 25)]
    |>.map (fun (x, y) => (x.toFloat, y.toFloat))

  bar data "Sales"
    |>.addLine (fun x => 5 * x + 5) "Trend"
    |>.withTitle "Sales with Trend Line"

#plot mixedTest

end LeanPlot.Test
