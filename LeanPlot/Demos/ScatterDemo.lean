import LeanPlot.Specification
import LeanPlot.Plot

open LeanPlot
open LeanPlot.PlotSpec

namespace LeanPlot.Demos

/-- A simple dataset consisting of 21 points sampled from the quadratic
function `y = x²` on the interval `[0,2]`. -/
@[inline] def quadPoints : Array (Float × Float) :=
  (List.range 21).toArray.map fun i =>
    let x : Float := i.toFloat * 0.1
    (x, x * x)

/- Render the scatter plot.  Put your cursor on the `#plot` line to see the
visualisation in the infoview. -/
#plot (withTitle (scatter quadPoints "Quadratic points") "Quadratic Function – Scatter")

end LeanPlot.Demos
