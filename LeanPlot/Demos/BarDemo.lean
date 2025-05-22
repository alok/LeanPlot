import LeanPlot.Specification
import LeanPlot.Plot

open LeanPlot
open LeanPlot.PlotSpec

namespace LeanPlot.Demos

/-- A tiny dataset of five bars illustrating the `bar` constructor. -/
@[inline] def barData : Array (Float × Float) := #[(0.0, 1.0), (1.0, 2.0), (2.0, 3.0), (3.0, 2.0), (4.0, 1.0)]

/- Render the bar chart. -/
#plot (bar barData "values").withTitle "Simple Bar Chart"

end LeanPlot.Demos
