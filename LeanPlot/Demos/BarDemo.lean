import LeanPlot.Specification
import LeanPlot.Plot

open LeanPlot
open LeanPlot.PlotSpec
set_option doc.verso true
namespace LeanPlot.Demos

/-- A tiny dataset of five bars illustrating the {name}`bar` constructor. -/
@[inline] def barData : Array (Float × Float) := #[(0.0, 1.0), (1.0, 2.0), (2.0, 3.0), (3.0, 2.0), (4.0, 1.0)]

/- TODO: `#plot` should take docstrings and put them below the figure as a caption. latex rendering ftw -/
/-- TODO this should be the title. -/
#plot (bar barData "values").withTitle "Simple Bar Chart"


end LeanPlot.Demos
