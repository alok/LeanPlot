import LeanPlot.Components
import LeanPlot.Plot
import LeanPlot.Palette

open Lean ProofWidgets Recharts LeanPlot.Components LeanPlot.Palette
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/-- Overlay of `y = x` and `y = x²` with a built-in legend.  Use
`#plot overlayLegend` in the infoview to render the chart. -/
def overlayLegend : Html :=
  let names := #["y", "y²"]
  let data := sampleMany #[("y", fun x => x), ("y²", fun x => x * x)]
  mkLineChartFull data (autoColours names) none none 400 400

#plot overlayLegend


/-- Archimedes' constant with sufficient precision for the purposes of these
demos.  We redeclare it here rather than depend on Mathlib. -/
def Float.pi : Float := 3.14159265358979323846

/-- Plot of the sine function on the interval `[-2π, 2π]`. -/
def sinChart : Html :=
  let data := sampleMany #[("sin", fun x => Float.sin x)] (min := -2 * Float.pi) (max := 2 * Float.pi)
  mkLineChartFull data (autoColours #["sin"])

#plot sinChart

end LeanPlot.Demos
