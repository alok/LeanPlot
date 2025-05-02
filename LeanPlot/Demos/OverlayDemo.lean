import LeanPlot.Components
import LeanPlot.Plot

open Lean ProofWidgets Recharts LeanPlot.Components
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/- Overlay of `y = x` and `y = x²` with built-in legend. Put the cursor on the `#plot` line below to render. -/
def overlayLegend : Html :=
  let data := sampleMany #[("y", fun x => x), ("y²", fun x => x * x)] 200 0 1
  mkLineChartFull data #[("y", "#1f77b4"), ("y²", "#ff7f0e")] none none 400 400

#plot overlayLegend








end LeanPlot.Demos
