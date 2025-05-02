import LeanPlot.Components
import LeanPlot.Plot
import LeanPlot.Palette

open Lean ProofWidgets Recharts LeanPlot.Components LeanPlot.Palette
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/- Overlay of `y = x` and `y = x²` with built-in legend. Put the cursor on the `#plot` line below to render. -/
def overlayLegend : Html :=
  let names := #["y", "y²"]
  let data := sampleMany01 #[("y", fun x => x), ("y²", fun x => x * x)] 200
  mkLineChartFull data (autoColours names) none none 400 400

#plot overlayLegend

end LeanPlot.Demos
