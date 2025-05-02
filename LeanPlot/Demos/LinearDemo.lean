import LeanPlot.Components
import LeanPlot.Plot
import LeanPlot.Palette

open Lean ProofWidgets Recharts LeanPlot.Components LeanPlot.Palette
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/- Plot `y = x` on the interval `[0,1]`.  Put your cursor on the `#html` line to render. -/
#plot mkLineChart (sample (fun x => x) 200 0 1) (autoColours #["y"]) 400 400

end LeanPlot.Demos
