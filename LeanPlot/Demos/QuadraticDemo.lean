import LeanPlot.Components
import LeanPlot.Plot

open Lean ProofWidgets Recharts LeanPlot.Components
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/- Plot `y = x²` on the interval `[0,1]`.  Put your cursor on the `#plot` line to render. -/
#plot mkLineChart (sample (fun x => x * x) 200 0 1) #[("y²", "#ff7f0e")] 400 400

end LeanPlot.Demos
