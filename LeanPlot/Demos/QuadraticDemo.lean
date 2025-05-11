import LeanPlot.API
import LeanPlot.Plot

open Lean ProofWidgets LeanPlot.API


namespace LeanPlot.Demos

/- Plot `y = x²` on the interval `[0,1]`.  Put your cursor on the `#plot` line to render. -/
#plot (LeanPlot.API.lineChart (fun x => x * x))

end LeanPlot.Demos
