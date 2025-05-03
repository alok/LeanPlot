import LeanPlot.API
import LeanPlot.Plot

open Lean ProofWidgets LeanPlot.API
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

-- Plot `y = x³` on `[0,1]` using the zero-config helper.
#plot (lineChart (fun x : Float => x * x * x))

end LeanPlot.Demos
