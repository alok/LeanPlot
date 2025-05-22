import LeanPlot.Plot
-- import LeanPlot.API -- No longer needed directly
import LeanPlot.Specification -- Import the new spec module

namespace LeanPlot.Demos

open LeanPlot -- Open top-level namespace for 'line' constructor
open LeanPlot.PlotSpec -- Open for combinators like 'withTitle' if needed


-- NOTE: Using the pipe operator `|>` currently causes build errors (application type mismatch).
-- Explicit function application works: #plot (withXLabel (withTitle (line (fun x => x)) "Linear Function (y=x)") "Input (x)")
-- Keeping the pipe syntax here as the desired target.
-- Reverting to explicit function application to fix build error
-- Further isolating the spec definition

/-- Plot `y = x` on the interval `[0,1]`. Uses the new PlotSpec workflow.
Put your cursor on the `#plot` line to render. -/
def linearPlotSpecification : PlotSpec :=
  withXLabel
    (withTitle
      (line (fun x => x) (name := "y=x")) 
      "Linear Function (y=x)")
    "Input (x)"

#plot linearPlotSpecification

#plot line (fun x => x^2)


end LeanPlot.Demos
