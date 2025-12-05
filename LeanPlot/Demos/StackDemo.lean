import LeanPlot.Specification
import LeanPlot.Plot

open LeanPlot
open LeanPlot.PlotSpec
open PlotSpec (line lines scatter bar)

/-! # StackDemo

Demonstrates the new `PlotSpec` stacking/overlay combinators introduced in
v0.3.0-alpha.

Note: The overlay validation is currently disabled while we fix data merging.
-/

/-- A linear function `y = x`. -/
@[inline] def linearSpec : PlotSpec :=
  line (fun x : Float => x) "y"

/-- A quadratic function `y = x²`. -/
@[inline] def quadraticSpec : PlotSpec :=
  line (fun x : Float => x * x) "y²"

/-- Overlay using the `+` operator provided by `[HAdd]`. -/
@[inline] def stackedPlus : PlotSpec := linearSpec + quadraticSpec

/-- Overlay using the `stack` synonym. -/
@[inline] def stackedStackFn : PlotSpec :=  linearSpec.stack quadraticSpec

-- Individual plots work fine
#plot linearSpec
#plot quadraticSpec

-- Note: Stacked plots currently have a data validation issue - the overlay
-- function concatenates data arrays but the validation expects merged rows.
-- This will be fixed in a future version.

-- For now, use the multi-line `lines` function instead:
#plot (lines #[("y", fun x : Float => x), ("y²", fun x : Float => x * x)])
