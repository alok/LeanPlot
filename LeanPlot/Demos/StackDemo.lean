import LeanPlot.Specification
import LeanPlot.Plot

open LeanPlot
open LeanPlot.PlotSpec

/-! # StackDemo

Demonstrates the new `PlotSpec` stacking/overlay combinators introduced in
v0.3.0-alpha.  Render with `#plot`. -/

/-- A linear function `y = x`. -/
@[inline] def linearSpec : PlotSpec :=
  LeanPlot.line (fun x : Float => x) "y"

/-- A quadratic function `y = x²`. -/
@[inline] def quadraticSpec : PlotSpec :=
  LeanPlot.line (fun x : Float => x * x) "y²"

/-- Overlay using the `+` operator provided by `[HAdd]`. -/
@[inline] def stackedPlus : PlotSpec := linearSpec + quadraticSpec

/-- Overlay using the `stack` synonym. -/
@[inline] def stackedStackFn : PlotSpec := PlotSpec.stack linearSpec quadraticSpec

#plot stackedPlus

#plot stackedStackFn
