import LeanPlot.Plot
import LeanPlot.Specification
import LeanPlot.Constants

namespace LeanPlot.Demos

open LeanPlot
open LeanPlot.PlotSpec (line lines scatter bar area)

/-- Plot y = sin(2πx) on the interval (0, 2) -/
def sineDemo : PlotSpec :=
  line (fun x => Float.sin (2 * Float.pi * x))
    (name := "sin(2πx)")
    (domainOpt := some (0, 2))

#plot sineDemo

/-- Overlay of sin and cos functions -/
def trigOverlayDemo : PlotSpec :=
  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (lines #[
          ("sin(2πx)", fun x => Float.sin (2 * Float.pi * x)),
          ("cos(2πx)", fun x => Float.cos (2 * Float.pi * x))
        ] (domainOpt := some (0, 2)))
        "Trigonometric Functions")
      "x")
    "y"

#plot trigOverlayDemo

/-- Damped sine wave: y = exp(-3x) · sin(8πx) -/
def dampedSineDemo : PlotSpec :=
  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (line (fun x => Float.exp (-3 * x) * Float.sin (8 * Float.pi * x))
          (name := "e^(-3x)·sin(8πx)")
          (domainOpt := some (0, 2)))
        "Damped Oscillation")
      "x")
    "y"

#plot dampedSineDemo

/-- Exponential growth: y = e^x -/
def exponentialGrowthDemo : PlotSpec :=
  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (line Float.exp
          (name := "e^x")
          (domainOpt := some (-2, 3)))
        "Exponential Growth")
      "x")
    "y"

#plot exponentialGrowthDemo

/-- Piecewise step function: y = floor(5x) / 5 -/
def stepFunctionDemo : PlotSpec :=
  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (line (fun x => (5 * x).floor / 5)
          (name := "⌊5x⌋ / 5")
          (domainOpt := some (0, 2))
          (steps := 500))  -- More steps for accurate step representation
        "Step Function")
      "x")
    "y"

#plot stepFunctionDemo

/-- Rational function with asymptote: y = 1 / (x - 0.5) -/
def rationalAsymptoteDemo : PlotSpec :=
  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (PlotSpec.withYDomain
          (line (fun x =>
            let denom := x - 0.5
            if Float.abs denom < 0.01 then
              if denom > 0 then 100 else -100  -- Cap near asymptote
            else 1 / denom)
            (name := "1/(x-0.5)")
            (domainOpt := some (-1, 2))
            (steps := 400))
          (-10) 10)  -- Limit y-axis range
        "Rational Function with Vertical Asymptote")
      "x")
    "y"

#plot rationalAsymptoteDemo

end LeanPlot.Demos
