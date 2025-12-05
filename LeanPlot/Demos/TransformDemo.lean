import LeanPlot.Plot
import LeanPlot.Specification
import LeanPlot.Transform

namespace LeanPlot.Demos

open LeanPlot
open LeanPlot.Transform
open LeanPlot.PlotSpec (line lines scatter bar area)

/-- Demonstrate log-transformed exponential function -/
def logTransformDemo : PlotSpec :=
  -- Original exponential function transformed with log scale
  let expFn := fun x => Float.exp (x * 2)
  let transformedFn := transformFunction expFn linearScale (logScale 10.0)

  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (lines #[
          ("exp(2x)", expFn),
          ("log₁₀(exp(2x))", transformedFn)
        ] (domainOpt := some (0, 3)))
        "Exponential Function with Log Transform")
      "x")
    "y"

#plot logTransformDemo

/-- Demonstrate square root scale for quadratic data -/
def sqrtScaleDemo : PlotSpec :=
  let quadFn := fun x => x * x
  let sqrtTransformed := transformFunction quadFn linearScale sqrtScale

  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (lines #[
          ("x²", quadFn),
          ("√(x²)", sqrtTransformed)
        ] (domainOpt := some (0, 5)))
        "Square Root Scale Transform")
      "x")
    "y"

#plot sqrtScaleDemo

/-- Demonstrate symlog scale for data spanning positive and negative -/
def symlogDemo : PlotSpec :=
  let sinhFn := fun x => Float.sinh x
  let symlogTransformed := transformFunction sinhFn linearScale (symlogScale 1.0)

  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (lines #[
          ("sinh(x)", sinhFn),
          ("symlog(sinh(x))", symlogTransformed)
        ] (domainOpt := some (-3, 3)))
        "Symlog Transform (handles ± values)")
      "x")
    "y"

#plot symlogDemo

/-- Demonstrate data normalization -/
def normalizationDemo : PlotSpec :=
  -- Generate some data points
  let xVals := (List.range 20).toArray.map (fun i => i.toFloat * 0.5)
  let yVals := xVals.map (fun x => Float.sin (x * 0.5) * Float.exp (-x * 0.1) + 2)
  let normalizedY := normalize yVals

  -- Convert to functions for lines
  let originalFn := fun x =>
    let idx := (x * 2).round.toUInt32.toNat
    if h : idx < yVals.size then yVals[idx] else 0
  let normalizedFn := fun x =>
    let idx := (x * 2).round.toUInt32.toNat
    if h : idx < normalizedY.size then normalizedY[idx] else 0

  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (lines #[
          ("Original", originalFn),
          ("Normalized [0,1]", normalizedFn)
        ] (domainOpt := some (0, 9.5)))
        "Data Normalization Example")
      "x")
    "y"

#plot normalizationDemo

/-- Demonstrate smoothing with moving average -/
def smoothingDemo : PlotSpec :=
  -- Generate noisy data
  let xVals := (List.range 50).toArray.map (fun i => i.toFloat * 0.2)
  let noisyY := xVals.map (fun x =>
    Float.sin (x) + 0.3 * Float.sin (10 * x) -- Signal + noise
  )
  let smoothedY := smoothMovingAverage 5 noisyY

  -- Convert to functions
  let noisyFn := fun x =>
    let idx := (x * 5).round.toUInt32.toNat
    if h : idx < noisyY.size then noisyY[idx] else 0
  let smoothFn := fun x =>
    let idx := (x * 5).round.toUInt32.toNat
    if h : idx < smoothedY.size then smoothedY[idx] else 0

  PlotSpec.withYLabel
    (PlotSpec.withXLabel
      (PlotSpec.withTitle
        (lines #[
          ("Noisy Signal", noisyFn),
          ("Smoothed (MA=5)", smoothFn)
        ] (domainOpt := some (0, 9.8)) (colors? := some #["#cccccc", "#2ecc71"]))
        "Moving Average Smoothing")
      "x")
    "y"

#plot smoothingDemo

end LeanPlot.Demos
