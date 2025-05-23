import LeanPlot.GrammarOfGraphics
import LeanPlot.Plot
import LeanPlot.Specification
import Lean.Data.Json

/-! # Grammar of Graphics Demo

Demonstrates the functional Grammar of Graphics DSL for building plots.
-/

namespace LeanPlot.Demos

open LeanPlot.GrammarOfGraphics
open LeanPlot
open Lean

/-- Simple line plot using the Grammar of Graphics DSL -/
def grammarLineDemo : PlotSpec :=
  plotLine (fun x => x^2)
    |> PlotSpec.withTitle "Quadratic Function"
    |> PlotSpec.withXLabel "x"
    |> PlotSpec.withYLabel "y = x²"
    |> PlotSpec.withSize 500 300

#plot grammarLineDemo

/-- Scatter plot with domain control -/
def grammarScatterDemo : PlotSpec :=
  let points := #[(1, 1), (2, 4), (3, 9), (4, 16), (5, 25), (6, 36), (7, 49), (8, 64)]
    |>.map (fun (x, y) => (x.toFloat, y.toFloat))
  plotScatter points
    |> PlotSpec.withTitle "Exponential Growth"
    |> PlotSpec.withYLabel "y"
    |> PlotSpec.withYDomain 0 70

#plot grammarScatterDemo

/-- Bar plot using the DSL -/
def grammarBarDemo : PlotSpec :=
  let categories := #[(1, 23), (2, 45), (3, 38), (4, 52), (5, 41)]
    |>.map (fun (x, y) => (x.toFloat, y.toFloat))
  plotBar categories "Sales"
    |> PlotSpec.withTitle "Sales by Quarter"
    |> PlotSpec.withXLabel "Quarter"
    |> PlotSpec.withYLabel "Sales ($1000s)"

#plot grammarBarDemo

/-- Advanced mixed chart demo -/
def grammarMixedDemo : PlotSpec :=
  -- Create base data
  let months := (List.range 12).toArray.map (·.toFloat + 1)
  let actualSales := months.zip (months.map fun m => 100 + 10 * m + 5 * Float.sin (m * Float.pi / 6))
  let forecast := months.zip (months.map fun m => 100 + 10 * m)

  -- Start with bar chart for actual sales
  plotBar actualSales "Actual Sales"
    |> addScatter forecast "Forecast" (some "#82ca9d")
    |> PlotSpec.withTitle "Sales Analysis: Actual vs Forecast"
    |> PlotSpec.withXLabel "Month"
    |> PlotSpec.withYLabel "Sales ($1000s)"
    |> PlotSpec.withSize 600 400

#plot grammarMixedDemo

/-- Area plot demonstration -/
def grammarAreaDemo : PlotSpec :=
  plotArea (fun x => 10 * Float.exp (-x^2)) "Density" 200 (some (-3, 3))
    |> PlotSpec.withTitle "Gaussian Distribution"
    |> PlotSpec.withXLabel "Standard Deviations"
    |> PlotSpec.withYLabel "Probability Density"

#plot grammarAreaDemo

/-- Multiple functions composed together -/
def grammarMultiLineDemo : PlotSpec :=
  plotLines #[
    ("sin", fun x => Float.sin (2 * Float.pi * x)),
    ("cos", fun x => Float.cos (2 * Float.pi * x)),
    ("sin²", fun x => Float.sin (2 * Float.pi * x)^2)
  ] 200 (some (0, 2))
    |> PlotSpec.withTitle "Trigonometric Functions"
    |> PlotSpec.withXLabel "x"
    |> PlotSpec.withYLabel "y"
    |> PlotSpec.withSize 600 400

#plot grammarMultiLineDemo

/-- Using the alternative >> syntax -/
def grammarAltSyntaxDemo : PlotSpec :=
  plotLine (fun x => x^3 - x) "f(x)" 200 (some (-2, 2))
    >> PlotSpec.withTitle "Cubic Function"
    >> PlotSpec.withXLabel "x"
    >> PlotSpec.withYLabel "y = x³ - x"
    >> PlotSpec.withLegend false

#plot grammarAltSyntaxDemo

end LeanPlot.Demos
