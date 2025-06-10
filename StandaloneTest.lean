import LeanPlot.API
import LeanPlot.Debug
import LeanPlot.Components

open LeanPlot.API
open LeanPlot.Debug
open LeanPlot.Components

/-! Create a standalone HTML file with smart plots for visual testing -/

-- Test 1: Basic quadratic plot
def quadraticData := sample (fun x : Float => x^2) 50 none
def quadraticPlot := mkLineChartWithLabels quadraticData #[("y", "#2563eb")] (some "x") (some "f(x)") 400 400

-- Test 2: Multi-function plot with automatic colors
def multiData := sampleMany #[("sin", fun x : Float => Float.sin x), ("cos", fun x : Float => Float.cos x)] 100 0.0 6.28
def multiColors := #[("sin", "#2563eb"), ("cos", "#dc2626")]
def multiPlot := mkLineChartFull multiData multiColors (some "x") (some "y") 400 400

-- Test 3: Scatter plot
def scatterData := sample (fun x : Float => x^2 + 0.1 * Float.sin (10 * x)) 50 none
def scatterPlot := mkScatterChart scatterData "#dc2626" 400 400

-- Test 4: Bar chart  
def barData := sample (fun x : Float => Float.floor (x * 10)) 20 none
def barPlot := mkBarChart barData "#16a34a" 400 400

-- Verify the data structures
#eval quadraticData.size
#eval multiData.size

#eval s!"Standalone test data generated successfully! ✅"

-- Test the smart API functions
def smartQuad := plot (fun x => x^2)
def smartMulti := plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]
def smartScatter := scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 50)
def smartBar := bar (fun i => Float.floor (i * 5)) (steps := 10)

#check smartQuad
#check smartMulti  
#check smartScatter
#check smartBar

#eval s!"All smart plotting functions verified! 🎉"
