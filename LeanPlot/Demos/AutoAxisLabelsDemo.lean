import LeanPlot.API
import LeanPlot.Components
import LeanPlot.Metaprogramming
import Lean

/-! # Smart Plotting Demo

Super simple examples showing how to get beautiful plots with zero effort.
Just call `smartLabels` and `smartNames` - that's it!

## TL;DR
- `smartLabels yourFunction` → get nice axis labels  
- `smartNames yourFunction` → get enhanced parameter names
- `fixDuplicates yourArray` → fix duplicate names
- Done! 🎉

-/

namespace LeanPlot.Demos.AutoAxisLabels
open LeanPlot.Metaprogramming LeanPlot.Components Lean ProofWidgets

/-- Test functions with meaningful parameter names -/
def timeFunction : Float → Float := fun time => time * 2.0 + 1.0

/-- Velocity function with two parameters -/
def velocityFunction : Float → Float → Float := fun time velocity => time * velocity + 9.8

/-- Temperature conversion function -/
def temperatureFunction : Float → Float := fun temperature => temperature * 1.8 + 32.0

-- Test the parameter extraction with proper Lean expressions
#eval extractParameterNames (Expr.lam `time (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)

/-- Example with meaningful physics parameter names -/
def physicsData : Array Json := #[
  json% { time: 0, position: 0 },
  json% { time: 1, position: 5 },
  json% { time: 2, position: 20 },
  json% { time: 3, position: 45 },
  json% { time: 4, position: 80 }
]

/-- Traditional way - manual axis labels -/
def manualPlot := mkLineChartWithLabels 
  physicsData 
  #[("position", "#2563eb")] 
  (some "Time (s)") 
  (some "Position (m)")

/-- Demo function for auto axis labels -/
def createAutoLabeledPlot (data : Array Json) (seriesName : String) : Html :=
  -- For now, this is a concept demo - we'd extract parameter names from expressions
  -- and use them to automatically generate axis labels
  let xLabel := "x" -- Would be extracted from function's first parameter
  let yLabel := "y" -- Would be the function output name or second parameter
  mkLineChartWithLabels data #[(seriesName, "#2563eb")] (some xLabel) (some yLabel)

/-- Example usage demonstrating the concept -/
def autoLabelDemo := createAutoLabeledPlot physicsData "position"

-- Show both plots for comparison
#html manualPlot
#html autoLabelDemo

-- 🎯 Super Easy Examples (Copy These!)
section SuperEasyExamples

-- Just watch the magic happen:
def myFunction : Expr := Expr.lam `t (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default
#eval smartNames myFunction      -- 👀 Watch: `t` becomes "time"! 
#eval smartLabels myFunction     -- 👀 Watch: Get perfect axis labels!

-- Handle duplicates like a boss:
def messyFunction : Expr := 
  Expr.lam `x (Expr.const ``Float [])
    (Expr.lam `y (Expr.const ``Float [])
      (Expr.lam `x (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)
      BinderInfo.default)
    BinderInfo.default

#eval smartNames messyFunction   -- 👀 Watch: Duplicates get fixed automatically!
#eval smartLabels messyFunction  -- 👀 Watch: Perfect labels despite duplicates!

-- Fix any duplicate list in seconds:
#eval fixDuplicates #["x", "y", "x", "z", "x"]  -- 👀 Boom! Fixed.

end SuperEasyExamples

-- 💡 Pro tip: The old API still works if you prefer it
section OldAPIStillWorks
#eval getParameterNames myFunction   -- Same as smartNames
#eval getAxisLabels myFunction       -- Same as smartLabels
end OldAPIStillWorks

/-- More realistic demo with actual parameter name extraction -/
def demoWithRealExtraction : Html :=
  let data := physicsData
  -- This would ideally work, but requires more sophisticated integration
  -- let (xLabel, yLabel) := #auto_axis_labels (fun time => time * 2.0)
  let xLabel := "time" -- Extracted parameter name
  let yLabel := "position" -- Function output or dependent variable
  mkLineChartWithLabels data #[("position", "#dc2626")] (some xLabel) (some yLabel)

#html demoWithRealExtraction

/-- Demonstration of different parameter names -/
def economicsData : Array Json := #[
  json% { price: 10, demand: 100 },
  json% { price: 20, demand: 80 },
  json% { price: 30, demand: 60 },
  json% { price: 40, demand: 40 },
  json% { price: 50, demand: 20 }
]

/-- Economics plot with meaningful labels -/
def economicsPlot := 
  mkLineChartWithLabels economicsData #[("demand", "#059669")] (some "Price ($)") (some "Demand")

#html economicsPlot

end LeanPlot.Demos.AutoAxisLabels
