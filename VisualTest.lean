import LeanPlot.API
import LeanPlot.Debug
import LeanPlot.DSL

open LeanPlot.API
open LeanPlot.Debug

/-! # Visual Test of Smart Plotting System

This file contains actual plot renders for visual testing.
Open this in VS Code with the infoview to see the plots!
-/

-- 🎉 NEW SIMPLE SYNTAX - Just pass the function directly!
-- No need to wrap in `plot(...)`
#plot (fun x => x^2)

-- With custom steps
#plot (fun t => Float.sin t) using 400

-- OLD SYNTAX (still works):
-- 🎯 Test 1: Simple quadratic - should show nice blue curve with "x" and "f(x)" labels
#plot plot (fun x => x^2)

-- 🎯 Test 2: Time function - should automatically detect "time" parameter
#plot plot (fun t => Float.sin t)

-- 🎯 Test 3: Multiple functions - should show legend with different colors
#plot plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- 🎯 Test 4: Custom domain with exponential decay
#plot plot (fun t => Float.exp (-t) * Float.sin (5 * t)) (domain := some (0.0, 3.0))

-- 🎯 Test 5: Scatter plot with noise pattern
#plot scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 50)

-- 🎯 Test 6: Bar chart for discrete data
#plot bar (fun i => Float.floor (i * 5)) (steps := 10)

-- Test the debug functionality separately
#check withSavePNG (plot (fun x => x^2)) "test1" "quadratic.png"

-- 🎯 Test 7: Compare old vs new API
#plot lineChart (fun x => x^2)  -- Old way
-- vs
#plot plot (fun x => x^2)       -- New smart way

-- 🎯 Test 8: High resolution smooth curve
#plot plot (fun x => Float.tanh (x - 1)) (steps := 500) (domain := some (-2.0, 4.0))

-- 🎯 Test 9: Polynomial comparison with automatic colors
#plot plotMany #[
  ("linear", fun x => x),
  ("quadratic", fun x => x^2),
  ("cubic", fun x => x^3)
] (domain := (-1.0, 1.0))
