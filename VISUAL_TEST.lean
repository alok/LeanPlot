import LeanPlot.API

open LeanPlot.API

/-! 
# 🎯 SMART PLOTTING VISUAL TEST

Open this file in VS Code and look at the infoview to see the plots!
Each plot should show automatic axis labels, colors, and styling.
-/

-- Test 1: Simple quadratic with automatic "x" and "f(x)" labels
#plot plot (fun x => x^2)

-- Test 2: Sine function - should automatically detect this as time-based  
#plot plot (fun t => Float.sin t)

-- Test 3: Multiple functions with automatic legend and different colors
#plot plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- Test 4: Custom domain
#plot plot (fun x => Float.tanh x) (domain := some (-3.0, 3.0))

-- Test 5: Scatter plot
#plot scatter (fun x => x + 0.2 * Float.sin (5 * x)) (steps := 30)

-- Test 6: Bar chart
#plot bar (fun i => i^2) (steps := 8) (domain := some (0.0, 3.0))

/-! 
Expected Results:
- All plots should render with beautiful automatic styling
- Axis labels should be meaningful ("x", "f(x)", etc.)
- Multiple function plots should have legends
- Colors should be automatically assigned
- No manual configuration needed!
-/
