import LeanPlot.API
import LeanPlot.Debug

open LeanPlot.API
open LeanPlot.Debug

/-! 
# PNG Export Test for Smart Plotting

This file tests the PNG export functionality with the new smart plotting system.
Each plot should have a "Save PNG" button that downloads the plot as an image.
-/

-- Test 1: Basic smart plot with PNG export
#html withSavePNG (plot (fun x => x^2)) "smart-quad" "smart-quadratic.png"

-- Test 2: Multi-function plot with PNG export  
#html withSavePNG (plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]) "smart-multi" "smart-multi-functions.png"

-- Test 3: Scatter plot with PNG export
#html withSavePNG (scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 50)) "smart-scatter" "smart-scatter.png"

-- Test 4: Bar chart with PNG export
#html withSavePNG (bar (fun i => Float.floor (i * 5)) (steps := 10)) "smart-bar" "smart-bar.png"

-- Test 5: Custom domain plot with PNG export
#html withSavePNG (plot (fun t => Float.exp (-t) * Float.sin (5 * t)) (domain := some (0.0, 3.0))) "smart-damped" "smart-damped-oscillation.png"

/-! 
To test:
1. Open this file in VS Code with the infoview
2. Click the "Save PNG" buttons to download the plots
3. Check that the downloaded images show:
   - Automatic axis labels ("x", "f(x)", etc.)
   - Beautiful colors and styling  
   - Professional appearance
   - No manual configuration needed
-/
