import LeanPlot.API
import LeanPlot.Debug
import LeanPlot.AutoPNG
import LeanPlot.DSL

open LeanPlot.API
open LeanPlot.Debug
open LeanPlot.AutoPNG

/-!
# Complete PNG Export Test for Smart Plotting System

This file comprehensively tests the smart plotting system with PNG export.
Open in VS Code, view in infoview, and click "Save PNG" buttons to download plots.

All plots should demonstrate:
✅ Automatic axis labels
✅ Smart parameter name detection
✅ Beautiful colors and styling
✅ Zero configuration needed
✅ PNG export capability
-/

-- ═══════════════════════════════════════════════════════════════════════════════
-- 🎯 SMART PLOTTING TESTS (No PNG Export)
-- ═══════════════════════════════════════════════════════════════════════════════

-- Test 1: Basic quadratic - should show "x" and "f(x)" labels
#plot (fun x => x^2)

-- Test 2: Time function - should automatically use "time" labels
#plot (fun t => Float.sin t)

-- Test 3: Index function - should use "index_i" label
#plot (fun i => i^3) using 20

-- Test 4: Multiple functions with automatic legend and colors
#html plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- Test 5: Scatter plot with automatic styling
#html scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 30)

-- Test 6: Bar chart with automatic styling
#html bar (fun i => Float.floor (i * 5)) (steps := 8)

-- ═══════════════════════════════════════════════════════════════════════════════
-- 📸 PNG EXPORT TESTS (Manual Save Buttons)
-- ═══════════════════════════════════════════════════════════════════════════════

-- Test 7: Quadratic with manual PNG export
#html withSavePNG (plot (fun x => x^2)) "manual-quad" "manual-quadratic.png"

-- Test 8: Multi-function with manual PNG export
#html withSavePNG (plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]) "manual-multi" "manual-trigonometric.png"

-- Test 9: Custom domain with manual PNG export
#html withSavePNG (plot (fun t => Float.exp (-t) * Float.sin (5 * t)) (domain := some (0.0, 3.0))) "manual-damped" "manual-damped-oscillation.png"

-- ═══════════════════════════════════════════════════════════════════════════════
-- 🤖 AUTOMATED PNG EXPORT TESTS
-- ═══════════════════════════════════════════════════════════════════════════════

-- Test 10: Smart plot with automated PNG export
#html smartPlotWithPNG (fun x => Float.tanh x) "tanh"

-- Test 11: Smart multi-plot with automated PNG export
#html smartMultiPlotWithPNG #[("linear", fun x => x), ("quadratic", fun x => x^2)] "polynomials"

-- Test 12: Batch of plots with automated PNG export
#html testBatch[0]!   -- Linear function
#html testBatch[1]!   -- Quadratic function
#html testBatch[2]!   -- Cubic function

-- ═══════════════════════════════════════════════════════════════════════════════
-- 🔬 ADVANCED TESTS
-- ═══════════════════════════════════════════════════════════════════════════════

-- Test 13: High resolution plot
#html withSavePNG (plot (fun x => Float.sin (x) + 0.5 * Float.sin (3 * x)) (steps := 500) (domain := some (0.0, 6.28))) "high-res" "high-resolution-wave.png"

-- Test 14: Complex mathematical function
#html withSavePNG (plot (fun x => Float.exp (-x/2) * Float.cos (2 * x)) (domain := some (0.0, 10.0))) "complex-math" "damped-cosine.png"

-- Test 15: Multiple overlapping functions
#html withSavePNG (plotMany #[
  ("sin", fun x => Float.sin x),
  ("cos", fun x => Float.cos x),
  ("sin+cos", fun x => Float.sin x + Float.cos x),
  ("sin*cos", fun x => Float.sin x * Float.cos x)
] (domain := (0.0, 6.28))) "complex-trig" "complex-trigonometric.png"

/-!
## Testing Instructions:

1. **Open this file in VS Code** with LeanPlot installed
2. **Open the infoview** (Ctrl+Shift+P → "Lean 4: Open Infoview")
3. **Scroll through the infoview** to see all the plots render
4. **Click "Save PNG" buttons** to download plot images
5. **Verify the downloaded images show:**
   - Automatic axis labels ("x", "f(x)", "time", "index_i", etc.)
   - Beautiful colors and professional styling
   - Legends for multi-function plots
   - Clean, publication-ready appearance

## Expected Results:

✅ **Smart Labels**: Parameter names should be automatically enhanced
✅ **Smart Colors**: Each series should get a different beautiful color
✅ **Smart Domains**: Sensible defaults with easy customization
✅ **PNG Export**: All plots should be downloadable as high-quality images
✅ **Zero Config**: No manual axis label or color configuration needed

This demonstrates the complete smart plotting system working end-to-end! 🎉
-/
