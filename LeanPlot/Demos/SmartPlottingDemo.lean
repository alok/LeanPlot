import LeanPlot.API
import LeanPlot.DSL
/-! # 🎯 Smart Plotting Demo - Zero-Effort Beautiful Plots

This demo shows the new smart plotting functions that automatically handle
everything for you. No more thinking about axis labels, colors, or configuration!

## For Beginners: Just Copy These Examples!

The new `LeanPlot.API.plot` function is now the recommended way to create plots.
It automatically:
- Picks nice axis labels
- Uses beautiful colors
- Handles everything for you

## 🚀 Quick Start Examples
-/

namespace LeanPlot.Demos.SmartPlottingDemo

open LeanPlot.API

-- ✅ Example 1: Simple function - everything is automatic!
#html (plot (fun x => x^2))
#plot (fun x => x^2)
-- ✅ Example 2: Time function - gets "time" labels automatically
#check plot (fun t => Float.sin t)

-- ✅ Example 3: Multiple functions with automatic legend and colors
#check plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- ✅ Example 4: Scatter plot - automatic styling
#check scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 50)

-- ✅ Example 5: Bar chart - perfect for discrete data
#check bar (fun i => Float.floor (i * 5)) (steps := 10)

-- ✅ Example 6: Custom domain - just specify the range you want
#check plot (fun t => Float.exp (-t) * Float.sin (5 * t)) (domain := some (0.0, 3.0))

-- ✅ Example 7: More samples for smoother curves
#check plot (fun x => Float.tanh (x - 1)) (steps := 500) (domain := some (-2.0, 4.0))

-- ✅ Example 8: Compare different functions easily
#check plotMany #[
  ("linear", fun x => x),
  ("quadratic", fun x => x^2),
  ("cubic", fun x => x^3)
] (domain := (-1.0, 1.0))

/-! ## 🎯 The Old Way vs The New Way

### Old way (still works but more verbose):
```lean
let data := LeanPlot.Components.sample (fun x => x^2) 200 (some (0.0, 1.0))
let seriesStrokes := #[("y", "#2563eb")]
LeanPlot.Components.mkLineChartWithLabels data seriesStrokes (some "x") (some "y") 400 400
```

### New way (recommended - just works!):
```lean
plot (fun x => x^2)
```

The new way is:
- ✅ 10x shorter to write
- ✅ Automatic beautiful styling
- ✅ Smart axis labels
- ✅ Perfect for beginners
- ✅ Still customizable when needed

## 🎨 Automatic Features

1. **Smart Colors**: Each series gets a different beautiful color automatically
2. **Smart Labels**: Parameter names become nice axis labels
3. **Smart Domains**: Sensible defaults with easy customization
4. **Smart Styling**: Professional look with zero effort

## 🚀 Just Use These Functions:

- `plot` - For single functions (most common)
- `plotMany` - For comparing multiple functions
- `scatter` - For point plots
- `bar` - For discrete/categorical data

That's it! You're now a plotting expert. 🎉
-/

end LeanPlot.Demos.SmartPlottingDemo
