import LeanPlot.API
import LeanPlot.DSL
import ProofWidgets.Component.HtmlDisplay

/-! # Simple Plotting Demo - Zero-Configuration Plots

This demo shows the simple plotting functions that automatically handle
everything for you. No more thinking about axis labels, colors, or configuration!

**For Beginners: Just Copy These Examples!**

The new `LeanPlot.API.plot` function is now the recommended way to create plots.
It automatically:
- Picks nice axis labels
- Uses beautiful colors
- Handles everything for you

**Quick Start Examples**
-/

namespace LeanPlot.Demos.SimplePlottingDemo

open LeanPlot.API

-- Example 1: Simple function - everything is automatic!
#plot (fun x => x^2+5)

-- Example 2: Time function - gets "time" labels automatically
#plot (fun t => Float.sin t)

-- Example 3: Multiple functions with automatic legend and colors
#html plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- Example 4: Scatter plot - automatic styling
#html scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 50)

-- Example 5: Bar chart - perfect for discrete data
#html bar (fun i => Float.floor (i * 5)) (steps := 10)

-- Example 6: Custom domain - just specify the range you want
#plot (fun t => Float.exp (-t) * Float.sin (5 * t)) using 200

-- Example 7: More samples for smoother curves
#plot (fun x => Float.tanh (x - 1)) using 500

-- Example 8: Compare different functions easily
#html plotMany #[
  ("linear", fun x => x),
  ("quadratic", fun x => x^2),
  ("cubic", fun x => x^3)
] (domain := (-1.0, 1.0))

/-! **The Old Way vs The New Way**

Old way (still works but more verbose):

```
let data := LeanPlot.Components.sample (fun x => x^2) 200 (some (0.0, 1.0))
let seriesStrokes := #[("y", "#2563eb")]
LeanPlot.Components.mkLineChartWithLabels data seriesStrokes (some "x") (some "y") 400 400
```

New way (recommended - just works!):

```
plot (fun x => x^2)
```

The new way is:
- 10x shorter to write
- Automatic beautiful styling
- Smart axis labels
- Perfect for beginners
- Still customizable when needed

**Automatic Features**

1. **Automatic Colors**: Each series gets a different beautiful color automatically
2. **Automatic Labels**: Parameter names become nice axis labels
3. **Automatic Domains**: Sensible defaults with easy customization
4. **Automatic Styling**: Professional look with zero effort

**Functions:**

- `plot` - For single functions (most common)
- `plotMany` - For comparing multiple functions
- `scatter` - For point plots
- `bar` - For discrete/categorical data

That's it!
-/

end LeanPlot.Demos.SimplePlottingDemo
