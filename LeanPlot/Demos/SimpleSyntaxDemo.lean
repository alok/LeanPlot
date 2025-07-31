import LeanPlot.API
import LeanPlot.DSL  -- This enables the simple syntax!

open LeanPlot.API

/-! # Simple Plot Syntax Demo

This demo shows the new ultra-simple `#plot` syntax.
Just pass a function directly - no wrapping needed!

## 🎉 The Dream is Real

You wanted `#plot (fun x => x^2)` to just work? Now it does!
-/

namespace LeanPlot.Demos.SimpleSyntaxDemo

-- Example 1: Simple plot
#html plot (fun x => x^2)

-- Example 2: With custom sample count
#html plot (fun t => Float.sin t) (steps := 400)

-- Example 3: Works with any function
#html plot (fun x => Float.exp (-x) * Float.cos (3 * x))

-- Example 4: Another example
#html plot (fun x => Float.tanh (x - 1))

-- Example 5: With type annotations
#html plot (fun x : Float => x^3 - 2*x)

/-! ## How It Works

When you `import LeanPlot.DSL`, it re-exports the plotting functions
for convenient access.

## Backwards Compatibility

All existing code continues to work:
- `#plot (plot (fun x => x^2))` ✓
- `#html plot (fun x => x^2)` ✓
- `#plot someHtmlValue` ✓

For now, you need to write:
- `#html plot (fun x => x^2)`

It's that simple!
-/

end LeanPlot.Demos.SimpleSyntaxDemo
