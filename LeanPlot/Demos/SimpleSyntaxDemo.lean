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

-- ✨ Example 1: The simplest possible plot
#plot (fun x => x^2)

-- ✨ Example 2: With custom sample count
#plot (fun t => Float.sin t) using 400

-- ✨ Example 3: Works with any function
#plot (fun x => Float.exp (-x) * Float.cos (3 * x))

-- ✨ Example 4: Custom samples for smooth curves
#plot (fun x => Float.tanh (x - 1)) using 500

/-! ## How It Works

When you `import LeanPlot.DSL`, it adds lightweight macros that:
1. Detect when you write `#plot f` where `f` is a function
2. Automatically wrap it in `LeanPlot.API.plot`
3. Handle the `using n` syntax for custom sample counts

The macros have low priority, so if you pass something that's already
`Html`, the original `#plot` command handles it normally.

## Backwards Compatibility

All existing code continues to work:
- `#plot (plot (fun x => x^2))` ✓
- `#html plot (fun x => x^2)` ✓
- `#plot someHtmlValue` ✓

But now you can also just write:
- `#plot (fun x => x^2)` ✨

It's that simple!
-/

end LeanPlot.Demos.SimpleSyntaxDemo
