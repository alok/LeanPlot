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

-- ✨ Example 4: Without parentheses also works
#plot fun x => Float.tanh (x - 1)

-- ✨ Example 5: With type annotations
#plot (fun x : Float => x^3 - 2*x)

/-! ## How It Works

When you `import LeanPlot.DSL`, it adds macro rules that:
1. Detect when you write `#plot f` where `f` is a lambda function
2. Automatically wrap it in `LeanPlot.API.plot`
3. Handle the `using n` syntax for custom sample counts

The implementation uses `macro_rules` to extend the existing `plotCmd` syntax,
so it integrates seamlessly without conflicts.

## Backwards Compatibility

All existing code continues to work:
- `#plot (plot (fun x => x^2))` ✓
- `#html plot (fun x => x^2)` ✓
- `#plot someHtmlValue` ✓

But now you can also just write:
- `#plot (fun x => x^2)` ✨
- `#plot fun x => x^2` ✨
- `#plot (fun t => Float.sin t) using 400` ✨

It's that simple!
-/

end LeanPlot.Demos.SimpleSyntaxDemo
