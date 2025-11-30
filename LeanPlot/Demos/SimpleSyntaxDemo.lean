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

-- Example 1: The simplest possible plot
#plot (fun x => x^2)

/-- Example 2: With custom sample count -/
#plot (fun t => Float.sin t) using 400

/-- Example 3: Works with any function
#plot (fun x => Float.exp (-x) * Float.cos (3 * x))

-- Example 4: Without parentheses also works
#plot fun x => Float.tanh (x - 1)

-- Example 5: With type annotations
#plot (fun x : Float => x^3 - 2*x)

/-! ## Doc Comments as Captions

You can add a doc comment before `#plot` to display a caption:
-/

/-- The classic parabola y = x² -/
#plot (fun x => x^2)

/-- A damped oscillation: exponential decay × cosine -/
#plot (fun x => Float.exp (-x) * Float.cos (3 * x))

/-- Sinusoidal wave with 400 samples for smooth curves -/
#plot (fun t => Float.sin t) using 400

/-! ## How It Works

When you `import LeanPlot.DSL`, it adds the ergonomic syntax:
1. Detect when you write `#plot f` where `f` is a lambda function
2. Automatically wrap it in `LeanPlot.API.plot`
3. Handle the `using n` syntax for custom sample counts
4. Doc comments become chart captions (a poor man's legend!)

## Backwards Compatibility

All existing code continues to work:
- `#plot (plot (fun x => x^2))` ✓
- `#html plot (fun x => x^2)` ✓
- `#plot someHtmlValue` ✓

Now you can write:
- `#plot (fun x => x^2)`
- `#plot fun x => x^2`
- `#plot (fun t => Float.sin t) using 400`
- `/-- Caption text -/ #plot (fun x => x^2)`

It's that simple!
-/

end LeanPlot.Demos.SimpleSyntaxDemo
