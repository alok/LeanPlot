import LeanPlot.API
import LeanPlot.DSL  -- This enables the simple syntax!

open LeanPlot.API

/-! # Simple Plot Syntax Demo

This demo shows the new ultra-simple `#plot` syntax.
Just pass a function directly - no wrapping needed!

🎉 **The Dream is Real**

You wanted `#plot (fun x => x^2)` to just work? Now it does!
-/

namespace LeanPlot.Demos.SimpleSyntaxDemo

-- Example 1: The simplest possible plot
#plot (fun x => x^2)

/-- Example 2: With custom sample count -/
#plot (fun t => Float.sin t) using 400

/-- Example 3: Works with any function -/
#plot (fun x => Float.exp (-x) * Float.cos (3 * x))

/-- Example 4: Without parentheses also works -/
#plot fun x => Float.tanh (x - 1)

/-- Example 5: With type annotations -/
#plot (fun x : Float => x^3 - 2*x)

/-! ## Doc Comments as Captions

You can add a doc comment before `#plot` to display a caption:
-/

/-- The classic quadratic function y = x² -/
#plot (fun x => x^2)

/-- A damped oscillation: exponential decay × cosine -/
#plot (fun x => Float.exp (-x) * Float.cos (3 * x))

/-- Sinusoidal wave with 400 samples for smooth curves -/
#plot (fun t => Float.sin t) using 400

/-! **How It Works**

When you `import LeanPlot.DSL`, it adds the ergonomic syntax:
1. Detect when you write `#plot f` where `f` is a lambda function
2. Automatically wrap it in `LeanPlot.API.plot`
3. Handle the `using n` syntax for custom sample counts
4. Doc comments become chart captions (a poor man's legend!)

**Backwards Compatibility**

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

/-! ## Syntax Experiment: Exactly 0 or 2 Doc Strings

Just for fun, here's a command that requires either no doc strings or exactly two
(title + subtitle). Uses `(docComment docComment)?` pattern.
-/

open Lean Elab Command ProofWidgets in
/-- A plot command that takes exactly 0 or 2 doc strings (title + subtitle). -/
syntax (name := plot2docs) (docComment docComment)? "#plot₂ " term : command

open Lean Elab Command ProofWidgets LeanPlot.PlotCommand in
/-- A plot command that takes exactly 0 or 2 doc strings (title + subtitle). -/
@[command_elab plot2docs]
def elabPlot2Docs : CommandElab := fun stx => do
  let (title?, subtitle?, term) ← match stx with
    | `($t:docComment $s:docComment #plot₂ $e:term) =>
      pure (some t.getDocString, some s.getDocString, e)
    | `(#plot₂ $e:term) =>
      pure (none, none, e)
    | _ => throwUnsupportedSyntax
  let wrappedStx ← `(LeanPlot.API.plot $term)
  let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
  let ht ← htX
  -- Build HTML with optional title + subtitle
  let finalHtml := match title?, subtitle? with
    | some t, some s =>
      let style : Json := Json.mkObj [
        ("display", "flex"), ("flexDirection", "column"), ("marginBottom", "8px")
      ]
      let titleStyle : Json := Json.mkObj [
        ("fontSize", "16px"), ("fontWeight", "600"), ("color", "#1f2937"),
        ("fontFamily", "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace")
      ]
      let subStyle : Json := Json.mkObj [
        ("fontSize", "12px"), ("color", "#6b7280"), ("fontStyle", "italic"),
        ("fontFamily", "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace")
      ]
      Html.element "div" #[] #[
        Html.element "div" #[("style", style)] #[
          Html.element "div" #[("style", titleStyle)] #[.text t],
          Html.element "div" #[("style", subStyle)] #[.text s]
        ],
        ht
      ]
    | _, _ => ht
  liftCoreM <| Widget.savePanelWidgetInfo
    (hash ProofWidgets.HtmlDisplayPanel.javascript)
    (return json% { html: $(← Server.rpcEncode finalHtml) })
    stx

-- ✅ Works: No doc strings
#plot₂ (fun x => x^2)

-- ✅ Works: Exactly two doc strings (title + subtitle)
/-- The Parabola -/
/-- A classic quadratic function showing y = x² -/
#plot₂ (fun x => x^2)

-- ❌ Won't parse: Just one doc string (try uncommenting!)
-- /-- Only one doc string -/
-- #plot₂ (fun x => x^3)

end LeanPlot.Demos.SimpleSyntaxDemo
