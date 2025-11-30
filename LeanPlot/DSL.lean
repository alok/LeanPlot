import LeanPlot.Plot
import LeanPlot.API
import Lean
import ProofWidgets.Component.HtmlDisplay

/-!
# LeanPlot.DSL – Ultra-simple `#plot` syntax

This module implements the ergonomic plotting syntax:

```lean
#plot (fun x => x^2)            -- default 200 samples
#plot (fun t => Float.sin t) using 400
```

With doc comments as captions:

```lean
/-- A damped oscillator -/
#plot (fun t => Float.exp (-t) * Float.sin (5 * t)) using 200
```

We intercept the #plot command and check if the argument looks like
a function. If so, we wrap it with LeanPlot.API.plot automatically.
-/

namespace LeanPlot.DSL

open Lean Elab Command Term
open ProofWidgets
open LeanPlot.PlotCommand (withCaption)

-- Store the original elaborator before removing it
private def originalElabPlotCmd := LeanPlot.PlotCommand.elabPlotCmd

-- Remove the original elaborator
attribute [-command_elab] LeanPlot.PlotCommand.elabPlotCmd

/-- Syntax for `#plot` with explicit sample count: `#plot f using 400` -/
syntax (name := plotCmdUsing) (docComment)? "#plot " term " using " num : command

/-- Elaborator for the basic `#plot` command. Wraps functions with `LeanPlot.API.plot`. -/
@[command_elab LeanPlot.PlotCommand.plotCmd]
def elabPlotNew : CommandElab := fun stx => do
  -- Extract doc comment and term from syntax
  let (doc?, term) ← match stx with
    | `($doc:docComment #plot $t:term) => pure (some doc, t)
    | `(#plot $t:term) => pure (none, t)
    | _ => throwUnsupportedSyntax
  -- Try to wrap with plot first (but avoid recursion)
  try
    -- Create the wrapped term
    let wrappedStx ← `(LeanPlot.API.plot $term)
    -- Evaluate it directly as Html
    let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
    let ht ← htX
    -- Wrap with caption if doc comment present
    let finalHtml := match doc? with
      | some doc => withCaption doc.getDocString ht
      | none => ht
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash ProofWidgets.HtmlDisplayPanel.javascript)
      (return json% { html: $(← Server.rpcEncode finalHtml) })
      stx
  catch _ =>
    -- If that fails, use the original implementation
    originalElabPlotCmd stx

/-- Elaborator for the `#plot ... using N` syntax with explicit sample count. -/
@[command_elab plotCmdUsing]
def elabPlotUsing : CommandElab := fun stx => do
  -- Extract doc comment, term, and sample count from syntax
  let (doc?, term, n) ← match stx with
    | `($doc:docComment #plot $t:term using $num) => pure (some doc, t, num)
    | `(#plot $t:term using $num) => pure (none, t, num)
    | _ => throwUnsupportedSyntax
  -- Create the wrapped term with steps
  let wrappedStx ← `(LeanPlot.API.plot $term (steps := $n))
  -- Evaluate it directly as Html
  let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
  let ht ← htX
  -- Wrap with caption if doc comment present
  let finalHtml := match doc? with
    | some doc => withCaption doc.getDocString ht
    | none => ht
  liftCoreM <| Widget.savePanelWidgetInfo
    (hash ProofWidgets.HtmlDisplayPanel.javascript)
    (return json% { html: $(← Server.rpcEncode finalHtml) })
    stx

end LeanPlot.DSL

-- Re-export for convenience
export LeanPlot.API (plot plotMany scatter bar)
