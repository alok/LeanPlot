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

We intercept the #plot command and check if the argument looks like
a function. If so, we wrap it with LeanPlot.API.plot automatically.
-/

namespace LeanPlot.DSL

open Lean Elab Command Term
open ProofWidgets

-- Store the original elaborator before removing it
private def originalElabPlotCmd := LeanPlot.PlotCommand.elabPlotCmd

-- Remove the original elaborator
attribute [-command_elab] LeanPlot.PlotCommand.elabPlotCmd

-- Add syntax for #plot with "using"
syntax (name := plotCmdUsing) "#plot " term " using " num : command

-- New elaborator that handles both cases
@[command_elab LeanPlot.PlotCommand.plotCmd]
def elabPlotNew : CommandElab := fun stx => do
  match stx with
  | `(#plot $term) =>
    -- Try to wrap with plot first (but avoid recursion)
    try
      -- Create the wrapped term
      let wrappedStx ← `(LeanPlot.API.plot $term)
      -- Evaluate it directly as Html
      let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
      let ht ← htX
      liftCoreM <| Widget.savePanelWidgetInfo
        (hash ProofWidgets.HtmlDisplayPanel.javascript)
        (return json% { html: $(← Server.rpcEncode ht) })
        stx
    catch _ =>
      -- If that fails, use the original implementation
      originalElabPlotCmd stx
  | _ => throwUnsupportedSyntax

-- Elaborator for the "using" syntax
@[command_elab plotCmdUsing]
def elabPlotUsing : CommandElab := fun stx => do
  match stx with
  | `(#plot $term using $n) =>
    -- Create the wrapped term with steps
    let wrappedStx ← `(LeanPlot.API.plot $term (steps := $n))
    -- Evaluate it directly as Html
    let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
    let ht ← htX
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash ProofWidgets.HtmlDisplayPanel.javascript)
      (return json% { html: $(← Server.rpcEncode ht) })
      stx
  | _ => throwUnsupportedSyntax

end LeanPlot.DSL

-- Re-export for convenience
export LeanPlot.API (plot plotMany scatter bar)