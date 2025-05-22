import ProofWidgets.Component.HtmlDisplay
import Lean.Elab.Command

/-! # `#plot` command

`#plot t` behaves exactly like `#html t` from ProofWidgets but is namespaced
under LeanPlot.  The intention is that users write chart-producing code in the
term position and render it with a dedicated keyword that improves discoverability.

At the moment we forward directly to `ProofWidgets.HtmlDisplay`; future
versions are free to add additional preprocessing (e.g. auto-sampling of
`Float → β` functions) without breaking user code that has already adopted
`#plot`.
-/

namespace LeanPlot.PlotCommand
open Lean Server ProofWidgets

/-- Any term `t` that can be evaluated to `Html` (via `ProofWidgets.HtmlEval`)
can be displayed with `#plot t`.  This mirrors the behavior of `#html`. -/
syntax (name := plotCmd) "#plot " term : command

open Elab Command ProofWidgets.HtmlCommand in
/-- The `#plot` command is an alias for `#html`.  It is namespaced under
`LeanPlot` to improve discoverability.  -/
@[command_elab plotCmd]
def elabPlotCmd : CommandElab := fun
  | stx@`(#plot $t:term) => do
    -- Evaluate the term into the `Html`.
    let htX ← liftTermElabM <| evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $t)
    let ht ← htX
    -- Reuse the HtmlDisplayPanel widget from ProofWidgets.
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash ProofWidgets.HtmlDisplayPanel.javascript)
      (return json% { html: $(← rpcEncode ht) })
      stx
  | stx => throwError "Unexpected syntax {stx}."

end LeanPlot.PlotCommand
