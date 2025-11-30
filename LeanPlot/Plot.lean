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

## Doc strings as captions

You can add a doc comment before `#plot` to display a caption/legend:

```lean
/-- The parabola y = x² -/
#plot (fun x => x^2)
```

The doc string appears as a title above the chart on hover.
-/

namespace LeanPlot.PlotCommand
open Lean Server ProofWidgets
open scoped ProofWidgets.Jsx

/-- Wrap an `Html` value with a caption title. -/
def withCaption (caption : String) (inner : Html) : Html :=
  let captionStyle : Json := Json.mkObj [
    ("fontSize",     "14px"),
    ("fontWeight",   "500"),
    ("color",        "#374151"),
    ("marginBottom", "8px"),
    ("fontFamily",   "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace")
  ]
  let containerStyle : Json := Json.mkObj [
    ("display",       "flex"),
    ("flexDirection", "column")
  ]
  Html.element "div" #[("style", containerStyle)] #[
    Html.element "div" #[("style", captionStyle)] #[.text caption],
    inner
  ]

/-- Any term `t` that can be evaluated to `Html` (via `ProofWidgets.HtmlEval`)
can be displayed with `#plot t`.  Optionally prefix with a doc comment to
add a caption.  This mirrors the behavior of `#html`. -/
syntax (name := plotCmd) (docComment)? "#plot " term : command

open Elab Command ProofWidgets.HtmlCommand in
/-- The `#plot` command is an alias for `#html`.  It is namespaced under
`LeanPlot` to improve discoverability. When preceded by a doc comment,
the doc string is displayed as a caption above the chart.  -/
@[command_elab plotCmd]
def elabPlotCmd : CommandElab := fun stx => do
  -- Pattern match the syntax to extract optional doc comment
  let (doc?, term) ← match stx with
    | `($doc:docComment #plot $t:term) => pure (some doc, t)
    | `(#plot $t:term) => pure (none, t)
    | _ => throwError "Unexpected syntax {stx}."
  -- Evaluate the term into `Html`
  let htX ← liftTermElabM <| evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $term)
  let ht ← htX
  -- Wrap with caption if doc comment is present
  let finalHtml := match doc? with
    | some doc => withCaption doc.getDocString ht
    | none => ht
  -- Reuse the HtmlDisplayPanel widget from ProofWidgets.
  liftCoreM <| Widget.savePanelWidgetInfo
    (hash ProofWidgets.HtmlDisplayPanel.javascript)
    (return json% { html: $(← rpcEncode finalHtml) })
    stx

end LeanPlot.PlotCommand
