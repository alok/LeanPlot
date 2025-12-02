import ProofWidgets.Component.HtmlDisplay
import Lean.Elab.Command

/-! # {lit}`#plot` command

{lit}`#plot t` behaves exactly like {lit}`#html t` from ProofWidgets but is namespaced
under LeanPlot.  The intention is that users write chart-producing code in the
term position and render it with a dedicated keyword that improves discoverability.

At the moment we forward directly to {lit}`ProofWidgets.HtmlDisplay`; future
versions are free to add additional preprocessing (e.g. auto-sampling of
{lit}`Float → β` functions) without breaking user code that has already adopted
{lit}`#plot`.

# Doc strings as captions

You can add a doc comment before {lit}`#plot` to display a caption/legend.
The doc string appears as a title above the chart on hover.
-/

namespace LeanPlot.PlotCommand
open Lean Server ProofWidgets
open scoped ProofWidgets.Jsx

/-- Wrap an {lean}`Html` value with a caption title. -/
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

/-- Any term {lit}`t` that can be evaluated to {lean}`Html` (via {name}`ProofWidgets.HtmlEval`)
can be displayed with {lit}`#plot t`.  Optionally prefix with a doc comment to
add a caption.  This mirrors the behavior of {lit}`#html`. -/
syntax (name := plotCmd) (docComment)? "#plot " term : command

open Elab Command ProofWidgets.HtmlCommand in
/-- The {lit}`#plot` command is an alias for {lit}`#html`.  It is namespaced under
{lit}`LeanPlot` to improve discoverability. When preceded by a doc comment,
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
