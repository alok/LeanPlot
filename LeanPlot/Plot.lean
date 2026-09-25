import ProofWidgets.Component.HtmlDisplay
import Lean.Elab.Command
import LeanPlot.Specification
import LeanPlot.Series
import LeanPlot.TunablePlot

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
add a caption.  This mirrors the behavior of {lit}`#html`. Use {lit}`+tunable`
to expose interactive controls that write parameters back to source. -/
syntax (name := plotCmd) (docComment)? "#plot" ("+tunable")? term : command

private def jsonNumToFloat? (j : Json) : Option Float :=
  match j.getNum? with
  | Except.ok n => some n.toFloat
  | Except.error _ => none

private def axisDomainToFloats? (axis? : Option AxisSpec) : Option (Array Float) :=
  match axis?.bind (·.domain) with
  | some arr =>
    if arr.size == 2 then
      match jsonNumToFloat? arr[0]!, jsonNumToFloat? arr[1]! with
      | some lo, some hi => some #[lo, hi]
      | _, _ => none
    else none
  | none => none

open Elab Command ProofWidgets.HtmlCommand in
/-- The {lit}`#plot` command is an alias for {lit}`#html`.  It is namespaced under
{lit}`LeanPlot` to improve discoverability. When preceded by a doc comment,
the doc string is displayed as a caption above the chart.  -/
@[command_elab plotCmd]
unsafe def elabPlotCmd : CommandElab := fun stx => do
  -- Pattern match the syntax to extract optional doc comment + tunable flag
  let (doc?, tunable, term) ← match stx with
    | `($doc:docComment #plot +tunable $t:term) => pure (some doc, true, t)
    | `(#plot +tunable $t:term) => pure (none, true, t)
    | `($doc:docComment #plot $t:term) => pure (some doc, false, t)
    | `(#plot $t:term) => pure (none, false, t)
    | _ => throwError "Unexpected syntax {stx}."

  if tunable then
    -- Evaluate the term into PlotSpec via typeclass conversion
    let spec ← liftTermElabM <| do
      let wrapped ← `(LeanPlot.toPlotSpec $term)
      let expr ← Lean.Elab.Term.elabTerm wrapped (some (mkConst ``PlotSpec))
      Lean.Elab.Term.synthesizeSyntheticMVarsNoPostponing
      Lean.Meta.evalExpr PlotSpec (mkConst ``PlotSpec) expr

    let series := spec.series.map fun s =>
      { name := SeriesDSpecPacked.name s
        dataKey := SeriesDSpecPacked.dataKey s
        kind := SeriesDSpecPacked.typeString s
        color := SeriesDSpecPacked.color s
        dot := SeriesDSpecPacked.dot? s |>.getD false
      : LeanPlot.TunablePlot.TunableSeries }

    -- Get document info for edits
    let fileMap ← getFileMap
    let some range := fileMap.lspRangeOfStx? stx
      | throwError "Could not determine source range"
    let lineNum := range.start.line
    let fileName ← getFileName
    let uri := s!"file://{fileName}"

    let props : LeanPlot.TunablePlot.TunablePlotProps := {
      data := Json.arr spec.chartData
      series := series
      width := spec.width
      height := spec.height
      title := spec.title.getD ""
      xLabel := spec.xAxis.bind (·.label) |>.getD ""
      yLabel := spec.yAxis.bind (·.label) |>.getD ""
      xDomain := axisDomainToFloats? spec.xAxis
      yDomain := axisDomainToFloats? spec.yAxis
      legend := spec.legend
      lineNum := lineNum
      uri := uri
      termStr := term.raw.reprint.getD (toString term)
      caption := doc?.map (·.getDocString) |>.getD ""
    }

    let msg ← liftCoreM <| MessageData.ofComponent LeanPlot.TunablePlot.TunablePlotPanel props
      s!"[Tunable Plot]"
    logInfo msg
  else
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
