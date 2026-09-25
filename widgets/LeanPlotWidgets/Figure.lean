import LeanPlotWidgets.Html

/-!
# `#figure`: show a figure in the infoview

```lean
open LeanPlot in
#figure Figure.new |>.axis 1 1 (Axis2.new (title := "sin") |>.linesFn Float.sin (Num.range 0 10 101))
```

`#figure e` elaborates `e`, evaluates it, renders the scene with LeanPlot's own SVG writer and
shows the result in the infoview as a ProofWidgets panel (put the cursor on the command). `e`
may be a `Figure`, a `Scene`, an `Axis2` or `Axis3` (shown alone in a default figure), or an
`IO` action returning one of these (e.g. a figure built from a data file). A doc comment on the
command becomes the figure's caption:

```lean
/-- Damped oscillation. -/
#figure Quick.lines xs ys
```

`set_option leanplot.widgets.format "png"` (or `"inline"`) switches the embedding format; see
`LeanPlot.Widgets.Format`. Nothing is written to disk.
-/

namespace LeanPlot.Widgets

open Lean Elab Command Term Meta ProofWidgets

register_option leanplot.widgets.format : String := {
  defValue := "svg"
  descr := "How #figure and #plot embed figures in the infoview: \"svg\" (an <img> with an SVG data URI), \
    \"png\" (an <img> with a PNG data URI) or \"inline\" (inline SVG markup)"
}

/-- Values `#figure` can show: anything with a `ToScene` instance, or an `IO` action returning
one (analogous to ProofWidgets' `HtmlEval` and core's `MonadEval`). -/
class FigureEval (α : Type) where
  /-- Produce the scene to display. -/
  eval : α → IO Scene

instance (priority := low) {α : Type} [ToScene α] : FigureEval α := ⟨fun a => pure (ToScene.toScene a)⟩
instance {α : Type} [ToScene α] : FigureEval (IO α) := ⟨fun x => ToScene.toScene <$> x⟩

/-- Display options from the current `set_option`s and an optional caption. -/
def optionsOf (o : Lean.Options) (caption : Option String) : CoreM Widgets.Options := do
  let s := leanplot.widgets.format.get o
  match Format.ofString? s with
  | some format => return { format, caption }
  | none => throwError "leanplot.widgets.format: expected \"svg\", \"png\" or \"inline\", got {repr s}"

/-- The caption given by a command's doc comment. -/
def captionOf (doc? : Option (TSyntax ``Lean.Parser.Command.docComment)) : CommandElabM (Option String) := do
  match doc? with
  | none => return none
  | some d => return some (← getDocStringText d).trimAscii.toString

unsafe def evalSceneIOUnsafe (e : Expr) : TermElabM (IO Scene) :=
  Meta.evalExpr (IO Scene) (mkApp (mkConst ``IO) (mkConst ``LeanPlot.Scene)) e

/-- Evaluate a closed expression of type `IO Scene` (compiled and run by the interpreter). -/
@[implemented_by evalSceneIOUnsafe]
opaque evalSceneIO (e : Expr) : TermElabM (IO Scene)

/-- Elaborate `stx` against `type`; `none` (with the state and message log restored) if it does
not elaborate without errors, leaves metavariables, or contains `sorry`. -/
def elabClosed? (stx : Term) (type? : Option Expr) : TermElabM (Option Expr) :=
  commitIfNoErrors? <| withoutErrToSorry do
    let v ← elabTerm stx type?
    let v ← match type? with
      | some ty => ensureHasType ty v
      | none => pure v
    synthesizeSyntheticMVarsNoPostponing
    let v ← instantiateMVars v
    if v.hasMVar || v.hasSyntheticSorry then throwError "not closed"
    return v

/-- `FigureEval.eval v` for a closed `v`, or `none` when its type has no `FigureEval` instance. -/
def figureEvalExpr? (v : Expr) : MetaM (Option Expr) := do
  let ty ← instantiateMVars (← inferType v)
  match ← synthInstance? (mkApp (mkConst ``FigureEval) ty) with
  | some inst => return some (mkApp3 (mkConst ``FigureEval.eval) ty inst v)
  | none => return none

/-- Run an `IO Scene` expression and show the result as a panel widget on `stx`. -/
def showScene (stx : Syntax) (sceneIO : Expr) (caption : Option String) : CommandElabM Unit := do
  let io ← liftTermElabM <| evalSceneIO sceneIO
  let scene ← io
  let opts ← liftCoreM <| optionsOf (← getOptions) caption
  let ht := sceneHtml scene opts
  liftCoreM <| Widget.savePanelWidgetInfo (hash HtmlDisplayPanel.javascript)
    (return json% { html: $(← Server.rpcEncode ht) }) stx

/-- Show a figure in the infoview: `#figure e` for `e` a `Figure`, `Scene`, `Axis2`, `Axis3`,
or an `IO` action returning one. A doc comment becomes the caption. -/
syntax (name := figureCmd) (docComment)? "#figure " term : command

@[command_elab figureCmd]
def elabFigure : CommandElab := fun stx => do
  match stx with
  | `($[$doc?:docComment]? #figure $e:term) =>
    let caption ← captionOf doc?
    let sceneIO ← liftTermElabM do
      let some v ← elabClosed? e none | do
        -- re-elaborate to report the actual error
        let _ ← withoutErrToSorry (elabTermAndSynthesize e none)
        throwErrorAt e "#figure: could not elaborate the figure"
      match ← figureEvalExpr? v with
      | some x => pure x
      | none =>
        throwErrorAt e m!"#figure: cannot display a value of type{indentExpr (← inferType v)}\n\
          expected a Figure, Scene, Axis2 or Axis3, or an IO action returning one"
    showScene stx sceneIO caption
  | _ => throwUnsupportedSyntax

end LeanPlot.Widgets
