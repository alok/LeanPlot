import LeanPlotWidgets

/-!
# Tests: `#figure` and `#plot`

`#guard_widget expected => cmd` elaborates `cmd`, collects the panel widgets it saved in the
info trees (what the infoview would display), and checks that there is exactly one, a
ProofWidgets `HtmlDisplayPanel` whose `html` prop is the wire JSON of `expected`. Together with
`LeanPlotWidgetsTest.Html` (which pins down the HTML itself, down to the SVG bytes), this checks
the commands end to end, short of a browser.
-/

namespace LeanPlotWidgetsTest.Commands

open Lean Elab Command LeanPlot LeanPlot.Widgets ProofWidgets

/-- The `html` props of the `HtmlDisplayPanel` widgets saved while elaborating `cmd`, and the
number of panel widgets of any other kind. -/
def panelHtmls (cmd : Syntax) : CommandElabM (Array Json × Nat) := do
  let saved := (← getInfoState).trees
  modifyInfoState fun s => { s with trees := {} }
  elabCommand cmd
  let trees := (← getInfoState).trees
  modifyInfoState fun s => { s with trees := saved ++ trees }
  let panel := hash HtmlDisplayPanel.javascript
  return trees.foldl (init := (#[], 0)) fun acc t =>
    t.foldInfo (init := acc) fun _ i (hs, other) => match i with
      | .ofUserWidgetInfo wi =>
        if wi.javascriptHash == panel then (hs.push ((wi.props.run' {}).getObjValD "html"), other)
        else (hs, other + 1)
      | _ => (hs, other)

unsafe def evalHtmlUnsafe (stx : Term) : TermElabM Html :=
  Term.evalTerm Html (mkConst ``Html) stx

@[implemented_by evalHtmlUnsafe] opaque evalHtml (stx : Term) : TermElabM Html

/-- `#guard_widget expected => cmd`: `cmd` shows exactly the HTML `expected` in the infoview. -/
elab "#guard_widget " expected:term " => " cmd:command : command => do
  let (hs, other) ← panelHtmls cmd
  let want := (Server.rpcEncode (← liftTermElabM (evalHtml expected))).run' {}
  unless other == 0 && hs.size == 1 do
    throwError "#guard_widget: expected one HtmlDisplayPanel widget, got {hs.size} (and {other} others)"
  unless hs[0]! == want do
    throwError "#guard_widget: the widget shows{indentD ((toString hs[0]!).take 400).toString}…\nexpected{indentD ((toString want).take 400).toString}…"

/-- `#guard_no_widget cmd`: `cmd` saves no panel widget (e.g. because it failed). -/
elab "#guard_no_widget " cmd:command : command => do
  let (hs, other) ← panelHtmls cmd
  unless hs.isEmpty && other == 0 do throwError "#guard_no_widget: {hs.size + other} widgets saved"

/-- A test figure. -/
def fig : Figure :=
  Figure.new (size := (200, 150)) |>.axis 1 1 (Axis2.new (title := "sin") |>.linesFn Float.sin (Num.range 0 10 21))

/-- A test axis. -/
def ax : Axis2 := Axis2.new |>.scatter ⟨#[1, 2, 3]⟩ ⟨#[3, 1, 2]⟩

/-! ## `#figure` -/

#guard_msgs in
#figure fig

#guard_widget fig.toHtml => #figure fig
#guard_widget fig.toHtml => #figure fig.toScene
#guard_widget fig.toHtml => #figure (pure fig : IO Figure)
#guard_widget fig.toHtml => #figure (do return fig : IO Figure)
#guard_widget ax.toHtml => #figure ax
#guard_widget (Axis3.new |>.scatterPts (Pts3.ofArrays ⟨#[0, 1]⟩ ⟨#[0, 1]⟩ ⟨#[0, 1]⟩)).toHtml =>
  #figure Axis3.new |>.scatterPts (Pts3.ofArrays ⟨#[0, 1]⟩ ⟨#[0, 1]⟩ ⟨#[0, 1]⟩)

-- a doc comment is the caption
#guard_widget fig.toHtml { caption := some "The sine." } =>
  /-- The sine. -/
  #figure fig

-- `leanplot.widgets.format` picks the embedding
#guard_widget fig.toHtml { format := .png } =>
  set_option leanplot.widgets.format "png" in #figure fig
#guard_widget fig.toHtml { format := .inline } =>
  set_option leanplot.widgets.format "inline" in #figure fig

/-- error: leanplot.widgets.format: expected "svg", "png" or "inline", got "gif" -/
#guard_msgs in
set_option leanplot.widgets.format "gif" in #figure fig

/--
error: #figure: cannot display a value of type
  Nat
expected a Figure, Scene, Axis2 or Axis3, or an IO action returning one
-/
#guard_msgs in
#figure (1 : Nat)

/-- error: Unknown identifier `noSuchFigure` -/
#guard_msgs in
#figure noSuchFigure

/-- error: boom -/
#guard_msgs in
#figure (throw (IO.userError "boom") : IO Figure)

#guard_no_widget #guard_msgs (drop error) in #figure (1 : Nat)

end LeanPlotWidgetsTest.Commands
