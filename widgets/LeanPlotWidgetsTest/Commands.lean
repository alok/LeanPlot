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

/-- error: #figure: the term contains `sorry` -/
#guard_msgs in
#figure (sorry : Figure)

/--
error: #figure: cannot display a value of type
  Type
expected a Figure, Scene, Axis2 or Axis3, or an IO action returning one
-/
#guard_msgs in
#figure Nat

#guard_no_widget #guard_msgs (drop error) in #figure (1 : Nat)

/-! ## `#plot` -/

#guard_msgs in
#plot Float.sin

-- functions: sampled on `range(-5, 5, length = 201)` by default
#guard_widget (plotFigure Float.sin).toHtml => #plot Float.sin
#guard_widget (plotFigure (fun x : Float => x * x)).toHtml => #plot fun x => x * x
#guard_widget (plotFigure (fun x : Float => x * x)).toHtml => #plot (fun x => x * x)
#guard_widget (plotFigure (fun x : Float => x ^ 2) { domain := (-1, 1) }).toHtml => #plot (fun x => x ^ 2) on -1..1
#guard_widget (plotFigure Float.tanh { domain := (-3, 3), samples := 50 }).toHtml => #plot Float.tanh on -3..3 using 50
#guard_widget (plotFigure Float.tanh { samples := 50 }).toHtml => #plot Float.tanh using 50
#guard_widget (plotFigure Float.sin { domain := (0, 2 * Num.pi) }).toHtml => #plot Float.sin on 0..2*Num.pi
#guard_widget (plotFigure Float.sin { domain := (-Num.pi, Num.pi) }).toHtml => #plot Float.sin on -Num.pi..Num.pi
#guard_widget (plotFigure Float.sin { domain := (-1, 1) }).toHtml => #plot Float.sin on (-1)..1
#guard_widget (plotFigure [Float.sin, Float.cos] { domain := (0, 6.3) }).toHtml => #plot [Float.sin, Float.cos] on 0..6.3
#guard_widget (plotFigure (fun t : Float => (Float.cos t, Float.sin t)) { domain := (0, 6.3) }).toHtml =>
  #plot (fun t => (Float.cos t, Float.sin t)) on 0..6.3

-- data: against 1, 2, …, n, or as points
#guard_widget (plotFigure #[1.0, 4.0, 9.0, 16.0]).toHtml => #plot #[1.0, 4.0, 9.0, 16.0]
#guard_widget (plotFigure #[1.0, 4.0, 9.0, 16.0]).toHtml => #plot [1, 4, 9, 16]
#guard_widget (plotFigure #[1.0, 4.0, 9.0, 16.0]).toHtml => #plot (FloatArray.mk #[1, 4, 9, 16])
#guard_widget (plotFigure #[-1.0, 2.0]).toHtml => #plot [(-1 : Int), 2]
#guard_widget (plotFigure (Pts2.ofArrays ⟨#[0, 1, 3]⟩ ⟨#[1, 0, 2]⟩)).toHtml => #plot [((0 : Float), (1 : Float)), (1, 0), (3, 2)]
#guard_widget (plotFigure (Pts2.ofArrays ⟨#[0, 1, 3]⟩ ⟨#[1, 0, 2]⟩)).toHtml => #plot (#[0.0, 1, 3], #[1.0, 0, 2])
#guard_widget (plotFigure (Pts2.sample Float.exp 0 1 5)).toHtml => #plot Pts2.sample Float.exp 0 1 5

-- the data instances draw what Makie's `lines` draws
#guard (Plottable.plot #[1.0, 4.0] {} Axis2.new).items.size == 1
#guard (Plottable.plot [Float.sin, Float.cos] {} Axis2.new).items.size == 2
#guard ((plotFigure Float.sin).size) == (600, 450)
#guard PlotSpec.xs {} == Num.range (-5) 5 201
#guard (PlotSpec.xs { domain := (0, 1), samples := 3 }).data == #[0, 0.5, 1]

-- figures pass through
#guard_widget fig.toHtml => #plot fig

-- a doc comment is the caption
#guard_widget (plotFigure Float.exp { domain := (0, 1) }).toHtml { caption := some "exp" } =>
  /-- exp -/
  #plot Float.exp on 0..1

/--
warning: #plot: `on`/`using` only apply to functions; ignored for
  Array Float
-/
#guard_msgs in
#plot #[1.0, 2.0] on 0..1

/--
error: #plot: cannot plot a value of type
  String
expected a function `Float → Float` (or `Float → Float × Float`), a list or array of them, data (`FloatArray`, arrays or lists of numbers or pairs, `Pts2`), or a Figure, Scene, Axis2 or Axis3
-/
#guard_msgs in
#plot "hello"

/-- error: #plot: the domain 1..1 is empty -/
#guard_msgs in
#plot Float.sin on 1..1

/-- error: #plot: the domain 0..inf is not finite -/
#guard_msgs in
#plot Float.sin on 0..(1 / 0)

/-- error: #plot: need at least 2 samples, got 1 -/
#guard_msgs in
#plot Float.sin using 1

/-- error: Unknown identifier `noSuchFunction` -/
#guard_msgs in
#plot noSuchFunction

/-- error: #plot: the term contains `sorry` -/
#guard_msgs in
#plot sorry

-- the failed attempt at `Float → Float` leaves no message behind
#guard_msgs in
#plot #[1.0, 2.0]

#guard_no_widget #guard_msgs (drop error) in #plot Float.sin using 1

end LeanPlotWidgetsTest.Commands
