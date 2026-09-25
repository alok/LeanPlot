import LeanPlotTest.Core.Harness
import LeanPlotTest.Figure.Specs

/-!
Layout goldens against CairoMakie (`LeanPlotTest/oracle/figure/figure.json`): for every
oracle figure, the solved axis viewports (exact integers), limits, protrusions, tick values,
tick positions and tick label strings, title/label anchors, colorbar and legend boxes, and
the `Axis3` viewport, limits, ticks, tick labels and axis labels.
-/

namespace LeanPlotTest.Figure

open LeanPlot LeanPlot.Layout LeanPlotTest.Core

/-- Oracle directory of this suite. -/
def oracleDir : System.FilePath := "LeanPlotTest" / "oracle" / "figure"

/-- Load `figure.json`. -/
def loadFigureOracle : TestM (Option J) := do
  let p := oracleDir / "figure.json"
  if !(← p.pathExists) then
    check "oracle figure.json" false (fun _ => s!"missing {p} (run from the package root)")
    return none
  try return some (← readJson p)
  catch e =>
    check "oracle figure.json" false (fun _ => toString e)
    return none

/-- Floats read back with `Float32` rounding (the oracle writes `Float32` data in shortest form). -/
def floats32 (j : J) : FloatArray :=
  ⟨j.arrD.map fun x => let v := x.float; if v.isNaN then v else v.toFloat32.toFloat⟩

/-- Figure 21: a streamplot drawn from Makie's own computed streamlines and arrowheads
(`streamplot.json`), through `Axis2.streamplot`. -/
def streamplotSpec : TestM (Option Figure) := do
  let p := oracleDir / "streamplot.json"
  if !(← p.pathExists) then
    check "oracle streamplot.json" false (fun _ => s!"missing {p}")
    return none
  let j ← readJson p
  let lines := Pts2.ofArrays (floats32 (j.get "line_x")) (floats32 (j.get "line_y"))
  let arrows := Pts2.ofArrays (floats32 (j.get "arrow_x")) (floats32 (j.get "arrow_y"))
  let dirs := Pts2.ofArrays (floats32 (j.get "arrow_u")) (floats32 (j.get "arrow_v"))
  let ax := Axis2.new |>.streamplot lines (floats32 (j.get "line_c")) arrows dirs (floats32 (j.get "arrow_c"))
  return some (Figure.new |>.axis 1 1 ax)

/-- All specs: the pure ones and those built from oracle data. -/
def allSpecs : TestM (Array (String × Figure)) := do
  let sp ← streamplotSpec
  return match sp with
    | some f => specs.push ("streamplot", f)
    | none => specs

/-- `[x, y, w, h]` of a box. -/
def rectOf (b : BBox) : Array Float := #[b.left, b.bottom, b.width, b.height]

/-- Compare float arrays within `tol`, reporting both. -/
def checkFloats (name : String) (tol : Float) (got want : Array Float) : TestM Unit :=
  check name (floatsApprox tol got want) fun _ => s!"got {showFloats got}, want {showFloats want}"

/-- Tick values agree to `Float32` precision (Makie's `LineAxis` stores them as `Float32`). -/
def ticksClose (a b : Array Float) : Bool :=
  a.size == b.size && (Array.range a.size).all fun i => (a[i]! - b[i]!).abs ≤ 1e-7 * (1 + b[i]!.abs)

/-- Relative comparison of limits. `Float32` data (e.g. Makie's `Point2f` streamlines) get
bounding boxes with `Float32` widths in Makie, so allow `Float32`-level differences. -/
def limitsClose (a b : Array Float) : Bool :=
  a.size == b.size && (Array.range a.size).all fun i => (a[i]! - b[i]!).abs ≤ 1e-7 * (1 + b[i]!.abs)

/-- A tick label as the oracle spells it (`base^exp` for superscripts). -/
def labelString : Num.TickLabel → String
  | .plain s => s
  | .sup b e => b ++ "^" ++ e

/-- Compare one 2D axis. -/
def checkAxis (fname : String) (k : Nat) (ax : Axis2) (sb : SolvedBlock) (p : Axis2Prep) (j : J) : TestM Unit := do
  let n := s!"{fname}/axis{k}"
  checkFloats s!"{n} viewport" 0 (rectOf sb.viewport) (j.get "viewport").floats
  checkFloats s!"{n} bbox" 2e-3 (rectOf sb.box) (j.get "bbox").floats
  let lims := #[p.xlims.1, p.xlims.2, p.ylims.1, p.ylims.2]
  check s!"{n} limits" (limitsClose lims (j.get "limits").floats) fun _ =>
    s!"got {showFloats lims}, want {showFloats (j.get "limits").floats}"
  let pr := p.protrusions
  checkFloats s!"{n} protrusions" 2e-3 #[pr.left, pr.right, pr.bottom, pr.top] (j.get "protrusions").floats
  let an := ax.anchors p sb.viewport
  for (dir, t, pos) in [("x", p.xt, an.xticks), ("y", p.yt, an.yticks)] do
    let jt := j.get s!"{dir}ticks"
    check s!"{n} {dir} tick values" (ticksClose t.values (jt.get "values").floats) fun _ =>
      s!"got {showFloats t.values}, want {showFloats (jt.get "values").floats}"
    let wantPos := (jt.get "positions").arrD.map fun q => if dir == "x" then q.arrD[0]!.float else q.arrD[1]!.float
    checkFloats s!"{n} {dir} tick positions" 2e-3 pos wantPos
    let labels := t.labels.map labelString
    let want := (jt.get "labels").arrD.map J.string
    check s!"{n} {dir} tick labels" (labels == want) fun _ => s!"got {labels}, want {want}"
    let side := if dir == "x" then (if ax.xaxistop then pr.top - ax.titleSpace else pr.bottom)
                else (if ax.yaxisright then pr.right else pr.left)
    check s!"{n} {dir} protrusion" ((side - (jt.get "protrusion").float).abs ≤ 2e-3)
      fun _ => s!"got {side}, want {(jt.get "protrusion").float}"
  checkFloats s!"{n} title" 2e-3 #[an.title.1, an.title.2] (j.get "title").floats
  checkFloats s!"{n} xlabel" 2e-3 #[an.xlabel.1, an.xlabel.2] (j.get "xlabel").floats
  checkFloats s!"{n} ylabel" 2e-3 #[an.ylabel.1, an.ylabel.2] (j.get "ylabel").floats

/-- Compare one colorbar. -/
def checkColorbar (fname : String) (k : Nat) (cb : Colorbar) (sb : SolvedBlock) (p : ColorbarPrep) (j : J) : TestM Unit := do
  let n := s!"{fname}/colorbar{k}"
  checkFloats s!"{n} bbox" 2e-3 (rectOf sb.box) (j.get "bbox").floats
  let pr := p.protrusions
  checkFloats s!"{n} protrusions" 2e-3 #[pr.left, pr.right, pr.bottom, pr.top] (j.get "protrusions").floats
  let jt := j.get "ticks"
  checkFloats s!"{n} tick values" 1e-12 p.ticks.values (jt.get "values").floats
  let wantPos := (jt.get "positions").arrD.map fun q => if cb.vertical then q.arrD[1]!.float else q.arrD[0]!.float
  checkFloats s!"{n} tick positions" 2e-3 (cb.tickPositions p sb.box) wantPos
  let labels := p.ticks.labels.map labelString
  let want := (jt.get "labels").arrD.map J.string
  check s!"{n} tick labels" (labels == want) fun _ => s!"got {labels}, want {want}"

/-- Compare one legend's boxes. -/
def checkLegend (fname : String) (k : Nat) (l : LegendBlock) (es : Array LegendEntry) (box : BBox) (j : J) : TestM Unit := do
  let n := s!"{fname}/legend{k}"
  checkFloats s!"{n} bbox" 2e-3 (rectOf box) (j.get "bbox").floats
  let g := Legend.geometry l.style l.title es box
  let wantP := (j.get "patches").arrD
  let wantL := (j.get "labels").arrD
  check s!"{n} entries" (g.patches.size == wantP.size && g.labels.size == wantL.size) fun _ =>
    s!"got {g.patches.size}/{g.labels.size}, want {wantP.size}/{wantL.size}"
  let wantT := (j.get "titles").arrD
  check s!"{n} title present" (g.title.isSome == !wantT.isEmpty)
  if let (some tb, some jt) := (g.title, wantT[0]?) then
    checkFloats s!"{n} title" 2e-3 (rectOf tb) (jt.get "bbox").floats
  for i in [0:min g.patches.size wantP.size] do
    checkFloats s!"{n} patch {i}" 2e-3 (rectOf g.patches[i]!) wantP[i]!.floats
  for i in [0:min g.labels.size wantL.size] do
    checkFloats s!"{n} label {i}" 2e-3 (rectOf g.labels[i]!) (wantL[i]!.get "bbox").floats

/-- Compare an axis legend (`axislegend`) with the oracle's legend box. -/
def checkAxisLegend (fname : String) (ax : Axis2) (vp : BBox) (j : J) : TestM Unit := do
  match ax.legend with
  | none => check s!"{fname} axislegend present" false
  | some lg =>
    let es := LegendEntry.ofItems ax.items
    let box := ax.legendBox lg es vp
    checkLegend fname 0 { style := lg.style, title := lg.title } es box j

/-- Compare one `Axis3`. -/
def checkAxis3 (fname : String) (k : Nat) (ax : Axis3) (sb : SolvedBlock) (p : Axis3Prep) (figH : Float) (j : J) : TestM Unit := do
  let n := s!"{fname}/axis3_{k}"
  checkFloats s!"{n} viewport" 0 (rectOf sb.viewport) (j.get "viewport").floats
  checkFloats s!"{n} bbox" 2e-3 (rectOf sb.box) (j.get "bbox").floats
  let l := p.limits
  let lims := #[l.origin.x, l.origin.x + l.widths.x, l.origin.y, l.origin.y + l.widths.y,
    l.origin.z, l.origin.z + l.widths.z]
  checkFloats s!"{n} limits" 0 lims (j.get "limits").floats
  let tv := (j.get "tickvalues").arrD
  let ts := #[p.xt, p.yt, p.zt]
  for d in [0:3] do
    checkFloats s!"{n} tick values {d}" 1e-12 ts[d]!.values (tv.getD d .null).floats
  let cam := ax.camera p sb.box figH
  let jt := (j.get "ticks").arrD
  let jl := (j.get "ticklabels").arrD
  let jlab := (j.get "labels").arrD
  for d in [0:3] do
    let dd := ax.dimDecor p cam figH d
    let got := dd.ticks.foldl (init := #[]) fun acc (a, b, c, e) => ((acc.push a).push b |>.push c).push e
    let want := (jt.getD d .null).arrD.foldl (init := #[]) fun acc q => acc ++ q.floats
    checkFloats s!"{n} ticks {d}" 0.02 got want
    let gotL := dd.tickLabels.foldl (init := #[]) fun acc (a, b) => (acc.push a).push b
    let wantL := ((jl.getD d .null).get "positions").arrD.foldl (init := #[]) fun acc q => acc ++ q.floats
    checkFloats s!"{n} tick labels {d}" 0.02 gotL wantL
    let al := ((jl.getD d .null).get "align").arrD.map J.string
    let hs := match dd.tickAlign.1 with | .left => "left" | .center => "center" | .right => "right"
    let vs := match dd.tickAlign.2 with | .top => "top" | .middle => "center" | .bottom => "bottom" | .baseline => "baseline"
    check s!"{n} tick label align {d}" (al == #[hs, vs]) fun _ => s!"got {hs} {vs}, want {al}"
    match dd.label, jlab[d]? with
    | some ((x, y), rot, va), some jd =>
      checkFloats s!"{n} label {d}" 0.02 #[x, y, rot] ((jd.get "position").floats.push (jd.get "rotation").float)
      let vs := match va with | .top => "top" | .bottom => "bottom" | _ => "center"
      check s!"{n} label align {d}" (vs == ((jd.get "align").arrD.getD 1 .null).string) fun _ => s!"got {vs}"
    | _, _ => check s!"{n} label {d} present" false
  match jlab[3]? with
  | some jd =>
    let (x, y) := ax.titleAnchor sb.box
    checkFloats s!"{n} title" 2e-3 #[x, y] (jd.get "position").floats
  | none => pure ()

/-- Compare one oracle figure. -/
def checkFigure (specs : Array (String × Figure)) (j : J) : TestM Unit := do
  let name := (j.get "name").string
  match specs.find? (·.1 == name) with
  | none => check s!"spec {name}" false (fun _ => "no Lean spec for this oracle figure")
  | some (_, f) =>
    let size := (j.get "size").floats
    check s!"{name} size" (size == #[f.dims.1, f.dims.2])
    let solved := f.solve
    let figH := f.dims.2
    let mut k2 := 0
    let mut k3 := 0
    let mut kc := 0
    let mut kl := 0
    let mut kb := 0
    let jLabels := (j.get "labels").arrD
    let jAxes := (j.get "axes").arrD
    let jAxes3 := (j.get "axes3").arrD
    let jCbs := (j.get "colorbars").arrD
    let jLegs := (j.get "legends").arrD
    for i in [0:f.content.size] do
      let sb := solved[i]!
      match f.content[i]!.block, sb.prep with
      | .axis2 ax, .axis2 p =>
        match jAxes[k2]? with
        | some ja => checkAxis name k2 ax sb p ja
        | none => check s!"{name} axis {k2} in oracle" false
        if ax.legend.isSome then
          match jLegs[kl]? with
          | some jl => checkAxisLegend name ax sb.viewport jl
          | none => check s!"{name} axislegend in oracle" false
          kl := kl + 1
        k2 := k2 + 1
      | .axis3 ax, .axis3 p =>
        match jAxes3[k3]? with
        | some ja => checkAxis3 name k3 ax sb p figH ja
        | none => check s!"{name} axis3 {k3} in oracle" false
        k3 := k3 + 1
      | .colorbar cb, .colorbar p =>
        match jCbs[kc]? with
        | some jc => checkColorbar name kc cb sb p jc
        | none => check s!"{name} colorbar {kc} in oracle" false
        kc := kc + 1
      | .legend l, .legend es _ _ =>
        match jLegs[kl]? with
        | some jl => checkLegend name kl l es sb.box jl
        | none => check s!"{name} legend {kl} in oracle" false
        kl := kl + 1
      | .label _, .label _ _ =>
        match jLabels[kb]? with
        | some jb => checkFloats s!"{name}/label{kb} bbox" 2e-3 (rectOf sb.box) (jb.get "bbox").floats
        | none => check s!"{name} label {kb} in oracle" false
        kb := kb + 1
      | _, _ => pure ()
    check s!"{name} label blocks" (kb == jLabels.size)
    check s!"{name} block counts" (k2 == jAxes.size && k3 == jAxes3.size && kc == jCbs.size && kl == jLegs.size)
      fun _ => s!"axes {k2}/{jAxes.size}, axes3 {k3}/{jAxes3.size}, colorbars {kc}/{jCbs.size}, legends {kl}/{jLegs.size}"

/-- The layout suite. -/
def layoutSuite : TestM Unit := do
  let some j ← loadFigureOracle | return
  let figs := (j.get "figures").arrD
  let all ← allSpecs
  check "oracle figure count" (figs.size == all.size) fun _ => s!"{figs.size} oracle figures, {all.size} specs"
  for fj in figs do checkFigure all fj

end LeanPlotTest.Figure
