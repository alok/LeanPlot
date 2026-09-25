import LeanPlotTest.Figure.RenderTest
import LeanPlot.Recipes.Algo

/-!
Figure features added after v0.2 against `LeanPlotTest/oracle/figure/features_oracle.jl`
(`features.json` and the CairoMakie renders `ref/<name>.png`):

* `contour_labels`: labelled contours (Makie `contour(...; labels = true)`), automatic levels in
  viridis and explicit levels in bold orange: label texts and anchors, the screen rotations of
  the drawn labels, the number of line segments left after masking, pixel parity;
* `heatmap_scales`: reversible scales (`AsinhScale` x axis, `PowerScale` colour scale) on a
  heatmap with a colorbar: limits, ticks of both axes and the colorbar, pixel parity.

Both figures also have byte-exact SVG goldens (`LeanPlotTest/Figure/golden/`).
-/

namespace LeanPlotTest.Figure

open LeanPlot LeanPlotTest.Core
open LeanPlot.Recipes.Algo

/-- Load `features.json`. -/
def loadFeatures : TestM (Option J) := do
  let p := oracleDir / "features.json"
  if !(← p.pathExists) then
    check "oracle features.json" false (fun _ => s!"missing {p} (run from the package root)")
    return none
  try return some (← readJson p)
  catch e =>
    check "oracle features.json: parse" false (fun _ => toString e)
    return none

/-! ## Labelled contours -/

/-- `LinRange(-3, 3, 60)`. -/
def cxs : FloatArray := Num.linRange (-3) 3 60
/-- `LinRange(-2, 2, 50)`. -/
def cys : FloatArray := Num.linRange (-2) 2 50

/-- `exp(-(x² + y²)/2)·cos(2x) + 0.1y`. -/
def contourZ : Grid2 60 50 := Grid2.ofFn 60 50 fun i j =>
  let x := cxs.get! i
  let y := cys.get! j
  Float.exp (-(x * x + y * y) / 2) * Float.cos (2 * x) + 0.1 * y

/-- Makie `contour!(ax, xs, ys, zs; levels, labels = true, …)` through the recipe API: the
traced lines coloured per level (viridis over the data range, or `color`), one label per line. -/
def labeledContour (ax : Axis2) (spec : Levels.LevelSpec) (color : Option RGBA) (labelsize : Float)
    (bold : Bool) : Axis2 :=
  let g32 := Contour.roundGrid contourZ
  let ls := Contour.makieContour (.rect cxs cys) contourZ spec
  let (pts, segs) := ls.flatten
  let (zlo, zhi) := Levels.dataRange g32.z
  let (clo, chi) := Contour.colorRange zlo zhi
  let zl := Levels.contourZLevels spec g32.z
  let f32Levels := match spec with | .count _ => true | _ => false
  let lcs := Contour.levelColors Colormap.viridis zl clo chi f32Levels
  let colOf (k : Nat) : RGBA := color.getD (lcs.getD k RGBA.black)
  let lineRGBA := segs.foldl (init := ByteArray.empty) fun acc (lvl, cnt) =>
    (List.range cnt).foldl (fun acc _ => RGBA.pushRGBA8 acc (colOf lvl)) acc
  let (pos, dir, txt, lvl) := Contour.labelData ls
  let labelRGBA := lvl.foldl (fun acc k => RGBA.pushRGBA8 acc (colOf k)) ByteArray.empty
  let lineColor : ColorSpec := match color with | some c => .solid c | none => .perElement lineRGBA
  ax.contourLabeled pts lineColor pos dir txt (.perElement labelRGBA) 1 labelsize bold

/-- The `contour_labels` figure. -/
def contourLabelsFig : Figure :=
  let ax := labeledContour Axis2.new (.count 6) none 10 false
  let ax := labeledContour ax (.values ⟨#[-0.1, 0.3]⟩) (some (RGBA.rgb 1 (165 / 255) 0)) 12 true
  Figure.new |>.axis 1 1 ax

/-- `(text, x, y)` sorted, for comparing label sets (Contour.jl lists a level's lines in `Dict`
order). -/
def sortKeys (t : Array String) (x y : Array Float) : Array (String × Float × Float) :=
  ((Array.range t.size).map fun i => (t[i]!, x[i]!, y[i]!)).qsort fun a b =>
    a.1 < b.1 || (a.1 == b.1 && (a.2.1 < b.2.1 || (a.2.1 == b.2.1 && a.2.2 < b.2.2)))

/-- The permutation sorting `(text, x, y)`. -/
def sortPerm (t : Array String) (x y : Array Float) : Array Nat :=
  (Array.range t.size).qsort fun a b =>
    t[a]! < t[b]! || (t[a]! == t[b]! && (x[a]! < x[b]! || (x[a]! == x[b]! && y[a]! < y[b]!)))

/-- Label and masking checks of one labelled contour item. -/
def checkLabels (name : String) (item : PlotItem) (sceneRots : Array Float) (sceneSegs : Nat) (j : J) : TestM Unit := do
  let .labeledLines _ _ l := item.mark | check s!"{name}: labelled mark" false
  let jt := (j.get "text").arrD.map (·.string)
  let jp := (j.get "pos").arrD
  let jx := jp.map fun p => p.arrD[0]!.float
  let jy := jp.map fun p => p.arrD[1]!.float
  let lx := l.pos.xs.data
  let ly := l.pos.ys.data
  let got := sortKeys l.strings lx ly
  let want := sortKeys jt jx jy
  check s!"{name}: label texts" (got.map (·.1) == want.map (·.1)) fun _ => s!"{got.map (·.1)} vs {want.map (·.1)}"
  check s!"{name}: label anchors" (got.size == want.size && (Array.range got.size).all fun i =>
      (got[i]!.2.1 - want[i]!.2.1).abs ≤ 1e-6 && (got[i]!.2.2 - want[i]!.2.2).abs ≤ 1e-6)
  let jr := (j.get "rot").floats
  let lp := sortPerm l.strings lx ly
  let jpm := sortPerm jt jx jy
  check s!"{name}: label rotations" (lp.size == jpm.size && sceneRots.size == lp.size && (Array.range lp.size).all fun i =>
      (sceneRots[lp[i]!]! - jr[jpm[i]!]!).abs ≤ 1e-4)
    fun _ => s!"{showFloats (lp.map (sceneRots.getD · 0))} vs {showFloats (jpm.map (jr.getD · 0))}"
  let want := (j.get "nseg").nat
  check s!"{name}: segments after masking {sceneSegs} vs {want}" (sceneSegs == want)

/-- The number of segments of a stroked path (its `lineTo` verbs). -/
def pathSegments (p : Path) : Nat := p.verbs.foldl (fun n v => if v == Verb.lineTo.toUInt8 then n + 1 else n) 0

/-- The labelled-contour suite. -/
def checkContourLabels : TestM Unit := do
  let some j ← loadFeatures | return
  let cl := j.get "contour_labels"
  let f := contourLabelsFig
  let sc := f.toScene
  -- label texts in drawing order, per size (10: the viridis plot, 12: the bold orange one)
  let rots (sz : Float) : Array Float := sc.ops.filterMap fun op => match op with
    | .text _ _ _ st (some _) => if st.size == sz then some st.rotation else none
    | _ => none
  -- the viridis lines are a per-segment-coloured `segments` op, the orange ones a stroked path
  let segs1 := sc.ops.foldl (init := 0) fun n op => match op with
    | .segments xs _ _ w _ (some _) => if w == 1 then n + xs.size / 2 else n
    | _ => n
  let segs2 := sc.ops.foldl (init := 0) fun n op => match op with
    | .path p none (some s) (some _) => if s.width == 1 && s.color.b == 0 then n + pathSegments p else n
    | _ => n
  let items := match f.blockAt? 1 1 with | some (.axis2 a) => a.items | _ => #[]
  let plots := (cl.get "plots").arrD
  check "contour_labels: two plots" (items.size == 2 && plots.size == 2)
  if items.size == 2 && plots.size == 2 then
    checkLabels "contour_labels viridis" items[0]! (rots 10) segs1 plots[0]!
    checkLabels "contour_labels orange" items[1]! (rots 12) segs2 plots[1]!
  checkParity "contour_labels" f

/-! ## Reversible scales -/

/-- `10 .^ range(0, 3; length = 25)`. -/
def hxs : FloatArray := ⟨(Num.range 0 3 25).data.map fun e => Float.pow 10 e⟩

/-- `1.0:1.0:12.0`. -/
def hys : FloatArray := Num.range 1 12 12

/-- The `heatmap_scales` axis: `AsinhScale(1.0)` on x, `PowerScale(0.5)` colours. -/
def scalesAxis : Axis2 :=
  let z : Grid2 25 12 := Grid2.ofFn 25 12 fun i j => hxs.get! i * hys.get! j
  Axis2.new (xscale := Scale.asinhScale 1)
    |>.add (.heatmap { xs := Recipes.cellEdges hxs 25, ys := Recipes.cellEdges hys 12, z := ⟨25, 12, z⟩
                       mapping := { colorscale := Scale.powerScale 0.5 } })

/-- The `heatmap_scales` figure: the heatmap and its colorbar. -/
def scalesFig : Figure := Figure.new |>.axis 1 1 scalesAxis |>.colorbar 1 2 (1, 1)

/-- The reversible-scale suite. -/
def checkScales : TestM Unit := do
  let some j ← loadFeatures | return
  let hs := j.get "heatmap_scales"
  let ((x0, x1), (y0, y1)) := scalesAxis.targetLimits
  let lim := (hs.get "limits").floats
  check "heatmap_scales: limits" (lim.size == 4 &&
      (x0 - lim[0]!).abs ≤ 1e-9 * x1 && (y0 - lim[1]!).abs ≤ 1e-9 &&
      (x1 - (lim[0]! + lim[2]!)).abs ≤ 1e-9 * x1 && (y1 - (lim[1]! + lim[3]!)).abs ≤ 1e-9)
    fun _ => s!"{showFloats #[x0, y0, x1 - x0, y1 - y0]} vs {showFloats lim}"
  let xt := Axis2.ticksFor .auto scalesAxis.xscale scalesAxis.style.x x0 x1
  check "heatmap_scales: x ticks" (ticksClose xt.values (hs.get "xticks" |>.get "values").floats)
    fun _ => showFloats xt.values
  let cbm := match scalesAxis.items[0]? with
    | some it => it.mark.colorMapping?
    | none => none
  match cbm with
  | some (m, lo, hi) =>
    let ct := Axis2.ticksFor .auto m.colorscale {} lo hi
    check "heatmap_scales: colorbar ticks" (ticksClose ct.values (hs.get "cbticks" |>.get "values").floats)
      fun _ => showFloats ct.values
  | none => check "heatmap_scales: colour mapping" false
  checkParity "heatmap_scales" scalesFig

/-- The feature suite. -/
def featureSuite : TestM Unit := do
  checkContourLabels
  checkScales
  checkSvg "contour_labels" contourLabelsFig
  checkSvg "heatmap_scales" scalesFig

end LeanPlotTest.Figure
