import LeanPlot.Figure.Legend
import LeanPlot.Core.Scale
import LeanPlot.Core.Ticks

/-!
# `Axis2`: a 2D axis (Makie `Axis`)

An `Axis2` holds plot items (marks with labels) and Makie's `Axis` attributes. Before layout,
`prepare` computes what does not depend on the axis size:

* **limits** (`reset_limits!` / `autolimits`): the union of the items' data limits expanded by
  the autolimit margins in scaled space (`expandlimits`, with Makie's singular-limit rule),
  zero margins when an item needs tight limits (heatmap, image), `defaultlimits(scale)` when
  there is no data, and per-side user overrides (`limits!`);
* **ticks** (`get_ticks`): `LeanPlot.Scale.ticks` (Wilkinson / log / decade ticks with Makie's
  label formatting), filtered with `is_within_limits`;
* **protrusions** (`LineAxis.calculate_protrusion`, `compute_protrusions`): tick length, tick
  label space (union of the label boxes) plus pad, axis label height plus padding, and the
  title height plus gap, measured with the embedded fonts.

After layout, `viewport` applies the `aspect` (`sceneareanode!`, rounded to integer pixels)
and `lower` emits the decorations and the marks with Makie's z-order (background −100, grid
−10, labels and plots 0, ticks 10, spines 20), all in figure pixels flipped to device
space. Plot marks are clipped to the viewport.
-/

namespace LeanPlot

open LeanPlot.Num LeanPlot.Layout

/-- Axis aspect (Makie `aspect`): free, a fixed width/height ratio, or `DataAspect()`. -/
inductive AxisAspect where
  | auto
  | ratio (r : Float)
  | data
  deriving Repr, Inhabited, BEq

/-- Tick specification (Makie `xticks`/`yticks`). -/
inductive TickSpec where
  /-- Automatic ticks (Wilkinson, log or decade ticks depending on the scale). -/
  | auto
  /-- Given values, automatically formatted labels. -/
  | values (vs : Array Float)
  /-- Given values and labels. -/
  | labeled (vs : Array Float) (labels : Array String)
  deriving Repr, Inhabited

/-- Palette counters per plot type (Makie cycles colours per plot function). -/
structure Cycle where
  lines : Nat := 0
  segments : Nat := 0
  scatter : Nat := 0
  band : Nat := 0
  poly : Nat := 0
  mesh : Nat := 0
  deriving Repr, Inhabited

/-- A legend drawn inside an axis (Makie `axislegend`): alignment in the axis viewport
(`halign` 0 left … 1 right, `valign` 0 bottom … 1 top) and style; margins default to 6. -/
structure AxisLegend where
  title : Option String := none
  halign : Float := 1
  valign : Float := 1
  style : LegendStyle := { margin := Sides.uniform 6 }
  deriving Inhabited

/-- A 2D axis. -/
structure Axis2 where
  items : Array PlotItem := #[]
  title : String := ""
  xlabel : String := ""
  ylabel : String := ""
  xscale : Scale := .identity
  yscale : Scale := .identity
  /-- User limits `(lo?, hi?)` per direction (`none` = automatic). -/
  xlimits : Option Float × Option Float := (none, none)
  ylimits : Option Float × Option Float := (none, none)
  xticks : TickSpec := .auto
  yticks : TickSpec := .auto
  aspect : AxisAspect := .auto
  /-- Makie `autolimitaspect`: expand the limits so one data unit has this pixel ratio. -/
  autolimitaspect : Option Float := none
  xreversed : Bool := false
  yreversed : Bool := false
  xautolimitmargin : Float × Float := (margin05, margin05)
  yautolimitmargin : Float × Float := (margin05, margin05)
  width : SizeAttr := .fill
  height : SizeAttr := .fill
  halign : Float := 0.5
  valign : Float := 0.5
  style : AxisStyle := {}
  theme : Theme := {}
  cycle : Cycle := {}
  legend : Option AxisLegend := none
  deriving Inhabited

/-- Ticks of one axis direction: values, labels and minor tick values, already filtered to
the limits. -/
structure AxisTicks where
  values : Array Float := #[]
  labels : Array TickLabel := #[]
  minor : Array Float := #[]
  deriving Inhabited

/-- Everything about an axis that is known before layout. -/
structure Axis2Prep where
  xlims : Float × Float
  ylims : Float × Float
  xt : AxisTicks
  yt : AxisTicks
  /-- Tick label space (x: height, y: width). -/
  xTickSpace : Float
  yTickSpace : Float
  protrusions : Sides
  deriving Inhabited

namespace Axis2

/-- Makie `expandlimits(lims, low, high, scale)`. -/
def expandLimits (lo hi mlo mhi : Float) (s : Scale) : Float × Float :=
  let lo' := min lo hi
  let hi' := max lo hi
  let a := s.forward lo'
  let b := s.forward hi'
  let w := b - a
  let r := (s.inverse (a - w * mlo), s.inverse (b + w * mhi))
  if r.2 - r.1 == 0 then
    let zerodist := (s.forward r.1).abs
    if zerodist == 0 && s == .identity then (-1, 1)
    else (s.inverse (s.forward r.1 - zerodist), s.inverse (s.forward r.2 + zerodist))
  else r

/-- Per-dimension bounds `(x, y)` of all items that take part in autolimits. -/
def dataBounds (ax : Axis2) : Option (Float × Float) × Option (Float × Float) :=
  ax.items.foldl (init := (none, none)) fun (bx, by_) it =>
    if it.autolimits then
      let (x, y, _) := it.mark.dimBounds
      (Mark.unionRange bx x, Mark.unionRange by_ y)
    else (bx, by_)

/-- `true` when some item needs tight limits (`tightlimits!` sets all margins to 0). -/
def tight (ax : Axis2) : Bool := ax.items.any (·.mark.tight)

/-- Makie `reset_limits!` (without `autolimitaspect`): final `(x0, x1)`, `(y0, y1)`. -/
def targetLimits (ax : Axis2) : (Float × Float) × (Float × Float) :=
  let (bx, by_) := ax.dataBounds
  let tight := ax.tight
  let dim (lims : Option Float × Option Float) (sc : Scale) (margin : Float × Float)
      (bb : Option (Float × Float)) : Float × Float :=
    match lims with
    | (some lo, some hi) => (lo, hi)
    | (ulo, uhi) =>
      let (mlo, mhi) := if tight then (0, 0) else margin
      let auto := match bb with
        | some (a, b) =>
          if a.isFinite && b.isFinite && sc.validLimits a b then expandLimits a b mlo mhi sc
          else sc.defaultLimits
        | none => sc.defaultLimits
      (ulo.getD auto.1, uhi.getD auto.2)
  (dim ax.xlimits ax.xscale ax.xautolimitmargin bx, dim ax.ylimits ax.yscale ax.yautolimitmargin by_)

/-- Makie `adjustlimits!` for `autolimitaspect` given the viewport size. -/
def adjustLimits (ax : Axis2) (xl yl : Float × Float) (vw vh : Float) : (Float × Float) × (Float × Float) :=
  match ax.autolimitaspect with
  | none => (xl, yl)
  | some asp =>
    if vw == 0 || vh == 0 then (xl, yl) else
    let sizeAspect := vw / vh
    let dataAspect := (xl.2 - xl.1) / (yl.2 - yl.1)
    let correction := asp / (dataAspect / sizeAspect)
    let ratios (m : Float × Float) : Float × Float :=
      let s := m.1 + m.2
      if s == 0 then (0.5, 0.5) else (m.1 / s, m.2 / s)
    if correction > 1 then
      let (r1, r2) := ratios ax.xautolimitmargin
      (expandLimits xl.1 xl.2 ((correction - 1) * r1) ((correction - 1) * r2) .identity, yl)
    else if correction < 1 then
      let (r1, r2) := ratios ax.yautolimitmargin
      (xl, expandLimits yl.1 yl.2 ((1 / correction - 1) * r1) ((1 / correction - 1) * r2) .identity)
    else (xl, yl)

/-- Ticks for one direction (`get_ticks` + `is_within_limits` filtering + minor ticks). -/
def ticksFor (spec : TickSpec) (sc : Scale) (st : LineAxisStyle) (lo hi : Float) : AxisTicks :=
  let (vals, labels) : Array Float × Array TickLabel := match spec with
    | .auto => sc.ticks lo hi
    | .values vs => (vs, formatTicksAuto vs)
    | .labeled vs ls => (vs, ls.map .plain)
  let (vals, labels) := Ticks.filterWithinLimits vals labels lo hi
  let minor :=
    if st.minorticksvisible || st.minorgridvisible then
      (sc.minorTicks st.minorIntervals true vals lo hi).filter fun v => Ticks.isWithinLimits v lo hi
    else #[]
  { values := vals, labels, minor }

/-- Makie `calculate_real_ticklabel_align` for automatic alignment: the tick label anchor
for an axis that is horizontal or not, flipped (top/right) or not, at a rotation. -/
def autoTickAlign (horizontal flipped : Bool) (rot : Float) : HAlign × VAlign :=
  let near (a b : Float) : Bool := (a - b).abs ≤ 1.4901161193847656e-8 * max a.abs b.abs
  if rot == 0 then
    if horizontal then (.center, if flipped then .bottom else .top) else (if flipped then .left else .right, .middle)
  else if near rot (Num.pi / 2) then
    if horizontal then (if flipped then .left else .right, .middle) else (.center, if flipped then .top else .bottom)
  else if near rot (-Num.pi / 2) then
    if horizontal then (if flipped then .right else .left, .middle) else (.center, if flipped then .bottom else .top)
  else if rot > 0 then
    if horizontal then (if flipped then .left else .right, if flipped then .bottom else .top)
    else (if flipped then .left else .right, .middle)
  else
    if horizontal then (if flipped then .right else .left, if flipped then .bottom else .top)
    else (if flipped then .left else .right, .middle)

/-- Tick label text style of one direction (`horizontal` for the x axis). -/
def tickLabelStyle (st : LineAxisStyle) (horizontal : Bool) : TextStyle :=
  let (ha, va) := autoTickAlign horizontal false st.ticklabelrotation
  { size := st.ticklabelsize, color := st.ticklabelcolor, rotation := st.ticklabelrotation
    halign := ha, valign := va }

/-- Axis label text style (`horizontal` for the x label). -/
def labelStyle (st : LineAxisStyle) (horizontal : Bool) : TextStyle :=
  { size := st.labelsize, color := st.labelcolor, halign := .center
    valign := if horizontal then .top else .bottom
    rotation := if horizontal then 0 else Num.pi / 2 }

/-- Title text style. -/
def titleStyle (ax : Axis2) : TextStyle :=
  { size := ax.style.titlesize, color := ax.style.titlecolor, bold := ax.style.titlebold
    halign := if ax.style.titlealign == 0 then .left else if ax.style.titlealign == 1 then .right else .center
    valign := .bottom }

/-- Space taken by the tick labels: the height (x) / width (y) of the union of their boxes. -/
def tickLabelSpace (st : LineAxisStyle) (horizontal : Bool) (t : AxisTicks) : Float :=
  match st.ticklabelspace with
  | some s => s
  | none =>
    if !st.ticklabelsvisible then 0 else
    let style := tickLabelStyle st horizontal
    let rects := (Array.range (min t.values.size t.labels.size)).map fun i =>
      -- positions along the axis do not change the extent across it
      FigText.labelBounds style t.labels[i]! 0 0
    match FigText.unionRects rects with
    | some r => let v := if horizontal then r.h else r.w; if v.isFinite then f32 v else 0
    | none => 0

/-- `calculate_protrusion` of one `LineAxis`. -/
def lineAxisProtrusion (st : LineAxisStyle) (label : String) (horizontal : Bool) (t : AxisTicks) (space : Float) : Float :=
  let labelSize :=
    if FigText.isBlank label then 0 else
    let (w, h) := FigText.stringSize (labelStyle st horizontal) label
    f32 (if horizontal then h else w)
  let labelspace := if st.labelvisible && !FigText.isBlank label then labelSize + st.labelpadding else 0
  let tickspace := if st.ticksvisible && !t.values.isEmpty then max 0 (st.ticksize * (1 - st.tickalign)) else 0
  let ticklabelgap := if st.ticklabelsvisible && space > 0 then space + st.ticklabelpad else 0
  f32 (tickspace + ticklabelgap + labelspace)

/-- Prepare with given limits. -/
def prepareWith (ax : Axis2) (xl yl : Float × Float) : Axis2Prep :=
  let xt := ticksFor ax.xticks ax.xscale ax.style.x xl.1 xl.2
  let yt := ticksFor ax.yticks ax.yscale ax.style.y yl.1 yl.2
  let xs := tickLabelSpace ax.style.x true xt
  let ys := tickLabelSpace ax.style.y false yt
  let xprot := lineAxisProtrusion ax.style.x ax.xlabel true xt xs
  let yprot := lineAxisProtrusion ax.style.y ax.ylabel false yt ys
  let top :=
    if !ax.style.titlevisible || FigText.isBlank ax.title then 0
    else f32 ((FigText.stringSize ax.titleStyle ax.title).2 + ax.style.titlegap)
  { xlims := xl, ylims := yl, xt, yt, xTickSpace := xs, yTickSpace := ys
    protrusions := { left := yprot, right := 0, bottom := xprot, top } }

/-- Prepare with the target limits. -/
def prepare (ax : Axis2) : Axis2Prep :=
  let (xl, yl) := ax.targetLimits
  ax.prepareWith xl yl

/-- The grid item this axis reports. -/
def item (ax : Axis2) (span : Span) (p : Axis2Prep) : Item :=
  { span, protrusions := p.protrusions
    width := reportedSize ax.width none true, height := reportedSize ax.height none true }

/-- The axis box inside a cell (`update_computedbbox!`). -/
def computedBox (ax : Axis2) (cell : BBox) : BBox :=
  place cell ax.width ax.height none none (reportedSize ax.width none true) (reportedSize ax.height none true)
    ax.halign ax.valign

/-- Makie `sceneareanode!`: the plot area inside the computed box (aspect applied, centred,
rounded to integer pixels). -/
def viewport (ax : Axis2) (box : BBox) (xl yl : Float × Float) : BBox :=
  let w := box.width
  let h := box.height
  let asp : Option Float := match ax.aspect with
    | .auto => none
    | .ratio r => some r
    | .data => some ((xl.2 - xl.1) / (yl.2 - yl.1))
  let (mw, mh) := match asp with
    | none => (w, h)
    | some a =>
      let as := w / h
      if as ≥ a then (w * (a / as), h) else (w, h * (as / a))
  let l := box.left + 0.5 * (w - mw)
  let b := box.bottom + 0.5 * (h - mh)
  let bb : BBox := ⟨f32 l, f32 (l + mw), f32 b, f32 (b + mh)⟩
  if bb.left.isFinite && bb.right.isFinite && bb.bottom.isFinite && bb.top.isFinite then bb.roundInt
  else box.roundInt

/-- Tick positions along the axis (figure pixels, Float32): `px_o + px_w · fraction`. -/
def tickPositions (sc : Scale) (lo hi : Float) (a b : Float) (reversed : Bool) (vals : Array Float) : Array Float :=
  let (o, e) := if reversed then (b, a) else (a, b)
  let s0 := sc.forward lo
  let s1 := sc.forward hi
  vals.map fun v => f32 (o + (e - o) * ((sc.forward v - s0) / (s1 - s0)))

/-- The data → device projector of the plot area. -/
def projector (ax : Axis2) (vp : BBox) (xl yl : Float × Float) (figH : Float) : Lower.Projector :=
  let sx0 := ax.xscale.forward xl.1
  let sx1 := ax.xscale.forward xl.2
  let sy0 := ax.yscale.forward yl.1
  let sy1 := ax.yscale.forward yl.2
  let (ox, ex) := if ax.xreversed then (vp.right, vp.left) else (vp.left, vp.right)
  let (oy, ey) := if ax.yreversed then (vp.top, vp.bottom) else (vp.bottom, vp.top)
  let kx := (ex - ox) / (sx1 - sx0)
  let ky := (ey - oy) / (sy1 - sy0)
  let xs := ax.xscale
  let ys := ax.yscale
  let proj : Vec3 → Vec3 :=
    if xs == .identity && ys == .identity then
      fun p => ⟨ox + (p.x - sx0) * kx, figH - (oy + (p.y - sy0) * ky), 0⟩
    else
      fun p => ⟨ox + (xs.forward p.x - sx0) * kx, figH - (oy + (ys.forward p.y - sy0) * ky), 0⟩
  { project := proj
    clip := some ⟨vp.left, figH - vp.top, vp.width, vp.height⟩
    axisAligned := true }

/-- Segment pairs as a stroked path op. -/
def segOp (pts : Array (Float × Float × Float × Float)) (color : RGBA) (width : Float) (dash : Array Float := #[]) : Option DrawOp :=
  if pts.isEmpty || !(width > 0) || color.a == 0 then none else
  let p := pts.foldl (init := ({} : Path)) fun p (x0, y0, x1, y1) => (p.moveTo x0 y0).lineTo x1 y1
  some (.path p none (some { color, width, cap := .butt, miterLimit := 1.1547005383792517, dash }) none)

/-- The box of an axis legend inside the viewport (`axislegend`: `bbox = viewport`). -/
def legendBox (_ax : Axis2) (lg : AxisLegend) (entries : Array LegendEntry) (vp : BBox) : BBox :=
  let (w, h) := Legend.autosize lg.style lg.title entries
  place vp .auto .auto (some w) (some h) (some w) (some h) lg.halign lg.valign

/-- Positions of an axis's decorations in figure pixels (y up), as Makie computes them. -/
structure Anchors where
  /-- Major tick positions along x (pixels) and along y. -/
  xticks : Array Float
  yticks : Array Float
  xminor : Array Float
  yminor : Array Float
  /-- y of the x tick label anchors; x of the y tick label anchors. -/
  xTickLabelY : Float
  yTickLabelX : Float
  xlabel : Float × Float
  ylabel : Float × Float
  title : Float × Float
  deriving Inhabited

/-- The decoration anchors for a prepared axis with viewport `vp`. -/
def anchors (ax : Axis2) (p : Axis2Prep) (vp : BBox) : Anchors :=
  let st := ax.style
  let sw := st.spinewidth
  let tickspace (ls : LineAxisStyle) : Float := if ls.ticksvisible then max 0 (ls.ticksize * (1 - ls.tickalign)) else 0
  let labelgap (ls : LineAxisStyle) (space : Float) : Float :=
    sw + tickspace ls + (if ls.ticklabelsvisible then space + ls.ticklabelpad else 0) + ls.labelpadding
  { xticks := tickPositions ax.xscale p.xlims.1 p.xlims.2 vp.left vp.right ax.xreversed p.xt.values
    yticks := tickPositions ax.yscale p.ylims.1 p.ylims.2 vp.bottom vp.top ax.yreversed p.yt.values
    xminor := tickPositions ax.xscale p.xlims.1 p.xlims.2 vp.left vp.right ax.xreversed p.xt.minor
    yminor := tickPositions ax.yscale p.ylims.1 p.ylims.2 vp.bottom vp.top ax.yreversed p.yt.minor
    xTickLabelY := f32 (vp.bottom - (sw + tickspace st.x + st.x.ticklabelpad))
    yTickLabelX := f32 (vp.left - (sw + tickspace st.y + st.y.ticklabelpad))
    xlabel := (f32 (vp.left + 0.5 * vp.width), f32 (vp.bottom - labelgap st.x p.xTickSpace))
    ylabel := (f32 (vp.left - labelgap st.y p.yTickSpace), f32 (vp.bottom + 0.5 * vp.height))
    title := (f32 (vp.left + st.titlealign * vp.width), f32 (vp.top + st.titlegap)) }

/-- Draw ops of the axis (decorations, marks, axis legend), each tagged with Makie's
z-value, for viewport `vp` (figure pixels, y up) in a figure of height `figH`. -/
def lower (ax : Axis2) (p : Axis2Prep) (vp : BBox) (figH : Float) : Array (Float × DrawOp) := Id.run do
  let st := ax.style
  let sw := st.spinewidth
  let dy (y : Float) : Float := figH - y
  let an := ax.anchors p vp
  let mut ops : Array (Float × DrawOp) := #[]
  -- background
  if st.backgroundcolor.a > 0 then
    ops := ops.push (-100, .path (Path.rect ⟨vp.left, dy vp.top, vp.width, vp.height⟩) (some { color := st.backgroundcolor }) none none)
  -- grid (z = -10): x grid, x minor grid, y grid, y minor grid
  let gridOps : Array (Option DrawOp) := #[
    if st.x.gridvisible then segOp (an.xticks.map fun x => (x, dy vp.bottom, x, dy vp.top)) st.x.gridcolor st.x.gridwidth (st.x.gridstyle.dashArray st.x.gridwidth) else none,
    if st.x.minorgridvisible then segOp (an.xminor.map fun x => (x, dy vp.bottom, x, dy vp.top)) st.x.minorgridcolor st.x.minorgridwidth else none,
    if st.y.gridvisible then segOp (an.yticks.map fun y => (vp.left, dy y, vp.right, dy y)) st.y.gridcolor st.y.gridwidth (st.y.gridstyle.dashArray st.y.gridwidth) else none,
    if st.y.minorgridvisible then segOp (an.yminor.map fun y => (vp.left, dy y, vp.right, dy y)) st.y.minorgridcolor st.y.minorgridwidth else none]
  for g in gridOps do
    if let some op := g then ops := ops.push (-10, op)
  -- one LineAxis: ticks (10), minor ticks (10), label (0), spine (20), tick labels (0)
  let lineAxis (horizontal : Bool) (ls : LineAxisStyle) (pos minor : Array Float) (t : AxisTicks)
      (label : String) (labelPos : Float × Float) (tickLabelAt : Float) (spineVisible : Bool) : Array (Float × DrawOp) := Id.run do
    let mut out : Array (Float × DrawOp) := #[]
    -- tick marks: start = pos + (align·size − ½ spinewidth) outward sign, length `size`
    let tickSegs (ps : Array Float) (size align : Float) : Array (Float × Float × Float × Float) :=
      ps.map fun q =>
        let s0 := align * size - 0.5 * sw
        let s1 := s0 - size
        if horizontal then (q, dy (vp.bottom + s0), q, dy (vp.bottom + s1))
        else (vp.left + s0, dy q, vp.left + s1, dy q)
    if ls.ticksvisible then
      if let some op := segOp (tickSegs pos ls.ticksize ls.tickalign) ls.tickcolor ls.tickwidth then out := out.push (10, op)
    if ls.minorticksvisible then
      if let some op := segOp (tickSegs minor ls.minorticksize ls.minortickalign) ls.minortickcolor ls.minortickwidth then out := out.push (10, op)
    -- axis label
    if ls.labelvisible && !FigText.isBlank label then
      out := out.push (0, .text labelPos.1 (dy labelPos.2) label (labelStyle ls horizontal) none)
    -- spine
    if spineVisible && sw > 0 then
      let seg := if horizontal then (vp.left - 0.5 * sw, dy vp.bottom, vp.right + 0.5 * sw, dy vp.bottom)
                 else (vp.left, dy (vp.bottom - 0.5 * sw), vp.left, dy (vp.top + 0.5 * sw))
      if let some op := segOp #[seg] st.spinecolor sw then out := out.push (20, op)
    -- tick labels
    if ls.ticklabelsvisible then
      let style := tickLabelStyle ls horizontal
      for i in [0:min pos.size t.labels.size] do
        let q := pos[i]!
        let (x, y) := if horizontal then (q, tickLabelAt) else (tickLabelAt, q)
        out := out.push (0, FigText.labelOp style t.labels[i]! x (dy y))
    return out
  ops := ops ++ lineAxis true st.x an.xticks an.xminor p.xt ax.xlabel an.xlabel an.xTickLabelY st.bottomspinevisible
  ops := ops ++ lineAxis false st.y an.yticks an.yminor p.yt ax.ylabel an.ylabel an.yTickLabelX st.leftspinevisible
  -- opposite spines
  if st.topspinevisible then
    if let some op := segOp #[(vp.left - 0.5 * sw, dy vp.top, vp.right + 0.5 * sw, dy vp.top)] st.spinecolor sw then
      ops := ops.push (20, op)
  if st.rightspinevisible then
    if let some op := segOp #[(vp.right, dy (vp.bottom - 0.5 * sw), vp.right, dy (vp.top + 0.5 * sw))] st.spinecolor sw then
      ops := ops.push (20, op)
  -- title
  if st.titlevisible && !FigText.isBlank ax.title then
    ops := ops.push (0, .text an.title.1 (dy an.title.2) ax.title ax.titleStyle none)
  -- plots
  let pr := ax.projector vp p.xlims p.ylims figH
  for it in ax.items do
    let mk := match it.mark with
      | .hlines ys s =>
        Mark.segments (.xy (ys.foldl (init := Pts2.empty) fun q y => (q.push p.xlims.1 y).push p.xlims.2 y)) s
      | .vlines xs s =>
        Mark.segments (.xy (xs.foldl (init := Pts2.empty) fun q x => (q.push x p.ylims.1).push x p.ylims.2)) s
      | m => m
    for op in Lower.mark pr mk do
      ops := ops.push (0, op)
  -- axis legend
  if let some lg := ax.legend then
    let entries := LegendEntry.ofItems ax.items
    if !entries.isEmpty then
      ops := ops ++ Legend.lower lg.style lg.title entries (ax.legendBox lg entries vp) figH
  return ops

end Axis2

end LeanPlot
