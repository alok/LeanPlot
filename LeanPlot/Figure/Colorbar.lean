import LeanPlot.Figure.Axis2

/-!
# Colorbar

A port of Makie's `Colorbar` block (`makielayout/blocks/colorbar.jl`):

* the bar is `size = 12` px thick (the block's automatic width, or height when horizontal) and
  fills its cell along the other direction;
* the colours are the colormap sampled at the midpoints of `LinRange(lo, hi, nsteps = 100)`,
  drawn as a 1×99 (or 99×1) image stretched over the bar with bilinear filtering, as
  CairoMakie does for continuous colormaps;
* a closed 1 px outline, and a `LineAxis` on the right (vertical, `flipaxis = true`) or top:
  ticks 5 px outward, tick labels 3 px further, optional label with padding 5;
* the protrusion on the axis side is `tickspace + ticklabelspace + ticklabelpad + label`.
-/

namespace LeanPlot

open LeanPlot.Num LeanPlot.Layout

/-- Where a colorbar gets its colormap and range. -/
inductive ColorbarSource where
  /-- An explicit colormap and range. -/
  | explicit (mapping : ColorMapping) (lo hi : Float)
  /-- The `index`-th colour-mapped item (default: the first) of the axis at grid cell
  `(row, col)` (1-based, as placed). -/
  | axisAt (row col : Nat) (index : Option Nat := none)
  deriving Inhabited

/-- A colorbar with Makie's defaults. -/
structure Colorbar where
  source : ColorbarSource := .explicit {} 0 1
  label : String := ""
  vertical : Bool := true
  flipaxis : Bool := true
  size : Float := 12
  nsteps : Nat := 100
  ticks : TickSpec := .auto
  axisStyle : LineAxisStyle := { ticklabelpad := 3, labelpadding := 5, gridvisible := false, minorIntervals := 5 }
  spinewidth : Float := 1
  spinecolor : RGBA := RGBA.black
  width : SizeAttr := .auto
  height : SizeAttr := .auto
  halign : Float := 0.5
  valign : Float := 0.5
  deriving Inhabited

/-- A colorbar resolved against its data: mapping and range, ticks and protrusions. -/
structure ColorbarPrep where
  mapping : ColorMapping
  lo : Float
  hi : Float
  ticks : AxisTicks
  tickSpace : Float
  protrusions : Sides
  deriving Inhabited

namespace Colorbar

/-- The axis direction of the tick labels is horizontal for horizontal bars. -/
def horizontal (cb : Colorbar) : Bool := !cb.vertical

/-- Tick label style: Makie's automatic alignment beside the (flipped) colorbar axis. -/
def tickLabelStyle (cb : Colorbar) : TextStyle :=
  let st := cb.axisStyle
  let (ha, va) := Axis2.autoTickAlign (!cb.vertical) cb.flipaxis st.ticklabelrotation
  { size := st.ticklabelsize, color := st.ticklabelcolor, rotation := st.ticklabelrotation, halign := ha, valign := va }

/-- Label style (rotated for vertical bars). -/
def labelStyle (cb : Colorbar) : TextStyle :=
  let st := cb.axisStyle
  if cb.vertical then
    { size := st.labelsize, color := st.labelcolor, halign := .center
      valign := if cb.flipaxis then .top else .bottom, rotation := Num.pi / 2 }
  else
    { size := st.labelsize, color := st.labelcolor, halign := .center, valign := if cb.flipaxis then .bottom else .top }

/-- Resolve against a mapping and range. -/
def prepareWith (cb : Colorbar) (m : ColorMapping) (lo hi : Float) : ColorbarPrep :=
  let st := cb.axisStyle
  let t := Axis2.ticksFor cb.ticks m.colorscale st lo hi
  let style := cb.tickLabelStyle
  let rects := t.labels.map fun l => FigText.labelBounds style l 0 0
  let space := match st.ticklabelspace with
    | some s => s
    | none =>
      if !st.ticklabelsvisible then 0 else
      match FigText.unionRects rects with
      | some r => let v := if cb.vertical then r.w else r.h; if v.isFinite then f32 v else 0
      | none => 0
  let labelSize :=
    if FigText.isBlank cb.label then 0 else
    let (w, h) := FigText.stringSize cb.labelStyle cb.label
    f32 (if cb.vertical then w else h)
  let labelspace := if st.labelvisible && !FigText.isBlank cb.label then labelSize + st.labelpadding else 0
  let tickspace := if st.ticksvisible && !t.values.isEmpty then max 0 (st.ticksize * (1 - st.tickalign)) else 0
  let gap := if st.ticklabelsvisible && space > 0 then space + st.ticklabelpad else 0
  let prot := f32 (tickspace + gap + labelspace)
  let sides : Sides :=
    if cb.vertical then (if cb.flipaxis then { right := prot } else { left := prot })
    else (if cb.flipaxis then { top := prot } else { bottom := prot })
  { mapping := m, lo, hi, ticks := t, tickSpace := space, protrusions := sides }

/-- The grid item. -/
def item (cb : Colorbar) (span : Span) (p : ColorbarPrep) : Item :=
  let (aw, ah) : Option Float × Option Float := if cb.vertical then (some cb.size, none) else (none, some cb.size)
  { span, protrusions := p.protrusions
    width := reportedSize cb.width aw true, height := reportedSize cb.height ah true }

/-- The colorbar box inside its cell. -/
def computedBox (cb : Colorbar) (cell : BBox) : BBox :=
  let (aw, ah) : Option Float × Option Float := if cb.vertical then (some cb.size, none) else (none, some cb.size)
  place cell cb.width cb.height aw ah (reportedSize cb.width aw true) (reportedSize cb.height ah true) cb.halign cb.valign

/-- Space reserved for the low/high clip triangles when the mapping has a
`lowclip`/`highclip` colour, else 0. Makie intends `sin(π/3)` times the bar thickness, but
its `tri_heights` returns before applying the factor, so the full thickness is reserved
(the triangles themselves are `sin(π/3)` tall, leaving a small gap); we match that. -/
def triHeights (cb : Colorbar) (m : ColorMapping) (fb : BBox) : Float × Float :=
  let t := if cb.vertical then fb.width else fb.height
  ((if m.lowclip.isSome then t else 0), (if m.highclip.isSome then t else 0))

/-- The bar box: the rounded frame box minus the clip triangles (Makie `barbox`). -/
def barBox (cb : Colorbar) (m : ColorMapping) (box : BBox) : BBox :=
  let fb := box.roundInt
  let (lo, hi) := cb.triHeights m fb
  if cb.vertical then ⟨fb.left, fb.right, fb.bottom + lo, fb.top - hi⟩
  else ⟨fb.left + lo, fb.right - hi, fb.bottom, fb.top⟩

/-- The axis line of the bar `(start, end, position across)` in figure pixels: along the
right (vertical, `flipaxis`) or top edge of the bar box. -/
def axisLineOf (cb : Colorbar) (bb : BBox) : Float × Float × Float :=
  if cb.vertical then (bb.bottom, bb.top, if cb.flipaxis then bb.right else bb.left)
  else (bb.left, bb.right, if cb.flipaxis then bb.top else bb.bottom)

/-- The axis line for a prepared colorbar in its computed box. -/
def axisLine (cb : Colorbar) (p : ColorbarPrep) (box : BBox) : Float × Float × Float :=
  cb.axisLineOf (cb.barBox p.mapping box)

/-- Tick positions along the bar (figure pixels). -/
def tickPositions (cb : Colorbar) (p : ColorbarPrep) (box : BBox) : Array Float :=
  let (a, b, _) := cb.axisLine p box
  Axis2.tickPositions p.mapping.colorscale p.lo p.hi a b false p.ticks.values

/-- Draw ops (z-tagged) for a colorbar with computed box `box`. -/
def lower (cb : Colorbar) (p : ColorbarPrep) (box : BBox) (figH : Float) : Array (Float × DrawOp) := Id.run do
  let st := cb.axisStyle
  let dy (y : Float) : Float := figH - y
  let mut ops : Array (Float × DrawOp) := #[]
  -- colours at the midpoints of LinRange(lo, hi, nsteps), mapped linearly: Makie draws the
  -- gradient as an `image!` without the colour scale (`makielayout/blocks/colorbar.jl:306-346`),
  -- so a nonlinear `colorscale` only moves the ticks
  let n := max 2 cb.nsteps
  let steps := Num.linRange p.lo p.hi n
  let mids : FloatArray := (Array.range (n - 1)).foldl (init := FloatArray.emptyWithCapacity (n - 1)) fun acc i =>
    acc.push ((steps[i]! + steps[i + 1]!) / 2)
  let rgba := { p.mapping with colorscale := .identity }.toRGBA8 p.lo p.hi mids
  let img : ByteArray :=
    if cb.vertical then
      -- one column, top row = largest value
      (Array.range (n - 1)).foldl (init := ByteArray.emptyWithCapacity (4 * (n - 1))) fun acc i =>
        let k := 4 * (n - 2 - i)
        (((acc.push (rgba.get! k)).push (rgba.get! (k + 1))).push (rgba.get! (k + 2))).push (rgba.get! (k + 3))
    else rgba
  let (iw, ih) := if cb.vertical then (1, n - 1) else (n - 1, 1)
  let bb := cb.barBox p.mapping box
  let dst : Rect := ⟨bb.left, dy bb.top, bb.width, bb.height⟩
  ops := ops.push (0, .image iw ih img dst .linear none)
  -- clip triangles (high first, as Makie creates them) and their tips
  let s3 := Float.sin (Num.pi / 3)
  let hiTip : Float × Float :=
    if cb.vertical then (bb.left + 0.5 * bb.width, bb.top + bb.width * s3)
    else (bb.right + bb.height * s3, bb.bottom + 0.5 * bb.height)
  let loTip : Float × Float :=
    if cb.vertical then (bb.left + 0.5 * bb.width, bb.bottom - bb.width * s3)
    else (bb.left - bb.height * s3, bb.bottom + 0.5 * bb.height)
  let tri (a b c : Float × Float) : Path :=
    ((((({} : Path).moveTo a.1 (dy a.2)).lineTo b.1 (dy b.2)).lineTo c.1 (dy c.2))).close
  if let some hc := p.mapping.highclip then
    let path := if cb.vertical then tri (bb.left, bb.top) (bb.right, bb.top) hiTip
                else tri (bb.right, bb.top) (bb.right, bb.bottom) hiTip
    ops := ops.push (0, .path path (some { color := hc }) none none)
  if let some lc := p.mapping.lowclip then
    let path := if cb.vertical then tri (bb.left, bb.bottom) (bb.right, bb.bottom) loTip
                else tri (bb.left, bb.bottom) (bb.left, bb.top) loTip
    ops := ops.push (0, .path path (some { color := lc }) none none)
  -- outline (a closed `lines` plot through the triangle tips)
  if cb.spinewidth > 0 then
    let pts : Array (Float × Float) :=
      if cb.vertical then
        #[(bb.right, bb.bottom), (bb.right, bb.top)] ++ (if p.mapping.highclip.isSome then #[hiTip] else #[]) ++
        #[(bb.left, bb.top), (bb.left, bb.bottom)] ++ (if p.mapping.lowclip.isSome then #[loTip] else #[]) ++
        #[(bb.right, bb.bottom)]
      else
        #[(bb.left, bb.bottom), (bb.right, bb.bottom)] ++ (if p.mapping.highclip.isSome then #[hiTip] else #[]) ++
        #[(bb.right, bb.top), (bb.left, bb.top)] ++ (if p.mapping.lowclip.isSome then #[loTip] else #[]) ++
        #[(bb.left, bb.bottom)]
    let path := pts.foldl (init := ({} : Path)) fun acc (x, y) =>
      if acc.verbs.isEmpty then acc.moveTo x (dy y) else acc.lineTo x (dy y)
    ops := ops.push (0, .path path.close none (some { color := cb.spinecolor, width := cb.spinewidth, miterLimit := 2.0 }) none)
  -- the LineAxis along the flipped side
  let (a, b, pos) := cb.axisLine p box
  let positions := cb.tickPositions p box
  let sgn : Float := if cb.flipaxis then 1 else -1
  let tickspace := if st.ticksvisible then max 0 (st.ticksize * (1 - st.tickalign)) else 0
  if st.ticksvisible then
    let segs := positions.map fun q =>
      let s0 := sgn * (0.5 * cb.spinewidth - st.tickalign * st.ticksize)
      let s1 := s0 + sgn * st.ticksize
      if cb.vertical then (pos + s0, dy q, pos + s1, dy q) else (q, dy (pos + s0), q, dy (pos + s1))
    if let some op := Axis2.segOp segs st.tickcolor st.tickwidth then ops := ops.push (10, op)
  -- label
  if st.labelvisible && !FigText.isBlank cb.label then
    let gap := cb.spinewidth + tickspace + (if st.ticklabelsvisible then p.tickSpace + st.ticklabelpad else 0) + st.labelpadding
    let (x, y) := if cb.vertical then (pos + sgn * gap, a + 0.5 * (b - a)) else (a + 0.5 * (b - a), pos + sgn * gap)
    ops := ops.push (0, .text (f32 x) (dy (f32 y)) cb.label cb.labelStyle none)
  -- tick labels
  if st.ticklabelsvisible then
    let shift := sgn * (cb.spinewidth + tickspace + st.ticklabelpad)
    let style := cb.tickLabelStyle
    for i in [0:min positions.size p.ticks.labels.size] do
      let q := positions[i]!
      let (x, y) := if cb.vertical then (f32 (pos + shift), q) else (q, f32 (pos + shift))
      ops := ops.push (0, FigText.labelOp style p.ticks.labels[i]! x (dy y))
  return ops

end Colorbar

end LeanPlot
