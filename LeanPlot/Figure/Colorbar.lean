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

/-- Tick label style: left/centre aligned beside a vertical bar (`flipaxis`), centre/bottom
above a horizontal one. -/
def tickLabelStyle (cb : Colorbar) : TextStyle :=
  let st := cb.axisStyle
  let base : TextStyle := { size := st.ticklabelsize, color := st.ticklabelcolor }
  if cb.vertical then { base with halign := if cb.flipaxis then .left else .right, valign := .middle }
  else { base with halign := .center, valign := if cb.flipaxis then .bottom else .top }

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

/-- Draw ops (z-tagged) for a colorbar with computed box `box`. -/
def lower (cb : Colorbar) (p : ColorbarPrep) (box : BBox) (figH : Float) : Array (Float × DrawOp) := Id.run do
  let st := cb.axisStyle
  let fb := box.roundInt
  let dy (y : Float) : Float := figH - y
  let mut ops : Array (Float × DrawOp) := #[]
  -- colours at the midpoints of LinRange(lo, hi, nsteps)
  let n := max 2 cb.nsteps
  let steps := Num.linRange p.lo p.hi n
  let mids : FloatArray := (Array.range (n - 1)).foldl (init := FloatArray.emptyWithCapacity (n - 1)) fun acc i =>
    acc.push ((steps[i]! + steps[i + 1]!) / 2)
  let rgba := p.mapping.toRGBA8 p.lo p.hi mids
  let img : ByteArray :=
    if cb.vertical then
      -- one column, top row = largest value
      (Array.range (n - 1)).foldl (init := ByteArray.emptyWithCapacity (4 * (n - 1))) fun acc i =>
        let k := 4 * (n - 2 - i)
        (((acc.push (rgba.get! k)).push (rgba.get! (k + 1))).push (rgba.get! (k + 2))).push (rgba.get! (k + 3))
    else rgba
  let (iw, ih) := if cb.vertical then (1, n - 1) else (n - 1, 1)
  let dst : Rect := ⟨fb.left, dy fb.top, fb.width, fb.height⟩
  ops := ops.push (0, .image iw ih img dst .linear none)
  -- outline (a closed `lines` plot)
  if cb.spinewidth > 0 then
    let path := (((((({} : Path).moveTo fb.right (dy fb.bottom)).lineTo fb.right (dy fb.top)).lineTo fb.left (dy fb.top)).lineTo
      fb.left (dy fb.bottom)).lineTo fb.right (dy fb.bottom)).close
    ops := ops.push (0, .path path none (some { color := cb.spinecolor, width := cb.spinewidth, miterLimit := 2.0 }) none)
  -- the LineAxis along the flipped side
  let (a, b, pos) : Float × Float × Float :=
    if cb.vertical then (fb.bottom, fb.top, if cb.flipaxis then fb.right else fb.left)
    else (fb.left, fb.right, if cb.flipaxis then fb.top else fb.bottom)
  let positions := Axis2.tickPositions p.mapping.colorscale p.lo p.hi a b false p.ticks.values
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
