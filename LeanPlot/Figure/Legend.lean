import LeanPlot.Figure.Lower
import LeanPlot.Figure.Theme

/-!
# Legend

A port of Makie's `Legend` block (`makielayout/blocks/legend.jl`):

* entries are built from labelled plots (`legendelements`): lines give a `LineElement`
  (a horizontal line through the middle of the patch), scatter a `MarkerElement` (one
  marker at the centre), band/poly/mesh/heatmap a `PolyElement` (the whole patch);
* the content is a nested grid solved with the same GridLayoutBase port as the figure: an
  outer grid with `Outside(padding)` holding an optional bold title and a subgrid of
  `patch | label` pairs (`patchsize` 20×20, `patchlabelgap` 5, `rowgap` 3, `colgap` 16,
  `titlegap` 8), `nbanks` columns (vertical) or rows (horizontal);
* the legend's automatic size is the grid's determined size plus the margins; it is drawn
  as a white frame (1 px black stroke) with the elements and labels on top.

Coordinates are Makie figure pixels (y up); `lower` flips to device space with the figure
height.
-/

namespace LeanPlot

open LeanPlot.Num LeanPlot.Layout

/-- One symbol drawn in a legend patch. -/
inductive LegendElement where
  | line (color : RGBA) (width : Float) (style : LineStyle)
  | marker (shape : MarkerShape) (size : Float) (color : RGBA) (strokeColor : RGBA) (strokeWidth : Float)
  | poly (color : RGBA) (strokeColor : RGBA) (strokeWidth : Float)
  deriving Inhabited

/-- A legend row: its label and the symbols drawn in its patch. -/
structure LegendEntry where
  label : String
  elements : Array LegendElement
  deriving Inhabited

namespace LegendEntry

/-- The legend elements of a mark (Makie `legendelements`). Colour-mapped colours fall back
to Makie's legend defaults (black lines/markers, grey polygons). -/
def elementsOf (m : Mark) : Array LegendElement :=
  let scalar (c : ColorSpec) (d : RGBA) : RGBA := match c with | .solid c => c | _ => d
  match m with
  | .lines _ s | .segments _ s => #[.line (scalar s.color RGBA.black) s.width s.style]
  | .scatter _ s => #[.marker s.shape s.size (scalar s.color RGBA.black) s.strokeColor s.strokeWidth]
  | .band _ _ c => #[.poly (scalar c RGBA.black) RGBA.transparent 0]
  | .poly _ s => #[.poly (scalar s.color (RGBA.gray 0.4)) s.strokeColor s.strokeWidth]
  | .mesh m => #[.poly (scalar m.color wongColors[0]!) RGBA.transparent 0]
  | .heatmap h => #[.poly (h.mapping.colormap.interpolatedGetIndex 0.5) RGBA.transparent 0]
  | .arrows _ _ s => #[.line (scalar s.color RGBA.black) s.shaftwidth .solid]
  | _ => #[]

/-- Entries for every labelled item, in plotting order (Makie `Legend(fig, ax)` with
`merge = false`). -/
def ofItems (items : Array PlotItem) : Array LegendEntry :=
  items.filterMap fun it => it.label.map fun l => ⟨l, elementsOf it.mark⟩

end LegendEntry

/-- Legend orientation. -/
inductive Orientation where
  | vertical | horizontal
  deriving Repr, Inhabited, BEq, DecidableEq

/-- Legend attributes with Makie's defaults. -/
structure LegendStyle where
  orientation : Orientation := .vertical
  nbanks : Nat := 1
  padding : Sides := Sides.uniform 6
  margin : Sides := {}
  backgroundcolor : RGBA := RGBA.white
  framecolor : RGBA := RGBA.black
  framewidth : Float := 1
  framevisible : Bool := true
  patchsize : Float × Float := (20, 20)
  patchlabelgap : Float := 5
  rowgap : Float := 3
  colgap : Float := 16
  titlegap : Float := 8
  labelsize : Float := 14
  labelcolor : RGBA := RGBA.black
  titlesize : Float := 14
  titlecolor : RGBA := RGBA.black
  titlebold : Bool := true
  /-- Line element endpoints as fractions of the patch (Makie `linepoints`). -/
  linepoints : Float × Float × Float × Float := (0, 0.5, 1, 0.5)
  deriving Inhabited

namespace Legend

/-- Label text style. -/
def labelStyle (s : LegendStyle) : TextStyle :=
  { size := s.labelsize, color := s.labelcolor, halign := .left, valign := .middle }

/-- Title text style. -/
def titleStyle (s : LegendStyle) : TextStyle :=
  { size := s.titlesize, color := s.titlecolor, bold := s.titlebold, halign := .center, valign := .middle }

/-- Grid position of entry `n` (0-based): `(row, bank)` for vertical legends, transposed for
horizontal ones (Makie's `rowcol`). -/
def entryCell (s : LegendStyle) (n : Nat) : Nat × Nat :=
  let nb := max 1 s.nbanks
  let r := n / nb
  let c := n % nb
  if s.orientation == .vertical then (r, c) else (c, r)

/-- The entry subgrid: its grid spec and items (patch and label per entry). -/
def subgrid (s : LegendStyle) (entries : Array LegendEntry) : Grid × Array Item :=
  let cells := (Array.range entries.size).map (entryCell s)
  let nr := cells.foldl (fun acc (r, _) => max acc (r + 1)) 0
  let nc := cells.foldl (fun acc (_, c) => max acc (c + 1)) 0
  let (pw, ph) := s.patchsize
  let items := (Array.range entries.size).foldl (init := #[]) fun acc k =>
    let (r, j) := cells[k]!
    let (lw, lh) := FigText.stringSize (labelStyle s) entries[k]!.label
    (acc.push { span := Span.cell r (2 * j), width := some pw, height := some ph }).push
      { span := Span.cell r (2 * j + 1), width := some (f32 lw), height := some (f32 lh) }
  let colGaps := (Array.range (2 * nc)).map fun c => if c % 2 == 0 then s.patchlabelgap else s.colgap
  ({ nrows := nr, ncols := 2 * nc, rowGaps := Array.replicate nr s.rowgap, colGaps
     align := .inside }, items)

/-- The outer grid: optional title row, then the entry subgrid (as one item reporting its
determined size). -/
def outer (s : LegendStyle) (title : Option String) (entries : Array LegendEntry) : Grid × Array Item :=
  let (sg, sitems) := subgrid s entries
  let sw := sg.determinedTotal sitems true
  let sh := sg.determinedTotal sitems false
  match title with
  | some t =>
    let (tw, th) := FigText.stringSize (titleStyle s) t
    ({ nrows := 2, ncols := 1, rowGaps := #[s.titlegap], align := .outside s.padding },
     #[{ span := Span.cell 0 0, width := some (f32 tw), height := some (f32 th) },
       { span := Span.cell 1 0, width := sw, height := sh }])
  | none =>
    ({ nrows := 1, ncols := 1, align := .outside s.padding },
     #[{ span := Span.cell 0 0, width := sw, height := sh }])

/-- Makie's automatic legend size (content plus paddings plus margins). -/
def autosize (s : LegendStyle) (title : Option String) (entries : Array LegendEntry) : Float × Float :=
  let (g, items) := outer s title entries
  let w := (g.determinedTotal items true).getD 0
  let h := (g.determinedTotal items false).getD 0
  (f32 (w + s.margin.left + s.margin.right), f32 (h + s.margin.bottom + s.margin.top))

/-- Device point of a figure point. -/
@[inline] def dev (figH x y : Float) : Float × Float := (x, figH - y)

/-- Draw ops (z-tagged, Makie's legend scene order) for a legend whose computed box is
`box` (figure pixels, y up). -/
def lower (s : LegendStyle) (title : Option String) (entries : Array LegendEntry) (box : BBox) (figH : Float) :
    Array (Float × DrawOp) := Id.run do
  let area := box.roundInt
  let rect := area.shrink s.margin.left s.margin.right s.margin.bottom s.margin.top
  let (og, oitems) := outer s title entries
  let osol := og.solve oitems rect
  let mut ops : Array (Float × DrawOp) := #[]
  -- frame / background (z = 10 - 7)
  if s.framevisible then
    let r : Rect := ⟨rect.left, figH - rect.top, rect.width, rect.height⟩
    ops := ops.push (3, .path (Path.rect r) (some { color := s.backgroundcolor })
      (if s.framewidth > 0 then some { color := s.framecolor, width := s.framewidth, miterLimit := 2.0 } else none) none)
  -- title
  let subItemIdx := if title.isSome then 1 else 0
  let mut labelOps : Array (Float × DrawOp) := #[]
  if let some t := title then
    let it := oitems[0]!
    let cell := osol.cell it.span
    let b := place cell .auto .auto it.width it.height it.width it.height 0.5 0.5
    let (x, y) := dev figH (b.left + 0.5 * b.width) (b.bottom + 0.5 * b.height)
    labelOps := labelOps.push (10, .text x y t (titleStyle s) none)
  -- entries
  let (sg, sitems) := subgrid s entries
  let scell := osol.cell oitems[subItemIdx]!.span
  let sw := oitems[subItemIdx]!.width
  let sh := oitems[subItemIdx]!.height
  let sbox := place scell .auto .auto sw sh sw sh 0.5 0.5
  -- the subgrid fills its computed box, aligned centrally (`gridshalign`/`gridsvalign`)
  let ssol := sg.solve sitems sbox
  let mut elemOps : Array (Float × DrawOp) := #[]
  for k in [0:entries.size] do
    let e := entries[k]!
    let pit := sitems[2 * k]!
    let lit := sitems[2 * k + 1]!
    let pcell := ssol.cell pit.span
    let pb := place pcell (.fixed s.patchsize.1) (.fixed s.patchsize.2) none none pit.width pit.height 0.5 0.5
    let lcell := ssol.cell lit.span
    let lb := place lcell .auto .auto lit.width lit.height lit.width lit.height 0 0.5
    let (lx, ly) := dev figH lb.left (lb.bottom + 0.5 * lb.height)
    labelOps := labelOps.push (10, .text lx ly e.label (labelStyle s) none)
    for el in e.elements do
      match el with
      | .line c w st =>
        let (fx0, fy0, fx1, fy1) := s.linepoints
        let (x0, y0) := dev figH (pb.left + fx0 * pb.width) (pb.bottom + fy0 * pb.height)
        let (x1, y1) := dev figH (pb.left + fx1 * pb.width) (pb.bottom + fy1 * pb.height)
        elemOps := elemOps.push (10, .path (({} : Path).moveTo x0 y0 |>.lineTo x1 y1) none
          (some { color := c, width := w, miterLimit := 2.0, dash := st.dashArray w }) none)
      | .marker shape sz c sc sw' =>
        let (x, y) := dev figH (pb.left + 0.5 * pb.width) (pb.bottom + 0.5 * pb.height)
        elemOps := elemOps.push (10, .path (shape.toPath sz x y) (some { color := c })
          (if sw' > 0 then some { color := sc, width := sw' } else none) none)
      | .poly c sc sw' =>
        let r : Rect := ⟨pb.left, figH - pb.top, pb.width, pb.height⟩
        elemOps := elemOps.push (10, .path (Path.rect r) (some { color := c })
          (if sw' > 0 then some { color := sc, width := sw', miterLimit := 2.0 } else none) none)
  -- Makie creates each entry's label before its symbol plots; titles and labels live in
  -- their own block scenes, drawn after the legend scene's element plots
  return ops ++ elemOps ++ labelOps

end Legend

end LeanPlot
