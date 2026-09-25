import LeanPlot.Figure.Colorbar
import LeanPlot.Figure.Axis3
import LeanPlot.IO

/-!
# Figure: blocks in a grid, lowered to a `Scene`

A `Figure` is a size, a theme and blocks placed in a grid (1-based rows and columns, spans
allowed), like `f[1, 2] = Axis(...)` in Makie. `Figure.solve` runs the layout
(GridLayoutBase port with `Outside(figure_padding)`, `rowgap = colgap = 18`), `toScene`
lowers every block to device-space draw ops and orders them by Makie's z-values (a stable
sort, as CairoMakie paints: backgrounds −100, grids −10, text and plots 0, legends 3–10,
ticks 10, spines 20), and `save` writes `.svg` or `.png`.

Colorbars and legends can refer to an axis by its grid cell (`ColorbarSource.axisAt`,
`LegendSource.axisAt`), so the figure stays a plain value.
-/

namespace LeanPlot

open LeanPlot.Num LeanPlot.Layout

/-- Where a legend gets its entries. -/
inductive LegendSource where
  | entries (es : Array LegendEntry)
  /-- The labelled items of the axis placed at `(row, col)` (1-based). -/
  | axisAt (row col : Nat)
  deriving Inhabited

/-- A legend block (Makie `Legend(f[i, j], ...)`). -/
structure LegendBlock where
  source : LegendSource := .entries #[]
  title : Option String := none
  style : LegendStyle := {}
  halign : Float := 0.5
  valign : Float := 0.5
  /-- `none` = Makie's `automatic`: tell the width for vertical legends, the height for
  horizontal ones. -/
  tellwidth : Option Bool := none
  tellheight : Option Bool := none
  deriving Inhabited

/-- A text block (Makie `Label`). -/
structure LabelBlock where
  text : String
  size : Float := 14
  color : RGBA := RGBA.black
  bold : Bool := false
  rotation : Float := 0
  padding : Sides := {}
  halign : Float := 0.5
  valign : Float := 0.5
  tellwidth : Bool := true
  tellheight : Bool := true
  deriving Inhabited

/-- A figure block. -/
inductive Block where
  | axis2 (a : Axis2)
  | axis3 (a : Axis3)
  | colorbar (c : Colorbar)
  | legend (l : LegendBlock)
  | label (l : LabelBlock)
  deriving Inhabited

/-- A block and its grid position (1-based, inclusive ranges). -/
structure Placed where
  rows : Nat × Nat
  cols : Nat × Nat
  block : Block
  deriving Inhabited

/-- A figure. -/
structure Figure where
  size : Nat × Nat := (600, 450)
  theme : Theme := {}
  content : Array Placed := #[]
  /-- Row sizes by 1-based index (default `auto`). -/
  rowSizes : Array (Nat × GridSize) := #[]
  /-- Column sizes by 1-based index (default `auto`). -/
  colSizes : Array (Nat × GridSize) := #[]
  /-- Gap overrides: `(i, px)` sets the gap after row `i` (1-based); `i = 0` sets every gap
  (Makie `rowgap!(layout, px)`). -/
  rowGaps : Array (Nat × Float) := #[]
  /-- Gap overrides after column `i` (1-based; `0` = all), Makie `colgap!`. -/
  colGaps : Array (Nat × Float) := #[]
  deriving Inhabited

/-- Per-block layout state. -/
inductive BlockPrep where
  | axis2 (p : Axis2Prep)
  | axis3 (p : Axis3Prep)
  | colorbar (p : ColorbarPrep)
  | legend (entries : Array LegendEntry) (w h : Float)
  | label (w h : Float)
  deriving Inhabited

/-- A solved block: its preparation, grid cell and computed box (figure pixels, y up). -/
structure SolvedBlock where
  prep : BlockPrep
  cell : BBox
  box : BBox
  /-- The plot area (axes only; integer pixels). -/
  viewport : BBox := ⟨0, 0, 0, 0⟩
  deriving Inhabited

namespace LabelBlock
/-- Text style. -/
def style (l : LabelBlock) : TextStyle :=
  { size := l.size, color := l.color, bold := l.bold, rotation := l.rotation, halign := .center, valign := .middle }
/-- Automatic size: text box plus padding. -/
def autosize (l : LabelBlock) : Float × Float :=
  let r := (Font.layoutStyled l.style l.text).deviceBounds l.size l.rotation 0 0
  (f32 (r.w + l.padding.left + l.padding.right), f32 (r.h + l.padding.bottom + l.padding.top))
end LabelBlock

namespace Figure

/-- An empty figure of the given size. -/
def new (size : Nat × Nat := (600, 450)) (theme : Theme := {}) : Figure := { size, theme }

/-- Place a block at rows `r0..r1`, columns `c0..c1` (1-based). -/
def placeSpan (f : Figure) (r0 r1 c0 c1 : Nat) (b : Block) : Figure :=
  { f with content := f.content.push ⟨(r0, r1), (c0, c1), b⟩ }

/-- Place a block in cell `(r, c)` (1-based). -/
def place (f : Figure) (r c : Nat) (b : Block) : Figure := f.placeSpan r r c c b

/-- Set a row size (1-based). -/
def rowsize (f : Figure) (r : Nat) (s : GridSize) : Figure := { f with rowSizes := f.rowSizes.push (r, s) }

/-- Set a column size (1-based). -/
def colsize (f : Figure) (c : Nat) (s : GridSize) : Figure := { f with colSizes := f.colSizes.push (c, s) }

/-- `colgap!(f.layout, px)` (`i = 0`, all gaps) or `colgap!(f.layout, i, px)`. -/
def colgap (f : Figure) (px : Float) (i : Nat := 0) : Figure := { f with colGaps := f.colGaps.push (i, px) }

/-- `rowgap!(f.layout, px)` (`i = 0`, all gaps) or `rowgap!(f.layout, i, px)`. -/
def rowgap (f : Figure) (px : Float) (i : Nat := 0) : Figure := { f with rowGaps := f.rowGaps.push (i, px) }

/-- Width and height in pixels as floats. -/
def dims (f : Figure) : Float × Float := (Num.ofInt f.size.1, Num.ofInt f.size.2)

/-- The block placed with top-left cell `(r, c)`. -/
def blockAt? (f : Figure) (r c : Nat) : Option Block :=
  (f.content.find? fun p => p.rows.1 == r && p.cols.1 == c).map (·.block)

/-- Items of the axis at `(r, c)` (2D or 3D). -/
def itemsAt (f : Figure) (r c : Nat) : Array PlotItem :=
  match f.blockAt? r c with
  | some (.axis2 a) => a.items
  | some (.axis3 a) => a.items
  | _ => #[]

/-- Resolve a colorbar's mapping and range. -/
def colorbarMapping (f : Figure) (cb : Colorbar) : ColorMapping × Float × Float :=
  match cb.source with
  | .explicit m lo hi => (m, lo, hi)
  | .axisAt r c idx =>
    let mapped := (f.itemsAt r c).filterMap (·.mark.colorMapping?)
    match mapped[idx.getD 0]? with
    | some x => x
    | none => ({}, 0, 1)

/-- Legend entries of a legend block. -/
def legendEntries (f : Figure) (l : LegendBlock) : Array LegendEntry :=
  match l.source with
  | .entries es => es
  | .axisAt r c => LegendEntry.ofItems (f.itemsAt r c)

/-- 0-based span of a placed block. -/
def span (p : Placed) : Span := ⟨p.rows.1 - 1, p.rows.2 - 1, p.cols.1 - 1, p.cols.2 - 1⟩

/-- Pre-layout preparation of a block; `lims` overrides an `Axis2`'s limits. -/
def prepBlock (f : Figure) (b : Block) (lims : Option ((Float × Float) × (Float × Float)) := none) : BlockPrep :=
  match b with
  | .axis2 a => match lims with
    | some (xl, yl) => .axis2 (a.prepareWith xl yl)
    | none => .axis2 a.prepare
  | .axis3 a => .axis3 a.prepare
  | .colorbar cb =>
    let (m, lo, hi) := f.colorbarMapping cb
    .colorbar (cb.prepareWith m lo hi)
  | .legend l =>
    let es := f.legendEntries l
    let (w, h) := Legend.autosize l.style l.title es
    .legend es w h
  | .label l => let (w, h) := l.autosize; .label w h

/-- The grid item a prepared block reports. -/
def itemOf (b : Block) (sp : Span) (p : BlockPrep) : Item :=
  match b, p with
  | .axis2 a, .axis2 pp => a.item sp pp
  | .axis3 a, _ => a.item sp
  | .colorbar cb, .colorbar pp => cb.item sp pp
  | .legend l, .legend es w h =>
    let vertical := l.style.orientation == .vertical
    let tw := l.tellwidth.getD vertical
    let th := l.tellheight.getD (!vertical)
    let _ := es
    { span := sp, width := if tw then some w else none, height := if th then some h else none }
  | .label l, .label w h =>
    { span := sp, width := if l.tellwidth then some w else none, height := if l.tellheight then some h else none }
  | _, _ => { span := sp }

/-- The block's computed box in its cell. -/
def boxOf (b : Block) (cell : BBox) (p : BlockPrep) : BBox :=
  match b, p with
  | .axis2 a, _ => a.computedBox cell
  | .axis3 a, _ => a.computedBox cell
  | .colorbar cb, _ => cb.computedBox cell
  | .legend l, .legend _ w h =>
    let vertical := l.style.orientation == .vertical
    let tw := l.tellwidth.getD vertical
    let th := l.tellheight.getD (!vertical)
    Layout.place cell .auto .auto (some w) (some h) (if tw then some w else none) (if th then some h else none) l.halign l.valign
  | .label l, .label w h =>
    Layout.place cell .auto .auto (some w) (some h) (if l.tellwidth then some w else none)
      (if l.tellheight then some h else none) l.halign l.valign
  | _, _ => cell

/-- The figure grid. -/
def grid (f : Figure) : Grid :=
  let nr := f.content.foldl (fun acc p => max acc p.rows.2) 0
  let nc := f.content.foldl (fun acc p => max acc p.cols.2) 0
  let rs := (Array.range nr).map fun i => ((f.rowSizes.reverse.find? (·.1 == i + 1)).map (·.2)).getD .auto
  let cs := (Array.range nc).map fun i => ((f.colSizes.reverse.find? (·.1 == i + 1)).map (·.2)).getD .auto
  -- later settings win; `0` addresses every gap
  let gapAt (gs : Array (Nat × Float)) (default : Float) (i : Nat) : Float :=
    gs.foldl (init := default) fun acc (k, px) => if k == 0 || k == i + 1 then px else acc
  { nrows := nr, ncols := nc, rowSizes := rs, colSizes := cs
    rowGaps := (Array.range (nr - 1)).map (gapAt f.rowGaps f.theme.rowgap)
    colGaps := (Array.range (nc - 1)).map (gapAt f.colGaps f.theme.colgap)
    defaultRowGap := f.theme.rowgap, defaultColGap := f.theme.colgap
    align := .outside f.theme.figurePadding }

/-- Solve the layout once with the given preparations. -/
def solveWith (f : Figure) (preps : Array BlockPrep) : Array SolvedBlock :=
  let (w, h) := f.dims
  let items := (Array.range f.content.size).map fun i =>
    itemOf f.content[i]!.block (span f.content[i]!) preps[i]!
  let sol := f.grid.solve items ⟨0, w, 0, h⟩
  (Array.range f.content.size).map fun i =>
    let pl := f.content[i]!
    let cell := sol.cell (span pl)
    let prep := preps[i]!
    let box := boxOf pl.block cell prep
    let viewport := match pl.block, prep with
      | .axis2 a, .axis2 pp => a.viewport box pp.xlims pp.ylims
      | .axis3 a, _ => a.sceneArea box
      | _, _ => box.roundInt
    { prep, cell, box, viewport }

/-- Solve the layout (iterating for axes with `autolimitaspect`, whose limits depend on the
viewport, as Makie's observables do). -/
def solve (f : Figure) : Array SolvedBlock := Id.run do
  let mut preps := f.content.map fun p => f.prepBlock p.block
  let mut solved := f.solveWith preps
  let needsIter := f.content.any fun p => match p.block with
    | .axis2 a => a.autolimitaspect.isSome
    | _ => false
  if !needsIter then return solved
  for _ in [0:4] do
    let newPreps := (Array.range f.content.size).map fun i =>
      match f.content[i]!.block, solved[i]! with
      | .axis2 a, sb =>
        if a.autolimitaspect.isNone then preps[i]! else
        let (xl, yl) := a.targetLimits
        let (xl, yl) := a.adjustLimits xl yl sb.viewport.width sb.viewport.height
        f.prepBlock (.axis2 a) (some (xl, yl))
      | _, _ => preps[i]!
    preps := newPreps
    solved := f.solveWith preps
  return solved

/-- Stable sort of z-tagged ops by z. -/
def sortOps (ops : Array (Float × DrawOp)) : Array DrawOp :=
  let tagged := (Array.range ops.size).map fun i => (ops[i]!.1, i)
  let order := tagged.qsort fun a b => a.1 < b.1 || (a.1 == b.1 && a.2 < b.2)
  order.map fun (_, i) => ops[i]!.2

/-- Lower one solved block. -/
def lowerBlock (f : Figure) (b : Block) (s : SolvedBlock) : Array (Float × DrawOp) :=
  let figH := f.dims.2
  match b, s.prep with
  | .axis2 a, .axis2 p => a.lower p s.viewport figH
  | .axis3 a, .axis3 p => a.lower p s.box figH
  | .colorbar cb, .colorbar p => cb.lower p s.box figH
  | .legend l, .legend es _ _ => Legend.lower l.style l.title es s.box figH
  | .label l, _ =>
    if FigText.isBlank l.text then #[] else
    let bx := s.box
    let (tw, th) := l.autosize
    let tw := tw - l.padding.left - l.padding.right
    let th := th - l.padding.bottom - l.padding.top
    let x := bx.left + l.padding.left + 0.5 * tw
    let y := bx.bottom + l.padding.bottom + 0.5 * th
    #[(0, .text (f32 x) (figH - f32 y) l.text l.style none)]
  | _, _ => #[]

/-- Lower the figure to a device-space scene. -/
def toScene (f : Figure) : Scene :=
  let solved := f.solve
  let ops := (Array.range f.content.size).foldl (init := #[]) fun acc i =>
    acc ++ f.lowerBlock f.content[i]!.block solved[i]!
  { width := f.size.1, height := f.size.2, background := f.theme.backgroundColor, ops := sortOps ops }

/-- SVG document of the figure (text as glyph outlines). -/
def toSVG (f : Figure) : String := f.toScene.renderSVG

/-- PNG bytes of the figure. -/
def toPNG (f : Figure) : ByteArray := f.toScene.renderPNG

/-- Write the figure to `path` (`.svg` or `.png`). -/
def save (f : Figure) (path : System.FilePath) : IO Unit := f.toScene.save path

end Figure

end LeanPlot
