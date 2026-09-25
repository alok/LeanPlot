import LeanPlot.Core.Num

/-!
# Grid layout (a port of GridLayoutBase.jl)

Makie places blocks (axes, colorbars, legends, labels) in a `GridLayout`. Each block
reports

* its **protrusions**: decorations that stick out of its "important" rectangle (tick
  labels, axis labels, titles), which live in the gaps between rows and columns, and
* optionally a **determined size** (`width`/`height` set, or an automatic size such as a
  colorbar's bar width or a legend's content size, when the block "tells" its size).

`Grid.solve` reproduces `GridLayoutBase.compute_rowcols` + `compute_col_row_sizes`:

1. per row/column, the maximum protrusion of the content touching each side (`maxgrid`);
2. gaps = protrusions of the neighbouring rows/columns + the added `rowgap`/`colgap`;
3. sizes: `Fixed`, then `Relative`, then determinable `Auto` sizes, then `Aspect` sizes,
   then the remaining space split among undetermined `Auto` sizes by their ratios;
4. row/column boundaries from the content box (`Outside` alignment subtracts the padding and
   the outer protrusions; `Inside` does not).

`Block.place` is GridLayoutBase's `update_computedbbox!`: a block's final rectangle inside
its cell given its size attributes and alignment.

Coordinates follow Makie: figure pixels with the origin at the **bottom-left**, y up.
GridLayoutBase computes in `Float32`; we compute in `Float` and round to `Float32` where
Makie stores `Float32` values, so the solved rectangles match Makie's to the last bit in
the common cases (and viewports, which Makie rounds to integers, match exactly).
-/

namespace LeanPlot.Layout

open LeanPlot.Num

/-- Round to `Float32` precision (Makie stores layout values as `Float32`). -/
@[inline] def f32 (x : Float) : Float := x.toFloat32.toFloat

/-- A rectangle by its sides (Makie `BBox(left, right, bottom, top)`), y up. -/
structure BBox where
  left : Float
  right : Float
  bottom : Float
  top : Float
  deriving Repr, Inhabited, BEq

namespace BBox
/-- Width. -/
@[inline] def width (b : BBox) : Float := b.right - b.left
/-- Height. -/
@[inline] def height (b : BBox) : Float := b.top - b.bottom
/-- From origin and size. -/
def ofRect (x y w h : Float) : BBox := ⟨x, x + w, y, y + h⟩
/-- Round every side to `Float32`. -/
def toF32 (b : BBox) : BBox := ⟨f32 b.left, f32 b.right, f32 b.bottom, f32 b.top⟩
/-- Makie `round_to_IRect2D`: round the corners to integers (ties to even), keeping
non-finite values out (`0`). -/
def roundInt (b : BBox) : BBox :=
  let r (x : Float) : Float := if x.isFinite then roundEven x else 0
  ⟨r b.left, r b.right, r b.bottom, r b.top⟩
/-- Shrink by per-side insets. -/
def shrink (b : BBox) (l r bo t : Float) : BBox := ⟨b.left + l, b.right - r, b.bottom + bo, b.top - t⟩
end BBox

/-- Per-side lengths (GridLayoutBase `RectSides`): protrusions, paddings. -/
structure Sides where
  left : Float := 0
  right : Float := 0
  bottom : Float := 0
  top : Float := 0
  deriving Repr, Inhabited, BEq

namespace Sides
/-- The same length on every side. -/
def uniform (x : Float) : Sides := ⟨x, x, x, x⟩
/-- Round to `Float32`. -/
def toF32 (s : Sides) : Sides := ⟨f32 s.left, f32 s.right, f32 s.bottom, f32 s.top⟩
end Sides

/-- How a grid aligns to its bounding box: `inside` puts the important lines (e.g. axis
spines) on the box and lets protrusions stick out; `outside` keeps the protrusions and a
padding inside the box (Makie's `Figure` uses `Outside(figure_padding)`). -/
inductive AlignMode where
  | inside
  | outside (pad : Sides)
  deriving Repr, Inhabited, BEq

/-- Row height / column width specification. -/
inductive GridSize where
  /-- Share the remaining space by `ratio`; with `tryDetermine`, use the size of single-span
  content that reports one. -/
  | auto (ratio : Float := 1) (tryDetermine : Bool := true)
  /-- Fixed pixels. -/
  | fixed (px : Float)
  /-- A fraction of the space available for rows/columns. -/
  | relative (frac : Float)
  /-- `ratio` times the size of row (for a column) / column (for a row) `index` (0-based). -/
  | aspect (index : Nat) (ratio : Float)
  deriving Repr, Inhabited, BEq

/-- Grid position of an item: rows `r0..r1`, columns `c0..c1` (0-based, inclusive). -/
structure Span where
  r0 : Nat
  r1 : Nat
  c0 : Nat
  c1 : Nat
  deriving Repr, Inhabited, BEq

namespace Span
/-- A single cell. -/
def cell (r c : Nat) : Span := ⟨r, r, c, c⟩
end Span

/-- What an item reports to its grid: its span, its protrusions and its determined inner
size, if any (`none` = fill the cell). -/
structure Item where
  span : Span
  protrusions : Sides := {}
  width : Option Float := none
  height : Option Float := none
  deriving Repr, Inhabited

/-- A grid layout specification. Gap arrays have `n - 1` entries (missing entries use the
default gap). -/
structure Grid where
  nrows : Nat
  ncols : Nat
  rowSizes : Array GridSize := #[]
  colSizes : Array GridSize := #[]
  rowGaps : Array Float := #[]
  colGaps : Array Float := #[]
  defaultRowGap : Float := 18
  defaultColGap : Float := 18
  align : AlignMode := .inside
  /-- Horizontal alignment of the grid within its box when it does not fill it (0 left). -/
  halign : Float := 0.5
  /-- Vertical alignment (0 bottom, 1 top). -/
  valign : Float := 0.5
  deriving Repr, Inhabited

/-- The solved grid: column and row boundaries and the per-row/column protrusions. -/
structure Solved where
  lefts : Array Float
  rights : Array Float
  tops : Array Float
  bottoms : Array Float
  /-- Maximum protrusions per column (left/right) and row (top/bottom). -/
  maxLefts : Array Float
  maxRights : Array Float
  maxTops : Array Float
  maxBottoms : Array Float
  deriving Repr, Inhabited

namespace Grid

/-- Size of row `i` (default `auto`). -/
def rowSize (g : Grid) (i : Nat) : GridSize := g.rowSizes.getD i (.auto)
/-- Size of column `i` (default `auto`). -/
def colSize (g : Grid) (i : Nat) : GridSize := g.colSizes.getD i (.auto)
/-- Added gap after row `i`. -/
def rowGap (g : Grid) (i : Nat) : Float := g.rowGaps.getD i g.defaultRowGap
/-- Added gap after column `i`. -/
def colGap (g : Grid) (i : Nat) : Float := g.colGaps.getD i g.defaultColGap

/-- Sum of a float array. -/
def sumF (a : Array Float) : Float := a.foldl (· + ·) 0

/-- Cumulative sums starting at zero (`zcumsum`): `[0, a₀, a₀ + a₁, …]` of length `n`. -/
def zcumsum (a : Array Float) (n : Nat) : Array Float :=
  (List.range n).foldl (init := (#[], 0.0)) (fun (acc : Array Float × Float) i =>
    (acc.1.push acc.2, acc.2 + a.getD i 0)) |>.1

/-- Maximum protrusions per row/column (`_compute_maxgrid`). -/
def maxGrid (g : Grid) (items : Array Item) : Array Float × Array Float × Array Float × Array Float :=
  items.foldl (init := (Array.replicate g.ncols 0, Array.replicate g.ncols 0,
      Array.replicate g.nrows 0, Array.replicate g.nrows 0)) fun (ls, rs, ts, bs) it =>
    let s := it.span
    let p := it.protrusions
    (ls.modify s.c0 (max · p.left), rs.modify s.c1 (max · p.right),
     ts.modify s.r0 (max · p.top), bs.modify s.r1 (max · p.bottom))

/-- Determined size of column (`col = true`) or row `i` from single-spanned content
(`determinedirsize(idir, gl, dir)` for an `Auto` size). -/
def determinedSize (items : Array Item) (col : Bool) (i : Nat) : Option Float :=
  items.foldl (init := none) fun acc it =>
    let single := if col then it.span.c0 == i && it.span.c1 == i else it.span.r0 == i && it.span.r1 == i
    if !single then acc else
    match (if col then it.width else it.height) with
    | some s => some (match acc with | some a => max a s | none => s)
    | none => acc

/-- `compute_col_row_sizes`. -/
def colRowSizes (g : Grid) (items : Array Item) (spaceCols spaceRows : Float) : Array Float × Array Float := Id.run do
  let nc := g.ncols
  let nr := g.nrows
  let mut cw : Array Float := Array.replicate nc 0
  let mut rh : Array Float := Array.replicate nr 0
  let mut dc : Array Bool := Array.replicate nc false
  let mut dr : Array Bool := Array.replicate nr false
  -- fixed
  for i in [0:nc] do
    if let .fixed x := g.colSize i then cw := cw.set! i x; dc := dc.set! i true
  for i in [0:nr] do
    if let .fixed x := g.rowSize i then rh := rh.set! i x; dr := dr.set! i true
  -- relative
  for i in [0:nc] do
    if let .relative x := g.colSize i then cw := cw.set! i (x * spaceCols); dc := dc.set! i true
  for i in [0:nr] do
    if let .relative x := g.rowSize i then rh := rh.set! i (x * spaceRows); dr := dr.set! i true
  -- determinable autos
  for i in [0:nc] do
    if let .auto _ true := g.colSize i then
      if let some s := determinedSize items true i then cw := cw.set! i s; dc := dc.set! i true
  for i in [0:nr] do
    if let .auto _ true := g.rowSize i then
      if let some s := determinedSize items false i then rh := rh.set! i s; dr := dr.set! i true
  -- aspects referring to determined counterparts
  for i in [0:nc] do
    if let .aspect j r := g.colSize i then
      if dr.getD j false then cw := cw.set! i (r * rh.getD j 0); dc := dc.set! i true
  for i in [0:nr] do
    if let .aspect j r := g.rowSize i then
      if dc.getD j false then rh := rh.set! i (r * cw.getD j 0); dr := dr.set! i true
  let colAspectsLeft := (List.range nc).any fun i => (match g.colSize i with | .aspect .. => true | _ => false) && !dc[i]!
  let rowAspectsLeft := (List.range nr).any fun i => (match g.rowSize i with | .aspect .. => true | _ => false) && !dr[i]!
  -- remaining autos on sides without pending aspects
  let distribute (sizes : Array GridSize) (w : Array Float) (d : Array Bool) (space : Float) :
      Array Float × Array Bool := Id.run do
    let remaining := space - sumF w
    let mut idx : Array Nat := #[]
    let mut ratios : Array Float := #[]
    for i in [0:w.size] do
      if let .auto r _ := sizes.getD i (.auto) then
        if !d[i]! then idx := idx.push i; ratios := ratios.push r
    let s := sumF ratios
    let mut w := w
    let mut d := d
    for k in [0:idx.size] do
      w := w.set! idx[k]! (ratios[k]! / s * remaining)
      d := d.set! idx[k]! true
    return (w, d)
  let colSizesArr := (Array.range nc).map g.colSize
  let rowSizesArr := (Array.range nr).map g.rowSize
  if !colAspectsLeft then
    let (w, d) := distribute colSizesArr cw dc spaceCols
    cw := w; dc := d
  if !rowAspectsLeft then
    let (w, d) := distribute rowSizesArr rh dr spaceRows
    rh := w; dr := d
  -- aspects again
  for i in [0:nc] do
    if let .aspect j r := g.colSize i then
      if dr.getD j false then cw := cw.set! i (r * rh.getD j 0); dc := dc.set! i true
  for i in [0:nr] do
    if let .aspect j r := g.rowSize i then
      if dc.getD j false then rh := rh.set! i (r * cw.getD j 0); dr := dr.set! i true
  -- last pass over undetermined autos
  let (w, _) := distribute colSizesArr cw dc spaceCols
  cw := w
  let (w, _) := distribute rowSizesArr rh dr spaceRows
  rh := w
  return (cw.map f32, rh.map f32)

/-- `compute_rowcols`: solve the grid in the box `bbox`. -/
def solve (g : Grid) (items : Array Item) (bbox : BBox) : Solved :=
  let content := match g.align with
    | .inside => bbox
    | .outside p => bbox.shrink p.left p.right p.bottom p.top
  let (mls, mrs, mts, mbs) := g.maxGrid items
  let nc := g.ncols
  let nr := g.nrows
  let topProt := mts.getD 0 0
  let bottomProt := mbs.getD (nr - 1) 0
  let leftProt := mls.getD 0 0
  let rightProt := mrs.getD (nc - 1) 0
  -- protrusion gaps between neighbouring columns / rows
  let colGapsP := (Array.range (nc - 1)).map fun i => mls[i + 1]! + mrs[i]!
  let rowGapsP := (Array.range (nr - 1)).map fun i => mts[i + 1]! + mbs[i]!
  let sumColGaps := if nc ≤ 1 then 0 else sumF colGapsP
  let sumRowGaps := if nr ≤ 1 then 0 else sumF rowGapsP
  let remH := f32 (match g.align with
    | .inside => content.width - sumColGaps
    | .outside _ => content.width - sumColGaps - leftProt - rightProt)
  let remV := f32 (match g.align with
    | .inside => content.height - sumRowGaps
    | .outside _ => content.height - sumRowGaps - topProt - bottomProt)
  let addedCol := (Array.range (nc - 1)).map g.colGap
  let addedRow := (Array.range (nr - 1)).map g.rowGap
  let spaceCols := remH - (if nc ≤ 1 then 0 else sumF addedCol)
  let spaceRows := remV - (if nr ≤ 1 then 0 else sumF addedRow)
  let (cw, rh) := g.colRowSizes items spaceCols spaceRows
  let cw := cw.map (max · 1)
  let rh := rh.map (max · 1)
  let finalColGaps := (Array.range (nc - 1)).map fun i => colGapsP[i]! + addedCol[i]!
  let finalRowGaps := (Array.range (nr - 1)).map fun i => rowGapsP[i]! + addedRow[i]!
  let (outW, outH) := match g.align with
    | .inside => (0, 0)
    | .outside _ => (leftProt + rightProt, topProt + bottomProt)
  let gridW := sumF cw + sumF finalColGaps + outW
  let gridH := sumF rh + sumF finalRowGaps + outH
  let xadj := g.halign * (content.width - gridW)
  let yadj := (1 - g.valign) * (content.height - gridH)
  let (lp, tp) := match g.align with
    | .inside => (0, 0)
    | .outside _ => (leftProt, topProt)
  let cwc := zcumsum cw nc
  let cgc := zcumsum finalColGaps nc
  let rhc := zcumsum rh nr
  let rgc := zcumsum finalRowGaps nr
  let lefts := (Array.range nc).map fun i => f32 (xadj + content.left + cwc[i]! + cgc[i]! + lp)
  let rights := (Array.range nc).map fun i => f32 (lefts[i]! + cw[i]!)
  let tops := (Array.range nr).map fun i => f32 (content.top - yadj - rhc[i]! - rgc[i]! - tp)
  let bottoms := (Array.range nr).map fun i => f32 (tops[i]! - rh[i]!)
  { lefts, rights, tops, bottoms, maxLefts := mls, maxRights := mrs, maxTops := mts, maxBottoms := mbs }

/-- `determinedirsize(gl, dir)`: the total size of the grid along columns (`col`) or rows,
if every row/column size can be determined. -/
def determinedTotal (g : Grid) (items : Array Item) (col : Bool) : Option Float := do
  let n := if col then g.ncols else g.nrows
  let sizes ← (List.range n).mapM fun i =>
    match (if col then g.colSize i else g.rowSize i) with
    | .fixed x => some x
    | .auto _ true => determinedSize items col i
    | _ => none
  let total := sizes.foldl (· + ·) 0
  let (mls, mrs, mts, mbs) := g.maxGrid items
  -- gaps from protrusions of neighbouring rows/columns (`dirgaps`)
  let starts := if col then mls else mts
  let stops := if col then mrs else mbs
  let inner := if n > 1 then sumF ((Array.range (n - 1)).map fun i => starts[i + 1]! + stops[i]!) else 0
  let added := if n ≤ 1 then 0 else sumF ((Array.range (n - 1)).map fun i => if col then g.colGap i else g.rowGap i)
  let s := total + inner + added
  match g.align with
  | .inside => some s
  | .outside p =>
    let pads := if col then p.left + p.right else p.top + p.bottom
    some (s + starts.getD 0 0 + stops.getD (n - 1) 0 + pads)

end Grid

namespace Solved

/-- The cell box of a span (`Inner` side). -/
def cell (s : Solved) (sp : Span) : BBox :=
  ⟨s.lefts.getD sp.c0 0, s.rights.getD sp.c1 0, s.bottoms.getD sp.r1 0, s.tops.getD sp.r0 0⟩

end Solved

/-- A block's size attribute (Makie `width`/`height`). -/
inductive SizeAttr where
  /-- `nothing`: fill the suggested box. -/
  | fill
  /-- A fixed size in pixels. -/
  | fixed (px : Float)
  /-- A fraction of the suggested box. -/
  | relative (frac : Float)
  /-- `Auto()`: the block's automatic size if it has one, else the suggested box. -/
  | auto
  deriving Repr, Inhabited, BEq

/-- `computed_size`: the size a block reports to its grid. -/
def reportedSize (attr : SizeAttr) (autoSize : Option Float) (tell : Bool) : Option Float :=
  if !tell then none else
  match attr with
  | .fill => none
  | .fixed x => some (f32 x)
  | .relative _ => none
  | .auto => autoSize

/-- `update_computedbbox!` for an `Inside`-aligned block: its rectangle inside the suggested
box `sb`, given size attributes, automatic sizes, reported sizes and alignment
(`halign`: 0 left … 1 right, `valign`: 0 bottom … 1 top). -/
def place (sb : BBox) (wattr hattr : SizeAttr) (autoW autoH : Option Float)
    (reportedW reportedH : Option Float) (halign valign : Float) : BBox :=
  let bw := sb.width
  let bh := sb.height
  let target (rep : Option Float) (attr : SizeAttr) (auto : Option Float) (b : Float) : Float :=
    match rep with
    | some c => c
    | none => match attr with
      | .relative x => x * b
      | .fill => b
      | .auto => auto.getD b
      | .fixed x => x
  let w := f32 (target reportedW wattr autoW bw)
  let h := f32 (target reportedH hattr autoH bh)
  let xs := halign * (bw - w)
  let ys := valign * (bh - h)
  let l := f32 (sb.left + xs)
  let b := f32 (sb.bottom + ys)
  ⟨l, f32 (l + w), b, f32 (b + h)⟩

end LeanPlot.Layout
