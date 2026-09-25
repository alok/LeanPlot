import LeanPlot.Scene.Style
import LeanPlot.Core.Data

/-!
# Marks: plot primitives in data space

A `Mark` is what a recipe produces and what an axis draws: geometry in *data*
coordinates plus a style. Axes turn marks into device-space `DrawOp`s after the layout is
solved (limits, scales and the pixel rectangle known).

Positions are `Pos`: 2D (`Pts2`, for `Axis2`) or 3D (`Pts3`, for `Axis3`). A 2D mark in an
`Axis3` lies in the plane `z = 0`; a 3D mark in an `Axis2` is projected onto `xy`. NaN
coordinates break lines (Makie convention).

| mark | Makie counterpart |
|---|---|
| `lines` | `lines` (NaN-separated polylines; per-vertex colours allowed) |
| `segments` | `linesegments` (point pairs; per-vertex or per-segment colours) |
| `scatter` | `scatter` |
| `band` | `band` (between two polylines) |
| `poly` | `poly` (one or more polygons) |
| `text` | `text` |
| `heatmap` | `heatmap` (cell edges, colormap, colour range) |
| `image` | `image` (an RGBA8 raster spanning a data rectangle) |
| `mesh` | `mesh` (triangles; per-vertex colours or values; optional shading in 3D) |
| `arrows` | `arrows2d` (origins + directions; pixel-space shafts and tips) |
| `labeledLines` | `contour(...; labels = true)` (lines masked under their level labels) |

`Mark.bounds?` is Makie's `data_limits` for autolimits and `Mark.tight` its
`needs_tight_limits` (heatmaps and images remove the autolimit margins).
-/

namespace LeanPlot

open LeanPlot.Num

/-! ## Bounding boxes -/

namespace Rect3

/-- Smallest box containing both. -/
def union (a b : Rect3) : Rect3 :=
  let alo := a.lo
  let ahi := a.hi
  let blo := b.lo
  let bhi := b.hi
  ofBounds ⟨min alo.x blo.x, min alo.y blo.y, min alo.z blo.z⟩ ⟨max ahi.x bhi.x, max ahi.y bhi.y, max ahi.z bhi.z⟩

/-- Union of optional boxes. -/
def unionOpt : Option Rect3 → Option Rect3 → Option Rect3
  | some a, some b => some (union a b)
  | some a, none => some a
  | none, b => b

/-- Box with explicit x/y/z ranges. -/
def ofRanges (x0 x1 y0 y1 z0 z1 : Float) : Rect3 := ofBounds ⟨x0, y0, z0⟩ ⟨x1, y1, z1⟩

end Rect3

/-! ## Marks -/

/-- The empty `0 × 0` grid. -/
instance : Inhabited AnyGrid2 := ⟨⟨0, 0, default⟩⟩

/-- The mesh with no vertices and no triangles. -/
instance : Inhabited TriMesh := ⟨⟨Pts3.empty, #[], rfl, fun _ hk => absurd hk (Nat.not_lt_zero _)⟩⟩

/-- Heatmap cell geometry and colours. `xs` (`nx + 1` entries) and `ys` (`ny + 1`) are the
cell *edges*; cell `(i, j)` spans `[xs[i], xs[i+1]] × [ys[j], ys[j+1]]` and has value
`z[i, j]` (see `Grid2`). -/
structure HeatmapData where
  xs : FloatArray
  ys : FloatArray
  z : AnyGrid2
  mapping : ColorMapping := {}
  deriving Inhabited

/-- A triangle mesh with per-vertex colour and flat/smooth shading option. -/
structure MeshData where
  mesh : TriMesh
  color : ColorSpec := .black
  /-- Lambert shading with Makie's default light (only used in `Axis3`). -/
  shading : Bool := false
  deriving Inhabited

/-- A plot primitive in data space. -/
inductive Mark where
  /-- Polyline(s) separated by NaN points. -/
  | lines (p : Pos) (s : LineSpec)
  /-- Independent segments `p[2k] → p[2k+1]`. -/
  | segments (p : Pos) (s : LineSpec)
  /-- Markers. -/
  | scatter (p : Pos) (s : MarkerSpec)
  /-- The region between `lower[k]` and `upper[k]` (quads between consecutive points). -/
  | band (lower upper : Pos) (fill : ColorSpec)
  /-- Filled polygons (one ring each, implicitly closed). -/
  | poly (rings : Array Pts2) (s : PolySpec)
  /-- Strings anchored at points. -/
  | text (p : Pos) (strings : Array String) (s : TextSpec)
  /-- Colour-mapped grid cells. -/
  | heatmap (h : HeatmapData)
  /-- A `w × h` RGBA8 raster (row-major, top row first) spanning `[x0, x1] × [y0, y1]`. -/
  | image (x0 x1 y0 y1 : Float) (w h : Nat) (rgba : ByteArray) (interp : Interp)
  /-- Triangles. -/
  | mesh (m : MeshData)
  /-- Arrows from `origins` along `dirs` (`arrows2d`). -/
  | arrows (origins dirs : Pos) (s : ArrowSpec)
  /-- Horizontal lines at `ys` spanning the whole x range of a 2D axis (`hlines`). -/
  | hlines (ys : FloatArray) (s : LineSpec)
  /-- Vertical lines at `xs` spanning the whole y range of a 2D axis (`vlines`). -/
  | vlines (xs : FloatArray) (s : LineSpec)
  /-- NaN-separated contour lines with level labels: the labels are drawn at their anchors and
  the line points under a label's box are dropped (Makie `contour(...; labels = true)`). -/
  | labeledLines (p : Pos) (s : LineSpec) (labels : ContourLabels)
  deriving Inhabited

namespace Mark

/-- Makie's plot-type name (`"lines"`, `"scatter"`, …). -/
def kind : Mark → String
  | lines .. => "lines" | segments .. => "linesegments" | scatter .. => "scatter"
  | band .. => "band" | poly .. => "poly" | text .. => "text" | heatmap .. => "heatmap"
  | image .. => "image" | mesh .. => "mesh" | arrows .. => "arrows2d"
  | hlines .. => "hlines" | vlines .. => "vlines" | labeledLines .. => "contour"

/-- Bounds of the polygons. -/
private def ringsBounds (rs : Array Pts2) : Option Rect3 :=
  rs.foldl (init := none) fun acc r => Rect3.unionOpt acc ((Pos.xy r).bounds?)

/-- Finite x/y extrema of a set of edges. -/
private def edgeRange (e : FloatArray) : Option (Float × Float) := extremaFinite e

/-- Makie `data_limits`: the finite bounding box of the mark in data space (`none` when it
has no finite point). -/
def bounds? : Mark → Option Rect3
  | lines p _ | segments p _ | scatter p _ | text p _ _ | labeledLines p _ _ => p.bounds?
  | band lo hi _ => Rect3.unionOpt lo.bounds? hi.bounds?
  | poly rs _ => ringsBounds rs
  | heatmap h =>
    match edgeRange h.xs, edgeRange h.ys with
    | some (x0, x1), some (y0, y1) => some (Rect3.ofRanges x0 x1 y0 y1 0 0)
    | _, _ => none
  | image x0 x1 y0 y1 .. => some (Rect3.ofRanges (min x0 x1) (max x0 x1) (min y0 y1) (max y0 y1) 0 0)
  | mesh m => (Pos.xyz m.mesh.pos).bounds?
  | hlines .. | vlines .. => none
  | arrows o d s =>
    -- union of start and end points (`_process_arrow_arguments` in direction mode)
    let n := min o.size d.size
    let rec go (i : Nat) (acc : Option Rect3) : Option Rect3 :=
      if i < n then
        let p := o.get3 i
        let v := d.get3 i
        let v := if s.normalize then Vec3.normalize v else v
        let v := Vec3.smul s.lengthscale v
        let st := p.sub (Vec3.smul s.align v)
        let en := st.add v
        let acc := if st.isFinite then Rect3.unionOpt acc (some ⟨st, ⟨0, 0, 0⟩⟩) else acc
        let acc := if en.isFinite then Rect3.unionOpt acc (some ⟨en, ⟨0, 0, 0⟩⟩) else acc
        go (i + 1) acc
      else acc
    termination_by n - i
    go 0 none

/-- Per-dimension data limits `(x, y, z)`: like `bounds?`, but `hlines`/`vlines` only
constrain one dimension. -/
def dimBounds (m : Mark) : Option (Float × Float) × Option (Float × Float) × Option (Float × Float) :=
  match m with
  | hlines ys _ => (none, extremaFinite ys, none)
  | vlines xs _ => (extremaFinite xs, none, none)
  | m => match m.bounds? with
    | some r => (some (r.lo.x, r.hi.x), some (r.lo.y, r.hi.y), some (r.lo.z, r.hi.z))
    | none => (none, none, none)

/-- Union of optional ranges. -/
def unionRange : Option (Float × Float) → Option (Float × Float) → Option (Float × Float)
  | some (a, b), some (c, d) => some (min a c, max b d)
  | some r, none => some r
  | none, r => r

/-- Makie `needs_tight_limits`: heatmaps and images set the axis autolimit margins to 0. -/
def tight : Mark → Bool
  | heatmap .. | image .. => true
  | _ => false

/-- The colour mapping and range of a colour-mapped mark (for `Colorbar`s). -/
def colorMapping? : Mark → Option (ColorMapping × Float × Float)
  | heatmap h => some (h.mapping, h.mapping.rangeFor h.z.grid.z)
  | lines _ s | segments _ s | hlines _ s | vlines _ s | labeledLines _ s _ => withRange s.color
  | scatter _ s => withRange s.color
  | band _ _ c => withRange c
  | poly _ s => withRange s.color
  | mesh m => withRange m.color
  | arrows _ _ s => withRange s.color
  | _ => none
where
  withRange (c : ColorSpec) : Option (ColorMapping × Float × Float) :=
    match c with
    | .values vs m => let (lo, hi) := m.rangeFor vs; some (m, lo, hi)
    | _ => none

/-- `true` when every position is 2D (the mark lives in a plane). -/
def is2D : Mark → Bool
  | lines p _ | segments p _ | scatter p _ | text p _ _ | labeledLines p _ _ => !p.is3D
  | band lo hi _ => !lo.is3D && !hi.is3D
  | arrows o d _ => !o.is3D && !d.is3D
  | mesh _ => false
  | _ => true

end Mark

/-- A mark as placed in an axis: the mark, its legend label and whether its colour came
from the palette cycle (Makie only counts plots with cycled colours). -/
structure PlotItem where
  mark : Mark
  label : Option String := none
  /-- Include this item's data in the automatic limits (Makie `xautolimits`/`yautolimits`). -/
  autolimits : Bool := true
  deriving Inhabited

end LeanPlot
