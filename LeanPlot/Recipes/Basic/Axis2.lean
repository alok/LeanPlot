import LeanPlot.Recipes.Basic.Common

/-!
# Basic 2D recipes (`Axis2` builders)

Functional counterparts of Makie's plotting functions: each takes an axis and returns the
axis with one more plot item. Omitted colours come from the palette cycle, per plot type,
exactly as Makie cycles them (`lines`, `linesegments`, `scatter` use the Wong colours;
`band`, `poly`, `mesh` the `patchcolor` palette); `linewidth`/`markersize` default to the
theme (1.5 / 9).

Inputs are plain `Core.Data` (`FloatArray`s, `Pts2`, `Grid2`, `TriMesh`), so the output of
other recipe modules (contour lines as NaN-separated `Pts2`, streamlines with per-vertex
speeds, arrow origins/directions) plugs in directly:

| builder | Makie |
|---|---|
| `lines`, `linesPts`, `linesColored` | `lines!` |
| `linesegments` | `linesegments!` |
| `scatter`, `scatterPts` | `scatter!` |
| `band` | `band!` |
| `poly`, `polys` | `poly!` |
| `text`, `texts` | `text!` |
| `heatmap` | `heatmap!` |
| `image` | `image!` |
| `mesh` | `mesh!` |
| `arrows2d` | `arrows2d!` |
| `hlines`, `vlines` | `hlines!`, `vlines!` |
| `limits`, `xlims`, `ylims`, `axislegend`, `hidedecorations`, `hidespines` | same names |
-/

namespace LeanPlot

open LeanPlot.Num

namespace Axis2

/-- A fresh axis with the most common attributes. -/
def new (title : String := "") (xlabel : String := "") (ylabel : String := "")
    (xscale : Scale := .identity) (yscale : Scale := .identity) (aspect : AxisAspect := .auto)
    (theme : Theme := {}) : Axis2 :=
  { title, xlabel, ylabel, xscale, yscale, aspect, theme }

/-- Append a plot item. -/
def add (ax : Axis2) (m : Mark) (label : Option String := none) (autolimits : Bool := true) : Axis2 :=
  { ax with items := ax.items.push { mark := m, label, autolimits } }

/-- Next palette colour for `lines` (and advance the counter when the colour is cycled). -/
def cycleLines (ax : Axis2) (c : Option ColorSpec) : ColorSpec × Axis2 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.color ax.cycle.lines), { ax with cycle := { ax.cycle with lines := ax.cycle.lines + 1 } })

/-- Next palette colour for `linesegments`. -/
def cycleSegments (ax : Axis2) (c : Option ColorSpec) : ColorSpec × Axis2 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.color ax.cycle.segments), { ax with cycle := { ax.cycle with segments := ax.cycle.segments + 1 } })

/-- Next palette colour for `scatter`. -/
def cycleScatter (ax : Axis2) (c : Option ColorSpec) : ColorSpec × Axis2 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.color ax.cycle.scatter), { ax with cycle := { ax.cycle with scatter := ax.cycle.scatter + 1 } })

/-- Next patch colour for `band`. -/
def cycleBand (ax : Axis2) (c : Option ColorSpec) : ColorSpec × Axis2 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.patchColor ax.cycle.band), { ax with cycle := { ax.cycle with band := ax.cycle.band + 1 } })

/-- Next patch colour for `poly`. -/
def cyclePoly (ax : Axis2) (c : Option ColorSpec) : ColorSpec × Axis2 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.patchColor ax.cycle.poly), { ax with cycle := { ax.cycle with poly := ax.cycle.poly + 1 } })

/-- Next patch colour for `mesh`. -/
def cycleMesh (ax : Axis2) (c : Option ColorSpec) : ColorSpec × Axis2 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.patchColor ax.cycle.mesh), { ax with cycle := { ax.cycle with mesh := ax.cycle.mesh + 1 } })

/-- `lines!(ax, p)`: a polyline (NaN points break it). -/
def linesPts (ax : Axis2) (p : Pts2) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cycleLines color
  ax.add (.lines (.xy p) { color := c, width := linewidth, style := linestyle }) label

/-- `lines!(ax, xs, ys)`. -/
def lines (ax : Axis2) (xs ys : FloatArray) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) (label : Option String := none) : Axis2 :=
  ax.linesPts (Pts2.ofArrays xs ys) color linewidth linestyle label

/-- `lines!(ax, xs, f)`: the graph of `f` at `xs`. -/
@[specialize] def linesFn (ax : Axis2) (f : Float → Float) (xs : FloatArray) (color : Option ColorSpec := none)
    (linewidth : Float := 1.5) (linestyle : LineStyle := .solid) (label : Option String := none) : Axis2 :=
  ax.linesPts (Pts2.ofFunction f xs) color linewidth linestyle label

/-- `lines!(ax, p, color = values, colormap = ...)`: a polyline coloured per vertex by values
(e.g. streamlines coloured by speed, Cartan's default colouring of curves). -/
def linesColored (ax : Axis2) (p : Pts2) (values : FloatArray) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (linewidth : Float := 1.5)
    (label : Option String := none) : Axis2 :=
  ax.add (.lines (.xy p) { color := .values values { colormap, colorrange }, width := linewidth }) label

/-- `linesegments!(ax, p)`: segments `p[2k] → p[2k+1]`; `color` may hold one colour per
point or per segment. -/
def linesegments (ax : Axis2) (p : Pts2) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cycleSegments color
  ax.add (.segments (.xy p) { color := c, width := linewidth, style := linestyle }) label

/-- `scatter!(ax, p)`. -/
def scatterPts (ax : Axis2) (p : Pts2) (color : Option ColorSpec := none) (marker : MarkerShape := .circle)
    (markersize : Float := 9) (strokecolor : RGBA := RGBA.black) (strokewidth : Float := 0)
    (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cycleScatter color
  ax.add (.scatter (.xy p) { shape := marker, size := markersize, color := c
                             strokeColor := strokecolor, strokeWidth := strokewidth }) label

/-- `scatter!(ax, xs, ys)`. -/
def scatter (ax : Axis2) (xs ys : FloatArray) (color : Option ColorSpec := none) (marker : MarkerShape := .circle)
    (markersize : Float := 9) (strokecolor : RGBA := RGBA.black) (strokewidth : Float := 0)
    (label : Option String := none) : Axis2 :=
  ax.scatterPts (Pts2.ofArrays xs ys) color marker markersize strokecolor strokewidth label

/-- `band!(ax, xs, ylow, yhigh)`. -/
def band (ax : Axis2) (xs ylow yhigh : FloatArray) (color : Option ColorSpec := none) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cycleBand color
  ax.add (.band (.xy (Pts2.ofArrays xs ylow)) (.xy (Pts2.ofArrays xs yhigh)) c) label

/-- `band!(ax, lower, upper)` between two polylines. -/
def bandPts (ax : Axis2) (lower upper : Pts2) (color : Option ColorSpec := none) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cycleBand color
  ax.add (.band (.xy lower) (.xy upper) c) label

/-- `poly!(ax, points)`: one filled polygon. -/
def poly (ax : Axis2) (ring : Pts2) (color : Option ColorSpec := none) (strokecolor : RGBA := RGBA.black)
    (strokewidth : Float := 0) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cyclePoly color
  ax.add (.poly #[ring] { color := c, strokeColor := strokecolor, strokeWidth := strokewidth }) label

/-- `poly!(ax, polygons)`: several polygons, with one colour or one colour per polygon
(e.g. `contourf` bands coloured by level). -/
def polys (ax : Axis2) (rings : Array Pts2) (color : Option ColorSpec := none) (strokecolor : RGBA := RGBA.black)
    (strokewidth : Float := 0) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cyclePoly color
  ax.add (.poly rings { color := c, strokeColor := strokecolor, strokeWidth := strokewidth }) label

/-- `text!(ax, x, y, text = s)` (Makie's default alignment `(:left, :bottom)`). -/
def text (ax : Axis2) (x y : Float) (s : String) (fontsize : Float := 14) (color : RGBA := RGBA.black)
    (halign : HAlign := .left) (valign : VAlign := .bottom) (rotation : Float := 0)
    (offset : Float × Float := (0, 0)) : Axis2 :=
  ax.add (.text (.xy (Pts2.ofArrays ⟨#[x]⟩ ⟨#[y]⟩)) #[s]
    { size := fontsize, color, halign, valign, rotation, offset })

/-- `text!(ax, points, text = strings)`. -/
def texts (ax : Axis2) (p : Pts2) (ss : Array String) (fontsize : Float := 14) (color : RGBA := RGBA.black)
    (halign : HAlign := .left) (valign : VAlign := .bottom) : Axis2 :=
  ax.add (.text (.xy p) ss { size := fontsize, color, halign, valign })

/-- `heatmap!(ax, xs, ys, z)`: `xs`/`ys` are cell centres (`nx`, `ny` entries) or edges
(`nx + 1`, `ny + 1`); the colour range defaults to the finite extrema of `z`. -/
def heatmap {nx ny : Nat} (ax : Axis2) (xs ys : FloatArray) (z : Grid2 nx ny) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (lowclip highclip : Option RGBA := none)
    (nanColor : RGBA := RGBA.transparent) (label : Option String := none) : Axis2 :=
  ax.add (.heatmap { xs := Recipes.cellEdges xs nx, ys := Recipes.cellEdges ys ny, z := ⟨nx, ny, z⟩
                     mapping := { colormap, colorrange, lowclip, highclip, nanColor } }) label

/-- `heatmap!(ax, z)`: cells centred on `1..nx × 1..ny`. -/
def heatmapGrid {nx ny : Nat} (ax : Axis2) (z : Grid2 nx ny) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) : Axis2 :=
  ax.heatmap (Recipes.oneTo nx) (Recipes.oneTo ny) z colormap colorrange

/-- `image!(ax, x0..x1, y0..y1, img)`: an RGBA8 raster given in `z[i, j]` order (`i ↔ x`,
4 bytes per pixel), stretched over the rectangle; bilinear like Makie's default. -/
def image (ax : Axis2) (x0 x1 y0 y1 : Float) (nx ny : Nat) (rgba : ByteArray) (interpolate : Bool := true) : Axis2 :=
  ax.add (.image x0 x1 y0 y1 nx ny (Recipes.rowsTopDown nx ny rgba) (if interpolate then .linear else .nearest))

/-- `mesh!(ax, m)` (2D: the z coordinates are ignored for drawing). -/
def mesh (ax : Axis2) (m : TriMesh) (color : Option ColorSpec := none) (label : Option String := none) : Axis2 :=
  let (c, ax) := ax.cycleMesh color
  ax.add (.mesh { mesh := m, color := c }) label

/-- `arrows2d!(ax, origins, directions)`. -/
def arrows2d (ax : Axis2) (origins dirs : Pts2) (color : ColorSpec := .black) (lengthscale : Float := 1)
    (normalize : Bool := false) (align : Float := 0) (shaftwidth : Float := 3) (tipwidth : Float := 14)
    (tiplength : Float := 8) (label : Option String := none) : Axis2 :=
  ax.add (.arrows (.xy origins) (.xy dirs) { color, lengthscale, normalize, align, shaftwidth, tipwidth, tiplength }) label

/-- `arrows2d!(ax, xs, ys, us, vs)`. -/
def arrows (ax : Axis2) (xs ys us vs : FloatArray) (color : ColorSpec := .black) (lengthscale : Float := 1)
    (normalize : Bool := false) (align : Float := 0) : Axis2 :=
  ax.arrows2d (Pts2.ofArrays xs ys) (Pts2.ofArrays us vs) color lengthscale normalize align

/-- `streamplot!` from precomputed data (e.g. the streamplot algorithm of the recipes
module): NaN-separated streamline points with one value per point (Makie colours by
`norm(f(p))`), and arrowheads at `arrowPos` pointing along `arrowDir` (`:utriangle`,
markersize 15, rotated in pixel space like Makie's `register_projected_rotations_2d!`).
Lines and arrowheads each get their own automatic colour range unless `colorrange` is set,
as in Makie. -/
def streamplot (ax : Axis2) (lines : Pts2) (lineValues : FloatArray) (arrowPos arrowDir : Pts2)
    (arrowValues : FloatArray) (colormap : Colormap := Colormap.viridis) (colorrange : Option (Float × Float) := none)
    (linewidth : Float := 1.5) (arrowSize : Float := 15) (label : Option String := none) : Axis2 :=
  let ax := ax.add (.lines (.xy lines) { color := .values lineValues { colormap, colorrange }, width := linewidth }) label
  ax.add (.scatter (.xy arrowPos) { shape := .utriangle, size := arrowSize
                                    color := .values arrowValues { colormap, colorrange }
                                    alongDirections := some (.xy arrowDir, -Num.pi / 2) })

/-- `contour!` from precomputed isolines: NaN-separated points with the level of each point,
coloured through `colormap` over the level range (Makie's contour `linewidth = 1`). -/
def contourLines (ax : Axis2) (lines : Pts2) (levels : FloatArray) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (linewidth : Float := 1) (label : Option String := none) : Axis2 :=
  ax.add (.lines (.xy lines) { color := .values levels { colormap, colorrange }, width := linewidth }) label

/-- `contour!(…; labels = true)`: NaN-separated isolines coloured by `lineColor` (one colour
or value per point), labelled with `labels` at `labelPos` (anchors) along `labelDir` (line
directions there), each label in its entry of `labelColors`; lines are masked under the labels
(Makie `contour` with `labels = true`, `labelsize = 10`). See `Contour.labelData`. -/
def contourLabeled (ax : Axis2) (lines : Pts2) (lineColor : ColorSpec) (labelPos labelDir : Pts2)
    (labels : Array String) (labelColors : ColorSpec) (linewidth : Float := 1) (labelsize : Float := 10)
    (bold : Bool := false) (label : Option String := none) : Axis2 :=
  ax.add (.labeledLines (.xy lines) { color := lineColor, width := linewidth }
    { pos := .xy labelPos, dir := .xy labelDir, strings := labels, colors := labelColors, size := labelsize, bold }) label

/-- `contour!` from isolines given one polyline per line with its level (e.g. traced by a
marching-squares recipe): concatenated with NaN breaks and coloured per line through
`colormap` over the level range. -/
def contourSet (ax : Axis2) (lines : Array Pts2) (levels : FloatArray) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (linewidth : Float := 1) (label : Option String := none) : Axis2 :=
  let (pts, vals) := (Array.range lines.size).foldl (init := (Pts2.empty, FloatArray.empty)) fun (acc, vs) k =>
    let l := lines[k]!
    let lv := levels.get! k
    let acc := if acc.size == 0 then acc else acc.push Num.nan Num.nan
    let vs := if vs.size == 0 then vs else vs.push lv
    (Pts2.ofArrays ⟨acc.xs.data ++ l.xs.data⟩ ⟨acc.ys.data ++ l.ys.data⟩,
     ⟨vs.data ++ Array.replicate l.size lv⟩)
  let range := colorrange.orElse fun _ => extremaFinite levels
  ax.contourLines pts vals colormap range linewidth label

/-- `contourf!` from precomputed bands: one polygon per band piece (holes as NaN-separated
rings, filled even-odd) with its band value, coloured through `colormap`. -/
def contourfBands (ax : Axis2) (polys : Array Pts2) (values : FloatArray) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (label : Option String := none) : Axis2 :=
  ax.add (.poly polys { color := .values values { colormap, colorrange } }) label

/-- `contourf!` from polygons given as an exterior ring and hole rings each (e.g. isoband
output grouped into polygons), one value per polygon. -/
def contourfPolygons (ax : Axis2) (polys : Array (Pts2 × Array Pts2)) (values : FloatArray)
    (colormap : Colormap := Colormap.viridis) (colorrange : Option (Float × Float) := none)
    (label : Option String := none) : Axis2 :=
  let join (outer : Pts2) (holes : Array Pts2) : Pts2 :=
    holes.foldl (init := outer) fun acc h =>
      Pts2.ofArrays ⟨(acc.xs.push Num.nan).data ++ h.xs.data⟩ ⟨(acc.ys.push Num.nan).data ++ h.ys.data⟩
  ax.contourfBands (polys.map fun (o, hs) => join o hs) values colormap colorrange label

/-- `hlines!(ax, ys)`: horizontal lines across the whole x range (they only take part in
the y autolimits). -/
def hlines (ax : Axis2) (ys : Array Float) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) : Axis2 :=
  let (c, ax) := ax.cycleSegments color
  ax.add (.hlines ⟨ys⟩ { color := c, width := linewidth, style := linestyle })

/-- `vlines!(ax, xs)`: vertical lines across the whole y range. -/
def vlines (ax : Axis2) (xs : Array Float) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) : Axis2 :=
  let (c, ax) := ax.cycleSegments color
  ax.add (.vlines ⟨xs⟩ { color := c, width := linewidth, style := linestyle })

/-- `limits!(ax, x0, x1, y0, y1)`. -/
def limits (ax : Axis2) (x0 x1 y0 y1 : Float) : Axis2 := { ax with xlimits := (some x0, some x1), ylimits := (some y0, some y1) }

/-- `xlims!(ax, lo, hi)`. -/
def xlims (ax : Axis2) (lo hi : Float) : Axis2 := { ax with xlimits := (some lo, some hi) }

/-- `ylims!(ax, lo, hi)`. -/
def ylims (ax : Axis2) (lo hi : Float) : Axis2 := { ax with ylimits := (some lo, some hi) }

/-- Legend positions of `axislegend` (`:rt`, `:lb`, …). -/
inductive LegendPos where
  | rt | rb | rc | lt | lb | lc | ct | cb | cc
  deriving Repr, Inhabited, BEq

/-- `axislegend(ax; position)`: a legend of the labelled items inside the plot area. -/
def axislegend (ax : Axis2) (position : LegendPos := .rt) (title : Option String := none) : Axis2 :=
  let (h, v) : Float × Float := match position with
    | .rt => (1, 1) | .rb => (1, 0) | .rc => (1, 0.5) | .lt => (0, 1) | .lb => (0, 0) | .lc => (0, 0.5)
    | .ct => (0.5, 1) | .cb => (0.5, 0) | .cc => (0.5, 0.5)
  { ax with legend := some { title, halign := h, valign := v } }

/-- Update the axis style. -/
def mapStyle (ax : Axis2) (f : AxisStyle → AxisStyle) : Axis2 := { ax with style := f ax.style }

/-- Update the x-axis style (`ax.xaxis fun s => { s with ticklabelrotation := π/4 }`). -/
def xaxis (ax : Axis2) (f : LineAxisStyle → LineAxisStyle) : Axis2 := ax.mapStyle fun s => { s with x := f s.x }

/-- Update the y-axis style. -/
def yaxis (ax : Axis2) (f : LineAxisStyle → LineAxisStyle) : Axis2 := ax.mapStyle fun s => { s with y := f s.y }

/-- Show minor ticks and minor grid lines (Makie `xminorticksvisible`, `xminorgridvisible`, …). -/
def minorGrid (ax : Axis2) (x : Bool := true) (y : Bool := true) : Axis2 :=
  let on (s : LineAxisStyle) : LineAxisStyle := { s with minorticksvisible := true, minorgridvisible := true }
  let ax := if x then ax.xaxis on else ax
  if y then ax.yaxis on else ax

/-- `hidedecorations!(ax)`: hide labels, tick labels, ticks and grids. -/
def hidedecorations (ax : Axis2) : Axis2 :=
  let hide (s : LineAxisStyle) : LineAxisStyle :=
    { s with labelvisible := false, ticklabelsvisible := false, ticksvisible := false, gridvisible := false
             minorgridvisible := false, minorticksvisible := false }
  { ax with style := { ax.style with x := hide ax.style.x, y := hide ax.style.y } }

/-- `hidespines!(ax)`. -/
def hidespines (ax : Axis2) : Axis2 :=
  { ax with style := { ax.style with leftspinevisible := false, rightspinevisible := false
                                     topspinevisible := false, bottomspinevisible := false } }

end Axis2

end LeanPlot
