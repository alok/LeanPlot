import LeanPlot.Recipes.Basic.Axis2

/-!
# Basic 3D recipes (`Axis3` builders)

| builder | Makie |
|---|---|
| `lines`, `linesPts`, `linesColored` | `lines!` (3D) |
| `linesegments` | `linesegments!` |
| `scatter`, `scatterPts` | `scatter!` |
| `mesh` | `mesh!` (shaded) |
| `surface` | `surface!` (a shaded mesh coloured by height with viridis) |
| `wireframe` | `wireframe!` (grid lines of a surface) |
| `arrows3d` | `arrows3d!` (drawn as projected 2D arrows, see below) |
| `text` | `text!` |
| `limits` | `limits!` |

Colour cycling follows the 2D rules. `arrows3d` is approximated by `arrows2d` geometry in
the projected plane (CairoMakie draws shaded 3D shafts and cones).
-/

namespace LeanPlot

open LeanPlot.Num

namespace Axis3

/-- A fresh 3D axis. -/
def new (title : String := "") (xlabel : String := "x") (ylabel : String := "y") (zlabel : String := "z")
    (azimuth : Float := 1.275 * Num.pi) (elevation : Float := Num.pi / 8) (perspectiveness : Float := 0)
    (aspect : Aspect3 := .ratio 1 1 (2 / 3)) (theme : Theme := {}) : Axis3 :=
  { title, xlabel, ylabel, zlabel, theme, view := { azimuth, elevation, perspectiveness, aspect } }

/-- Append a plot item. -/
def add (ax : Axis3) (m : Mark) (label : Option String := none) : Axis3 :=
  { ax with items := ax.items.push { mark := m, label } }

/-- Next palette colour for `lines`. -/
def cycleLines (ax : Axis3) (c : Option ColorSpec) : ColorSpec × Axis3 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.color ax.cycle.lines), { ax with cycle := { ax.cycle with lines := ax.cycle.lines + 1 } })

/-- Next palette colour for `linesegments`/`wireframe`. -/
def cycleSegments (ax : Axis3) (c : Option ColorSpec) : ColorSpec × Axis3 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.color ax.cycle.segments), { ax with cycle := { ax.cycle with segments := ax.cycle.segments + 1 } })

/-- Next palette colour for `scatter`. -/
def cycleScatter (ax : Axis3) (c : Option ColorSpec) : ColorSpec × Axis3 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.color ax.cycle.scatter), { ax with cycle := { ax.cycle with scatter := ax.cycle.scatter + 1 } })

/-- Next patch colour for `mesh`. -/
def cycleMesh (ax : Axis3) (c : Option ColorSpec) : ColorSpec × Axis3 :=
  match c with
  | some c => (c, ax)
  | none => (.solid (ax.theme.patchColor ax.cycle.mesh), { ax with cycle := { ax.cycle with mesh := ax.cycle.mesh + 1 } })

/-- `lines!(ax, p)` for 3D points. -/
def linesPts (ax : Axis3) (p : Pts3) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) (label : Option String := none) : Axis3 :=
  let (c, ax) := ax.cycleLines color
  ax.add (.lines (.xyz p) { color := c, width := linewidth, style := linestyle }) label

/-- `lines!(ax, xs, ys, zs)`. -/
def lines (ax : Axis3) (xs ys zs : FloatArray) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (linestyle : LineStyle := .solid) (label : Option String := none) : Axis3 :=
  ax.linesPts (Pts3.ofArrays xs ys zs) color linewidth linestyle label

/-- A 3D polyline coloured per vertex by values. -/
def linesColored (ax : Axis3) (p : Pts3) (values : FloatArray) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (linewidth : Float := 1.5) (label : Option String := none) : Axis3 :=
  ax.add (.lines (.xyz p) { color := .values values { colormap, colorrange }, width := linewidth }) label

/-- `linesegments!(ax, p)`. -/
def linesegments (ax : Axis3) (p : Pts3) (color : Option ColorSpec := none) (linewidth : Float := 1.5)
    (label : Option String := none) : Axis3 :=
  let (c, ax) := ax.cycleSegments color
  ax.add (.segments (.xyz p) { color := c, width := linewidth }) label

/-- `scatter!(ax, p)` for 3D points. -/
def scatterPts (ax : Axis3) (p : Pts3) (color : Option ColorSpec := none) (marker : MarkerShape := .circle)
    (markersize : Float := 9) (label : Option String := none) : Axis3 :=
  let (c, ax) := ax.cycleScatter color
  ax.add (.scatter (.xyz p) { shape := marker, size := markersize, color := c }) label

/-- `scatter!(ax, xs, ys, zs)`. -/
def scatter (ax : Axis3) (xs ys zs : FloatArray) (color : Option ColorSpec := none) (marker : MarkerShape := .circle)
    (markersize : Float := 9) (label : Option String := none) : Axis3 :=
  ax.scatterPts (Pts3.ofArrays xs ys zs) color marker markersize label

/-- `mesh!(ax, m)`: shaded triangles (per-vertex colours or values). -/
def mesh (ax : Axis3) (m : TriMesh) (color : Option ColorSpec := none) (shading : Bool := true)
    (label : Option String := none) : Axis3 :=
  let (c, ax) := ax.cycleMesh color
  ax.add (.mesh { mesh := m, color := c, shading }) label

/-- `surface!(ax, xs, ys, z)`: the height field as a shaded mesh coloured by `z` (or by
`color` values) through `colormap`. -/
def surface {nx ny : Nat} (ax : Axis3) (xs ys : FloatArray) (z : Grid2 nx ny) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (color : Option FloatArray := none) (shading : Bool := true)
    (label : Option String := none) : Axis3 :=
  let g : AnyGrid2 := ⟨nx, ny, z⟩
  let m := Recipes.surfaceMesh xs ys g
  ax.add (.mesh { mesh := m, color := .values (color.getD z.z) { colormap, colorrange }, shading }) label

/-- `wireframe!(ax, xs, ys, z)`: the grid lines of a surface. -/
def wireframe {nx ny : Nat} (ax : Axis3) (xs ys : FloatArray) (z : Grid2 nx ny) (color : Option ColorSpec := none)
    (linewidth : Float := 1.5) (label : Option String := none) : Axis3 :=
  ax.linesegments (Recipes.wireframeSegments xs ys ⟨nx, ny, z⟩) color linewidth label

/-- `arrows3d!(ax, origins, directions)`, drawn with 2D arrow geometry after projection. -/
def arrows3d (ax : Axis3) (origins dirs : Pts3) (color : ColorSpec := .black) (lengthscale : Float := 1)
    (normalize : Bool := false) (align : Float := 0) (label : Option String := none) : Axis3 :=
  ax.add (.arrows (.xyz origins) (.xyz dirs) { color, lengthscale, normalize, align }) label

/-- `text!(ax, x, y, z, text = s)`. -/
def text (ax : Axis3) (x y z : Float) (s : String) (fontsize : Float := 14) (color : RGBA := RGBA.black)
    (halign : HAlign := .left) (valign : VAlign := .bottom) : Axis3 :=
  ax.add (.text (.xyz (Pts3.ofArrays ⟨#[x]⟩ ⟨#[y]⟩ ⟨#[z]⟩)) #[s] { size := fontsize, color, halign, valign })

/-- 3D `streamplot!` from precomputed data: coloured streamlines plus arrowheads at the seeds
drawn as `:utriangle` markers pointing along the projected directions (CairoMakie draws
cone meshes). -/
def streamplot (ax : Axis3) (lines : Pts3) (lineValues : FloatArray) (arrowPos arrowDir : Pts3)
    (arrowValues : FloatArray) (colormap : Colormap := Colormap.viridis) (colorrange : Option (Float × Float) := none)
    (linewidth : Float := 1.5) (arrowSize : Float := 10) (label : Option String := none) : Axis3 :=
  let ax := ax.add (.lines (.xyz lines) { color := .values lineValues { colormap, colorrange }, width := linewidth }) label
  ax.add (.scatter (.xyz arrowPos) { shape := .utriangle, size := arrowSize
                                     color := .values arrowValues { colormap, colorrange }
                                     alongDirections := some (.xyz arrowDir, -Num.pi / 2) })

/-- `hidedecorations!(ax)`: hide labels, tick labels, ticks and grids (an `LScene`-like view
keeps only the frame; add `hidespines` for a bare scene). -/
def hidedecorations (ax : Axis3) : Axis3 :=
  let hide (d : Axis3DimStyle) : Axis3DimStyle :=
    { d with labelvisible := false, ticklabelsvisible := false, ticksvisible := false, gridvisible := false }
  { ax with style := { ax.style with x := hide ax.style.x, y := hide ax.style.y, z := hide ax.style.z } }

/-- `hidespines!(ax)`. -/
def hidespines (ax : Axis3) : Axis3 :=
  let hide (d : Axis3DimStyle) : Axis3DimStyle := { d with spinesvisible := false }
  { ax with style := { ax.style with x := hide ax.style.x, y := hide ax.style.y, z := hide ax.style.z } }

/-- `limits!(ax, x0, x1, y0, y1, z0, z1)`. -/
def limits (ax : Axis3) (x0 x1 y0 y1 z0 z1 : Float) : Axis3 :=
  { ax with xlimits := (some x0, some x1), ylimits := (some y0, some y1), zlimits := (some z0, some z1) }

end Axis3

end LeanPlot
