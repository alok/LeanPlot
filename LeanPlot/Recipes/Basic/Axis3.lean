import LeanPlot.Recipes.Basic.Axis2
import LeanPlot.Recipes.Algo.Volume

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
| `volume`, `volumeIndexed`, `contourVolume` | `volume!` (`:mip`, `:absorption`, `:iso`, `:indexedabsorption`), `contour!` of a volume |
| `volumeslices` | `volumeslices!` (three axis-aligned heatmaps and the bounding box) |
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

/-- A volume mark over the box `[x0, x1] × [y0, y1] × [z0, z1]` from a ray-march context
(`ctx.light` is replaced by the scene light at render time). -/
def volumeCtx (ax : Axis3) (x0 x1 y0 y1 z0 z1 : Float) (ctx : Recipes.Algo.Volume.Ctx)
    (mapping : Option (ColorMapping × Float × Float)) (label : Option String := none) : Axis3 :=
  let box : Rect3 := ⟨⟨min x0 x1, min y0 y1, min z0 z1⟩, ⟨(x1 - x0).abs, (y1 - y0).abs, (z1 - z0).abs⟩⟩
  ax.add (.volume box { ray := Recipes.Algo.Volume.rayFn ctx, mapping }) label

/-- `volume!(ax, x0..x1, y0..y1, z0..z1, data; algorithm, colormap, colorrange, absorption,
isovalue, isorange, interpolate, alpha)` for float data (Makie's defaults: `:mip`, the data
extrema, `absorption = 1`, `isovalue = 0.5`, `isorange = 0.05`, 200 samples). Ray cast in 3D
axes (see `Recipes.Algo.Volume`). -/
def volume (ax : Axis3) (x0 x1 y0 y1 z0 z1 : Float) (d : Recipes.Algo.Volume.Data)
    (algorithm : Recipes.Algo.Volume.Algorithm := .mip) (colormap : Colormap := Colormap.viridis)
    (colorrange : Option (Float × Float) := none) (absorption : Float := 1) (isovalue : Float := 0.5)
    (isorange : Float := 0.05) (interpolate : Bool := true) (alpha : Float := 1) (samples : Nat := 200)
    (label : Option String := none) : Axis3 :=
  let (lo, hi) := colorrange.getD d.extrema
  let ctx : Recipes.Algo.Volume.Ctx :=
    { data := d, interp := interpolate, algorithm, lut := .ofColormap colormap alpha, lo, hi, absorption
      isovalue, isorange, samples, light := {} }
  ax.volumeCtx x0 x1 y0 y1 z0 z1 ctx (some ({ colormap, colorrange := some (lo, hi), alpha }, lo, hi)) label

/-- `volume!(…; algorithm = :indexedabsorption, colormap = colors)`: each sample indexes
`colors` (`int(value) - 1`), accumulated with `absorption`. -/
def volumeIndexed (ax : Axis3) (x0 x1 y0 y1 z0 z1 : Float) (d : Recipes.Algo.Volume.Data) (colors : Array RGBA)
    (absorption : Float := 1) (interpolate : Bool := true) (samples : Nat := 200) (label : Option String := none) : Axis3 :=
  let ctx : Recipes.Algo.Volume.Ctx :=
    { data := d, interp := interpolate, algorithm := .indexedAbsorption, lut := .ofColors colors, lo := 0, hi := 1
      absorption, isovalue := 0, isorange := 0, samples, light := {} }
  ax.volumeCtx x0 x1 y0 y1 z0 z1 ctx none label

/-- Makie `contour!(ax, x0..x1, y0..y1, z0..z1, volume; levels, alpha, isorange)`: lit
isosurface shells of the levels (`Recipes.Algo.Volume.contourColormap`, algorithm 7). -/
def contourVolume (ax : Axis3) (x0 x1 y0 y1 z0 z1 : Float) (d : Recipes.Algo.Volume.Data)
    (levels : Recipes.Algo.Levels.LevelSpec := .count 5) (colormap : Colormap := Colormap.viridis) (alpha : Float := 1)
    (isorange : Option Float := none) (samples : Nat := 200) (label : Option String := none) : Axis3 :=
  let (_, lut, plo, phi) := Recipes.Algo.Volume.contourColormap d levels colormap alpha isorange
  let ctx : Recipes.Algo.Volume.Ctx :=
    { data := d, interp := true, algorithm := .contour, lut, lo := plo, hi := phi, absorption := 1
      isovalue := 0, isorange := 0, samples, light := {} }
  let (vlo, vhi) := d.extrema
  ax.volumeCtx x0 x1 y0 y1 z0 z1 ctx (some ({ colormap, colorrange := some (vlo, vhi) }, vlo, vhi)) label

/-- Flat-coloured quads of a heatmap placed in a coordinate plane of 3D space (Makie `heatmap!`
after `transform!(h, (plane, offset))`): cells between the edges `us × vs`, mapped by `place u v`
to 3D, appended to mesh buffers. -/
def planeHeatmapQuads (us vs : FloatArray) (vals : FloatArray) (m : ColorMapping) (lo hi : Float)
    (place : Float → Float → Vec3) (acc : FloatArray × FloatArray × FloatArray × ByteArray × Array UInt32) :
    FloatArray × FloatArray × FloatArray × ByteArray × Array UInt32 :=
  let nu := us.size - 1
  let nv := vs.size - 1
  let n := nu * nv
  let rgba := m.toRGBA8 lo hi vals
  let rec go (k : Nat) (xs ys zs : FloatArray) (cs : ByteArray) (tri : Array UInt32) :
      FloatArray × FloatArray × FloatArray × ByteArray × Array UInt32 :=
    if k < n then
      let i := k % nu
      let j := k / nu
      let c := RGBA.ofRGBA8At rgba k
      let ps := #[place (us.get! i) (vs.get! j), place (us.get! (i + 1)) (vs.get! j),
                  place (us.get! (i + 1)) (vs.get! (j + 1)), place (us.get! i) (vs.get! (j + 1))]
      let base := xs.size.toUInt32
      let (xs, ys, zs, cs) := ps.foldl (init := (xs, ys, zs, cs)) fun (xs, ys, zs, cs) p =>
        (xs.push p.x, ys.push p.y, zs.push p.z, RGBA.pushRGBA8 cs c)
      go (k + 1) xs ys zs cs ((((((tri.push base).push (base + 1)).push (base + 2)).push base).push (base + 2)).push (base + 3))
    else (xs, ys, zs, cs, tri)
  termination_by n - k
  let (xs, ys, zs, cs, tri) := acc
  go 0 xs ys zs cs tri

/-- `volumeslices!(ax, xs, ys, zs, v)` (`basic_recipes/volumeslices.jl`): heatmaps of the
first slices `v[1, :, :]` (plane `yz` at `x = xs[1]`), `v[:, 1, :]` (`xz` at `ys[1]`) and
`v[:, :, 1]` (`xy` at `zs[1]`), cell centres at the grid points, coloured over the volume's
extrema, then the bounding box in `RGBAf(0.5, 0.5, 0.5, 0.5)`. The slices are one mesh painted
back to front (CairoMakie orders them by their `translate!` z instead, which only happens to
agree for views from above). -/
def volumeslices (ax : Axis3) (xs ys zs : FloatArray) (d : Recipes.Algo.Volume.Data)
    (colormap : Colormap := Colormap.viridis) (colorrange : Option (Float × Float) := none)
    (bbox : Bool := true) : Axis3 :=
  let (lo, hi) := colorrange.getD d.extrema
  let m : ColorMapping := { colormap, colorrange := some (lo, hi) }
  let at3 (i j k : Nat) : Float := d.values.get! (i + d.nx * (j + d.ny * k))
  let slice (na nb : Nat) (f : Nat → Nat → Float) : FloatArray :=
    (List.range (na * nb)).foldl (init := FloatArray.emptyWithCapacity (na * nb)) fun acc t => acc.push (f (t % na) (t / na))
  let ex := Recipes.cellEdges xs d.nx
  let ey := Recipes.cellEdges ys d.ny
  let ez := Recipes.cellEdges zs d.nz
  let x0 := xs.get! 0
  let y0 := ys.get! 0
  let z0 := zs.get! 0
  -- one mesh, so the painter's sort orders the three slices against each other
  let acc := (FloatArray.empty, FloatArray.empty, FloatArray.empty, ByteArray.empty, (#[] : Array UInt32))
  let acc := planeHeatmapQuads ey ez (slice d.ny d.nz fun j k => at3 0 j k) m lo hi (fun u v => ⟨x0, u, v⟩) acc
  let acc := planeHeatmapQuads ex ez (slice d.nx d.nz fun i k => at3 i 0 k) m lo hi (fun u v => ⟨u, y0, v⟩) acc
  let acc := planeHeatmapQuads ex ey (slice d.nx d.ny fun i j => at3 i j 0) m lo hi (fun u v => ⟨u, v, z0⟩) acc
  let (mx, my, mz, cs, tri) := acc
  let ax := match TriMesh.mk? (Pts3.ofArrays mx my mz) tri with
    | some mesh => { ax with items := ax.items.push { mark := .mesh { mesh, color := .perElement cs, shading := false }
                                                      autolimits := false } }
    | none => ax
  -- Makie's `data_limits` of a heatmap ignore its model matrix: each slice counts as its
  -- untransformed rectangle `us × vs × {0}` (invisible markers)
  let rect (us vs : FloatArray) (ax : Axis3) : Axis3 :=
    match extremaFinite us, extremaFinite vs with
    | some (u0, u1), some (v0, v1) => ax.add (.scatter (.xyz (Pts3.ofArrays ⟨#[u0, u1]⟩ ⟨#[v0, v1]⟩ ⟨#[0, 0]⟩)) { size := 0 })
    | _, _ => ax
  let ax := rect ex ey (rect ex ez (rect ey ez ax))
  if !bbox then ax else
  match extremaFinite xs, extremaFinite ys, extremaFinite zs with
  | some (a0, a1), some (b0, b1), some (c0, c1) =>
    -- the 12 edges of `Rect3(mx, my, mz, Mx - mx, My - my, Mz - mz)` as segment pairs
    let cs : Array Vec3 := #[⟨a0, b0, c0⟩, ⟨a1, b0, c0⟩, ⟨a0, b1, c0⟩, ⟨a1, b1, c0⟩,
                             ⟨a0, b0, c1⟩, ⟨a1, b0, c1⟩, ⟨a0, b1, c1⟩, ⟨a1, b1, c1⟩]
    let es : Array (Nat × Nat) := #[(0, 1), (0, 2), (1, 3), (2, 3), (4, 5), (4, 6), (5, 7), (6, 7),
                                    (0, 4), (1, 5), (2, 6), (3, 7)]
    let (px, py, pz) := es.foldl (init := ((#[] : Array Float), (#[] : Array Float), (#[] : Array Float)))
      fun (px, py, pz) (a, b) =>
        let u := cs[a]!
        let v := cs[b]!
        ((px.push u.x).push v.x, (py.push u.y).push v.y, (pz.push u.z).push v.z)
    ax.add (.segments (.xyz (Pts3.ofArrays ⟨px⟩ ⟨py⟩ ⟨pz⟩)) { color := .solid ⟨0.5, 0.5, 0.5, 0.5⟩, width := 1.5 })
  | _, _, _ => ax

/-- `limits!(ax, x0, x1, y0, y1, z0, z1)`. -/
def limits (ax : Axis3) (x0 x1 y0 y1 z0 z1 : Float) : Axis3 :=
  { ax with xlimits := (some x0, some x1), ylimits := (some y0, some y1), zlimits := (some z0, some z1) }

end Axis3

end LeanPlot
