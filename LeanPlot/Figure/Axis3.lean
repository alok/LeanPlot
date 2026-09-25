import LeanPlot.Figure.Axis2
import LeanPlot.Core.Camera

/-!
# `Axis3`: a 3D axis (Makie `Axis3`)

The camera is `LeanPlot.Camera3` (Makie's `calculate_matrices`, validated against the oracle).
This module adds the block around it, following `makielayout/blocks/axis3d.jl`:

* **limits**: data limits of the items expanded by the `0.05` margins per dimension, stored
  as a `Rect3f` (Float32 origin and widths), default `(0, 1)³` without data;
* **layout**: fixed protrusions (30 px per side by default); the scene area is the computed
  box grown by the protrusions and rounded to integer pixels;
* **decorations**: the three back panels (transparent by default), grid lines on the two
  back planes of each dimension, three frame lines per dimension, ticks 6 px long along
  the projected direction away from the box, tick labels 5 px (z: 10 px) beyond, axis
  labels 40 px (z: 50 px) from the edge midpoint, rotated to the edge direction; the
  visible faces follow Makie's `azimuth`/`elevation` rules;
* **marks**: projected with the camera and clipped to the scene area; meshes are painted
  back to front and optionally Lambert/Blinn–Phong shaded with Makie's default light
  (ambient 0.45, directional 0.5 from the camera-relative direction
  `(-0.457, -0.629, -0.629)`, specular 0.2, shininess 32).

Draw order follows CairoMakie's z-sort of the Axis3 plots: ticks, panels, grid and frame
lines, axis labels and title, then the plots, then the tick labels.
-/

namespace LeanPlot

open LeanPlot.Num LeanPlot.Layout

/-- Per-dimension decoration style of an `Axis3` (Makie defaults). -/
structure Axis3DimStyle where
  ticksize : Float := 6
  tickwidth : Float := 1
  tickcolor : RGBA := RGBA.black
  ticksvisible : Bool := true
  ticklabelsize : Float := 14
  ticklabelcolor : RGBA := RGBA.black
  ticklabelpad : Float := 5
  ticklabelsvisible : Bool := true
  labelsize : Float := 14
  labelcolor : RGBA := RGBA.black
  labeloffset : Float := 40
  labelvisible : Bool := true
  gridvisible : Bool := true
  gridwidth : Float := 1
  gridcolor : RGBA := MakieTheme.gridColor
  spinesvisible : Bool := true
  spinewidth : Float := 1
  spinecolor : RGBA := RGBA.black
  deriving Inhabited

/-- Style of an `Axis3`. -/
structure Axis3Style where
  x : Axis3DimStyle := {}
  y : Axis3DimStyle := {}
  z : Axis3DimStyle := { ticklabelpad := 10, labeloffset := 50 }
  /-- Panel colours (xy, yz, xz); transparent by default. -/
  xypanelcolor : RGBA := RGBA.transparent
  yzpanelcolor : RGBA := RGBA.transparent
  xzpanelcolor : RGBA := RGBA.transparent
  titlesize : Float := 14
  titlegap : Float := 4
  titlecolor : RGBA := RGBA.black
  titlealign : Float := 0.5
  deriving Inhabited

/-- A 3D axis. -/
structure Axis3 where
  items : Array PlotItem := #[]
  title : String := ""
  xlabel : String := "x"
  ylabel : String := "y"
  zlabel : String := "z"
  view : Axis3View := {}
  xlimits : Option Float × Option Float := (none, none)
  ylimits : Option Float × Option Float := (none, none)
  zlimits : Option Float × Option Float := (none, none)
  xticks : TickSpec := .auto
  yticks : TickSpec := .auto
  zticks : TickSpec := .auto
  margin : Float := 0.05
  width : SizeAttr := .fill
  height : SizeAttr := .fill
  halign : Float := 0.5
  valign : Float := 0.5
  style : Axis3Style := {}
  theme : Theme := {}
  cycle : Cycle := {}
  deriving Inhabited

/-- An `Axis3` resolved before layout: limits (Float32 box) and ticks per dimension. -/
structure Axis3Prep where
  limits : Rect3
  xt : AxisTicks
  yt : AxisTicks
  zt : AxisTicks
  deriving Inhabited

namespace Axis3

/-- Round to `Float32`. -/
@[inline] private def f32 (x : Float) : Float := x.toFloat32.toFloat

/-- Final limits: Makie's `reset_limits!` for `Axis3`, stored as `Rect3f`. -/
def targetLimits (ax : Axis3) : Rect3 :=
  let (bx, by_, bz) := ax.items.foldl (init := (none, none, none)) fun (bx, by_, bz) it =>
    if it.autolimits then
      let (x, y, z) := it.mark.dimBounds
      (Mark.unionRange bx x, Mark.unionRange by_ y, Mark.unionRange bz z)
    else (bx, by_, bz)
  let dim (lims : Option Float × Option Float) (bb : Option (Float × Float)) : Float × Float :=
    match lims with
    | (some lo, some hi) => (lo, hi)
    | (ulo, uhi) =>
      let auto := match bb with
        | some (a, b) => if a.isFinite && b.isFinite then Axis2.expandLimits a b ax.margin ax.margin .identity else (0, 1)
        | none => (0, 1)
      (ulo.getD auto.1, uhi.getD auto.2)
  let (x0, x1) := dim ax.xlimits bx
  let (y0, y1) := dim ax.ylimits by_
  let (z0, z1) := dim ax.zlimits bz
  ⟨⟨f32 x0, f32 y0, f32 z0⟩, ⟨f32 (x1 - x0), f32 (y1 - y0), f32 (z1 - z0)⟩⟩

/-- The upper limit `origin + width`: Makie stores the `Rect3f` target limits in a `Rect3d`
(`finallimits`), so the sum of the Float32-rounded origin and width is taken in `Float64`. -/
@[inline] private def hi32 (o w : Float) : Float := o + w

/-- Ticks of one dimension (`get_ticks` with Wilkinson ticks, no filtering). -/
def ticksFor (spec : TickSpec) (lo hi : Float) : AxisTicks :=
  let (vals, labels) : Array Float × Array TickLabel := match spec with
    | .auto => Scale.identity.ticks lo hi
    | .values vs => (vs, formatTicksAuto vs)
    | .labeled vs ls => (vs, ls.map .plain)
  { values := vals, labels }

/-- Prepare limits and ticks. -/
def prepare (ax : Axis3) : Axis3Prep :=
  let l := ax.targetLimits
  let o := l.origin
  let w := l.widths
  { limits := l
    xt := ticksFor ax.xticks o.x (hi32 o.x w.x)
    yt := ticksFor ax.yticks o.y (hi32 o.y w.y)
    zt := ticksFor ax.zticks o.z (hi32 o.z w.z) }

/-- Layout protrusions (Makie `protrusions`, 30 by default). -/
def protrusions (ax : Axis3) : Sides :=
  let p := ax.view.protrusions
  Sides.toF32 { left := p.left, right := p.right, bottom := p.bottom, top := p.top }

/-- The grid item. -/
def item (ax : Axis3) (span : Span) : Item :=
  { span, protrusions := ax.protrusions
    width := reportedSize ax.width none true, height := reportedSize ax.height none true }

/-- The computed box in a cell. -/
def computedBox (ax : Axis3) (cell : BBox) : BBox :=
  place cell ax.width ax.height none none (reportedSize ax.width none true) (reportedSize ax.height none true)
    ax.halign ax.valign

/-- The scene area: the computed box grown by the protrusions, rounded to integers. -/
def sceneArea (ax : Axis3) (box : BBox) : BBox :=
  let p := ax.protrusions
  (BBox.toF32 ⟨box.left - p.left, box.right + p.right, box.bottom - p.bottom, box.top + p.top⟩).roundInt

/-- Julia `mod1(x, y)` for floats. -/
def mod1F (x y : Float) : Float :=
  let m := x - y * (x / y).floor
  if m == 0 then y else m

/-- Coordinates of a point given the value along `dim` and the two other coordinates
(Makie `dimpoint`). -/
def dimpoint (dim : Nat) (v v1 v2 : Float) : Vec3 :=
  match dim with
  | 0 => ⟨v, v1, v2⟩
  | 1 => ⟨v1, v, v2⟩
  | _ => ⟨v1, v2, v⟩

/-- The other two dimensions (Makie `dim1`, `dim2`). -/
def otherDims (dim : Nat) : Nat × Nat :=
  match dim with
  | 0 => (1, 2)
  | 1 => (0, 2)
  | _ => (0, 1)

/-- Component `k` of a vector. -/
def comp (v : Vec3) : Nat → Float
  | 0 => v.x
  | 1 => v.y
  | _ => v.z

/-- Normalize a 2D vector (zero stays zero). -/
def norm2 (x y : Float) : Float × Float :=
  let n := Float.sqrt (x * x + y * y)
  if n == 0 then (0, 0) else (x / n, y / n)

/-- Julia `isapprox` with the default tolerance. -/
def approx (a b : Float) : Bool := a == b || (a - b).abs ≤ 1.4901161193847656e-8 * max a.abs b.abs

/-- Makie's default camera-relative light direction. -/
def lightDirection : Vec3 := ⟨-0.45679495, -0.6293204, -0.6287243⟩

/-- Per-vertex normals of a mesh as SoA `(nx, ny, nz)`: normalized sums of the
(unnormalized) face normals (GeometryBasics `normals`). -/
def vertexNormals (m : TriMesh) : FloatArray × FloatArray × FloatArray :=
  let nv := m.numVertices
  let nt := m.numTriangles
  let z := FloatArray.mk (Array.replicate nv 0)
  let rec faces (t : Nat) (ax ay az : FloatArray) : FloatArray × FloatArray × FloatArray :=
    if t < nt then
      let ia := m.tri[3 * t]!.toNat
      let ib := m.tri[3 * t + 1]!.toNat
      let ic := m.tri[3 * t + 2]!.toNat
      let (a, b, c) := m.triangle t
      let n := (b.sub a).cross (c.sub a)
      if n.isFinite then
        let add (arr : FloatArray) (i : Nat) (v : Float) : FloatArray := arr.set! i (arr.get! i + v)
        faces (t + 1) (add (add (add ax ia n.x) ib n.x) ic n.x) (add (add (add ay ia n.y) ib n.y) ic n.y)
          (add (add (add az ia n.z) ib n.z) ic n.z)
      else faces (t + 1) ax ay az
    else (ax, ay, az)
  termination_by nt - t
  let (ax, ay, az) := faces 0 z z z
  let rec norm (i : Nat) (ox oy oz : FloatArray) : FloatArray × FloatArray × FloatArray :=
    if i < nv then
      let v := Vec3.normalize ⟨ax.get! i, ay.get! i, az.get! i⟩
      norm (i + 1) (ox.push v.x) (oy.push v.y) (oz.push v.z)
    else (ox, oy, oz)
  termination_by nv - i
  norm 0 (FloatArray.emptyWithCapacity nv) (FloatArray.emptyWithCapacity nv) (FloatArray.emptyWithCapacity nv)

/-- Makie's default light: ambient 0.45, directional light colour 0.5, specular 0.2,
shininess 32. -/
structure Light where
  ambient : Float := 0.45
  color : Float := 0.5
  specular : Float := 0.2
  shininess : Float := 32
  deriving Inhabited

/-- Shade per-vertex colours like CairoMakie's `_calculate_shaded_vertexcolors`: the vertex
normals go through the normal matrix of the axis model, the light direction is camera
relative, and each colour becomes `(ambient + light·max(L·(−N), 0))·c + light·specular·
max(H·(−N), 0)^shininess` with `H = normalize(L + v)`, `v` the camera-to-vertex direction. -/
def shadeColors (cam : Camera3) (m : MeshData) (light : Light := {}) : ByteArray :=
  let nv := m.mesh.numVertices
  let base := m.color.resolve nv
  let (nxs, nys, nzs) := vertexNormals m.mesh
  let model := cam.model
  -- normal matrix: transpose(inv(M₃)) for the diagonal-scale-plus-translation model
  let sx := if model.m00 != 0 then 1 / model.m00 else 0
  let sy := if model.m11 != 0 then 1 / model.m11 else 0
  let sz := if model.m22 != 0 then 1 / model.m22 else 0
  -- camera-relative light: inverse(view)[1:3, 1:3] * dir = transpose of the rotation
  let v := cam.view
  let d := lightDirection
  let L : Vec3 := ⟨v.m00 * d.x + v.m10 * d.y + v.m20 * d.z, v.m01 * d.x + v.m11 * d.y + v.m21 * d.z,
    v.m02 * d.x + v.m12 * d.y + v.m22 * d.z⟩
  let eye := cam.eyepos
  let rec go (i : Nat) (out : ByteArray) : ByteArray :=
    if i < nv then
      let c := RGBA.ofRGBA8At base i
      let N := (Vec3.mk (nxs.get! i * sx) (nys.get! i * sy) (nzs.get! i * sz)).normalize
      let w := model.mulPoint (m.mesh.pos.get! i)
      let world : Vec3 := ⟨w.x / w.w, w.y / w.w, w.z / w.w⟩
      let view := (world.sub eye).normalize
      let diff := max 0 (L.dot N.neg)
      let H := (L.add view).normalize
      let spec := Float.pow (max 0 (H.dot N.neg)) light.shininess
      let k := light.ambient + light.color * diff
      let s := light.color * light.specular * spec
      go (i + 1) (RGBA.pushRGBA8 out ⟨k * c.r + s, k * c.g + s, k * c.b + s, c.a⟩)
    else out
  termination_by nv - i
  go 0 (ByteArray.emptyWithCapacity (4 * nv))

/-- Decorations of one dimension in figure pixels (y up): tick segments, tick label anchors
and alignment, and the axis label anchor, rotation and vertical alignment. -/
structure DimDecor where
  ticks : Array (Float × Float × Float × Float) := #[]
  tickLabels : Array (Float × Float) := #[]
  tickAlign : HAlign × VAlign := (.left, .bottom)
  label : Option ((Float × Float) × Float × VAlign) := none
  deriving Inhabited

/-- The visible-face flags `(mi1, mi2, mi3)` of Makie's `Axis3` for the view. -/
def faceFlags (v : Axis3View) : Bool × Bool × Bool :=
  let a := mod1F v.azimuth (2 * Num.pi)
  (!(Num.pi / 2 ≤ a && a < 3 * Num.pi / 2), 0 ≤ a && a < Num.pi, v.elevation > 0)

/-- The camera of the axis for its computed box. -/
def camera (ax : Axis3) (p : Axis3Prep) (box : BBox) (figH : Float) : Camera3 :=
  let area := ax.sceneArea box
  Camera3.ofLimits ax.view p.limits ⟨area.left, figH - area.top, area.width, area.height⟩

/-- Makie's tick, tick label and axis label geometry for dimension `d` (0 = x). -/
def dimDecor (ax : Axis3) (p : Axis3Prep) (cam : Camera3) (figH : Float) (d : Nat) : DimDecor :=
  let proj (q : Vec3) : Float × Float := let r := cam.project q; (r.x, figH - r.y)
  let lo := p.limits.origin
  let w := p.limits.widths
  let mi : Vec3 := lo
  let ma : Vec3 := ⟨hi32 lo.x w.x, hi32 lo.y w.y, hi32 lo.z w.z⟩
  let (mi1, mi2, mi3) := faceFlags ax.view
  let revs := #[ax.view.xreversed, ax.view.yreversed, ax.view.zreversed]
  let st := match d with | 0 => ax.style.x | 1 => ax.style.y | _ => ax.style.z
  let t := match d with | 0 => p.xt | 1 => p.yt | _ => p.zt
  let lbl := match d with | 0 => ax.xlabel | 1 => ax.ylabel | _ => ax.zlabel
  let (miv, min1, min2) : Bool × Bool × Bool := match d with
    | 0 => (mi1, mi2, mi3) | 1 => (mi2, mi1, mi3) | _ => (mi3, mi1, mi2)
  let (d1, d2) := otherDims d
  let rev1 := revs[d1]!
  let rev2 := revs[d2]!
  let revd := revs[d]!
  let f1 := if !(min1 != rev1) then comp mi d1 else comp ma d1
  let f2 := if min2 != rev2 then comp mi d2 else comp ma d2
  let f1o := if min1 != rev1 then comp mi d1 else comp ma d1
  let f2o := if !(min2 != rev2) then comp mi d2 else comp ma d2
  let df1 := f1 - f1o
  let df2 := f2 - f2o
  let zAlongD1 := let a := mod1F (ax.view.azimuth * 180 / Num.pi) 180; 45 ≤ a && a ≤ 135
  -- tick segments, Float32 like `Point2f`
  let segs : Array (Float × Float × Float × Float) := t.values.map fun v =>
    let p1 := dimpoint d v f1 f2
    let p2 := if d == 2 then (if zAlongD1 then dimpoint d v (f1 + df1) f2 else dimpoint d v f1 (f2 + df2))
              else dimpoint d v (f1 + df1) f2
    let (x1, y1) := proj p1
    let (x2, y2) := proj p2
    let (ux, uy) := norm2 (f32 x2 - f32 x1) (f32 y2 - f32 y1)
    (f32 x1, f32 y1, f32 (f32 x1 + st.ticksize * ux), f32 (f32 y1 + st.ticksize * uy))
  let labelsAt := segs.map fun (a, b, c, e) =>
    let (ux, uy) := norm2 (c - a) (e - b)
    (f32 (c + st.ticklabelpad * ux), f32 (e + st.ticklabelpad * uy))
  let tickAlign : HAlign × VAlign := match d with
    | 0 => (if miv != min1 then .right else .left, if min2 then .top else .bottom)
    | 1 => (if miv != min1 then .left else .right, if min2 then .top else .bottom)
    | _ => (if min1 != min2 then .left else .right, .middle)
  let label : Option ((Float × Float) × Float × VAlign) :=
    if FigText.isBlank lbl then none else
    let minr1 := min1 != rev1
    let minr2 := min2 != rev2
    let g1 := if !minr1 then comp mi d1 else comp ma d1
    let g2 := if minr2 then comp mi d2 else comp ma d2
    let (x1, y1) := proj (dimpoint d (comp mi d) g1 g2)
    let (x2, y2) := proj (dimpoint d (comp ma d) g1 g2)
    let (x1, y1, x2, y2) := (f32 x1, f32 y1, f32 x2, f32 y2)
    let mx := (x1 + x2) / 2
    let my := (y1 + y2) / 2
    let diffsign : Float :=
      if d == 0 || d == 2 then (if !((min1 != min2) != revd) then 1 else -1)
      else (if (min1 != min2) != revd then 1 else -1)
    let (nx, ny) := norm2 (diffsign * (x2 - x1)) (diffsign * (y2 - y1))
    -- rotate by +90°
    let ovx := f32 (-ny)
    let ovy := f32 nx
    let px := f32 (mx + st.labeloffset * ovx)
    let py := f32 (my + st.labeloffset * ovy)
    let ang := Float.atan2 ovy ovx
    let r0 := ang + Num.pi / 2 + Num.pi / 2
    let up0 := (r0 - Num.pi * (r0 / Num.pi).toInt64.toFloat) - Num.pi / 2
    let flip := up0 < -(88 * Num.pi / 180)
    let up := if flip then up0 + Num.pi else up0
    some ((px, py), f32 up, if ovy > 0 || flip then .bottom else .top)
  { ticks := segs, tickLabels := labelsAt, tickAlign, label }

/-- The title anchor (figure pixels, y up). -/
def titleAnchor (ax : Axis3) (box : BBox) : Float × Float :=
  (f32 (box.left + ax.style.titlealign * box.width), f32 (box.top + ax.style.titlegap))

/-- Draw ops (z-ordered as CairoMakie paints an `Axis3`: ticks, panels, grid and frame
lines, plots, axis labels and title, tick labels) for the computed box `box`. -/
def lower (ax : Axis3) (p : Axis3Prep) (box : BBox) (figH : Float) : Array (Float × DrawOp) := Id.run do
  let area := ax.sceneArea box
  let devVp : Rect := ⟨area.left, figH - area.top, area.width, area.height⟩
  let cam := ax.camera p box figH
  let lo := p.limits.origin
  let w := p.limits.widths
  let mi : Vec3 := lo
  let ma : Vec3 := ⟨hi32 lo.x w.x, hi32 lo.y w.y, hi32 lo.z w.z⟩
  let (mi1, mi2, mi3) := faceFlags ax.view
  let revs := #[ax.view.xreversed, ax.view.yreversed, ax.view.zreversed]
  let dstyle (d : Nat) : Axis3DimStyle := match d with | 0 => ax.style.x | 1 => ax.style.y | _ => ax.style.z
  let dticks (d : Nat) : AxisTicks := match d with | 0 => p.xt | 1 => p.yt | _ => p.zt
  let dlabel (d : Nat) : String := match d with | 0 => ax.xlabel | 1 => ax.ylabel | _ => ax.zlabel
  let flags (d : Nat) : Bool × Bool × Bool := match d with
    | 0 => (mi1, mi2, mi3) | 1 => (mi2, mi1, mi3) | _ => (mi3, mi1, mi2)
  let segs3 (pts : Array (Vec3 × Vec3)) (c : RGBA) (width : Float) : Option DrawOp :=
    Axis2.segOp (pts.map fun (a, b) => let pa := cam.project a; let pb := cam.project b; (pa.x, pa.y, pb.x, pb.y)) c width
  let mut ticksOps : Array (Float × DrawOp) := #[]
  let mut backOps : Array (Float × DrawOp) := #[]
  let mut labelOps : Array (Float × DrawOp) := #[]
  let mut tickLabelOps : Array (Float × DrawOp) := #[]
  -- panels: xy at z = (mi3 ? zmin : zmax), yz at x = (mi1 ? xmin : xmax), xz at y = (mi2 ? ymin : ymax)
  let panel (d1 d2 d3 : Nat) (atMin : Bool) (c : RGBA) : Option DrawOp :=
    if c.a == 0 then none else
    let off := if atMin then comp mi d3 else comp ma d3
    let pt (a b : Float) : Vec3 :=
      match d3 with
      | 2 => ⟨a, b, off⟩
      | 0 => ⟨off, a, b⟩
      | _ => ⟨a, off, b⟩
    let corners := #[pt (comp mi d1) (comp mi d2), pt (comp ma d1) (comp mi d2), pt (comp ma d1) (comp ma d2), pt (comp mi d1) (comp ma d2)]
    let path := corners.foldl (init := ({} : Path)) fun acc q =>
      let r := cam.project q
      if acc.verbs.isEmpty then acc.moveTo r.x r.y else acc.lineTo r.x r.y
    some (.path path.close (some { color := c }) none none)
  for op in [panel 0 1 2 mi3 ax.style.xypanelcolor, panel 1 2 0 mi1 ax.style.yzpanelcolor, panel 0 2 1 mi2 ax.style.xzpanelcolor] do
    if let some o := op then backOps := backOps.push (0, o)
  -- grid lines and frames per dimension
  for d in [0:3] do
    let st := dstyle d
    let (_, min1, min2) := flags d
    let (d1, d2) := otherDims d
    let rev1 := revs[d1]!
    let rev2 := revs[d2]!
    let f1 := if min1 != rev1 then comp mi d1 else comp ma d1
    let f2 := if min2 != rev2 then comp mi d2 else comp ma d2
    let ticks := (dticks d).values.filter fun t => !(approx t (comp mi d) || approx t (comp ma d))
    if st.gridvisible then
      let g1 := ticks.map fun t => (dimpoint d t f1 (comp mi d2), dimpoint d t f1 (comp ma d2))
      let g2 := ticks.map fun t => (dimpoint d t (comp mi d1) f2, dimpoint d t (comp ma d1) f2)
      if let some o := segs3 g1 st.gridcolor st.gridwidth then backOps := backOps.push (0, o)
      if let some o := segs3 g2 st.gridcolor st.gridwidth then backOps := backOps.push (0, o)
    if st.spinesvisible then
      let m1 := min1 != rev1
      let m2 := min2 != rev2
      let sel (isMin : Bool) (k : Nat) : Float := if isMin then comp mi k else comp ma k
      let frame := #[
        (dimpoint d (comp mi d) (sel (!m1) d1) (sel m2 d2), dimpoint d (comp ma d) (sel (!m1) d1) (sel m2 d2)),
        (dimpoint d (comp mi d) (sel m1 d1) (sel m2 d2), dimpoint d (comp ma d) (sel m1 d1) (sel m2 d2)),
        (dimpoint d (comp mi d) (sel m1 d1) (sel (!m2) d2), dimpoint d (comp ma d) (sel m1 d1) (sel (!m2) d2))]
      if let some o := segs3 frame st.spinecolor st.spinewidth then backOps := backOps.push (0, o)
  -- ticks, tick labels and axis labels per dimension
  for d in [0:3] do
    let st := dstyle d
    let dd := ax.dimDecor p cam figH d
    if st.ticksvisible then
      if let some o := Axis2.segOp (dd.ticks.map fun (a, b, c, e) => (a, figH - b, c, figH - e)) st.tickcolor st.tickwidth then
        ticksOps := ticksOps.push (0, o)
    if st.ticklabelsvisible then
      let style : TextStyle := { size := st.ticklabelsize, color := st.ticklabelcolor
                                 halign := dd.tickAlign.1, valign := dd.tickAlign.2 }
      let t := dticks d
      for i in [0:min dd.tickLabels.size t.labels.size] do
        let (x, y) := dd.tickLabels[i]!
        tickLabelOps := tickLabelOps.push (0, FigText.labelOp style t.labels[i]! x (figH - y))
    if st.labelvisible then
      if let some ((x, y), rot, va) := dd.label then
        let style : TextStyle := { size := st.labelsize, color := st.labelcolor, halign := .center, valign := va, rotation := rot }
        labelOps := labelOps.push (0, .text x (figH - y) (dlabel d) style none)
  -- title
  if !FigText.isBlank ax.title then
    let style : TextStyle := { size := ax.style.titlesize, color := ax.style.titlecolor, bold := true
                               halign := if ax.style.titlealign == 0 then .left else if ax.style.titlealign == 1 then .right else .center
                               valign := .bottom }
    let (x, y) := ax.titleAnchor box
    labelOps := labelOps.push (0, .text x (figH - y) ax.title style none)
  -- plots
  let pr : Lower.Projector := { project := cam.project, clip := some devVp, depthSort := true }
  let mut plotOps : Array (Float × DrawOp) := #[]
  for it in ax.items do
    let ops := match it.mark with
      | .mesh m => if m.shading then Lower.mesh pr m (some (shadeColors cam m)) else Lower.mesh pr m
      | other => Lower.mark pr other
    for op in ops do plotOps := plotOps.push (0, op)
  return ticksOps ++ backOps ++ plotOps ++ labelOps ++ tickLabelOps

end Axis3

end LeanPlot
