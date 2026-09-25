import LeanPlot.Core.Geometry

/-
The `Axis3` camera, ported from Makie 0.24 (`makielayout/blocks/axis3d.jl`,
`calculate_matrices` and `projectionmatrix`; `camera/projection_math.jl`).

Given the final 3D limits, the scene viewport size and the view settings
(azimuth, elevation, perspectiveness, aspect, viewmode, protrusions), Makie
builds

* a model matrix that centres the limits box and scales it by `aspect`,
* a `lookat` view matrix from an eye on a sphere of radius
  `axis_radius / sin(fov/2)` (`fov = 0.5° + 89.5°·perspectiveness`, so even
  `perspectiveness = 0` is a very narrow perspective),
* a perspective projection, rescaled for `:fitzoom`/`:stretch`/`:fit` so the
  box fills the viewport minus the protrusions.

`Camera3.project` maps a data point to device pixels (origin top-left, y down,
like `LeanPlot.Scene`) plus the normalised depth used for painter's sorting.
Makie defaults: azimuth `1.275π`, elevation `π/8`, perspectiveness `0`, aspect
`(1, 1, 2/3)`, viewmode `:fitzoom`, protrusions `30`, near `1e-3`.
-/

namespace LeanPlot

open LeanPlot.Num

/-- `Axis3.aspect`. -/
inductive Aspect3 where
  /-- `:equal`: every axis spans the same length. -/
  | equal
  /-- `:data`: lengths proportional to the data widths. -/
  | data
  /-- Explicit relative lengths `(ax, ay, az)`. -/
  | ratio (ax ay az : Float)
  deriving Repr, Inhabited, BEq

/-- `Axis3.viewmode`. -/
inductive ViewMode where
  | fit | fitzoom | stretch | free
  deriving Repr, Inhabited, BEq, DecidableEq

/-- `Axis3.protrusions` (left, right, bottom, top) in pixels. -/
structure Protrusions where
  left : Float := 30
  right : Float := 30
  bottom : Float := 30
  top : Float := 30
  deriving Repr, Inhabited, BEq

/-- The view settings of an `Axis3`, with Makie's defaults. -/
structure Axis3View where
  azimuth : Float := 1.275 * Num.pi
  elevation : Float := Num.pi / 8
  perspectiveness : Float := 0
  aspect : Aspect3 := .ratio 1 1 (2 / 3)
  viewmode : ViewMode := .fitzoom
  protrusions : Protrusions := {}
  near : Float := 1.0e-3
  xreversed : Bool := false
  yreversed : Bool := false
  zreversed : Bool := false
  zoomMult : Float := 1
  /-- `axis_offset` (only used by `:free`). -/
  offset : Vec2 := ⟨0, 0⟩
  deriving Repr, Inhabited

/-- Camera matrices and the device-space viewport of an `Axis3` scene. -/
structure Camera3 where
  model : Mat4
  view : Mat4
  proj : Mat4
  lookat : Vec3
  eyepos : Vec3
  /-- Scene viewport in device pixels (top-left origin, y down). -/
  viewport : Rect
  deriving Repr, Inhabited

namespace Camera3

/-- Makie's far-plane factor `1 + 1e-3`. -/
def farFactor : Float := 1 + 1.0e-3

/-- Makie `projectionmatrix(viewmatrix, limits, radius, fov, width, height, protrusions,
viewmode, near, axis_radius)`. Protrusion arithmetic is done in `Float32`, as in
Makie (protrusions are `RectSides{Float32}`). -/
def projection (viewmatrix : Mat4) (limits : Rect3) (radius fov width height : Float)
    (prot : Protrusions) (mode : ViewMode) (nearLimit axisRadius : Float) : Mat4 :=
  let near := jmax nearLimit (radius - axisRadius)
  let far := jmax (farFactor * near) (radius + axisRadius)
  let aspectRatio := width / height
  let fov := if height > width then fov / aspectRatio else fov
  let pm := Mat4.perspective fov aspectRatio near far
  let f32 (x : Float) : Float32 := x.toFloat32
  let l := f32 prot.left
  let r := f32 prot.right
  let b := f32 prot.bottom
  let t := f32 prot.top
  let wF := f32 width
  let hF := f32 height
  let dx := ((l - r) / wF).toFloat
  let dy := ((b - t) / hF).toFloat
  let w := wF - l - r
  let h := hF - b - t
  match mode with
  | .fitzoom | .stretch =>
    let pv := pm * viewmatrix
    let wEff := (w / wF).toFloat
    let hEff := (h / hF).toFloat
    let pts := limits.corners.map fun p => pv.mulPoint p
    let maxx := pts.foldl (init := fZero) fun acc q => jmax acc (q.x / (wEff * q.w)).abs
    let maxy := pts.foldl (init := fZero) fun acc q => jmax acc (q.y / (hEff * q.w)).abs
    let ratioX := fOne / maxx
    let ratioY := fOne / maxy
    if mode == .fitzoom then
      let s := jmin ratioX ratioY
      Mat4.transformation ⟨dx, dy, 0⟩ ⟨s, s, 1⟩ * pm
    else Mat4.transformation ⟨dx, dy, 0⟩ ⟨ratioX, ratioY, 1⟩ * pm
  | _ =>
    let wh := ((if w < h then w else h) / (if wF < hF then wF else hF)).toFloat
    Mat4.transformation ⟨dx, dy, 0⟩ ⟨wh, wh, 1⟩ * pm

/-- Makie `calculate_matrices(limits, viewport, …)` for an `Axis3` whose scene
viewport is `viewport` (device pixels; only its width and height matter for
the matrices). -/
def ofLimits (v : Axis3View) (limits : Rect3) (viewport : Rect) : Camera3 :=
  let ori := limits.origin
  let ws := limits.widths
  let flip (rev : Bool) (o w : Float) : Float × Float := if rev then (o + w, -w) else (o, w)
  let (ox, wx) := flip v.xreversed ori.x ws.x
  let (oy, wy) := flip v.yreversed ori.y ws.y
  let (oz, wz) := flip v.zreversed ori.z ws.z
  let lim : Rect3 := ⟨⟨ox, oy, oz⟩, ⟨wx, wy, wz⟩⟩
  let maxw := jmax wx (jmax wy wz)
  let (scales, axisRadius) : Vec3 × Float :=
    match v.aspect with
    | .equal => (⟨2 / wx, 2 / wy, 2 / wz⟩, Float.sqrt 3)
    | .data =>
      let s : Vec3 := ⟨2 * sign wx / maxw, 2 * sign wy / maxw, 2 * sign wz / maxw⟩
      (s, (Vec3.mk (wx / maxw) (wy / maxw) (wz / maxw)).norm)
    | .ratio ax ay az =>
      let m := jmax ax (jmax ay az)
      let na : Vec3 := ⟨ax / m, ay / m, az / m⟩
      (⟨2 / wx * na.x, 2 / wy * na.y, 2 / wz * na.z⟩, na.norm)
  let fixScale (s : Float) : Float := if s.abs < floatMin32 then (if s < 0 then -1 else 1) else s
  let scales : Vec3 := ⟨fixScale scales.x, fixScale scales.y, fixScale scales.z⟩
  let model :=
    Mat4.translation ⟨-0.5 * wx * scales.x, -0.5 * wy * scales.y, -0.5 * wz * scales.z⟩ *
      Mat4.scale scales * Mat4.translation ⟨-ox, -oy, -oz⟩
  let fov := 0.5 + (90 - 0.5) * v.perspectiveness
  let radius := v.zoomMult * axisRadius / Float.sin (fov / 2 * Num.pi / 180)
  let camdir : Vec3 :=
    ⟨Float.cos v.elevation * Float.cos v.azimuth, Float.cos v.elevation * Float.sin v.azimuth, Float.sin v.elevation⟩
  let eyepos0 := Vec3.smul radius camdir
  let (lookatPt, eyepos) : Vec3 × Vec3 :=
    if v.viewmode == .free then
      let up : Vec3 := ⟨0, 0, 1⟩
      let ux := (up.cross camdir).normalize
      let uy := camdir.cross ux
      let la := Vec3.smul (v.zoomMult * axisRadius) ((Vec3.smul v.offset.x ux).add (Vec3.smul v.offset.y uy))
      (la, eyepos0.add la)
    else (Vec3.zero, eyepos0)
  let view := Mat4.lookat eyepos lookatPt ⟨0, 0, 1⟩
  let proj := projection (view * model) lim radius fov viewport.w viewport.h v.protrusions v.viewmode v.near axisRadius
  { model, view, proj, lookat := lookatPt, eyepos, viewport }

/-- `proj * view * model`. -/
def projectionView (c : Camera3) : Mat4 := c.proj * c.view * c.model

/-- Clip-space position of a data point. -/
def toClip (c : Camera3) (p : Vec3) : Vec4 := (c.proj * (c.view * c.model)).mulPoint p

/-- Project a data point to device pixels: `x`, `y` (top-left origin, y down)
and `z` = normalised device depth in `[-1, 1]` (smaller is nearer). -/
def project (c : Camera3) (p : Vec3) : Vec3 :=
  let q := c.toClip p
  let nx := q.x / q.w
  let ny := q.y / q.w
  let nz := q.z / q.w
  ⟨c.viewport.x + (nx + 1) / 2 * c.viewport.w, c.viewport.y + (1 - ny) / 2 * c.viewport.h, nz⟩

/-- The data-space ray through device pixel `(px, py)`: the points of normalised depth `-1`
(near plane) and `1` (far plane) unprojected through `(proj·view·model)⁻¹`, as an origin and a
direction pointing away from the eye; `none` for a singular camera. -/
def ray (c : Camera3) (px py : Float) : Option (Vec3 × Vec3) :=
  match (c.proj * (c.view * c.model)).inverse? with
  | none => none
  | some inv =>
    let nx := (px - c.viewport.x) / c.viewport.w * 2 - 1
    let ny := 1 - (py - c.viewport.y) / c.viewport.h * 2
    let a := inv.mulVec ⟨nx, ny, -1, 1⟩
    let b := inv.mulVec ⟨nx, ny, 1, 1⟩
    let p0 : Vec3 := ⟨a.x / a.w, a.y / a.w, a.z / a.w⟩
    let p1 : Vec3 := ⟨b.x / b.w, b.y / b.w, b.z / b.w⟩
    some (p0, p1.sub p0)

/-- `ray` with the inverse matrix precomputed (`inv = (proj·view·model)⁻¹`). -/
@[inline] def rayWith (c : Camera3) (inv : Mat4) (px py : Float) : Vec3 × Vec3 :=
  let nx := (px - c.viewport.x) / c.viewport.w * 2 - 1
  let ny := 1 - (py - c.viewport.y) / c.viewport.h * 2
  let a := inv.mulVec ⟨nx, ny, -1, 1⟩
  let b := inv.mulVec ⟨nx, ny, 1, 1⟩
  let p0 : Vec3 := ⟨a.x / a.w, a.y / a.w, a.z / a.w⟩
  let p1 : Vec3 := ⟨b.x / b.w, b.y / b.w, b.z / b.w⟩
  (p0, p1.sub p0)

/-- Project many points (SoA in, SoA out: device `xs`, `ys` and depths). -/
def projectAll (c : Camera3) (xs ys zs : FloatArray) : FloatArray × FloatArray × FloatArray :=
  let m := c.proj * (c.view * c.model)
  let n := min xs.size (min ys.size zs.size)
  let rec go (i : Nat) (ox oy oz : FloatArray) : FloatArray × FloatArray × FloatArray :=
    if i < n then
      let q := m.mulPoint ⟨xs[i]!, ys[i]!, zs[i]!⟩
      let px := c.viewport.x + (q.x / q.w + 1) / 2 * c.viewport.w
      let py := c.viewport.y + (1 - q.y / q.w) / 2 * c.viewport.h
      go (i + 1) (ox.push px) (oy.push py) (oz.push (q.z / q.w))
    else (ox, oy, oz)
  termination_by n - i
  go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)

end Camera3

end LeanPlot
