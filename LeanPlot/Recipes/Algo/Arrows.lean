import LeanPlot.Core.Data
import LeanPlot.Recipes.Algo.Streamplot

/-!
# Arrow geometry (Makie 0.24 `arrows2d` / `arrows3d`) and Cartan's arrow scaling

* `processArrows`: Makie `_process_arrow_arguments` (data space, binary64): start
  and end points from positions and directions (or end points), `align`
  (`:tail`, `:center`, `:tip` or a fraction), `lengthscale` and `normalize`.
* `metrics2d` / `shapes2d`: the `arrows2d` pipeline in pixel space. Each arrow's
  tail/shaft/tip lengths and widths are scaled so the drawn arrow spans its
  pixel direction exactly (the shaft absorbs the slack within
  `[minshaftlength, maxshaftlength]`); the component shapes (tail heptagon,
  shaft rectangle, tip triangle) are scaled, offset along the arrow, rotated and
  translated like `_apply_arrow_transform`. Output polygons are in Makie pixel
  space (y up), binary32.
* `metrics3d` / `placements3d` / `cylinderMarker` / `coneMarker` /
  `arrows3dMesh`: `arrows3d` places a tail cylinder, a shaft cylinder and a tip
  cone per arrow (`meshscatter` with scales `(2r, 2r, l)` rotated from `+z` to
  the arrow direction); the markers follow GeometryBasics' `Cylinder`/`Cone`
  tessellation with face-view normals.
* `Cartan`: `spacing` (mean sample distance of embedded points) and the
  `scaledarrows` length scale `spacing / mean‖t‖ / 3`.
-/

namespace LeanPlot.Recipes.Algo.Arrows

open LeanPlot.Num
open LeanPlot.Recipes.Algo
open LeanPlot.Recipes.Algo.F32

/-! ## Arguments -/

/-- Which part of the arrow sits at the given position (Makie `align`). -/
inductive Align where
  /-- Tail at the position (`0`). -/
  | tail
  /-- Centre at the position (`0.5`). -/
  | center
  /-- Tip at the position (`1`). -/
  | tip
  /-- A fraction along the arrow. -/
  | frac (a : Float)
  deriving Inhabited, Repr

/-- `_arrow_align_val`. -/
def Align.value : Align → Float
  | .tail => 0.0
  | .center => 0.5
  | .tip => 1.0
  | .frac a => a

/-- Whether the second argument holds directions or end points (Makie `argmode`). -/
inductive ArgMode where
  /-- Directions. -/
  | direction
  /-- End points. -/
  | endpoint
  deriving Inhabited, BEq, Repr

/-- Julia `norm` of a vector of points (`LinearAlgebra.generic_norm2`): the square
root of the sum of squared point norms, rescaled when needed. -/
def arrayNorm (dim : Nat) (p : Pts3) : Float :=
  let n := p.size
  let pn (i : Nat) : Float := let v := p.get! i; Stream.jnorm false dim v.x v.y v.z
  let maxabs := (List.range n).foldl (fun m i => jmax m (pn i)) 0
  if maxabs == 0 || maxabs.isNaN || maxabs == inf then maxabs else
  if (Num.ofInt n * maxabs * maxabs).isFinite && maxabs * maxabs != 0 then
    Float.sqrt ((List.range n).foldl (fun s i => let v := pn i; s + v * v) 0)
  else
    maxabs * Float.sqrt ((List.range n).foldl (fun s i => let v := pn i / maxabs; s + v * v) 0)

/-- Makie `_process_arrow_arguments(pos, pos_or_dir, align, lengthscale, normalize, argmode)`
for `dim`-dimensional points (z ignored when `dim = 2`). Returns start and end
points (binary64). -/
def processArrows (dim : Nat) (pos dir : Pts3) (align : Align := .tail) (lengthscale : Float := 1.0)
    (normalize : Bool := false) (argmode : ArgMode := .direction) : Pts3 × Pts3 :=
  let a := align.value
  let n := min pos.size dir.size
  let get (p : Pts3) (i : Nat) : Vec3 := p.get! i
  let z (v : Float) : Float := if dim == 3 then v else 0
  match argmode with
  | .direction =>
    let rec go (i : Nat) (sx sy sz ex ey ez : FloatArray) : Pts3 × Pts3 :=
      if i < n then
        let p := get pos i
        let d := get dir i
        let d := if normalize then
            let k := 1 / Stream.jnorm false dim d.x d.y (z d.z)
            (⟨k * d.x, k * d.y, k * z d.z⟩ : Vec3)
          else ⟨d.x, d.y, z d.z⟩
        let d : Vec3 := ⟨lengthscale * d.x, lengthscale * d.y, lengthscale * d.z⟩
        let s : Vec3 := ⟨p.x - a * d.x, p.y - a * d.y, z (p.z - a * d.z)⟩
        go (i + 1) (sx.push s.x) (sy.push s.y) (sz.push s.z) (ex.push (s.x + d.x)) (ey.push (s.y + d.y))
          (ez.push (z (s.z + d.z)))
      else (Pts3.ofArrays sx sy sz, Pts3.ofArrays ex ey ez)
    termination_by n - i
    go 0 .empty .empty .empty .empty .empty .empty
  | .endpoint =>
    let dirs := Pts3.ofArrays
      ((Array.range n).foldl (fun acc i => acc.push ((get dir i).x - (get pos i).x)) .empty)
      ((Array.range n).foldl (fun acc i => acc.push ((get dir i).y - (get pos i).y)) .empty)
      ((Array.range n).foldl (fun acc i => acc.push (z ((get dir i).z - (get pos i).z))) .empty)
    let k := if normalize then
        let nrm := arrayNorm dim dirs
        if nrm ≥ 1 / prevFloat inf then some (1 / nrm) else none
      else some 1
    let rec goE (i : Nat) (sx sy sz ex ey ez : FloatArray) : Pts3 × Pts3 :=
      if i < n then
        let p := get pos i
        let d0 := get dirs i
        let o : Vec3 := ⟨p.x + a * d0.x, p.y + a * d0.y, z (p.z + a * d0.z)⟩
        let d := match k with
          | some k => if normalize then (⟨d0.x * k, d0.y * k, d0.z * k⟩ : Vec3) else d0
          | none => d0
        let d : Vec3 := ⟨d.x * lengthscale, d.y * lengthscale, d.z * lengthscale⟩
        let s : Vec3 := ⟨o.x - a * d.x, o.y - a * d.y, z (o.z - a * d.z)⟩
        goE (i + 1) (sx.push s.x) (sy.push s.y) (sz.push s.z) (ex.push (s.x + d.x)) (ey.push (s.y + d.y))
          (ez.push (z (s.z + d.z)))
      else (Pts3.ofArrays sx sy sz, Pts3.ofArrays ex ey ez)
    termination_by n - i
    goE 0 .empty .empty .empty .empty .empty .empty

/-! ## 2D arrows (pixel space) -/

/-- `arrows2d` shape attributes (Makie defaults, pixel units). `shaftlength =
none` is `automatic`. -/
structure Style2D where
  tailwidth : Float := 14
  taillength : Float := 0
  shaftwidth : Float := 3
  shaftlength : Option Float := none
  minshaftlength : Float := 10
  maxshaftlength : Float := inf
  tipwidth : Float := 14
  tiplength : Float := 8
  /-- Width removed from every component and re-added as an outline stroke. -/
  strokemask : Float := 0.75
  /-- The length/width attributes are Julia integers (like Makie's defaults).
  Integer attributes keep parts of the metric arithmetic in binary32 (the
  pixel direction is a `Point2f`); floating attributes promote it to binary64. -/
  integerAttrs : Bool := true
  deriving Inhabited, Repr

/-- Scaled `(taillength, tailwidth, shaftlength, shaftwidth, tiplength, tipwidth)`
of one arrow. -/
structure Metrics where
  taillength : Float
  tailwidth : Float
  shaftlength : Float
  shaftwidth : Float
  tiplength : Float
  tipwidth : Float
  deriving Inhabited, Repr, BEq

/-- The `arrow_metrics` node for a pixel-space direction `(dx, dy)` (binary32):
the shaft takes the length left after tail and tip (clamped), and all lengths
and widths are scaled so the arrow spans the direction exactly. Julia's type
promotion is reproduced: with integer attributes an explicit `shaftlength` (or
a finite `maxshaftlength`) keeps the scaling in binary32. -/
def metrics2d (s : Style2D) (dx dy : Float) : Metrics :=
  let target := Stream.jnorm true 2 dx dy 0
  let (tsl, f32) : Float × Bool := match s.shaftlength with
    | some l => (l, s.integerAttrs)
    | none =>
      let sub := if s.integerAttrs then r32 (r32 (target - s.taillength) - s.tiplength)
                 else target - s.taillength - s.tiplength
      (clamp sub s.minshaftlength s.maxshaftlength, s.integerAttrs && s.maxshaftlength.isFinite)
  let k := rnd f32 (target / rnd f32 (rnd f32 (tsl + s.taillength) + s.tiplength))
  ⟨rnd f32 (k * s.taillength), rnd f32 (k * s.tailwidth), rnd f32 (k * tsl), rnd f32 (k * s.shaftwidth), rnd f32 (k * s.tiplength),
   rnd f32 (k * s.tipwidth)⟩

/-- Pixel-space arrow polygons: vertices of all polygons concatenated
(binary32), with per-polygon offsets, component (`0` tail, `1` shaft, `2` tip)
and arrow index; `z` is the arrow's pixel depth (used for Makie's back-to-front
order, which the polygons already follow). -/
structure Shapes2D where
  xs : FloatArray
  ys : FloatArray
  zs : FloatArray
  /-- `offsets[k] .. offsets[k+1]` are the vertices of polygon `k`. -/
  offsets : Array Nat
  component : Array Nat
  arrow : Array Nat
  deriving Inhabited

/-- Number of polygons. -/
def Shapes2D.count (s : Shapes2D) : Nat := s.component.size

/-- Vertices of polygon `k`. -/
def Shapes2D.polygon (s : Shapes2D) (k : Nat) : Pts2 :=
  let a := s.offsets[k]!
  let b := s.offsets[k + 1]!
  Pts2.ofArrays ⟨s.xs.data.extract a b⟩ ⟨s.ys.data.extract a b⟩

/-- Makie `arrowtail2d(l, W, metrics)` (the default tail shape; `w` = shaft width). -/
def tailShape (l bigW w : Float) : Array (Float × Float) :=
  #[(0, 0), (-0.3 * bigW, -0.5 * bigW), (l - 0.3 * bigW, -0.5 * bigW), (l, 0 - 0.5 * w),
    (l, 0.5 * w), (l - 0.3 * bigW, 0.5 * bigW), (-0.3 * bigW, 0.5 * bigW)]

/-- Unit shaft rectangle `Rect2f(0, -0.5, 1, 1)` in mesh vertex order. -/
def shaftUnit : Array (Float × Float) := #[(0, -0.5), (1, -0.5), (1, 0.5), (0, 0.5)]

/-- Unit tip triangle. -/
def tipUnit : Array (Float × Float) := #[(0, -0.5), (1, 0), (0, 0.5)]

/-- Julia's stable `sortperm` by a key. -/
def sortPerm (keys : FloatArray) : Array Nat :=
  (Array.range keys.size).qsort fun i j =>
    let a := keys.get! i
    let b := keys.get! j
    a < b || (a == b && i < j)

/-- The `meshes` node of `arrows2d`: for pixel-space start points `(sx, sy, sz)`
and directions `(dx, dy)` (binary32, Makie pixel space), the tail, shaft and tip
polygons of every arrow, drawn in increasing start-point depth. -/
def shapes2d (s : Style2D) (sx sy sz dx dy : FloatArray) : Shapes2D := Id.run do
  let n := min sx.size dx.size
  let render : Array Bool := #[s.taillength > 0 && s.tailwidth > 0, s.shaftwidth > 0, s.tiplength > 0 && s.tipwidth > 0]
  let mut out : Shapes2D := ⟨.empty, .empty, .empty, #[0], #[], #[]⟩
  for i in sortPerm ⟨(sz.data.extract 0 n)⟩ do
    let ddx := dx.get! i
    let ddy := dy.get! i
    let m := metrics2d s ddx ddy
    let angle := (Float32.atan2 ddy.toFloat32 ddx.toFloat32)
    let c := (Float32.cos angle).toFloat
    let sn := (Float32.sin angle).toFloat
    let ox := sx.get! i
    let oy := sy.get! i
    let lens := #[m.taillength, m.shaftlength, m.tiplength]
    let wids := #[m.tailwidth, m.shaftwidth, m.tipwidth]
    let mut offset : Float := 0
    for comp in [0:3] do
      if render[comp]! then
        let len := lens[comp]!
        let width := jmax 0 (wids[comp]! - s.strokemask)
        let pts : Array (Float × Float) :=
          if comp == 0 then (tailShape len width m.shaftwidth).map fun (x, y) => (r32 x, r32 y)
          else (if comp == 1 then shaftUnit else tipUnit).map fun (x, y) => (r32 (len * x), r32 (width * y))
        for (px, py) in pts do
          let vx := px + offset
          let vy := py + 0
          -- `R * v` with StaticArrays' muladd chain, then `Point3f` and the origin
          let rx := r32 (Float.fma (-sn) vy (c * vx))
          let ry := r32 (Float.fma c vy (sn * vx))
          out := { out with xs := out.xs.push (r32 (ox + rx)), ys := out.ys.push (r32 (oy + ry)),
                            zs := out.zs.push (sz.get! i) }
        out := { out with offsets := out.offsets.push out.xs.size, component := out.component.push comp,
                          arrow := out.arrow.push i }
        offset := offset + len
  return out

/-! ## 3D arrows -/

/-- `arrows3d` attributes (Makie defaults, relative to `markerscale`). -/
structure Style3D where
  tailradius : Float := 0.15
  taillength : Float := 0
  shaftradius : Float := 0.05
  shaftlength : Option Float := none
  minshaftlength : Float := 0.6
  maxshaftlength : Float := inf
  tipradius : Float := 0.15
  tiplength : Float := 0.4
  /-- `none` is `automatic`: the norm of the data bounding-box widths. -/
  markerscale : Option Float := none
  /-- Vertices per marker ring pair (`quality`). -/
  quality : Nat := 32
  deriving Inhabited, Repr

/-- Makie's automatic `arrowscale`: the norm of the widths of
`update_boundingbox(Rect3d(starts), Rect3d(ends))` (1 if zero) for binary32
world points. Each box's widths are computed in binary32 (the points' type) and
the union is formed from `origin + widths`, as GeometryBasics does. -/
def autoScale3 (starts ends : Pts3) : Float :=
  let ext (a b : FloatArray) : Float :=
    match extremaNaN a, extremaNaN b with
    | some (l1, h1), some (l2, h2) => jmax (l1 + r32 (h1 - l1)) (l2 + r32 (h2 - l2)) - jmin l1 l2
    | _, _ => 0
  let w := Stream.jnorm false 3 (ext starts.xs ends.xs) (ext starts.ys ends.ys) (ext starts.zs ends.zs)
  if w == 0 then 1 else w

/-- The `arrow_metrics` node of `arrows3d` for one binary32 world direction, with
the attribute scale `k` (`markerscale`). Radii are in the width slots. -/
def metrics3d (s : Style3D) (k : Float) (dx dy dz : Float) : Metrics :=
  let tl := k * s.taillength
  let tr := k * s.tailradius
  let mn := k * s.minshaftlength
  let mx := k * s.maxshaftlength
  let sr := k * s.shaftradius
  let tpl := k * s.tiplength
  let tpr := k * s.tipradius
  let constlength := tl + tpl
  let target := Stream.jnorm true 3 dx dy dz
  let tsl := match s.shaftlength with
    | some l => k * l
    | none => clamp (target - constlength) mn mx
  let sc := target / (tsl + constlength)
  ⟨sc * tl, sc * tr, sc * tsl, sc * sr, sc * tpl, sc * tpr⟩

/-- Placement of one `meshscatter` marker: position, scale and unit direction. -/
structure Placement where
  pos : Vec3
  scale : Vec3
  dir : Vec3
  deriving Inhabited, Repr

/-- Per arrow: metrics and the tail, shaft and tip placements (Makie's
`world_startpoints`, `shaft_pos`, `tip_pos` (binary64), `*_scale`, `rot`
(binary32)). Starts and ends are data-space points; Makie projects them to
`Point3f` first. -/
def placements3d (s : Style3D) (starts ends : Pts3) : Array (Metrics × Placement × Placement × Placement) :=
  let s32 := Pts3.ofArrays (roundArray starts.xs) (roundArray starts.ys) (roundArray starts.zs)
  let e32 := Pts3.ofArrays (roundArray ends.xs) (roundArray ends.ys) (roundArray ends.zs)
  let k := match s.markerscale with | some m => m | none => autoScale3 s32 e32
  (Array.range (min s32.size e32.size)).map fun i =>
    let p := s32.get! i
    let q := e32.get! i
    let d : Vec3 := ⟨r32 (q.x - p.x), r32 (q.y - p.y), r32 (q.z - p.z)⟩
    let m := metrics3d s k d.x d.y d.z
    let inv := r32 (1 / Stream.jnorm true 3 d.x d.y d.z)
    let u : Vec3 := ⟨r32 (inv * d.x), r32 (inv * d.y), r32 (inv * d.z)⟩
    let posAt (t : Float) : Vec3 := if t == 0 then p else ⟨p.x + t * u.x, p.y + t * u.y, p.z + t * u.z⟩
    let sc (l r : Float) : Vec3 := ⟨r32 (2 * r), r32 (2 * r), r32 l⟩
    (m, ⟨p, sc m.taillength m.tailwidth, u⟩, ⟨posAt m.taillength, sc m.shaftlength m.shaftwidth, u⟩,
      ⟨posAt (m.taillength + m.shaftlength), sc m.tiplength m.tipwidth, u⟩)

/-! ### Markers and rotations -/

/-- A quaternion `(x, y, z, w)` (Makie `Quaternion` data order). -/
structure Quat where
  x : Float
  y : Float
  z : Float
  w : Float
  deriving Inhabited, Repr

/-- Makie `rotation_between(u, v)` for `u = (0, 0, 1)` (binary64), as used by
`to_rotation(::Vec3)`. -/
def rotationFromZ (v : Vec3) : Quat :=
  let kcos := v.z
  let k := Float.sqrt (1 * (v.norm * v.norm))
  if isApprox (kcos / k) (-1) (Float.sqrt eps64) then
    -- 180° about Makie's `orthogonal((0,0,1)) = (0,0,1) × (0,1,0) = (-1, 0, 0)`
    ⟨-1, 0, 0, 0⟩
  else
    -- cross((0,0,1), v) = (-v.y, v.x, 0)
    let q : Quat := ⟨-v.y, v.x, 0, kcos + k⟩
    let n := Float.sqrt (q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w)
    ⟨q.x / n, q.y / n, q.z / n, q.w / n⟩

/-- Rotate a vector by a quaternion through its rotation matrix (Makie
`Mat3(q)` / `rotationmatrix4(q)`, as CairoMakie's `meshscatter` uses it). -/
def Quat.rotate (q : Quat) (v : Vec3) : Vec3 :=
  let sx := 2 * q.w * q.x
  let sy := 2 * q.w * q.y
  let sz := 2 * q.w * q.z
  let xx := 2 * (q.x * q.x)
  let xy := 2 * q.x * q.y
  let xz := 2 * q.x * q.z
  let yy := 2 * (q.y * q.y)
  let yz := 2 * q.y * q.z
  let zz := 2 * (q.z * q.z)
  ⟨(1 - (yy + zz)) * v.x + (xy - sz) * v.y + (xz + sy) * v.z,
   (xy + sz) * v.x + (1 - (xx + zz)) * v.y + (yz - sx) * v.z,
   (xz - sy) * v.x + (yz + sx) * v.y + (1 - (xx + yy)) * v.z⟩

/-- The empty triangle mesh. -/
def emptyMesh : TriMesh := ⟨Pts3.empty, #[], rfl, fun _ hk => absurd hk (Nat.not_lt_zero _)⟩

/-- A mesh with per-vertex normals (`normals` parallel to `mesh.pos`). -/
structure NormalMesh where
  mesh : TriMesh
  normals : Pts3

instance : Inhabited NormalMesh := ⟨⟨emptyMesh, Pts3.empty⟩⟩

/-- GeometryBasics `rotation(d)` for `d = (0, 0, 1)` applied to `(x, y, 0)`:
the basis `v = (0, -1, 0)`, `w = (1, 0, 0)`, `u = d` maps it to `(y, -x, 0)`. -/
@[inline] private def ringPoint (r phi : Float) : Float × Float :=
  let x := r32 (r * Float.cos phi)
  let y := r32 (r * Float.sin phi)
  (y, -x)

/-- Build a mesh from GeometryBasics face views: triangles as pairs of
(position index, normal index) per corner; vertices are the distinct pairs in
order of first use. -/
private def expandFaceViews (pos : Array Vec3) (nrm : Array Vec3) (tris : Array ((Nat × Nat) × (Nat × Nat) × (Nat × Nat))) :
    NormalMesh := Id.run do
  let mut keys : Array (Nat × Nat) := #[]
  let mut idx : Array UInt32 := #[]
  for (a, b, c) in tris do
    for k in [a, b, c] do
      match keys.findIdx? (· == k) with
      | some j => idx := idx.push j.toUInt32
      | none => keys := keys.push k; idx := idx.push (keys.size - 1).toUInt32
  let px := keys.map fun (p, _) => pos[p]!
  let nx := keys.map fun (_, n) => nrm[n]!
  let pts : Pts3 := toPts3 px
  match TriMesh.mk? pts idx with
  | some m => return ⟨m, toPts3 nx⟩
  | none => return ⟨emptyMesh, Pts3.empty⟩

/-- GeometryBasics `Tessellation(Cylinder((0,0,0), (0,0,1), 0.5), quality)` with
its face-view normals: bottom disk, quad mantle (split into two triangles),
top disk. -/
def cylinderMarker (quality : Nat := 32) : NormalMesh :=
  let nv := quality + quality % 2
  let nh := nv / 2
  let step := 2 * Num.pi / Num.ofInt nh
  let ring (z : Float) : Array Vec3 := (Array.range nh).map fun (i : Nat) =>
    let (x, y) := ringPoint 0.5 (Num.ofInt i * step); ⟨x, y, z⟩
  let pos := ring 0 ++ ring 1 ++ #[⟨0, 0, 0⟩, ⟨0, 0, 1⟩]
  let nrm : Array Vec3 := (Array.range nh).map (fun (i : Nat) =>
      let phi := Num.ofInt i * step
      (⟨r32 (Float.sin phi), r32 (-(Float.cos phi)), 0⟩ : Vec3)) ++ #[⟨0, 0, -1⟩, ⟨0, 0, 1⟩]
  let m1 (i : Nat) : Nat := (i + 1) % nh
  -- faces (0-based): disk1 (nv, m1 i, i), mantle quads (i, m1 i, m1 i + nh, i + nh), disk2 (nv+1, i+nh, m1 i+nh)
  let disk1 := (Array.range nh).map fun (i : Nat) => ((nv, nh), (m1 i, nh), (i, nh))
  let mantle := (Array.range nh).foldl (init := #[]) fun acc i =>
    let a := (i, i)
    let b := (m1 i, m1 i)
    let c := (m1 i + nh, m1 i)
    let d := (i + nh, i)
    (acc.push (a, b, c)).push (a, c, d)
  let disk2 := (Array.range nh).map fun (i : Nat) => ((nv + 1, nh + 1), (i + nh, nh + 1), (m1 i + nh, nh + 1))
  expandFaceViews pos nrm (disk1 ++ mantle ++ disk2)

/-- GeometryBasics `Tessellation(Cone((0,0,0), (0,0,1), 0.5), quality)` with its
face-view normals: shell triangles to the tip (tip normal `0`) and a base cap. -/
def coneMarker (quality : Nat := 32) : NormalMesh :=
  let nv := quality + quality % 2
  let nh := nv / 2
  let step := 2 * Num.pi / Num.ofInt nh
  let ring : Array Vec3 := (Array.range nh).map fun (i : Nat) =>
    let (x, y) := ringPoint 0.5 (Num.ofInt i * step); ⟨x, y, 0⟩
  let pos := ring ++ #[⟨0, 0, 1⟩, ⟨0, 0, 0⟩]
  let zc := r32 (0.5 / 1.0)
  let nn := 1.0 / r32 (Float.sqrt (r32 (1 + zc * zc)))
  let nrm : Array Vec3 := (Array.range nh).map (fun (i : Nat) =>
      let phi := Num.ofInt i * step
      let a := r32 (nn * r32 (Float.cos phi))
      let b := r32 (nn * r32 (Float.sin phi))
      let c := r32 (nn * zc)
      (⟨b, -a, c⟩ : Vec3)) ++ #[⟨0, 0, 0⟩, ⟨0, 0, -1⟩]
  let m1 (i : Nat) : Nat := (i + 1) % nh
  let shell := (Array.range nh).map fun (i : Nat) => ((i, i), (m1 i, m1 i), (nh, nh))
  let cap := (Array.range nh).map fun (i : Nat) => ((i, nh + 1), (m1 i, nh + 1), (nh + 1, nh + 1))
  expandFaceViews pos nrm (shell ++ cap)

/-- World-space vertices of a marker placed like CairoMakie's `meshscatter`:
`pos + R(q)·(scale ⊙ v)` with `q = rotationFromZ dir`. -/
def placeMarker (m : NormalMesh) (pl : Placement) : Pts3 × Pts3 :=
  let q := rotationFromZ pl.dir
  let n := m.mesh.pos.size
  let rec go (i : Nat) (xs ys zs nx ny nz : FloatArray) : Pts3 × Pts3 :=
    if i < n then
      let v := m.mesh.pos.get! i
      let w := q.rotate ⟨pl.scale.x * v.x, pl.scale.y * v.y, pl.scale.z * v.z⟩
      let nv := m.normals.get! i
      -- normals transform with the inverse transpose of rotation·scale
      let nw := (q.rotate ⟨nv.x / pl.scale.x, nv.y / pl.scale.y, nv.z / pl.scale.z⟩).normalize
      go (i + 1) (xs.push (pl.pos.x + w.x)) (ys.push (pl.pos.y + w.y)) (zs.push (pl.pos.z + w.z))
        (nx.push nw.x) (ny.push nw.y) (nz.push nw.z)
    else (Pts3.ofArrays xs ys zs, Pts3.ofArrays nx ny nz)
  termination_by n - i
  go 0 .empty .empty .empty .empty .empty .empty

/-- All arrows of an `arrows3d` plot as one data-space triangle mesh with
normals (tail cylinders when `taillength > 0`, shaft cylinders, tip cones). -/
def arrows3dMesh (s : Style3D) (starts ends : Pts3) : NormalMesh := Id.run do
  let cyl := cylinderMarker s.quality
  let cone := coneMarker s.quality
  let mut xs : FloatArray := .empty
  let mut ys : FloatArray := .empty
  let mut zs : FloatArray := .empty
  let mut nx : FloatArray := .empty
  let mut ny : FloatArray := .empty
  let mut nz : FloatArray := .empty
  let mut tri : Array UInt32 := #[]
  for (_, tail, shaft, tip) in placements3d s starts ends do
    let parts := (if s.taillength > 0 then #[(cyl, tail)] else #[]) ++ #[(cyl, shaft)] ++
      (if s.tiplength > 0 then #[(cone, tip)] else #[])
    for (m, pl) in parts do
      let base := xs.size
      let (p, n) := placeMarker m pl
      xs := appendFloats xs p.xs; ys := appendFloats ys p.ys; zs := appendFloats zs p.zs
      nx := appendFloats nx n.xs; ny := appendFloats ny n.ys; nz := appendFloats nz n.zs
      tri := tri ++ m.mesh.tri.map (· + base.toUInt32)
  match TriMesh.mk? (Pts3.ofArrays xs ys zs) tri with
  | some m => return ⟨m, Pts3.ofArrays nx ny nz⟩
  | none => return ⟨emptyMesh, Pts3.empty⟩

/-! ## Cartan helpers -/

namespace Cartan

/-- Cartan `spacing(x)` of a sampled curve: `Σ‖x_{i+1} - x_i‖/(n - 1)`. -/
def spacing (p : Pts3) : Float :=
  let n := p.size
  if n < 2 then nan else
  let s := (List.range (n - 1)).foldl (fun s i => s + ((p.get! (i + 1)).sub (p.get! i)).norm) 0
  s / Num.ofInt (n - 1)

/-- Cartan `spacing(x)` of an `n₁ × n₂` grid of points (column-major):
`min` over both directions of the mean distance between neighbours. -/
def spacingGrid (n1 n2 : Nat) (p : Pts3) : Float :=
  let pt (i j : Nat) : Vec3 := p.get! (i + n1 * j)
  let meanAlong (di dj : Nat) : Float :=
    let pairs : Array Float := (List.range n2).foldl (init := #[]) fun acc j =>
      (List.range n1).foldl (init := acc) fun acc i =>
        if i + di < n1 && j + dj < n2 then acc.push ((pt (i + di) (j + dj)).sub (pt i j)).norm else acc
    if pairs.isEmpty then inf else pairs.foldl (· + ·) 0 / Num.ofInt pairs.size
  jmin (meanAlong 1 0) (meanAlong 0 1)

/-- Mean vector norm `Σ‖t_i‖/n`. -/
def meanNorm (t : Pts3) : Float :=
  if t.size == 0 then nan else
  (List.range t.size).foldl (fun s i => s + (t.get! i).norm) 0 / Num.ofInt t.size

/-- Cartan `scaledarrows(M, t)`: Makie `lengthscale = s/3` with
`s = spacing / mean‖t‖`. -/
def scaledArrowsLengthscale (spacing : Float) (t : Pts3) : Float := spacing / meanNorm t / 3

/-- Cartan `arrowsbundle(M, t)`: `lengthscale = s/2`. -/
def arrowsBundleLengthscale (spacing : Float) (t : Pts3) : Float := spacing / meanNorm t / 2

/-- Cartan `scaledarrows(M, t::TensorOperator)`: one arrow field per column,
`lengthscale = s/3` with `s = spacing / max(mean column norm)`. -/
def scaledArrowsLengthscaleOp (spacing : Float) (cols : Array Pts3) : Float :=
  spacing / cols.foldl (fun m c => jmax m (meanNorm c)) 0 / 3

/-- Cartan `arrowsbundle`/`planesbundle`/`scaledplanes` for a `TensorOperator`:
`s = spacing / min(mean column norm)`, `lengthscale = s/2`. -/
def bundleLengthscaleOp (spacing : Float) (cols : Array Pts3) : Float :=
  spacing / cols.foldl (fun m c => jmin m (meanNorm c)) inf / 2

end Cartan

end LeanPlot.Recipes.Algo.Arrows
