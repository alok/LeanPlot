import LeanPlot.Core.Data
import LeanPlot.Core.Color
import LeanPlot.Recipes.Algo.Streamplot

/-!
# Surface, wireframe and mesh helpers (Makie `surface2mesh`, normals, CairoMakie shading)

* `gridQuads`: GeometryBasics `faces(Rect2, (nx, ny))` quads of a vertex grid.
* `gridMesh` / `surfaceMesh`: Makie `surface2mesh(x, y, z)`: grid points
  (`matrix_grid`, including Makie's 2×2 vertex order), quads containing a `NaN`
  vertex dropped, `nan_aware_normals` (per quad, the cross product of its first
  three vertices, summed per vertex and normalised; binary32 result), and the
  quads split into triangles `(a, b, c), (a, c, d)`.
* `meshNormals`: GeometryBasics `normals(vertices, faces)` for triangle meshes
  (Makie's `mesh(vertices, faces)` conversion).
* `wireframe`: the line segments of Makie's `wireframe(x, y, z)`.
* `Lighting` / `shade` / `shadeFace`: CairoMakie's per-vertex Blinn–Phong
  colours for 3D meshes and surfaces (`_calculate_shaded_vertexcolors`):
  `(ambient + light·max(L·(-N), 0)·diffuse)·c + light·specular·max(H·(-N), 0)^shininess`,
  with Makie's default scene light (camera-relative directional light) and
  material.
-/

namespace LeanPlot.Recipes.Algo.Surface

open LeanPlot.Num
open LeanPlot.Recipes.Algo
open LeanPlot.Recipes.Algo.F32

/-- GeometryBasics `faces(Rect2, (nx, ny))`: the quads `(i,j), (i+1,j), (i+1,j+1),
(i,j+1)` of a column-major vertex grid (0-based vertex indices, `j` outer). A
2×2 grid is the single quad `(0, 1, 2, 3)`. -/
def gridQuads (nx ny : Nat) : Array (Nat × Nat × Nat × Nat) :=
  if nx == 2 && ny == 2 then #[(0, 1, 2, 3)] else
  (Array.range ((ny - 1) * (nx - 1))).map fun k =>
    let i := k % (nx - 1)
    let j := k / (nx - 1)
    let a := i + nx * j
    (a, a + 1, a + 1 + nx, a + nx)

/-- The point has a `NaN` coordinate (Julia `isnan(::Point)`). -/
@[inline] def isNaN3 (v : Vec3) : Bool := v.x.isNaN || v.y.isNaN || v.z.isNaN

/-- Julia `normalize(::StaticVector)` = `inv(norm(v)) * v` (binary64 or binary32). -/
def normalize3 (f32 : Bool) (v : Vec3) : Vec3 :=
  let k := rnd f32 (1 / Stream.jnorm f32 3 v.x v.y v.z)
  ⟨rnd f32 (k * v.x), rnd f32 (k * v.y), rnd f32 (k * v.z)⟩

/-- A triangle mesh with per-vertex normals (binary32, parallel to the vertices;
`NaN` for vertices in no face). -/
structure NMesh where
  mesh : TriMesh
  normals : Pts3

/-- Accumulated (un-normalised) vertex normals, three coordinate buffers. -/
structure NAcc where
  x : FloatArray
  y : FloatArray
  z : FloatArray

/-- Add a normal to vertex `k`. -/
@[inline] def NAcc.add (f32 : Bool) (acc : NAcc) (k : Nat) (nx ny nz : Float) : NAcc :=
  ⟨acc.x.set! k (rnd f32 (acc.x.get! k + nx)), acc.y.set! k (rnd f32 (acc.y.get! k + ny)),
   acc.z.set! k (rnd f32 (acc.z.get! k + nz))⟩

/-- Add the face normal `cross(b - a, c - a)` of vertices `a b c` to the
vertices `a b c` and, for a quad (`d? = some d`), `d`
(`nan_aware_orthogonal_vector`: zero if a vertex has a `NaN`). -/
@[inline] def addFaceNormal (f32 : Bool) (pos : Pts3) (a b c : Nat) (d? : Option Nat) (acc : NAcc) : NAcc :=
  let ax := pos.xs.get! a
  let ay := pos.ys.get! a
  let az := pos.zs.get! a
  let bx := pos.xs.get! b
  let by' := pos.ys.get! b
  let bz := pos.zs.get! b
  let cx := pos.xs.get! c
  let cy := pos.ys.get! c
  let cz := pos.zs.get! c
  if ax.isNaN || ay.isNaN || az.isNaN || bx.isNaN || by'.isNaN || bz.isNaN || cx.isNaN || cy.isNaN || cz.isNaN then acc
  else
    let ux := rnd f32 (bx - ax)
    let uy := rnd f32 (by' - ay)
    let uz := rnd f32 (bz - az)
    let vx := rnd f32 (cx - ax)
    let vy := rnd f32 (cy - ay)
    let vz := rnd f32 (cz - az)
    let nx := rnd f32 (rnd f32 (uy * vz) - rnd f32 (uz * vy))
    let ny := rnd f32 (rnd f32 (uz * vx) - rnd f32 (ux * vz))
    let nz := rnd f32 (rnd f32 (ux * vy) - rnd f32 (uy * vx))
    let acc := ((acc.add f32 a nx ny nz).add f32 b nx ny nz).add f32 c nx ny nz
    match d? with
    | some d => acc.add f32 d nx ny nz
    | none => acc

/-- Normalise accumulated normals and round them to `Vec3f` (`normalize(0)` is `NaN`). -/
def finishNormals (f32 : Bool) (acc : NAcc) : Pts3 :=
  let n := acc.x.size
  let rec go (k : Nat) (ox oy oz : FloatArray) : Pts3 :=
    if k < n then
      let w := normalize3 f32 ⟨acc.x.get! k, acc.y.get! k, acc.z.get! k⟩
      go (k + 1) (ox.push (r32 w.x)) (oy.push (r32 w.y)) (oz.push (r32 w.z))
    else Pts3.ofArrays ox oy oz
  termination_by n - k
  go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)

/-- Zeroed accumulators for `n` vertices. -/
def NAcc.zeros (n : Nat) : NAcc :=
  ⟨⟨Array.replicate n 0⟩, ⟨Array.replicate n 0⟩, ⟨Array.replicate n 0⟩⟩

/-- Makie `nan_aware_normals` over quads (first three vertices of each quad), or
GeometryBasics `normals` over triangles: face cross products summed per vertex,
normalised, rounded to `Vec3f`. `f32` evaluates in binary32 (binary32 vertices). -/
def vertexNormals (f32 : Bool) (pos : Pts3) (faces : Array (Array Nat)) : Pts3 :=
  finishNormals f32 (faces.foldl (init := NAcc.zeros pos.size) fun acc f =>
    addFaceNormal f32 pos f[0]! f[1]! f[2]! f[3]? acc)

/-- Build a triangle mesh (quads split `(a, b, c), (a, c, d)`) from vertices and quads. -/
def quadsToTriMesh (pos : Pts3) (quads : Array (Nat × Nat × Nat × Nat)) : Option TriMesh :=
  let tri : Array UInt32 := quads.foldl (init := #[]) fun acc (a, b, c, d) =>
    ((((((acc.push a.toUInt32).push b.toUInt32).push c.toUInt32).push a.toUInt32).push c.toUInt32).push d.toUInt32)
  TriMesh.mk? pos tri

/-- The empty mesh. -/
def emptyNMesh : NMesh := ⟨⟨Pts3.empty, #[], rfl, fun _ hk => absurd hk (Nat.not_lt_zero _)⟩, Pts3.empty⟩

/-- `arr[k] += v` (in the selected precision). -/
@[inline] def addAt (f32 : Bool) (arr : FloatArray) (k : Nat) (v : Float) : FloatArray :=
  arr.set! k (rnd f32 (arr.get! k + v))

/-- The point `k` has a `NaN` coordinate. -/
@[inline] def nanAt (pos : Pts3) (k : Nat) : Bool :=
  (pos.xs.get! k).isNaN || (pos.ys.get! k).isNaN || (pos.zs.get! k).isNaN

/-- A quad-grid surface from `nx × ny` column-major vertices (Makie
`surface2mesh` after `matrix_grid`): drop quads with a `NaN` vertex, compute
`nan_aware_normals` (binary64 arithmetic unless `f32`), triangulate. -/
def gridMesh (nx ny : Nat) (pos : Pts3) (f32 : Bool := false) : NMesh :=
  if nx < 2 || ny < 2 then emptyNMesh else
  let two := nx == 2 && ny == 2
  let nq := if two then 1 else (nx - 1) * (ny - 1)
  let px := pos.xs
  let py := pos.ys
  let pz := pos.zs
  -- quad with lower-left vertex `a`: `(a, a+1, a+1+nx, a+nx)`; the 2×2 grid is `(0, 1, 2, 3)`.
  -- Accumulators are separate arguments so a step allocates nothing.
  let rec go (k i a : Nat) (tri : Array UInt32) (ax ay az : FloatArray) : Array UInt32 × NAcc :=
    if k < nq then
      let b := if two then 1 else a + 1
      let c := if two then 2 else a + 1 + nx
      let d := if two then 3 else a + nx
      let i' := if i + 2 < nx then i + 1 else 0
      let a' := if i + 2 < nx then a + 1 else a + 2
      if nanAt pos a || nanAt pos b || nanAt pos c || nanAt pos d then go (k + 1) i' a' tri ax ay az
      else
        let tri := (((((tri.push a.toUInt32).push b.toUInt32).push c.toUInt32).push a.toUInt32).push
          c.toUInt32).push d.toUInt32
        -- `nan_aware_normals`: the normal of a quad's first three vertices is added to all four
        let ux := rnd f32 (px.get! b - px.get! a)
        let uy := rnd f32 (py.get! b - py.get! a)
        let uz := rnd f32 (pz.get! b - pz.get! a)
        let vx := rnd f32 (px.get! c - px.get! a)
        let vy := rnd f32 (py.get! c - py.get! a)
        let vz := rnd f32 (pz.get! c - pz.get! a)
        let nx' := rnd f32 (rnd f32 (uy * vz) - rnd f32 (uz * vy))
        let ny' := rnd f32 (rnd f32 (uz * vx) - rnd f32 (ux * vz))
        let nz' := rnd f32 (rnd f32 (ux * vy) - rnd f32 (uy * vx))
        let ax := addAt f32 (addAt f32 (addAt f32 (addAt f32 ax a nx') b nx') c nx') d nx'
        let ay := addAt f32 (addAt f32 (addAt f32 (addAt f32 ay a ny') b ny') c ny') d ny'
        let az := addAt f32 (addAt f32 (addAt f32 (addAt f32 az a nz') b nz') c nz') d nz'
        go (k + 1) i' a' tri ax ay az
    else (tri, ⟨ax, ay, az⟩)
  termination_by nq - k
  let z := NAcc.zeros pos.size
  let (tri, acc) := go 0 0 0 (Array.mkEmpty (6 * nq)) z.x z.y z.z
  match TriMesh.mk? pos tri with
  | some m => ⟨m, finishNormals f32 acc⟩
  | none => emptyNMesh

instance : Inhabited NMesh := ⟨emptyNMesh⟩

/-- Makie `matrix_grid(x, y, z)`: the vertices `(x[i], y[j], z[i, j])` in
column-major order; for a 2×2 grid Makie lists `(x₁,y₁), (x₂,y₁), (x₂,y₂), (x₁,y₂)`
paired with `z[:]` (a Makie quirk, reproduced). -/
def matrixGrid {nx ny : Nat} (xs ys : FloatArray) (g : Grid2 nx ny) : Pts3 :=
  if nx == 2 && ny == 2 then
    Pts3.ofArrays ⟨#[xs.get! 0, xs.get! 1, xs.get! 1, xs.get! 0]⟩ ⟨#[ys.get! 0, ys.get! 0, ys.get! 1, ys.get! 1]⟩ g.z
  else
    let n := nx * ny
    let rec go (k : Nat) (a b c : FloatArray) : Pts3 :=
      if k < n then go (k + 1) (a.push (xs.get! (k % nx))) (b.push (ys.get! (k / nx))) (c.push (g.z.get! k))
      else Pts3.ofArrays a b c
    termination_by n - k
    go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)

/-- A surface mesh with the per-vertex colour values Makie uses by default
(`color = z`, binary32). -/
structure SurfaceMesh where
  mesh : TriMesh
  normals : Pts3
  values : FloatArray

/-- Makie `surface(x, y, z)` as a mesh (`surface2mesh`): `z` is converted to
binary32 (`el32convert`), `x`/`y` stay binary64. -/
def surfaceMesh {nx ny : Nat} (xs ys : FloatArray) (g : Grid2 nx ny) : SurfaceMesh :=
  let g32 := g.map r32
  let nm := gridMesh nx ny (matrixGrid xs ys g32)
  ⟨nm.mesh, nm.normals, g32.z⟩

/-- Makie `surface(x, y, z)` with coordinate matrices `x(i,j)`, `y(i,j)`
(curvilinear grids, `nx × ny` column-major): `z` is converted to binary32. -/
def surfaceMeshCurvilinear {nx ny : Nat} (xg yg : FloatArray) (g : Grid2 nx ny) : SurfaceMesh :=
  let g32 := g.map r32
  let nm := gridMesh nx ny (Pts3.ofArrays xg yg g32.z)
  ⟨nm.mesh, nm.normals, g32.z⟩

/-- GeometryBasics `normals(vertices, faces)` of a triangle mesh (Makie's
`mesh(vertices, faces)` conversion), binary32 result; `f32` for binary32 vertices. -/
def meshNormals (m : TriMesh) (f32 : Bool := false) : Pts3 :=
  vertexNormals f32 m.pos ((Array.range m.numTriangles).map fun t =>
    #[(m.tri[3 * t]!).toNat, (m.tri[3 * t + 1]!).toNat, (m.tri[3 * t + 2]!).toNat])

/-- Line segments along the edges of a grid of `nx × ny` points (column-major):
per quad `(a, b), (b, c), (c, d), (d, a)`, points rounded to `Point3f`
(Makie `wireframe` of a grid, as GeometryBasics decomposes the quads into line
faces). -/
def wireframeGrid (nx ny : Nat) (pos : Pts3) : Pts3 := Id.run do
  let pt (k : Nat) : Vec3 := ⟨r32 (pos.xs.get! k), r32 (pos.ys.get! k), r32 (pos.zs.get! k)⟩
  let mut ax : FloatArray := .empty
  let mut ay : FloatArray := .empty
  let mut az : FloatArray := .empty
  for (a, b, c, d) in gridQuads nx ny do
    for (u, v) in [(a, b), (b, c), (c, d), (d, a)] do
      let p := pt u
      let q := pt v
      ax := (ax.push p.x).push q.x; ay := (ay.push p.y).push q.y; az := (az.push p.z).push q.z
  return Pts3.ofArrays ax ay az

/-- Makie `wireframe(x, y, z)` for axis vectors `x`, `y` (points `(x[i], y[j], z[i, j])`). -/
def wireframe {nx ny : Nat} (xs ys : FloatArray) (g : Grid2 nx ny) : Pts3 :=
  let n := nx * ny
  wireframeGrid nx ny (Pts3.ofArrays ⟨(Array.range n).map fun k => xs.get! (k % nx)⟩
    ⟨(Array.range n).map fun k => ys.get! (k / nx)⟩ g.z)

/-! ## Lighting (CairoMakie) -/

/-- Makie theme `light_direction` (normalised in binary32). -/
def defaultLightDirection : Vec3 :=
  normalize3 true ⟨r32 (-0.45679495), r32 (-0.6293204), r32 (-0.6287243)⟩

/-- Makie's default scene light and material (theme `light_direction`,
`light_color`, `ambient`; `diffuse = 1`, `specular = 0.2`, `shininess = 32`). -/
structure Lighting where
  /-- Ambient light colour (RGB). -/
  ambient : Vec3 := ⟨r32 0.45, r32 0.45, r32 0.45⟩
  /-- Directional light colour (RGB). -/
  lightColor : Vec3 := ⟨0.5, 0.5, 0.5⟩
  /-- Final (world-space) light direction; see `finalLightDirection`. -/
  direction : Vec3 := defaultLightDirection
  /-- Diffuse reflectance per channel. -/
  diffuse : Vec3 := ⟨1, 1, 1⟩
  /-- Specular reflectance per channel. -/
  specular : Vec3 := ⟨r32 0.2, r32 0.2, r32 0.2⟩
  /-- Blinn–Phong exponent. -/
  shininess : Float := 32
  deriving Inhabited, Repr

/-- `dirlight_final_direction`: a camera-relative light direction is rotated by
the inverse view matrix (`inv(view)[1:3, 1:3] * dir`, binary32). -/
def finalLightDirection (view : Mat4) (dir : Vec3) (cameraRelative : Bool := true) : Vec3 :=
  if !cameraRelative then dir else
  -- inverse of the upper-left 3×3 block of an affine view matrix
  let a := view.m00
  let b := view.m01
  let c := view.m02
  let d := view.m10
  let e := view.m11
  let f := view.m12
  let g := view.m20
  let h := view.m21
  let i := view.m22
  let det := a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
  let inv00 := (e * i - f * h) / det
  let inv01 := (c * h - b * i) / det
  let inv02 := (b * f - c * e) / det
  let inv10 := (f * g - d * i) / det
  let inv11 := (a * i - c * g) / det
  let inv12 := (c * d - a * f) / det
  let inv20 := (d * h - e * g) / det
  let inv21 := (b * g - a * h) / det
  let inv22 := (a * e - b * d) / det
  ⟨r32 (inv00 * dir.x + inv01 * dir.y + inv02 * dir.z),
   r32 (inv10 * dir.x + inv11 * dir.y + inv12 * dir.z),
   r32 (inv20 * dir.x + inv21 * dir.y + inv22 * dir.z)⟩

/-- CairoMakie `_calculate_shaded_vertexcolors(N, v, c, …)`: the lit colour of a
vertex with unit normal `n` (binary64), unit camera-to-vertex direction `v`
(binary32) and base colour `c`. -/
def shade (l : Lighting) (n v : Vec3) (c : RGBA) : RGBA :=
  let L := l.direction
  let dot (a b : Vec3) : Float := a.x * b.x + a.y * b.y + a.z * b.z
  let nn : Vec3 := ⟨-n.x, -n.y, -n.z⟩
  let diff := jmax (dot L nn) 0
  let hsum : Vec3 := ⟨r32 (L.x + v.x), r32 (L.y + v.y), r32 (L.z + v.z)⟩
  let hv := normalize3 true hsum
  let spec := powF (jmax (dot hv nn) 0) l.shininess
  let ch (amb lc dif sp col : Float) : Float :=
    r32 ((amb + lc * diff * dif) * col + r32 (lc * sp) * spec)
  { r := ch l.ambient.x l.lightColor.x l.diffuse.x l.specular.x (r32 c.r)
    g := ch l.ambient.y l.lightColor.y l.diffuse.y l.specular.y (r32 c.g)
    b := ch l.ambient.z l.lightColor.z l.diffuse.z l.specular.z (r32 c.b)
    a := r32 c.a }

/-- The shading normal CairoMakie uses for corner `k` of a triangle with binary32
vertex normals `n0 n1 n2`: `normalize(n_k + 1e-20·mean)` (binary64), so that
zero normals still get the face's direction. -/
def cornerNormal (n0 n1 n2 : Vec3) (k : Nat) : Vec3 :=
  let s : Vec3 := ⟨r32 (r32 (n0.x + n1.x) + n2.x), r32 (r32 (n0.y + n1.y) + n2.y), r32 (r32 (n0.z + n1.z) + n2.z)⟩
  let m : Vec3 := ⟨r32 (s.x / 3), r32 (s.y / 3), r32 (s.z / 3)⟩
  let nk := if k == 0 then n0 else if k == 1 then n1 else n2
  normalize3 false ⟨nk.x + 1.0e-20 * m.x, nk.y + 1.0e-20 * m.y, nk.z + 1.0e-20 * m.z⟩

/-- CairoMakie `zero_normalize(M * n)` of a binary32 normal under a 3×3 normal
matrix (rows `r0 r1 r2`, binary32). -/
def transformNormal (r0 r1 r2 : Vec3) (n : Vec3) : Vec3 :=
  let row (r : Vec3) : Float := r32 (Float.fma r.z n.z (r32 (Float.fma r.y n.y (r32 (r.x * n.x)))))
  let w : Vec3 := ⟨row r0, row r1, row r2⟩
  let d := r32 (Stream.jnorm true 3 w.x w.y w.z + Float.ofBits 0x36A0000000000000)
  ⟨r32 (w.x / d), r32 (w.y / d), r32 (w.z / d)⟩

end LeanPlot.Recipes.Algo.Surface
