import Std.Data.HashMap
import LeanPlot.Core.Data
import LeanPlot.Recipes.Algo.Surface
import LeanPlot.Recipes.Algo.Levels

/-!
# Isosurfaces of volume data (marching tetrahedra)

Makie draws `contour(x, y, z, volume)` as a volume rendering with a transfer
function that is opaque only near the levels, and CairoMakie draws nothing for
it. LeanPlot instead extracts one triangle mesh per level (the redesign noted in
the Grassmann plot inventory, §4.6).

Algorithm: every grid cell is split into the six Kuhn tetrahedra around its
main diagonal (a subdivision that is consistent across neighbouring cells, so
the surface is watertight). Each tetrahedron with vertices on both sides of the
level (`v > level` is inside, the Contour.jl convention) contributes one
triangle (one or three vertices inside) or a quad split into two triangles (two
inside). Crossings are interpolated linearly along tetrahedron edges and shared
between neighbouring tetrahedra through an edge-keyed table; triangles are
oriented so their normals point from higher to lower values (along `-∇f`,
outwards for a density blob). Vertex normals are `-∇f` (central differences,
interpolated along the cut edge) or GeometryBasics face normals.
-/

namespace LeanPlot.Recipes.Algo.Isosurface

open LeanPlot.Num
open LeanPlot.Recipes.Algo

/-- A scalar volume on a rectilinear grid: `v[i + nx*(j + ny*k)]` at
`(xs[i], ys[j], zs[k])` (Julia's column-major `Array{T,3}`). -/
structure Volume where
  /-- x coordinates. -/
  xs : FloatArray
  /-- y coordinates. -/
  ys : FloatArray
  /-- z coordinates. -/
  zs : FloatArray
  /-- Values, `v[i + nx*(j + ny*k)]`. -/
  values : FloatArray

namespace Volume

/-- Grid sizes. -/
def nx (v : Volume) : Nat := v.xs.size
/-- Grid sizes. -/
def ny (v : Volume) : Nat := v.ys.size
/-- Grid sizes. -/
def nz (v : Volume) : Nat := v.zs.size

/-- Sample `f` on the grid `xs × ys × zs`. -/
@[specialize] def sample (f : Float → Float → Float → Float) (xs ys zs : FloatArray) : Volume :=
  let nx := xs.size
  let ny := ys.size
  let n := nx * ny * zs.size
  let rec go (k : Nat) (acc : FloatArray) : FloatArray :=
    if k < n then
      let i := k % nx
      let j := (k / nx) % ny
      let l := k / (nx * ny)
      go (k + 1) (acc.push (f (xs.get! i) (ys.get! j) (zs.get! l)))
    else acc
  termination_by n - k
  ⟨xs, ys, zs, go 0 (FloatArray.emptyWithCapacity n)⟩

/-- Central-difference gradient at grid vertex `g` (one-sided at the faces). -/
def grad (v : Volume) (g : Nat) : Vec3 :=
  let nx := v.nx
  let ny := v.ny
  let nz := v.nz
  let i := g % nx
  let j := (g / nx) % ny
  let k := g / (nx * ny)
  let d (lo hi : Nat) (stride : Nat) (coords : FloatArray) (c : Nat) : Float :=
    let a := g - (c - lo) * stride
    let b := g + (hi - c) * stride
    let df := v.values.get! b - v.values.get! a
    let dx := coords.get! hi - coords.get! lo
    if dx == 0 then 0 else df / dx
  ⟨d (i - (if i > 0 then 1 else 0)) (min (i + 1) (nx - 1)) 1 v.xs i,
   d (j - (if j > 0 then 1 else 0)) (min (j + 1) (ny - 1)) nx v.ys j,
   d (k - (if k > 0 then 1 else 0)) (min (k + 1) (nz - 1)) (nx * ny) v.zs k⟩

/-- Position of grid vertex `g`. -/
@[inline] def pos (v : Volume) (g : Nat) : Vec3 :=
  let nx := v.nx
  let ny := v.ny
  ⟨v.xs.get! (g % nx), v.ys.get! ((g / nx) % ny), v.zs.get! (g / (nx * ny))⟩

/-- A plane of the volume (Makie `volumeslices`, which shows the planes at index
1 of each axis by default): `axis = 0` gives the `yz` plane `v[i, :, :]`
(`ny × nz`), `1` the `xz` plane `v[:, j, :]` (`nx × nz`), `2` the `xy` plane
`v[:, :, k]` (`nx × ny`), as a column-major grid with its two coordinate axes. -/
def slice (v : Volume) (axis index : Nat) : AnyGrid2 × FloatArray × FloatArray :=
  let nx := v.nx
  let ny := v.ny
  let nz := v.nz
  let at3 (i j k : Nat) : Float := v.values.get! (i + nx * (j + ny * k))
  if axis == 0 then (⟨ny, nz, Grid2.ofFn ny nz fun j k => at3 index j k⟩, v.ys, v.zs)
  else if axis == 1 then (⟨nx, nz, Grid2.ofFn nx nz fun i k => at3 i index k⟩, v.xs, v.zs)
  else (⟨nx, ny, Grid2.ofFn nx ny fun i j => at3 i j index⟩, v.xs, v.ys)

end Volume

/-- The six Kuhn tetrahedra of a cell, as corner indices (bit 0 = +x, bit 1 = +y,
bit 2 = +z), all sharing the diagonal `0 → 7`. -/
def kuhnTets : Array (Nat × Nat × Nat × Nat) :=
  #[(0, 1, 3, 7), (0, 1, 5, 7), (0, 2, 3, 7), (0, 2, 6, 7), (0, 4, 5, 7), (0, 4, 6, 7)]

/-- Mesh under construction: vertex buffers, triangle indices and the table of
edge crossings already emitted (key `min·N + max` of the grid vertices). -/
structure Builder where
  /-- Vertex x coordinates. -/
  xs : FloatArray
  /-- Vertex y coordinates. -/
  ys : FloatArray
  /-- Vertex z coordinates. -/
  zs : FloatArray
  /-- `-∇f` x components, interpolated along the edge (normalised at the end). -/
  gx : FloatArray
  /-- Normal y components. -/
  gy : FloatArray
  /-- Normal z components. -/
  gz : FloatArray
  /-- Triangle vertex indices. -/
  tri : Array UInt32
  /-- Grid edge key → vertex index. -/
  edges : Std.HashMap UInt64 Nat

/-- The vertex on grid edge `(u, w)` (interpolated from the lower index, so both
neighbours compute the same point). -/
def edgeVertex (vol : Volume) (level : Float) (b : Builder) (u w : Nat) : Builder × Nat :=
  let (a, c) := if u < w then (u, w) else (w, u)
  let key : UInt64 := (a * vol.values.size + c).toUInt64
  match b.edges.get? key with
  | some k => (b, k)
  | none =>
    let fa := vol.values.get! a
    let fc := vol.values.get! c
    let t := (level - fa) / (fc - fa)
    let pa := vol.pos a
    let pc := vol.pos c
    let ga := vol.grad a
    let gc := vol.grad c
    let k := b.xs.size
    ({ b with xs := b.xs.push (pa.x + t * (pc.x - pa.x)), ys := b.ys.push (pa.y + t * (pc.y - pa.y)),
              zs := b.zs.push (pa.z + t * (pc.z - pa.z)),
              gx := b.gx.push (-(ga.x + t * (gc.x - ga.x))), gy := b.gy.push (-(ga.y + t * (gc.y - ga.y))),
              gz := b.gz.push (-(ga.z + t * (gc.z - ga.z))), edges := b.edges.insert key k }, k)

/-- Append triangle `(p, q, r)`, flipped if needed so its normal points along
`out` (from inside to outside). -/
def pushTri (b : Builder) (p q r : Nat) (out : Vec3) : Builder :=
  let vp : Vec3 := ⟨b.xs.get! p, b.ys.get! p, b.zs.get! p⟩
  let vq : Vec3 := ⟨b.xs.get! q, b.ys.get! q, b.zs.get! q⟩
  let vr : Vec3 := ⟨b.xs.get! r, b.ys.get! r, b.zs.get! r⟩
  let n := (vq.sub vp).cross (vr.sub vp)
  if p == q || q == r || p == r then b
  else if n.dot out ≥ 0 then { b with tri := ((b.tri.push p.toUInt32).push q.toUInt32).push r.toUInt32 }
  else { b with tri := ((b.tri.push p.toUInt32).push r.toUInt32).push q.toUInt32 }

/-- Polygonise one tetrahedron with grid vertices `g`. -/
def tetra (vol : Volume) (level : Float) (b : Builder) (g : Array Nat) : Builder :=
  let inside : Array Bool := g.map fun v => decide (vol.values.get! v > level)
  let ins := (Array.range 4).filter (inside[·]!)
  let outs := (Array.range 4).filter (!inside[·]!)
  if ins.size == 0 || ins.size == 4 then b else
  -- direction from an inside vertex to an outside vertex
  let out := (vol.pos g[outs[0]!]!).sub (vol.pos g[ins[0]!]!)
  -- a vertex with a NaN value makes no crossing
  if (g.any fun v => (vol.values.get! v).isNaN) then b else
  if ins.size == 1 || ins.size == 3 then
    let (apex, others) := if ins.size == 1 then (ins[0]!, outs) else (outs[0]!, ins)
    let (b, p) := edgeVertex vol level b g[apex]! g[others[0]!]!
    let (b, q) := edgeVertex vol level b g[apex]! g[others[1]!]!
    let (b, r) := edgeVertex vol level b g[apex]! g[others[2]!]!
    pushTri b p q r out
  else
    let (i0, i1, o0, o1) := (ins[0]!, ins[1]!, outs[0]!, outs[1]!)
    let (b, p) := edgeVertex vol level b g[i0]! g[o0]!
    let (b, q) := edgeVertex vol level b g[i0]! g[o1]!
    let (b, r) := edgeVertex vol level b g[i1]! g[o1]!
    let (b, s) := edgeVertex vol level b g[i1]! g[o0]!
    pushTri (pushTri b p q r out) p r s out

/-- How isosurface vertex normals are computed. -/
inductive NormalMode where
  /-- `-∇f` from central differences, interpolated along the cut edge (smooth). -/
  | gradient
  /-- GeometryBasics area-weighted face normals (faceted on coarse grids). -/
  | faces
  deriving Inhabited, BEq, Repr

/-- A triangle mesh of the isosurface `value = level` with unit vertex normals
(binary32). Empty when the level is not crossed. -/
def extract (vol : Volume) (level : Float) (normals : NormalMode := .gradient) : Surface.NMesh :=
  let nx := vol.nx
  let ny := vol.ny
  let nz := vol.nz
  if nx < 2 || ny < 2 || nz < 2 || vol.values.size != nx * ny * nz then Surface.emptyNMesh else
  let ncell := (nx - 1) * (ny - 1) * (nz - 1)
  let rec go (c : Nat) (b : Builder) : Builder :=
    if c < ncell then
      let i := c % (nx - 1)
      let j := (c / (nx - 1)) % (ny - 1)
      let k := c / ((nx - 1) * (ny - 1))
      let base := i + nx * (j + ny * k)
      let corner (m : Nat) : Nat := base + (m % 2) + nx * ((m / 2) % 2) + nx * ny * (m / 4)
      -- skip cells entirely on one side of the level (most cells)
      let above (m : Nat) : Bool := vol.values.get! (corner m) > level
      let a0 := above 0
      let uniform := (above 1 == a0) && (above 2 == a0) && (above 3 == a0) && (above 4 == a0) &&
        (above 5 == a0) && (above 6 == a0) && (above 7 == a0)
      if uniform then go (c + 1) b else
      let b := kuhnTets.foldl (init := b) fun b (a, e, f, h) =>
        tetra vol level b #[corner a, corner e, corner f, corner h]
      go (c + 1) b
    else b
  termination_by ncell - c
  let b := go 0 ⟨.empty, .empty, .empty, .empty, .empty, .empty, #[], {}⟩
  let pos := Pts3.ofArrays b.xs b.ys b.zs
  match TriMesh.mk? pos b.tri with
  | some m =>
    match normals with
    | .faces => ⟨m, Surface.meshNormals m⟩
    | .gradient =>
      let n := b.gx.size
      let rec go2 (i : Nat) (ox oy oz : FloatArray) : Pts3 :=
        if i < n then
          let w := Surface.normalize3 false ⟨b.gx.get! i, b.gy.get! i, b.gz.get! i⟩
          go2 (i + 1) (ox.push (F32.r32 w.x)) (oy.push (F32.r32 w.y)) (oz.push (F32.r32 w.z))
        else Pts3.ofArrays ox oy oz
      termination_by n - i
      ⟨m, go2 0 .empty .empty .empty⟩
  | none => Surface.emptyNMesh

/-- Makie's automatic levels for a volume contour: `to_levels(n, nan_extrema(volume))`
on the binary32 data. -/
def volumeLevels (vol : Volume) (n : Nat := 5) : FloatArray :=
  let v32 := F32.roundArray vol.values
  match extremaNaN v32 with
  | some (lo, hi) => Levels.contourLevels n lo hi
  | none => .empty

/-- One isosurface per level. -/
def isosurfaces (vol : Volume) (levels : FloatArray) : Array (Float × Surface.NMesh) :=
  levels.toList.toArray.map fun l => (l, extract vol l)

end LeanPlot.Recipes.Algo.Isosurface
