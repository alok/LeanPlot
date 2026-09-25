import LeanPlot.Core.Geometry
import LeanPlot.Core.Range

/-
Plot data as structure-of-arrays with erased size invariants.

* `Pts2`, `Pts3`: coordinates in parallel `FloatArray`s (`xs.size = ys.size`…).
  NaN coordinates are allowed and mean "line break" (Makie convention).
* `Grid2 nx ny`: values on an `nx × ny` grid stored column-major like a Julia
  matrix `z[i, j]` with `i ↔ x`: entry `(i, j)` is `z[i + nx*j]`, and
  `z.size = nx * ny` is a proof field, so `get` needs no bounds check.
* `TriMesh`: vertices (`Pts3`) and a flat index buffer whose entries are all
  checked `< nverts` once, by the smart constructor `TriMesh.mk?`.
* Conversion classes `ToPts2`, `ToPts3`, `ToGrid2` (the Lean counterpart of
  Makie's `convert_arguments`); downstream packages add instances for their own
  types (e.g. Grassmann chains).
-/

namespace LeanPlot

open LeanPlot.Num

/-! ## Point sets -/

/-- 2D points as two parallel coordinate arrays. -/
structure Pts2 where
  xs : FloatArray
  ys : FloatArray
  h : xs.size = ys.size

namespace Pts2

/-- The empty point set. -/
instance : Inhabited Pts2 := ⟨⟨.empty, .empty, rfl⟩⟩

/-- No points. -/
def empty : Pts2 := ⟨.empty, .empty, rfl⟩

/-- Number of points. -/
@[inline] def size (p : Pts2) : Nat := p.xs.size

/-- Pair two arrays of equal length. -/
def ofArrays? (xs ys : FloatArray) : Option Pts2 :=
  if h : xs.size = ys.size then some ⟨xs, ys, h⟩ else none

/-- Pair two arrays, truncating the longer one. -/
def ofArrays (xs ys : FloatArray) : Pts2 :=
  if h : xs.size = ys.size then ⟨xs, ys, h⟩ else
  let n := min xs.size ys.size
  let xs' : FloatArray := ⟨xs.data.extract 0 n⟩
  let ys' : FloatArray := ⟨ys.data.extract 0 n⟩
  if h' : xs'.size = ys'.size then ⟨xs', ys', h'⟩ else empty

/-- Point `i` (in range). -/
@[inline] def get (p : Pts2) (i : Nat) (hi : i < p.size) : Vec2 :=
  ⟨p.xs[i]'hi, p.ys[i]'(p.h ▸ hi)⟩

/-- Point `i`, or `(NaN, NaN)` out of range. -/
def get! (p : Pts2) (i : Nat) : Vec2 :=
  if hi : i < p.size then p.get i hi else ⟨nan, nan⟩

/-- Append a point. -/
def push (p : Pts2) (x y : Float) : Pts2 := ofArrays (p.xs.push x) (p.ys.push y)

/-- Points `(xs[i], f xs[i])`. -/
@[specialize] def ofFunction (f : Float → Float) (xs : FloatArray) : Pts2 :=
  let rec go (i : Nat) (ys : FloatArray) : FloatArray :=
    if h : i < xs.size then go (i + 1) (ys.push (f xs[i])) else ys
  termination_by xs.size - i
  ofArrays xs (go 0 (FloatArray.emptyWithCapacity xs.size))

/-- Sample `f` at Julia's `range(a, b; length = n)` (the x values Julia code
such as `lines(range(a, b, length = n), f)` would use). -/
@[specialize] def sample (f : Float → Float) (a b : Float) (n : Nat) : Pts2 :=
  ofFunction f (Num.range a b n)

/-- Sample a parametric curve `t ↦ (x t, y t)` at `range(t0, t1; length = n)`. -/
@[specialize] def parametric (fx fy : Float → Float) (t0 t1 : Float) (n : Nat) : Pts2 :=
  let ts := Num.range t0 t1 n
  let rec go (i : Nat) (xs ys : FloatArray) : FloatArray × FloatArray :=
    if h : i < ts.size then go (i + 1) (xs.push (fx ts[i])) (ys.push (fy ts[i])) else (xs, ys)
  termination_by ts.size - i
  let (xs, ys) := go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)
  ofArrays xs ys

/-- Apply a map to every point. -/
@[specialize] def map (f : Vec2 → Vec2) (p : Pts2) : Pts2 :=
  let rec go (i : Nat) (xs ys : FloatArray) : FloatArray × FloatArray :=
    if h : i < p.size then
      let q := f (p.get i h)
      go (i + 1) (xs.push q.x) (ys.push q.y)
    else (xs, ys)
  termination_by p.size - i
  let (xs, ys) := go 0 (FloatArray.emptyWithCapacity p.size) (FloatArray.emptyWithCapacity p.size)
  ofArrays xs ys

/-- Bounding box of the finite points. -/
def bounds? (p : Pts2) : Option Rect := Rect.bounds? p.xs p.ys

/-- The points as an array of vectors (allocates; not for hot paths). -/
def toArray (p : Pts2) : Array Vec2 := (Array.range p.size).map p.get!

end Pts2

/-- 3D points as three parallel coordinate arrays. -/
structure Pts3 where
  xs : FloatArray
  ys : FloatArray
  zs : FloatArray
  hxy : xs.size = ys.size
  hxz : xs.size = zs.size

namespace Pts3

/-- The empty point set. -/
instance : Inhabited Pts3 := ⟨⟨.empty, .empty, .empty, rfl, rfl⟩⟩

/-- No points. -/
def empty : Pts3 := ⟨.empty, .empty, .empty, rfl, rfl⟩

/-- Number of points. -/
@[inline] def size (p : Pts3) : Nat := p.xs.size

/-- Combine three arrays of equal length. -/
def ofArrays? (xs ys zs : FloatArray) : Option Pts3 :=
  if h1 : xs.size = ys.size then
    if h2 : xs.size = zs.size then some ⟨xs, ys, zs, h1, h2⟩ else none
  else none

/-- Combine three arrays, truncating to the shortest. -/
def ofArrays (xs ys zs : FloatArray) : Pts3 :=
  match ofArrays? xs ys zs with
  | some p => p
  | none =>
    let n := min xs.size (min ys.size zs.size)
    (ofArrays? ⟨xs.data.extract 0 n⟩ ⟨ys.data.extract 0 n⟩ ⟨zs.data.extract 0 n⟩).getD empty

/-- Point `i` (in range). -/
@[inline] def get (p : Pts3) (i : Nat) (hi : i < p.size) : Vec3 :=
  ⟨p.xs[i]'hi, p.ys[i]'(p.hxy ▸ hi), p.zs[i]'(p.hxz ▸ hi)⟩

/-- Point `i`, or NaNs out of range. -/
def get! (p : Pts3) (i : Nat) : Vec3 :=
  if hi : i < p.size then p.get i hi else ⟨nan, nan, nan⟩

/-- Sample a space curve at `range(t0, t1; length = n)`. -/
@[specialize] def parametric (fx fy fz : Float → Float) (t0 t1 : Float) (n : Nat) : Pts3 :=
  let ts := Num.range t0 t1 n
  let rec go (i : Nat) (xs ys zs : FloatArray) : FloatArray × FloatArray × FloatArray :=
    if h : i < ts.size then go (i + 1) (xs.push (fx ts[i])) (ys.push (fy ts[i])) (zs.push (fz ts[i]))
    else (xs, ys, zs)
  termination_by ts.size - i
  let (xs, ys, zs) := go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)
  ofArrays xs ys zs

/-- Bounding box of the finite points. -/
def bounds? (p : Pts3) : Option Rect3 :=
  match extremaFinite p.xs, extremaFinite p.ys, extremaFinite p.zs with
  | some (x0, x1), some (y0, y1), some (z0, z1) => some (Rect3.ofBounds ⟨x0, y0, z0⟩ ⟨x1, y1, z1⟩)
  | _, _, _ => none

/-- Drop the z coordinate. -/
def xy (p : Pts3) : Pts2 := ⟨p.xs, p.ys, p.hxy⟩

end Pts3

/-! ## Grids -/

/-- Column-major grid indices are in bounds: `i + nx*j < nx*ny`. -/
theorem grid_idx_lt {nx ny i j : Nat} (hi : i < nx) (hj : j < ny) : i + nx * j < nx * ny := by
  have h1 : nx * (j + 1) ≤ nx * ny := Nat.mul_le_mul_left nx hj
  rw [Nat.mul_succ] at h1
  omega

/-- Values on an `nx × ny` grid, column-major (`z[i + nx*j]`, `i ↔ x`). -/
structure Grid2 (nx ny : Nat) where
  z : FloatArray
  h : z.size = nx * ny

namespace Grid2

variable {nx ny : Nat}

/-- A grid filled with `v`. -/
def fill (nx ny : Nat) (v : Float) : Grid2 nx ny :=
  ⟨⟨Array.replicate (nx * ny) v⟩, by simp [FloatArray.size]⟩

/-- The zero grid. -/
instance : Inhabited (Grid2 nx ny) := ⟨fill nx ny 0⟩

/-- Wrap a buffer of the right size. -/
def ofFloatArray? (nx ny : Nat) (z : FloatArray) : Option (Grid2 nx ny) :=
  if h : z.size = nx * ny then some ⟨z, h⟩ else none

/-- Value at `(i, j)`. -/
@[inline] def get (g : Grid2 nx ny) (i j : Nat) (hi : i < nx) (hj : j < ny) : Float :=
  g.z[i + nx * j]'(g.h ▸ grid_idx_lt hi hj)

/-- Value at `(i, j)`, NaN out of range. -/
def get! (g : Grid2 nx ny) (i j : Nat) : Float :=
  if hi : i < nx then if hj : j < ny then g.get i j hi hj else nan else nan

/-- Build from `f i j` (x index first), column-major. -/
@[specialize] def ofFn (nx ny : Nat) (f : Nat → Nat → Float) : Grid2 nx ny :=
  let n := nx * ny
  let rec go (k : Nat) (acc : FloatArray) : FloatArray :=
    if k < n then go (k + 1) (acc.push (f (k % nx) (k / nx))) else acc
  termination_by n - k
  (ofFloatArray? nx ny (go 0 (FloatArray.emptyWithCapacity n))).getD (fill nx ny nan)

/-- Sample `f x y` on the tensor grid `xs × ys`. -/
@[specialize] def sample (f : Float → Float → Float) (xs ys : FloatArray) : Grid2 xs.size ys.size :=
  ofFn xs.size ys.size fun i j => f xs[i]! ys[j]!

/-- Apply `f` to every value. -/
@[specialize] def map (f : Float → Float) (g : Grid2 nx ny) : Grid2 nx ny :=
  let rec go (k : Nat) (acc : FloatArray) : FloatArray :=
    if h : k < g.z.size then go (k + 1) (acc.push (f g.z[k])) else acc
  termination_by g.z.size - k
  (ofFloatArray? nx ny (go 0 (FloatArray.emptyWithCapacity g.z.size))).getD (fill nx ny nan)

/-- Transpose (`z'[j, i] = z[i, j]`). -/
def transpose (g : Grid2 nx ny) : Grid2 ny nx := ofFn ny nx fun j i => g.get! i j

/-- Finite extrema of the values. -/
def extrema? (g : Grid2 nx ny) : Option (Float × Float) := extremaFinite g.z

/-- Row-major copy (`y` outer, top row = largest `j` first), as images want it. -/
def toRowsTopDown (g : Grid2 nx ny) : FloatArray :=
  let rec go (k : Nat) (acc : FloatArray) : FloatArray :=
    if k < nx * ny then
      let row := k / nx
      let i := k % nx
      go (k + 1) (acc.push (g.get! i (ny - 1 - row)))
    else acc
  termination_by nx * ny - k
  go 0 (FloatArray.emptyWithCapacity (nx * ny))

end Grid2

/-- A grid together with its dimensions. -/
structure AnyGrid2 where
  nx : Nat
  ny : Nat
  grid : Grid2 nx ny

/-! ## Triangle meshes -/

/-- A triangle mesh: vertices plus a flat index buffer (3 per triangle) whose
entries are all valid vertex indices. -/
structure TriMesh where
  pos : Pts3
  tri : Array UInt32
  hmod : tri.size % 3 = 0
  valid : ∀ (k : Nat) (hk : k < tri.size), tri[k].toNat < pos.size

namespace TriMesh

/-- Check the indices once and build the mesh. -/
def mk? (pos : Pts3) (tri : Array UInt32) : Option TriMesh :=
  if hmod : tri.size % 3 = 0 then
    if hv : tri.all (fun t => decide (t.toNat < pos.size)) = true then
      some ⟨pos, tri, hmod, fun k hk => by
        have := Array.all_eq_true.mp hv k hk
        simpa using this⟩
    else none
  else none

/-- Number of vertices. -/
@[inline] def numVertices (m : TriMesh) : Nat := m.pos.size

/-- Number of triangles. -/
@[inline] def numTriangles (m : TriMesh) : Nat := m.tri.size / 3

/-- Vertex `k` of the index buffer (a valid vertex index by construction). -/
@[inline] def index (m : TriMesh) (k : Nat) (hk : k < m.tri.size) : Fin m.pos.size :=
  ⟨m.tri[k].toNat, m.valid k hk⟩

/-- The three vertex positions of triangle `t` (NaNs out of range). -/
def triangle (m : TriMesh) (t : Nat) : Vec3 × Vec3 × Vec3 :=
  let v (k : Nat) : Vec3 := if hk : k < m.tri.size then
      let i := m.index k hk
      m.pos.get i.val i.isLt
    else ⟨nan, nan, nan⟩
  (v (3 * t), v (3 * t + 1), v (3 * t + 2))

/-- The index buffer as little-endian `UInt32` bytes (the `DrawOp.triangles` layout). -/
def indexBytes (m : TriMesh) : ByteArray :=
  m.tri.foldl (init := ByteArray.emptyWithCapacity (4 * m.tri.size)) fun acc (t : UInt32) =>
    (((acc.push t.toUInt8).push (t >>> 8).toUInt8).push (t >>> 16).toUInt8).push (t >>> 24).toUInt8

/-- Unit normal of triangle `t` (right-handed winding); zero if degenerate. -/
def faceNormal (m : TriMesh) (t : Nat) : Vec3 :=
  let (a, b, c) := m.triangle t
  ((b.sub a).cross (c.sub a)).normalize

end TriMesh

/-! ## Conversion classes -/

/-- Types that can be plotted as 2D points (Makie `convert_arguments` → `Point2`). -/
class ToPts2 (α : Type) where
  toPts2 : α → Pts2

/-- Types that can be plotted as 3D points. -/
class ToPts3 (α : Type) where
  toPts3 : α → Pts3

/-- Types that can be plotted as grid data (heatmap/contour/surface). -/
class ToGrid2 (α : Type) where
  toGrid2 : α → AnyGrid2

export ToPts2 (toPts2)
export ToPts3 (toPts3)
export ToGrid2 (toGrid2)

/-- Identity. -/
instance : ToPts2 Pts2 := ⟨id⟩
/-- Coordinate arrays (truncated to the shorter). -/
instance : ToPts2 (FloatArray × FloatArray) := ⟨fun (xs, ys) => Pts2.ofArrays xs ys⟩
/-- Coordinate arrays (truncated to the shorter). -/
instance : ToPts2 (Array Float × Array Float) := ⟨fun (xs, ys) => Pts2.ofArrays ⟨xs⟩ ⟨ys⟩⟩
/-- An array of points. -/
instance : ToPts2 (Array Vec2) := ⟨fun ps => Pts2.ofArrays ⟨ps.map (·.x)⟩ ⟨ps.map (·.y)⟩⟩
/-- An array of `(x, y)` pairs. -/
instance : ToPts2 (Array (Float × Float)) := ⟨fun ps => Pts2.ofArrays ⟨ps.map (·.1)⟩ ⟨ps.map (·.2)⟩⟩
/-- A bare array of y values is plotted against `1, 2, …, n` (Makie `lines(ys)`). -/
instance : ToPts2 FloatArray := ⟨fun ys => Pts2.ofArrays ⟨(Array.range ys.size).map fun i => Num.ofInt (i + 1 : Nat)⟩ ys⟩
/-- Projection onto the xy-plane. -/
instance : ToPts2 Pts3 := ⟨Pts3.xy⟩

/-- Identity. -/
instance : ToPts3 Pts3 := ⟨id⟩
/-- Coordinate arrays (truncated to the shortest). -/
instance : ToPts3 (FloatArray × FloatArray × FloatArray) := ⟨fun (xs, ys, zs) => Pts3.ofArrays xs ys zs⟩
/-- An array of points. -/
instance : ToPts3 (Array Vec3) := ⟨fun ps => Pts3.ofArrays ⟨ps.map (·.x)⟩ ⟨ps.map (·.y)⟩ ⟨ps.map (·.z)⟩⟩
/-- An array of `(x, y, z)` triples. -/
instance : ToPts3 (Array (Float × Float × Float)) :=
  ⟨fun ps => Pts3.ofArrays ⟨ps.map (·.1)⟩ ⟨ps.map (·.2.1)⟩ ⟨ps.map (·.2.2)⟩⟩

/-- Identity. -/
instance : ToGrid2 AnyGrid2 := ⟨id⟩
/-- Forget the static dimensions. -/
instance {nx ny : Nat} : ToGrid2 (Grid2 nx ny) := ⟨fun g => ⟨nx, ny, g⟩⟩
/-- A nested array `m[i][j]` is read as the Julia matrix `z[i, j]` (outer index
↔ x); ragged rows are padded with NaN. -/
instance : ToGrid2 (Array (Array Float)) :=
  ⟨fun m =>
    let nx := m.size
    let ny := m.foldl (fun acc r => max acc r.size) 0
    ⟨nx, ny, Grid2.ofFn nx ny fun i j => (m[i]!).getD j nan⟩⟩

end LeanPlot
