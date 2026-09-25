import LeanPlot.Core.Colormap
import LeanPlot.Core.Scale
import LeanPlot.Recipes.Algo.Surface

/-!
# Voxels (Makie `voxels`, as CairoMakie draws them)

Makie 0.24 `basic_recipes/voxels.jl` + CairoMakie `draw_atomic(::Voxels)`:

* the chunk is mapped to `UInt8` voxel ids: `0` for air (`is_air`, default
  `isnan`), otherwise `trunc(clamp(norm·(scale(x) - min) + 2, 1, 255))` with
  `norm = 252.99998/(max - min)` over the (scaled) value limits, so ids `2…254`
  cover the colour range and `1`/`255` catch values below/above it;
* the id colormap is `resample_cmap(cmap, N)` with `N = 253 + [lowclip automatic]
  + [highclip automatic]`, with explicit `lowclip`/`highclip` colours prepended /
  appended;
* CairoMakie draws one axis-aligned cube per non-air voxel, centred at
  `min + step·(i - ½, j - ½, k - ½)` with size `step - gap`
  (`step = (max - min)/size`), in `k, j, i` loop order.
-/

namespace LeanPlot.Recipes.Algo.Voxels

open LeanPlot.Num
open LeanPlot.Recipes.Algo

/-- Voxel plot attributes (Makie defaults). -/
structure Options where
  /-- Colour range; `none` is automatic (non-air extrema). -/
  colorrange : Option (Float × Float) := none
  /-- Voxels that are not drawn (Makie `is_air`, default `isnan`). -/
  isAir : Float → Bool := Float.isNaN
  /-- Colour scale. -/
  scale : Scale := .identity
  /-- Gap between voxels (subtracted from the voxel size, data units). -/
  gap : Float := 0
  /-- Colour of ids below the range; `none` is automatic (first colormap colour). -/
  lowclip : Option RGBA := none
  /-- Colour of ids above the range; `none` is automatic (last colormap colour). -/
  highclip : Option RGBA := none
  /-- Colormap alpha multiplier. -/
  alpha : Float := 1
  deriving Inhabited

/-- A voxel chunk: `values[i + nx*(j + ny*k)]` with its extent (Makie's
`EndPoints`, binary32). -/
structure Chunk where
  /-- Number of voxels along x. -/
  nx : Nat
  /-- Number of voxels along y. -/
  ny : Nat
  /-- Number of voxels along z. -/
  nz : Nat
  /-- Values, column-major. -/
  values : FloatArray
  /-- Extent `(lo, hi)` along x. -/
  x : Float × Float
  /-- Extent `(lo, hi)` along y. -/
  y : Float × Float
  /-- Extent `(lo, hi)` along z. -/
  z : Float × Float

/-- Makie `voxels(chunk)`: the extent is centred, `±Float32(size/2)` per axis. -/
def Chunk.centered (nx ny nz : Nat) (values : FloatArray) : Chunk :=
  let e (n : Nat) : Float × Float := (F32.r32 (-0.5 * Num.ofInt n), F32.r32 (0.5 * Num.ofInt n))
  ⟨nx, ny, nz, values, e nx, e ny, e nz⟩

/-- Makie `voxels(xs, ys, zs, chunk)` with interval end points (converted to binary32). -/
def Chunk.withExtent (nx ny nz : Nat) (values : FloatArray) (x y z : Float × Float) : Chunk :=
  let r (p : Float × Float) : Float × Float := (F32.r32 p.1, F32.r32 p.2)
  ⟨nx, ny, nz, values, r x, r y, r z⟩

/-- The `value_limits` node: the colour range, or the extrema of the non-air values. -/
def valueLimits (c : Chunk) (o : Options) : Float × Float :=
  match o.colorrange with
  | some r => r
  | none =>
    c.values.toList.foldl (fun (lo, hi) v => if o.isAir v then (lo, hi) else (jmin lo v, jmax hi v)) (inf, -inf)

/-- Julia `eps(x)` for a finite binary64 `x` (the gap to the next float). -/
def epsOf (x : Float) : Float := (nextFloat x.abs) - x.abs

/-- The `chunk_u8` node: voxel ids (`0` air, `1`/`255` below/above the range,
`2…254` inside), column-major. -/
def voxelIds (c : Chunk) (o : Options) : ByteArray :=
  let (lo, hi) := valueLimits c o
  let mini := o.scale.forward lo
  let maxi := jmax (mini + 10 * epsOf mini) (o.scale.forward hi)
  let norm := 252.99998 / (maxi - mini)
  c.values.toList.foldl (init := ByteArray.emptyWithCapacity c.values.size) fun acc x =>
    if o.isAir x then acc.push 0 else
    let lin := norm * (o.scale.forward x - mini)
    let idf := clamp (lin + 2) 1 255
    acc.push idf.toUInt8

/-- The `voxel_colormap` node: colour of id `k` is entry `k - 1`. -/
def voxelColormap (cm : Colormap) (o : Options) : Array RGBA :=
  let n := 253 + (if o.lowclip.isNone then 1 else 0) + (if o.highclip.isNone then 1 else 0)
  let base := (cm.resample n).colors.map fun c => RGBA.toF32 { c with a := c.a * o.alpha }
  let base := match o.lowclip with | some c => #[RGBA.toF32 c] ++ base | none => base
  match o.highclip with | some c => base.push (RGBA.toF32 c) | none => base

/-- Extent width `max - min` as Makie's `data_limits` stores it (a binary32
difference of the binary32 end points). -/
@[inline] def width (e : Float × Float) : Float := F32.r32 (e.2 - e.1)

/-- CairoMakie `voxel_size`: `(max - min)/size - gap` (binary32). -/
def voxelSize (c : Chunk) (o : Options) : Vec3 :=
  ⟨F32.r32 (width c.x / Num.ofInt c.nx - o.gap), F32.r32 (width c.y / Num.ofInt c.ny - o.gap),
   F32.r32 (width c.z / Num.ofInt c.nz - o.gap)⟩

/-- CairoMakie `voxel_positions` and `voxel_colors`: centres (binary32) and
colours of the non-air voxels, in Makie's `k, j, i` order. -/
def voxelCubes (c : Chunk) (o : Options) (cm : Colormap) : Pts3 × Array RGBA := Id.run do
  let ids := voxelIds c o
  let cmap := voxelColormap cm o
  let sx := width c.x / Num.ofInt c.nx
  let sy := width c.y / Num.ofInt c.ny
  let sz := width c.z / Num.ofInt c.nz
  let mut xs : FloatArray := .empty
  let mut ys : FloatArray := .empty
  let mut zs : FloatArray := .empty
  let mut cols : Array RGBA := #[]
  for k in [0:c.nz] do
    for j in [0:c.ny] do
      for i in [0:c.nx] do
        let id := ids.get! (i + c.nx * (j + c.ny * k))
        if id != 0 then
          xs := xs.push (F32.r32 (c.x.1 + sx * (Num.ofInt i + 0.5)))
          ys := ys.push (F32.r32 (c.y.1 + sy * (Num.ofInt j + 0.5)))
          zs := zs.push (F32.r32 (c.z.1 + sz * (Num.ofInt k + 0.5)))
          cols := cols.push (cmap.getD (id.toNat - 1) RGBA.transparent)
  return (Pts3.ofArrays xs ys zs, cols)

/-- The cube marker CairoMakie uses (`normal_mesh(Rect3f(-0.5, 1))` with
expanded face views): 24 vertices (4 per face, with the face normal) and 12
triangles. -/
def cubeMarker : Surface.NMesh := Id.run do
  -- faces: (normal, four corners counter-clockwise seen from outside)
  let faces : Array (Vec3 × Array Vec3) := #[
    (⟨-1, 0, 0⟩, #[⟨-0.5, -0.5, -0.5⟩, ⟨-0.5, -0.5, 0.5⟩, ⟨-0.5, 0.5, 0.5⟩, ⟨-0.5, 0.5, -0.5⟩]),
    (⟨1, 0, 0⟩, #[⟨0.5, -0.5, -0.5⟩, ⟨0.5, 0.5, -0.5⟩, ⟨0.5, 0.5, 0.5⟩, ⟨0.5, -0.5, 0.5⟩]),
    (⟨0, -1, 0⟩, #[⟨-0.5, -0.5, -0.5⟩, ⟨0.5, -0.5, -0.5⟩, ⟨0.5, -0.5, 0.5⟩, ⟨-0.5, -0.5, 0.5⟩]),
    (⟨0, 1, 0⟩, #[⟨-0.5, 0.5, -0.5⟩, ⟨-0.5, 0.5, 0.5⟩, ⟨0.5, 0.5, 0.5⟩, ⟨0.5, 0.5, -0.5⟩]),
    (⟨0, 0, -1⟩, #[⟨-0.5, -0.5, -0.5⟩, ⟨-0.5, 0.5, -0.5⟩, ⟨0.5, 0.5, -0.5⟩, ⟨0.5, -0.5, -0.5⟩]),
    (⟨0, 0, 1⟩, #[⟨-0.5, -0.5, 0.5⟩, ⟨0.5, -0.5, 0.5⟩, ⟨0.5, 0.5, 0.5⟩, ⟨-0.5, 0.5, 0.5⟩])]
  let mut px : Array Vec3 := #[]
  let mut nx : Array Vec3 := #[]
  let mut tri : Array UInt32 := #[]
  for (n, cs) in faces do
    let b := px.size.toUInt32
    px := px ++ cs
    nx := nx ++ #[n, n, n, n]
    tri := tri ++ #[b, b + 1, b + 2, b, b + 2, b + 3]
  match TriMesh.mk? (toPts3 px) tri with
  | some m => return ⟨m, toPts3 nx⟩
  | none => return Surface.emptyNMesh

end LeanPlot.Recipes.Algo.Voxels
