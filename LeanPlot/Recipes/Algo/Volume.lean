import LeanPlot.Core.Colormap
import LeanPlot.Core.Geometry
import LeanPlot.Recipes.Algo.Levels

/-!
# Volume ray marching (Makie `volume`)

Makie's `volume(x, y, z, data; algorithm)` is drawn by casting one ray per pixel through the
box `[x₀, x₁] × [y₀, y₁] × [z₀, z₁]` and integrating samples of the data along it. CairoMakie
draws nothing for a volume; this module ports the GPU backend, GLMakie's
`assets/shader/volume.frag`, onto the CPU, with the attribute semantics of Makie 0.24
(`src/basic_plots.jl:320-414`: `absorption` scales `:absorption` and `:indexedabsorption`,
`samples` rays steps):

* texture space: the data fills the unit cube, texel `i` of an axis with `n` samples centred at
  `(i + ½)/n`, sampled with trilinear filtering (`interpolate = true`) or nearest, clamped to the
  edge (OpenGL `GL_LINEAR`/`GL_NEAREST`, `GL_CLAMP_TO_EDGE`);
* each ray enters the cube at `front` (nearest to the eye) and leaves at `back`; `samples` samples
  are taken at `front + i·(back - front)/samples`, `i = 0 … samples - 1`;
* the colormap is a 1-D texture (linear filtering between entries, texel centres at
  `(i + ½)/n`) addressed by `(v - lo)/(hi - lo)` for the colour range `(lo, hi)`;
* `step_size = 1.3 / samples` (the shader's `max_distance / num_samples`).

Algorithms (`RaymarchAlgorithm`):

* `mip`: the colour of the largest sample (`maximum` starts at `0`);
* `absorption`: front-to-back emission–absorption: per sample `opacity = step_size·a·absorption`
  from the colormapped `(rgb, a)`, `T ← T·(1 - opacity)`, stop when `T ≤ 0.01`,
  `L ← L + T·opacity·rgb`; the pixel is `(L, 1 - T)`;
* `indexedAbsorption`: the colormap entry `int(sample) - 1` (no interpolation, transparent out of
  range), `L ← L + T·opacity·rgb` before `T ← T·(1 - opacity)`;
* `iso`: the first sample within `isorange` of `isovalue`, in the colormapped colour of
  `isovalue`, lit (Blinn–Phong with Makie's default light) along the central-difference normal
  of the data (`gennormal`);
* `contour` (Makie's 3-D `contour` of a volume, algorithm 7): every sample with a non-zero
  colormap alpha is lit and composited front to back.

Rays are traced in texture space; lighting takes the light direction and the view direction in
that space. The hot loops are tail-recursive with `Float` accumulators and read `FloatArray`s.
-/

namespace LeanPlot.Recipes.Algo.Volume

open LeanPlot LeanPlot.Num

/-! Float constants of the hot loops, as module-level values (a literal inside a loop is
rebuilt by `Float.ofScientific` at every use; docs/PERF.md). -/

/-- The shader's `T ≤ 0.01` early exit. -/
def tCut : Float := 0.01
/-- `max_distance = 1.3`. -/
def maxDistance : Float := 1.3
/-- `1e-12`, the degenerate-gradient threshold of `gennormal`. -/
def tiny : Float := 1.0e-12
/-- `smooth_zero_max` constants (`a = 8`). -/
def szmC : Float := 0.00390625
/-- `smooth_zero_max` `xswap`. -/
def szmXswap : Float := 0.6406707120152759
/-- `smooth_zero_max` `yswap`. -/
def szmYswap : Float := 0.20508383900190955
/-- `8.0`. -/
def fEight : Float := 8.0

/-- Makie's `RaymarchAlgorithm` (the RGBA-data algorithms `absorptionrgba`/`additive` are not
supported: LeanPlot volumes hold float data). -/
inductive Algorithm where
  | iso
  | absorption
  | mip
  | indexedAbsorption
  | contour
  deriving Repr, Inhabited, BEq, DecidableEq

namespace Algorithm
/-- Parse Makie's symbol (`"iso"`, `"absorption"`, `"mip"`, `"indexedabsorption"`). -/
def ofString? : String → Option Algorithm
  | "iso" => some .iso | "absorption" => some .absorption | "mip" => some .mip
  | "indexedabsorption" => some .indexedAbsorption | "contour" => some .contour
  | _ => none
end Algorithm

/-- Volume samples `values[i + nx·(j + ny·k)]` (Julia's column-major `volume[i+1, j+1, k+1]`). -/
structure Data where
  nx : Nat
  ny : Nat
  nz : Nat
  values : FloatArray
  deriving Inhabited

namespace Data

/-- Build from a function of the 0-based indices. -/
def ofFn (nx ny nz : Nat) (f : Nat → Nat → Nat → Float) : Data :=
  let n := nx * ny * nz
  let rec go (t : Nat) (acc : FloatArray) : FloatArray :=
    if t < n then go (t + 1) (acc.push (f (t % nx) ((t / nx) % ny) (t / (nx * ny)))) else acc
  termination_by n - t
  ⟨nx, ny, nz, go 0 (FloatArray.emptyWithCapacity n)⟩

/-- `floor` as an integer of a finite float of moderate size. -/
@[inline] def floorInt (x : Float) : Int := (Float.floor x).toInt64.toInt

/-- An integral float clamped into `0 … n - 1` (`GL_CLAMP_TO_EDGE`). -/
@[inline] def clampIdx (x : Float) (n : Nat) : Nat :=
  if x ≤ fZero then 0 else
  let m := n - 1
  if x ≥ m.toUInt64.toFloat then m else x.toUInt64.toNat

/-- `texture(volumedata, (u, v, w)).x`: trilinear (`interp`) or nearest sampling with texel
centres at `(i + ½)/n`, clamped to the edge. -/
@[inline] def sample (d : Data) (interp : Bool) (u v w : Float) : Float :=
  let sy := d.nx
  let sz := d.nx * d.ny
  if interp then
    let x := u * d.nx.toUInt64.toFloat - fHalf
    let y := v * d.ny.toUInt64.toFloat - fHalf
    let z := w * d.nz.toUInt64.toFloat - fHalf
    let x0 := Float.floor x
    let y0 := Float.floor y
    let z0 := Float.floor z
    let fx := x - x0
    let fy := y - y0
    let fz := z - z0
    let i0 := clampIdx x0 d.nx
    let i1 := clampIdx (x0 + 1) d.nx
    let r0 := clampIdx y0 d.ny * sy
    let r1 := clampIdx (y0 + 1) d.ny * sy
    let p0 := clampIdx z0 d.nz * sz
    let p1 := clampIdx (z0 + 1) d.nz * sz
    let g (o : Nat) : Float := d.values.get! o
    let l (a b t : Float) : Float := a + (b - a) * t
    let c00 := l (g (i0 + r0 + p0)) (g (i1 + r0 + p0)) fx
    let c10 := l (g (i0 + r1 + p0)) (g (i1 + r1 + p0)) fx
    let c01 := l (g (i0 + r0 + p1)) (g (i1 + r0 + p1)) fx
    let c11 := l (g (i0 + r1 + p1)) (g (i1 + r1 + p1)) fx
    l (l c00 c10 fy) (l c01 c11 fy) fz
  else
    let i := clampIdx (Float.floor (u * d.nx.toUInt64.toFloat)) d.nx
    let j := clampIdx (Float.floor (v * d.ny.toUInt64.toFloat)) d.ny
    let k := clampIdx (Float.floor (w * d.nz.toUInt64.toFloat)) d.nz
    d.values.get! (i + sy * j + sz * k)

/-- Finite extrema of the values (`(0, 1)` without finite values). -/
def extrema (d : Data) : Float × Float := (extremaFinite d.values).getD (0, 1)

end Data

/-- A colormap as a 1-D texture: `4·n` floats `r g b a` per entry. -/
structure Lut where
  n : Nat
  rgba : FloatArray
  deriving Inhabited

namespace Lut

/-- The entries of a colormap with an alpha factor. -/
def ofColormap (cm : Colormap) (alpha : Float := 1) : Lut :=
  let n := cm.size
  ⟨n, (List.range n).foldl (init := FloatArray.emptyWithCapacity (4 * n)) fun acc i =>
    let c := cm.get i
    (((acc.push c.r).push c.g).push c.b).push (c.a * alpha)⟩

/-- A lookup table from explicit colours. -/
def ofColors (cs : Array RGBA) : Lut :=
  ⟨cs.size, cs.foldl (init := FloatArray.emptyWithCapacity (4 * cs.size)) fun acc c =>
    (((acc.push c.r).push c.g).push c.b).push c.a⟩

/-- `texelFetch(colormap, i)`: entry `i`, transparent black out of range. -/
@[inline] def fetch (l : Lut) (i : Int) (c : Nat) : Float :=
  if i < 0 || i ≥ l.n then 0 else l.rgba.get! (4 * i.toNat + c)

/-- The two entries and the weight of `texture(color_ramp, t)` with linear filtering (texel
centres at `(i + ½)/n`, clamped): `(4·i₀, 4·i₁, f)`. -/
@[inline] def taps (l : Lut) (t : Float) : Nat × Nat × Float :=
  let x := t * l.n.toUInt64.toFloat - fHalf
  let x0 := Float.floor x
  (4 * Data.clampIdx x0 l.n, 4 * Data.clampIdx (x0 + 1) l.n, x - x0)

/-- Channel `c` between the taps `o₀`, `o₁` at weight `f`. -/
@[inline] def chan (l : Lut) (o0 o1 : Nat) (f : Float) (c : Nat) : Float :=
  let a := l.rgba.get! (o0 + c)
  a + (l.rgba.get! (o1 + c) - a) * f

/-- `texture(color_ramp, t)` channel `c`. -/
@[inline] def lerpAt (l : Lut) (t : Float) (c : Nat) : Float :=
  let (o0, o1, f) := l.taps t
  l.chan o0 o1 f c

end Lut

/-- The light of the `iso`/`contour` algorithms: Makie's default ambient `0.45`, a directional
light of colour `0.5` along `dir`, `diffuse = 1`, `specular = 0.2`, `shininess = 32` (GLMakie
`lighting.frag`, `FAST_SHADING`, `backlight = 0`). -/
structure Light where
  dir : Vec3 := ⟨-0.4570860418, -0.6285724384, -0.6285724384⟩
  ambient : Float := 0.45
  color : Float := 0.5
  specular : Float := 0.2
  shininess : Float := 32
  deriving Inhabited

/-- GLMakie `smooth_zero_max`: a smoothed `max(x, 0)` for `-1 ≤ x ≤ 1`. -/
@[inline] def smoothZeroMax (x : Float) : Float :=
  if x < szmYswap then szmC * Float.pow (x + (fOne + szmXswap - szmYswap)) fEight else x

/-- GLMakie `illuminate(world_pos, camdir, N, color)` with `FAST_SHADING`: ambient plus
Blinn–Phong (`blinn_phong`), one channel `col` at a time with the shared coefficients. -/
@[inline] def shade (lt : Light) (diff spec col : Float) : Float :=
  lt.ambient * col + lt.color * (diff * col + lt.specular * spec)

/-- The diffuse and specular coefficients of `blinn_phong` for a normal and view direction. -/
@[inline] def coefficients (lt : Light) (n cam : Vec3) : Float × Float :=
  let diff := smoothZeroMax (lt.dir.dot n.neg)
  let h := (lt.dir.add cam).normalize
  let spec := Float.pow (max (h.dot n.neg) 0) lt.shininess
  (diff, if diff ≤ 0 || spec.isNaN then 0 else spec)

/-- Everything a ray needs. -/
structure Ctx where
  data : Data
  interp : Bool
  algorithm : Algorithm
  lut : Lut
  /-- Colour range `(lo, hi)`. -/
  lo : Float
  hi : Float
  absorption : Float
  isovalue : Float
  isorange : Float
  samples : Nat
  light : Light
  deriving Inhabited

/-- `(v - lo)/(hi - lo)`. -/
@[inline] def Ctx.norm (c : Ctx) (v : Float) : Float := (v - c.lo) / (c.hi - c.lo)

/-- The shader's `step_size = max_distance / num_samples`. -/
@[inline] def Ctx.stepSize (c : Ctx) : Float := maxDistance / c.samples.toUInt64.toFloat

/-- GLMakie `gennormal(uvw, d, o)`: the unit gradient direction `a - b` of the data by central
differences `±o` (half a texel), or an outward face normal within `d` of the cube faces. -/
def gennormal (c : Ctx) (u v w d : Float) : Vec3 :=
  if u + d ≥ fOne then ⟨1, 0, 0⟩ else if v + d ≥ fOne then ⟨0, 1, 0⟩ else if w + d ≥ fOne then ⟨0, 0, 1⟩
  else if u - d ≤ fZero then ⟨-1, 0, 0⟩ else if v - d ≤ fZero then ⟨0, -1, 0⟩ else if w - d ≤ fZero then ⟨0, 0, -1⟩
  else
    let ox := fHalf / c.data.nx.toUInt64.toFloat
    let oy := fHalf / c.data.ny.toUInt64.toFloat
    let oz := fHalf / c.data.nz.toUInt64.toFloat
    let s (a b e : Float) : Float := c.data.sample c.interp a b e
    let dx := s (u - ox) v w - s (u + ox) v w
    let dy := s u (v - oy) w - s u (v + oy) w
    let dz := s u v (w - oz) - s u v (w + oz)
    let n := Float.sqrt (dx * dx + dy * dy + dz * dz)
    if n < tiny then ⟨dx, dy, dz⟩ else ⟨dx / n, dy / n, dz / n⟩

/-- `mip`: the running maximum over the samples (tail-recursive). -/
def mipLoop (c : Ctx) (u v w du dv dw : Float) (m : Float) : Nat → Float
  | 0 => m
  | k + 1 =>
    let s := c.data.sample c.interp u v w
    mipLoop c (u + du) (v + dv) (w + dw) du dv dw (if m < s then s else m) k

/-- `absorption`: `(L, T)` after front-to-back compositing. -/
def absorbLoop (c : Ctx) (st : Float) (u v w du dv dw : Float) (lr lg lb t : Float) : Nat → Float × Float × Float × Float
  | 0 => (lr, lg, lb, t)
  | k + 1 =>
    let (o0, o1, f) := c.lut.taps (c.norm (c.data.sample c.interp u v w))
    let op := st * c.lut.chan o0 o1 f 3 * c.absorption
    let t := t * (fOne - op)
    if t ≤ tCut then (lr, lg, lb, t) else
    let q := t * op
    absorbLoop c st (u + du) (v + dv) (w + dw) du dv dw
      (lr + q * c.lut.chan o0 o1 f 0) (lg + q * c.lut.chan o0 o1 f 1) (lb + q * c.lut.chan o0 o1 f 2) t k

/-- `indexedAbsorption`: colormap entries by index `int(sample) - 1`. -/
def indexedLoop (c : Ctx) (st : Float) (u v w du dv dw : Float) (lr lg lb t : Float) : Nat → Float × Float × Float × Float
  | 0 => (lr, lg, lb, t)
  | k + 1 =>
    let s := c.data.sample c.interp u v w
    -- GLSL `int(x)` truncates toward zero
    let idx : Int := (if s.isFinite then s.toInt64.toInt else 0) - 1
    let op := st * c.lut.fetch idx 3 * c.absorption
    let f := t * op
    let lr := lr + f * c.lut.fetch idx 0
    let lg := lg + f * c.lut.fetch idx 1
    let lb := lb + f * c.lut.fetch idx 2
    let t := t * (fOne - op)
    if t ≤ tCut then (lr, lg, lb, t) else
    indexedLoop c st (u + du) (v + dv) (w + dw) du dv dw lr lg lb t k

/-- `iso`: the first sample within `isorange` of `isovalue` (`none` if the ray misses). -/
def isoLoop (c : Ctx) (u v w du dv dw : Float) : Nat → Option Vec3
  | 0 => none
  | k + 1 =>
    let s := c.data.sample c.interp u v w
    if (s - c.isovalue).abs < c.isorange then some ⟨u, v, w⟩
    else isoLoop c (u + du) (v + dv) (w + dw) du dv dw k

/-- `contour`: lit, composited samples of non-zero colormap alpha. -/
def contourLoop (c : Ctx) (cam : Vec3) (st : Float) (u v w du dv dw : Float) (lr lg lb t : Float) :
    Nat → Float × Float × Float × Float
  | 0 => (lr, lg, lb, t)
  | k + 1 =>
    let (o0, o1, f) := c.lut.taps (c.norm (c.data.sample c.interp u v w))
    let op := c.lut.chan o0 o1 f 3
    if op > 0 then
      let n := gennormal c u v w st
      let (dif, spe) := coefficients c.light n cam
      let q := t * op
      let lr := lr + q * shade c.light dif spe (c.lut.chan o0 o1 f 0)
      let lg := lg + q * shade c.light dif spe (c.lut.chan o0 o1 f 1)
      let lb := lb + q * shade c.light dif spe (c.lut.chan o0 o1 f 2)
      let t := t * (fOne - op)
      if t ≤ tCut then (lr, lg, lb, t) else
      contourLoop c cam st (u + du) (v + dv) (w + dw) du dv dw lr lg lb t k
    else contourLoop c cam st (u + du) (v + dv) (w + dw) du dv dw lr lg lb t k

/-- The straight-alpha colour of one ray from `front` to `back` (texture coordinates). -/
def march (c : Ctx) (front back : Vec3) : RGBA :=
  let n := c.samples
  let nf := n.toUInt64.toFloat
  let du := (back.x - front.x) / nf
  let dv := (back.y - front.y) / nf
  let dw := (back.z - front.z) / nf
  let st := c.stepSize
  let clamp01 (x : Float) : Float := if x < 0 then 0 else if x > 1 then 1 else x
  match c.algorithm with
  | .mip =>
    let m := mipLoop c front.x front.y front.z du dv dw fZero n
    let t := c.norm m
    ⟨c.lut.lerpAt t 0, c.lut.lerpAt t 1, c.lut.lerpAt t 2, c.lut.lerpAt t 3⟩
  | .absorption =>
    let (r, g, b, t) := absorbLoop c st front.x front.y front.z du dv dw fZero fZero fZero fOne n
    ⟨clamp01 r, clamp01 g, clamp01 b, clamp01 (1 - t)⟩
  | .indexedAbsorption =>
    let (r, g, b, t) := indexedLoop c st front.x front.y front.z du dv dw fZero fZero fZero fOne n
    ⟨clamp01 r, clamp01 g, clamp01 b, clamp01 (1 - t)⟩
  | .iso =>
    match isoLoop c front.x front.y front.z du dv dw n with
    | none => RGBA.transparent
    | some p =>
      let tn := c.norm c.isovalue
      let nrm := gennormal c p.x p.y p.z st
      let cam := (Vec3.mk du dv dw).normalize
      let (dif, spe) := coefficients c.light nrm cam
      ⟨clamp01 (shade c.light dif spe (c.lut.lerpAt tn 0)), clamp01 (shade c.light dif spe (c.lut.lerpAt tn 1)),
       clamp01 (shade c.light dif spe (c.lut.lerpAt tn 2)), c.lut.lerpAt tn 3⟩
  | .contour =>
    let cam := (Vec3.mk du dv dw).normalize
    let (r, g, b, t) := contourLoop c cam st front.x front.y front.z du dv dw fZero fZero fZero fOne n
    ⟨clamp01 r, clamp01 g, clamp01 b, clamp01 (1 - t)⟩

/-- The ray function of a `VolumeSpec`: `march` with the scene light direction `l`. -/
def rayFn (c : Ctx) (front back l : Vec3) : RGBA :=
  march { c with light := { c.light with dir := l } } front back

/-- The texture-space segment of a ray `o + t·d` (unit-cube coordinates) inside `[0, 1]³` with
`t ≥ 0` (slab test): `(front, back)`, or `none` when it misses. -/
def clipRay (o d : Vec3) : Option (Vec3 × Vec3) :=
  let slab (o d : Float) : Float × Float :=
    if d == 0 then (if o < 0 || o > 1 then (inf, -inf) else (-inf, inf))
    else let a := (0 - o) / d; let b := (1 - o) / d; if a < b then (a, b) else (b, a)
  let (x0, x1) := slab o.x d.x
  let (y0, y1) := slab o.y d.y
  let (z0, z1) := slab o.z d.z
  let t0 := max (max x0 y0) (max z0 0)
  let t1 := min x1 (min y1 z1)
  if t1 > t0 then some (o.add (Vec3.smul t0 d), o.add (Vec3.smul t1 d)) else none

/-! ## Makie's 3-D `contour` of a volume -/

/-- Makie `contour(x, y, z, volume; levels, isorange, alpha, colormap)`
(`basic_recipes/contours.jl:131-205`): the level values, the colour range of the volume
(`colorrange` padded by `2·isorange`) and the colormap that is opaque (`alpha`) only within
`isorange` of a level: `N = clamp(⌈2.5·(max - min)/isorange⌉, 100, 4096)` entries over the padded
range, each the colormap colour of its value over the tight range. Automatic `isorange` is a
tenth of the smallest level gap (of the value range for one level). -/
def contourColormap (d : Data) (levels : Levels.LevelSpec) (cm : Colormap) (alpha : Float := 1)
    (isorange : Option Float := none) (colorrange : Option (Float × Float) := none) :
    FloatArray × Lut × Float × Float :=
  let (vmin, vmax) := d.extrema
  let lv : FloatArray := match levels with
    | .count n =>
      let dz := (vmax - vmin) / (n + 1).toUInt64.toFloat
      ⟨(Array.range n).map fun i => vmin + dz * (i + 1).toUInt64.toFloat⟩
    | .values v => v
  let iso := isorange.getD <|
    if lv.size > 1 then
      0.1 * (List.range (lv.size - 1)).foldl (init := inf) fun m i => min m (lv.get! (i + 1) - lv.get! i)
    else 0.1 * (vmax - vmin)
  let (tlo, thi) := colorrange.getD (vmin, vmax)
  let plo := tlo - 2 * iso
  let phi := thi + 2 * iso
  let clamped := lv.data.filter fun l => tlo ≤ l && l ≤ thi
  let nRaw := Float.ceil (2.5 * (phi - plo) / iso)
  let nE : Nat := if !(nRaw.isFinite) then 100 else max 100 (min 4096 nRaw.toUInt64.toNat)
  let colors := (Array.range nE).map fun i =>
    let isoval := plo + i.toUInt64.toFloat / (nE - 1).toUInt64.toFloat * (phi - plo)
    let c := cm.interpolatedGetIndex (clamp ((isoval - tlo) / (thi - tlo)) 0 1)
    let inClip := tlo - iso ≤ isoval && isoval ≤ thi + iso
    let near := clamped.any fun l => l - iso < isoval && isoval < l + iso
    { c with a := if inClip && near then alpha else 0 }
  (lv, Lut.ofColors colors, plo, phi)

end LeanPlot.Recipes.Algo.Volume
