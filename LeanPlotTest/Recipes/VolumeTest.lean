import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Volume
import LeanPlot.Core.Camera

/-!
Tests of `LeanPlot.Recipes.Algo.Volume` against `volume.json`, a Float64 Julia transcription of
GLMakie's ray marcher (`volume_oracle.jl`; CairoMakie draws no volumes): `mip`, `absorption`,
`indexedAbsorption` and lit `iso` colours of 40 rays through a 6×5×4 volume, with and without
interpolation. Plus the ray geometry: the slab clip of rays against the unit cube, and the
camera's inverse (`Mat4.inverse?`, `Camera3.ray` through a projected point).
-/

namespace LeanPlotTest.Recipes.VolumeTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Four channels within `tol`. -/
def rgbaNear (c : RGBA) (w : Array Float) (tol : Float) : Bool :=
  w.size == 4 && (c.r - w[0]!).abs ≤ tol && (c.g - w[1]!).abs ≤ tol && (c.b - w[2]!).abs ≤ tol && (c.a - w[3]!).abs ≤ tol

/-- A `Vec3` from a JSON triple. -/
def vec3 (j : J) : Vec3 := let f := j.floats; ⟨f[0]!, f[1]!, f[2]!⟩

/-- The ray-march suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "volume.json" | return
  let d : Volume.Data := ⟨(j.get "nx").nat, (j.get "ny").nat, (j.get "nz").nat, floatArr (j.get "values")⟩
  let lo := (j.get "lo").float
  let hi := (j.get "hi").float
  let light : Volume.Light := { dir := vec3 (j.get "light") }
  let idx : Array RGBA := #[⟨1, 0, 0, 1⟩, ⟨0, 0, 0, 0⟩, ⟨0, 1, 0, 0.5⟩, ⟨0, 0, 1, 1⟩]
  let base (interp : Bool) : Volume.Ctx :=
    { data := d, interp, algorithm := .mip, lut := .ofColormap Colormap.viridis, lo, hi, absorption := 1
      isovalue := 0.5, isorange := 0.05, samples := 200, light }
  let cases := (j.get "cases").arrD
  let mut k := 0
  for c in cases do
    let f := vec3 (c.get "front")
    let b := vec3 (c.get "back")
    let interp := (c.get "interp").boolean
    let cx := base interp
    let tag := s!"ray {k} interp={interp}"
    let mip := Volume.march cx f b
    check s!"{tag}: mip" (rgbaNear mip (c.get "mip").floats 1e-12) fun _ => s!"{repr mip}"
    let ab := Volume.march { cx with algorithm := .absorption, absorption := 1.5 } f b
    check s!"{tag}: absorption" (rgbaNear ab (c.get "absorption").floats 1e-12) fun _ => s!"{repr ab}"
    let ix := Volume.march { cx with algorithm := .indexedAbsorption, lut := .ofColors idx, absorption := 2 } f b
    check s!"{tag}: indexedabsorption" (rgbaNear ix (c.get "indexed").floats 1e-12) fun _ => s!"{repr ix}"
    let iso := Volume.march { cx with algorithm := .iso, isovalue := 0.8, isorange := 0.1, samples := 150 } f b
    check s!"{tag}: iso" (rgbaNear iso (c.get "iso").floats 1e-12) fun _ => s!"{repr iso}"
    k := k + 1
  check "cases" (k == 40)

/-- Ray geometry: slab clipping and the camera inverse. -/
def geometrySuite : TestM Unit := do
  -- a ray through the cube along +x from outside, and one that misses
  match Volume.clipRay ⟨-1, 0.5, 0.5⟩ ⟨1, 0, 0⟩ with
  | some (f, b) => check "clip: enters at x = 0, leaves at x = 1" ((f.x - 0).abs < 1e-15 && (b.x - 1).abs < 1e-15 && f.y == 0.5)
  | none => check "clip: hit" false
  check "clip: miss" (Volume.clipRay ⟨-1, 2, 0.5⟩ ⟨1, 0, 0⟩).isNone
  check "clip: origin inside starts at the origin" <|
    match Volume.clipRay ⟨0.25, 0.5, 0.5⟩ ⟨0, 0, -1⟩ with
    | some (f, b) => f.z == 0.5 && b.z.abs < 1e-15
    | none => false
  -- the Axis3 camera: the ray through a projected point passes through it
  let cam := Camera3.ofLimits {} ⟨⟨-1, -2, 0⟩, ⟨2, 4, 3⟩⟩ ⟨10, 20, 400, 300⟩
  let m := cam.proj * (cam.view * cam.model)
  match m.inverse? with
  | none => check "inverse exists" false
  | some inv =>
    let p := m * inv
    let id := Mat4.identity
    let err := (Array.range 16).foldl (fun e i => max e ((p.get (i / 4) (i % 4) - id.get (i / 4) (i % 4)).abs)) 0
    check s!"M·M⁻¹ = I (max err {err})" (err < 1e-9)
    for q in #[(⟨0.5, 1, 2⟩ : Vec3), ⟨-1, -2, 0⟩, ⟨1, 2, 3⟩] do
      let s := cam.project q
      let (o, d) := cam.rayWith inv s.x s.y
      -- distance from q to the line o + t d
      let w := q.sub o
      let t := w.dot d / d.dot d
      let dist := (w.sub (Vec3.smul t d)).norm
      check s!"ray through the projection of {repr q} (distance {dist})" (dist < 1e-6 && t > 0 && t < 1)

end LeanPlotTest.Recipes.VolumeTest
