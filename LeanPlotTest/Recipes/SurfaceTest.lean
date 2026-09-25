import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Surface

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Surface` against `surface.json`:
`surface2mesh` vertices, triangles and normals, `mesh(vertices, faces)` normals,
`wireframe` segments (all bit-exact), and CairoMakie's shading formula, corner
normals and normal transform (within 2e-7: binary32 `fma` and matrix-inverse
rounding).
-/

namespace LeanPlotTest.Recipes.SurfaceTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Points from a JSON list of three coordinate columns. -/
def cols3 (j : J) : Pts3 :=
  let c := j.arrD
  Pts3.ofArrays (floatArr c[0]!) (floatArr c[1]!) (floatArr c[2]!)

/-- Exact equality of point sets. -/
def ptsEq (a b : Pts3) : Bool := faBitEq a.xs b.xs && faBitEq a.ys b.ys && faBitEq a.zs b.zs

/-- Largest coordinate difference. -/
def ptsDiff (a b : Pts3) : Float :=
  LeanPlot.Num.jmax (maxAbsDiff a.xs b.xs) (LeanPlot.Num.jmax (maxAbsDiff a.ys b.ys) (maxAbsDiff a.zs b.zs))

/-- A `Vec3` from a JSON array. -/
def vec3 (j : J) : Vec3 := let f := j.floats; ⟨f[0]!, f[1]!, f[2]!⟩

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "surface.json" | return
  for c in (j.get "surfaces").arrD do
    let name := (c.get "name").string
    let nx := (c.get "nx").nat
    let ny := (c.get "ny").nat
    let some g := Grid2.ofFloatArray? nx ny (floatArr (c.get "z"))
      | check s!"{name} grid" false (fun _ => "bad grid size")
    let xs := floatArr (c.get "x")
    let ys := floatArr (c.get "y")
    let sm := Surface.surfaceMesh xs ys g
    check s!"surface {name} positions" (ptsEq sm.mesh.pos (cols3 (c.get "pos")))
      (fun _ => s!"diff {ptsDiff sm.mesh.pos (cols3 (c.get "pos"))}")
    let wantF := (c.get "faces").arrD.foldl (fun acc f => acc ++ f.arrD.map (·.nat.toUInt32)) #[]
    check s!"surface {name} faces" (sm.mesh.tri == wantF) (fun _ => s!"got {sm.mesh.tri.size} want {wantF.size}")
    check s!"surface {name} normals" (ptsEq sm.normals (cols3 (c.get "normals")))
      (fun _ => s!"diff {ptsDiff sm.normals (cols3 (c.get "normals"))}")
    let w := Surface.wireframe xs ys g
    check s!"wireframe {name}" (ptsEq w (cols3 (c.get "wire"))) (fun _ => s!"diff {ptsDiff w (cols3 (c.get "wire"))}")
  -- curvilinear surface
  let cv := j.get "curvilinear"
  if let some g := Grid2.ofFloatArray? (cv.get "nx").nat (cv.get "ny").nat (floatArr (cv.get "z")) then
    let sm := Surface.surfaceMeshCurvilinear (floatArr (cv.get "x")) (floatArr (cv.get "y")) g
    check "curvilinear positions" (ptsEq sm.mesh.pos (cols3 (cv.get "pos")))
    let wantF := (cv.get "faces").arrD.foldl (fun acc f => acc ++ f.arrD.map (·.nat.toUInt32)) #[]
    check "curvilinear faces" (sm.mesh.tri == wantF)
    check "curvilinear normals" (ptsEq sm.normals (cols3 (cv.get "normals")))
      (fun _ => s!"diff {ptsDiff sm.normals (cols3 (cv.get "normals"))}")
    let w := Surface.wireframeGrid (cv.get "nx").nat (cv.get "ny").nat
      (Pts3.ofArrays (floatArr (cv.get "x")) (floatArr (cv.get "y")) (floatArr (cv.get "z")))
    check "curvilinear wireframe" (ptsEq w (cols3 (cv.get "wire"))) (fun _ => s!"diff {ptsDiff w (cols3 (cv.get "wire"))}")
  else check "curvilinear grid" false
  for c in (j.get "meshes").arrD do
    let f32 := (c.get "f32").boolean
    let pos := cols3 (c.get "pos")
    let tri := (c.get "faces").arrD.foldl (fun acc f => acc ++ f.arrD.map (·.nat.toUInt32)) #[]
    match TriMesh.mk? pos tri with
    | none => check "mesh build" false
    | some m =>
      let n := Surface.meshNormals m f32
      check s!"mesh normals f32={f32}" (ptsEq n (cols3 (c.get "normals")))
        (fun _ => s!"diff {ptsDiff n (cols3 (c.get "normals"))}")
  -- lighting
  let l := j.get "lighting"
  let dflt : Surface.Lighting := {}
  check "default ambient" (bitEq dflt.ambient.x (l.get "ambient").floats[0]!)
  check "default light colour" (bitEq dflt.lightColor.x (l.get "color").floats[0]!)
  let raw := vec3 (l.get "rawdirection")
  check "default light direction" (dflt.direction == raw) (fun _ => s!"got {repr dflt.direction} want {repr raw}")
  let view := Mat4.ofColumnMajor (l.get "view").floats
  let fin := Surface.finalLightDirection view raw
  let want := vec3 (l.get "direction")
  let dd := LeanPlot.Num.jmax (fin.x - want.x).abs (LeanPlot.Num.jmax (fin.y - want.y).abs (fin.z - want.z).abs)
  check "final light direction" (dd ≤ 2e-7) (fun _ => s!"got {repr fin} want {repr want}")
  let spec := vec3 (l.get "specular")
  let dif := vec3 (l.get "diffuse")
  let shin := (l.get "shininess").float
  let lt : Surface.Lighting := { direction := want, specular := spec, diffuse := dif, shininess := shin }
  let mut worst : Float := 0
  let mut exact := 0
  for s in (l.get "samples").arrD do
    let cc := (s.get "c").floats
    let o := Surface.shade lt (vec3 (s.get "n")) (vec3 (s.get "v")) ⟨cc[0]!, cc[1]!, cc[2]!, cc[3]!⟩
    let wo := (s.get "out").floats
    let got := #[o.r, o.g, o.b, o.a]
    if floatsBitEq got wo then exact := exact + 1
    for k in [0:4] do worst := LeanPlot.Num.jmax worst (got[k]! - wo[k]!).abs
  check "shading" (worst ≤ 2e-7) (fun _ => s!"max diff {worst}")
  check "shading bit-exact" (exact == (l.get "samples").arrD.size) (fun _ => s!"{exact} exact")
  for s in (l.get "corners").arrD do
    let ns := (s.get "ns").arrD.map vec3
    let outs := (s.get "out").arrD.map vec3
    for k in [0:3] do
      let cn := Surface.cornerNormal ns[0]! ns[1]! ns[2]! k
      check s!"corner normal {k}" (cn == outs[k]!) (fun _ => s!"got {repr cn} want {repr outs[k]!}")
    let m := (s.get "m").floats  -- column-major 3×3
    let r0 : Vec3 := ⟨m[0]!, m[3]!, m[6]!⟩
    let r1 : Vec3 := ⟨m[1]!, m[4]!, m[7]!⟩
    let r2 : Vec3 := ⟨m[2]!, m[5]!, m[8]!⟩
    let tns := (s.get "tn").arrD.map vec3
    for k in [0:3] do
      let t := Surface.transformNormal r0 r1 r2 ns[k]!
      let w := tns[k]!
      let d := LeanPlot.Num.jmax (t.x - w.x).abs (LeanPlot.Num.jmax (t.y - w.y).abs (t.z - w.z).abs)
      check s!"normal transform {k}" (d ≤ 2e-7) (fun _ => s!"got {repr t} want {repr w}")

end LeanPlotTest.Recipes.SurfaceTest
