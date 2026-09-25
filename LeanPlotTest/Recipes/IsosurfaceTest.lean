import Std.Data.HashMap
import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Isosurface

/-!
Property tests for `LeanPlot.Recipes.Algo.Isosurface` (CairoMakie draws no
volume contours, so there is no pixel oracle; the isosurfaces are validated
geometrically): watertight and consistently oriented meshes, the expected Euler
characteristic, vertices on the analytic surface, normals along `-∇f` and area.
-/

namespace LeanPlotTest.Recipes.IsosurfaceTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Undirected and directed edge statistics of a triangle mesh:
`(edges, all undirected edges shared by exactly two triangles, every directed
edge used once)`. -/
def edgeStats (m : TriMesh) : Nat × Bool × Bool := Id.run do
  let mut und : Std.HashMap (Nat × Nat) Nat := {}
  let mut dir : Std.HashMap (Nat × Nat) Nat := {}
  for t in [0:m.numTriangles] do
    let v := #[(m.tri[3 * t]!).toNat, (m.tri[3 * t + 1]!).toNat, (m.tri[3 * t + 2]!).toNat]
    for k in [0:3] do
      let a := v[k]!
      let b := v[(k + 1) % 3]!
      let key := if a < b then (a, b) else (b, a)
      und := und.insert key (und.getD key 0 + 1)
      dir := dir.insert (a, b) (dir.getD (a, b) 0 + 1)
  let closed := und.fold (fun ok _ c => ok && c == 2) true
  let oriented := dir.fold (fun ok _ c => ok && c == 1) true
  return (und.size, closed, oriented)

/-- Euler characteristic `V - E + F`. -/
def euler (m : TriMesh) : Int :=
  let (e, _, _) := edgeStats m
  (m.numVertices : Int) - e + m.numTriangles

/-- Total area and whether every face normal satisfies `outward`. -/
def areaAndOutward (m : TriMesh) (outward : Vec3 → Vec3 → Bool) : Float × Bool := Id.run do
  let mut area : Float := 0
  let mut ok := true
  for t in [0:m.numTriangles] do
    let (a, b, c) := m.triangle t
    let n := (b.sub a).cross (c.sub a)
    area := area + 0.5 * n.norm
    let centroid : Vec3 := ⟨(a.x + b.x + c.x) / 3, (a.y + b.y + c.y) / 3, (a.z + b.z + c.z) / 3⟩
    if !outward n centroid then ok := false
  return (area, ok)

/-- Run the suite. -/
def suite : TestM Unit := do
  let g21 := LeanPlot.Num.range (-1) 1 21
  -- sphere of radius √0.47
  let sph := Isosurface.Volume.sample (fun x y z => x * x + y * y + z * z) g21 g21 g21
  let s := Isosurface.extract sph 0.47
  let (_, closed, oriented) := edgeStats s.mesh
  check "sphere non-empty" (s.mesh.numTriangles > 500) (fun _ => s!"{s.mesh.numTriangles} triangles")
  check "sphere watertight" closed
  check "sphere oriented" oriented
  check "sphere euler = 2" (euler s.mesh == 2) (fun _ => s!"{euler s.mesh}")
  let r := Float.sqrt 0.47
  let maxErr := (List.range s.mesh.numVertices).foldl (fun e i => LeanPlot.Num.jmax e ((s.mesh.pos.get! i).norm - r).abs) 0
  check "sphere vertices on surface" (maxErr < 0.01) (fun _ => s!"max radial error {maxErr}")
  -- normals point towards decreasing values (-∇f): inwards for this distance field
  let (area, inward) := areaAndOutward s.mesh fun n c => n.dot c < 0
  check "sphere normals along -grad" inward
  let exact := 4 * LeanPlot.Num.pi * 0.47
  check "sphere area" ((area - exact).abs < 0.02 * exact) (fun _ => s!"area {area} vs {exact}")
  let nOk := (List.range s.mesh.numVertices).all fun i =>
    let p := s.mesh.pos.get! i
    let n := s.normals.get! i
    n.dot p < 0 && (n.norm - 1).abs < 1e-5
  check "sphere vertex normals" nOk
  let sf := Isosurface.extract sph 0.47 .faces
  let fOk := (List.range sf.mesh.numVertices).all fun i =>
    (sf.normals.get! i).dot (sf.mesh.pos.get! i) < 0
  check "sphere face normals" (fOk && sf.mesh.numTriangles == s.mesh.numTriangles)
  -- torus (genus 1)
  let g31 := LeanPlot.Num.range (-1) 1 31
  let tor := Isosurface.Volume.sample (fun x y z =>
    let q := Float.sqrt (x * x + y * y) - 0.6
    q * q + z * z) g31 g31 g31
  let t := Isosurface.extract tor 0.0441
  let (_, tclosed, toriented) := edgeStats t.mesh
  check "torus watertight" (tclosed && toriented)
  check "torus euler = 0" (euler t.mesh == 0) (fun _ => s!"{euler t.mesh}")
  -- two disjoint spheres
  let two := Isosurface.Volume.sample (fun x y z =>
    LeanPlot.Num.jmin ((x - 0.5) * (x - 0.5) + y * y + z * z) ((x + 0.5) * (x + 0.5) + y * y + z * z)) g21 g21 g21
  let tw := Isosurface.extract two 0.0937
  check "two spheres euler = 4" (euler tw.mesh == 4) (fun _ => s!"{euler tw.mesh}")
  -- level outside the data: empty
  check "empty isosurface" ((Isosurface.extract sph 5.0).mesh.numTriangles == 0)
  -- volume slices
  let (sl, ax1, ax2) := sph.slice 2 10
  check "volume slice xy" (sl.nx == 21 && sl.ny == 21 && ax1.size == 21 && ax2.size == 21 &&
    bitEq (sl.grid.get! 3 4) (sph.values.get! (3 + 21 * (4 + 21 * 10))))
  let (sl0, _, _) := sph.slice 0 0
  check "volume slice yz" (bitEq (sl0.grid.get! 5 7) (sph.values.get! (0 + 21 * (5 + 21 * 7))))
  -- levels follow Makie's to_levels on binary32 data
  check "volume levels" (faBitEq (Isosurface.volumeLevels sph 3) (Levels.contourLevels 3 0 3))
    (fun _ => showFA (Isosurface.volumeLevels sph 3))

end LeanPlotTest.Recipes.IsosurfaceTest
