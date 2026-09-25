import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Voxels

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Voxels` against `voxels.json`: voxel ids,
the id colormap, and CairoMakie's cube positions, size and colours (bit-exact).
-/

namespace LeanPlotTest.Recipes.VoxelsTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Colours as bit strings. -/
def rgbaKeys (cs : Array RGBA) : Array String := cs.map fun c => s!"{c.r.toBits}/{c.g.toBits}/{c.b.toBits}/{c.a.toBits}"

/-- Colours from a JSON list of `[r, g, b, a]`. -/
def jsonColors (j : J) : Array RGBA := j.arrD.map fun q => let f := q.floats; ⟨f[0]!, f[1]!, f[2]!, f[3]!⟩

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "voxels.json" | return
  for c in (j.get "cases").arrD do
    let name := (c.get "name").string
    let dims := (c.get "dims").arrD.map (·.nat)
    let vals := floatArr (c.get "values")
    let chunk : Voxels.Chunk := match (c.get "extent") with
      | .arr es =>
        let e (k : Nat) : Float × Float := let f := es[k]!.floats; (f[0]!, f[1]!)
        Voxels.Chunk.withExtent dims[0]! dims[1]! dims[2]! vals (e 0) (e 1) (e 2)
      | _ => Voxels.Chunk.centered dims[0]! dims[1]! dims[2]! vals
    let (o, cm) : Voxels.Options × Colormap := match name with
      | "clips" => ({ colorrange := some (0.2, 0.8), lowclip := some (RGBA.parse? "red").get!,
                      highclip := some (RGBA.parse? "orange").get! }, Colormap.viridis)
      | "log" => ({ scale := .log10, gap := 0.1 }, Colormap.viridis)
      | "holes" => ({ isAir := fun x => !(0.9 ≤ x && x ≤ 1.7) }, Colormap.viridis)
      | "alpha" => ({ alpha := 0.5 }, Colormap.named "magma")
      | _ => ({}, Colormap.viridis)
    let ids := Voxels.voxelIds chunk o
    let wantIds := (c.get "ids").arrD.map (·.nat)
    check s!"voxels {name} ids" (ids.toList.map (·.toNat) == wantIds.toList)
      (fun _ => s!"got {ids.toList.take 12} want {wantIds.toList.take 12}")
    let cmap := Voxels.voxelColormap cm o
    check s!"voxels {name} colormap" (rgbaKeys cmap == rgbaKeys (jsonColors (c.get "colormap")))
      (fun _ => s!"sizes {cmap.size} {(c.get "colormap").arrD.size}")
    let (pos, cols) := Voxels.voxelCubes chunk o cm
    let wp := (c.get "pos").arrD
    let posOk := faBitEq pos.xs (floatArr wp[0]!) && faBitEq pos.ys (floatArr wp[1]!) && faBitEq pos.zs (floatArr wp[2]!)
    check s!"voxels {name} positions" posOk (fun _ => s!"{pos.size} cubes, diff {maxAbsDiff pos.xs (floatArr wp[0]!)}")
    let sz := Voxels.voxelSize chunk o
    check s!"voxels {name} size" (faBitEq ⟨#[sz.x, sz.y, sz.z]⟩ (floatArr (c.get "size")))
      (fun _ => s!"got {repr sz} want {showFA (floatArr (c.get "size"))}")
    check s!"voxels {name} colours" (rgbaKeys cols == rgbaKeys (jsonColors (c.get "colors")))
  -- the cube marker: 24 vertices, 12 triangles, unit normals, outward
  let m := Voxels.cubeMarker
  let outward := (List.range m.mesh.numTriangles).all fun t =>
    let (a, b, c) := m.mesh.triangle t
    let n := (b.sub a).cross (c.sub a)
    let cen : Vec3 := ⟨(a.x + b.x + c.x) / 3, (a.y + b.y + c.y) / 3, (a.z + b.z + c.z) / 3⟩
    n.dot cen > 0
  check "cube marker" (m.mesh.numVertices == 24 && m.mesh.numTriangles == 12 && outward)

end LeanPlotTest.Recipes.VoxelsTest
