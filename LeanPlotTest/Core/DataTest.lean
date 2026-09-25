import LeanPlotTest.Core.Harness
import LeanPlot.Core.Data

/-!
Unit tests for the SoA data types: `Pts2`/`Pts3` construction and sampling,
`Grid2` indexing (Julia column-major), `TriMesh` index validation, and the
`ToPts2`/`ToPts3`/`ToGrid2` conversions.
-/

namespace LeanPlotTest.Core.DataTest

open LeanPlot LeanPlot.Num

/-- The data suite. -/
def suite : TestM Unit := do
  -- Pts2
  let p := Pts2.sample (fun x => x * x) 0 1 11
  check "Pts2.sample size" (p.size == 11) fun _ => s!"{p.size}"
  check "Pts2.sample x uses Julia range" (bitEq (p.get! 3).x 0.3) fun _ => showFloat (p.get! 3).x
  check "Pts2.sample y" (bitEq (p.get! 10).y 1.0) fun _ => showFloat (p.get! 10).y
  check "Pts2.ofArrays? mismatch" (Pts2.ofArrays? ⟨#[1, 2]⟩ ⟨#[1]⟩).isNone fun _ => "accepted"
  check "Pts2.ofArrays truncates" ((Pts2.ofArrays ⟨#[1, 2, 3]⟩ ⟨#[4, 5]⟩).size == 2) fun _ => "size"
  check "Pts2.get! out of range" ((Pts2.empty.get! 0).x.isNaN) fun _ => "not NaN"
  let q := Pts2.parametric Float.cos Float.sin 0 (2 * Num.pi) 5
  check "Pts2.parametric" (q.size == 5 && ((q.get! 4).x - 1).abs < 1e-12) fun _ => showFloat (q.get! 4).x
  match (Pts2.ofArrays ⟨#[1, nan, -3]⟩ ⟨#[2, 7, 5]⟩).bounds? with
  | some b => check "Pts2.bounds?" (b.x == -3 && b.w == 4 && b.y == 2 && b.h == 3) fun _ => s!"{b.x} {b.w}"
  | none => check "Pts2.bounds?" false fun _ => "none"
  let m := (Pts2.ofArrays ⟨#[1, 2]⟩ ⟨#[3, 4]⟩).map fun v => ⟨v.y, v.x⟩
  check "Pts2.map" ((m.get! 1).x == 4 && (m.get! 1).y == 2) fun _ => "map"
  -- Pts3
  let c := Pts3.parametric Float.cos Float.sin id 0 1 4
  check "Pts3.parametric" (c.size == 4 && (c.get! 3).z == 1) fun _ => s!"{c.size}"
  check "Pts3.bounds?" (c.bounds?.isSome) fun _ => "none"
  -- Grid2: z[i + nx*j]
  let g := Grid2.ofFn 3 2 fun i j => Num.ofInt (i + 10 * j : Nat)
  check "Grid2 layout" (g.z.data == #[0, 1, 2, 10, 11, 12]) fun _ => showFloats g.z.data
  check "Grid2.get!" (g.get! 2 1 == 12 && (g.get! 3 0).isNaN) fun _ => "get"
  let gt := g.transpose
  check "Grid2.transpose" (gt.get! 1 2 == 12) fun _ => showFloats gt.z.data
  let gs := Grid2.sample (fun x y => x + y) ⟨#[0, 1]⟩ ⟨#[10, 20, 30]⟩
  check "Grid2.sample" (gs.get! 1 2 == 31) fun _ => showFloats gs.z.data
  check "Grid2.toRowsTopDown" ((g.toRowsTopDown).data == #[10, 11, 12, 0, 1, 2]) fun _ => showFloats g.toRowsTopDown.data
  check "Grid2.extrema?" (g.extrema? == some (0, 12)) fun _ => "extrema"
  check "Grid2.ofFloatArray? size" (Grid2.ofFloatArray? 2 2 ⟨#[1, 2, 3]⟩).isNone fun _ => "accepted"
  -- TriMesh
  let pos := Pts3.ofArrays ⟨#[0, 1, 0, 1]⟩ ⟨#[0, 0, 1, 1]⟩ ⟨#[0, 0, 0, 0]⟩
  match TriMesh.mk? pos #[0, 1, 2, 1, 3, 2] with
  | some mesh =>
    check "TriMesh counts" (mesh.numTriangles == 2 && mesh.numVertices == 4) fun _ => "counts"
    check "TriMesh normal" ((mesh.faceNormal 0).z == 1) fun _ => showFloat (mesh.faceNormal 0).z
    check "TriMesh indexBytes" (mesh.indexBytes.size == 24 && mesh.indexBytes.get! 16 == 3) fun _ => "bytes"
  | none => check "TriMesh.mk?" false fun _ => "rejected valid mesh"
  check "TriMesh bad index" (TriMesh.mk? pos #[0, 1, 4]).isNone fun _ => "accepted"
  check "TriMesh bad count" (TriMesh.mk? pos #[0, 1]).isNone fun _ => "accepted"
  -- conversions
  let a : Array (Float × Float) := #[(1, 2), (3, 4)]
  check "ToPts2 pairs" ((toPts2 a).get! 1 == ⟨3, 4⟩) fun _ => "pairs"
  let ys : FloatArray := ⟨#[5, 6, 7]⟩
  check "ToPts2 ys" ((toPts2 ys).get! 2 == ⟨3, 7⟩) fun _ => "ys"
  let mat : Array (Array Float) := #[#[1, 2], #[3, 4], #[5]]
  let ag := toGrid2 mat
  check "ToGrid2 nested" (ag.nx == 3 && ag.ny == 2 && ag.grid.get! 1 1 == 4 && (ag.grid.get! 2 1).isNaN)
    fun _ => s!"{ag.nx}x{ag.ny}"

end LeanPlotTest.Core.DataTest
