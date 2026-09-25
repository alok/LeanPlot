import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Arrows

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Arrows` against `arrows.json`:
`_process_arrow_arguments` (bit-exact), `arrows2d` metrics (bit-exact) and
pixel-space component polygons (within 1e-4 px: Julia's binary32 `atan`/`sin`/`cos`
may differ from libm by an ulp), `arrows3d` placements, the GeometryBasics
Cylinder/Cone markers, and Cartan `spacing`.
-/

namespace LeanPlotTest.Recipes.ArrowsTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Points from a JSON list of coordinate columns (2 or 3). -/
def cols3 (j : J) : Pts3 :=
  let c := j.arrD
  let xs := floatArr c[0]!
  Pts3.ofArrays xs (floatArr c[1]!) (if c.size > 2 then floatArr c[2]! else ⟨Array.replicate xs.size 0⟩)

/-- Maximum coordinate difference between two point sets (`inf` on size mismatch). -/
def ptsDiff (a b : Pts3) : Float :=
  let m1 := maxAbsDiff a.xs b.xs
  let m2 := maxAbsDiff a.ys b.ys
  let m3 := maxAbsDiff a.zs b.zs
  LeanPlot.Num.jmax m1 (LeanPlot.Num.jmax m2 m3)

/-- Exact equality of point sets. -/
def ptsEq (a b : Pts3) : Bool := faBitEq a.xs b.xs && faBitEq a.ys b.ys && faBitEq a.zs b.zs

/-- A float field of a JSON object with a default. -/
def fld (j : J) (k : String) (d : Float) : Float := match j.get? k with | some v => v.float | none => d

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "arrows.json" | return
  -- _process_arrow_arguments
  for c in (j.get "process").arrD do
    let dim := (c.get "dim").nat
    let align : Arrows.Align := match (c.get "align").string with
      | "tail" => .tail | "center" => .center | "tip" => .tip | s => .frac ((LeanPlot.Num.parseFloat? s).getD 0)
    let mode : Arrows.ArgMode := if (c.get "argmode").string == "endpoint" then .endpoint else .direction
    let nrm := (c.get "normalize").boolean
    let (s, e) := Arrows.processArrows dim (cols3 (c.get "pos")) (cols3 (c.get "dir")) align
      (c.get "lengthscale").float nrm mode
    let ws := cols3 (c.get "starts")
    let we := cols3 (c.get "ends")
    let tag := s!"process dim={dim} {(c.get "align").string} ls={(c.get "lengthscale").float} norm={nrm} {(c.get "argmode").string}"
    if mode == .endpoint && nrm then
      -- whole-array normalisation: LinearAlgebra's generic_norm2 summation order
      check tag (ptsDiff s ws ≤ 1e-12 && ptsDiff e we ≤ 1e-12) (fun _ => s!"diff {ptsDiff s ws} {ptsDiff e we}")
    else
      check tag (ptsEq s ws && ptsEq e we) (fun _ => s!"diff {ptsDiff s ws} {ptsDiff e we}")
  -- arrows2d
  for c in (j.get "arrows2d").arrD do
    let name := (c.get "name").string
    let kw := c.get "kw"
    let st : Arrows.Style2D := {
      tailwidth := fld kw "tailwidth" 14, taillength := fld kw "taillength" 0, shaftwidth := fld kw "shaftwidth" 3,
      shaftlength := (kw.get? "shaftlength").map (·.float), minshaftlength := fld kw "minshaftlength" 10,
      tipwidth := fld kw "tipwidth" 14, tiplength := fld kw "tiplength" 8, strokemask := fld kw "strokemask" 0.75 }
    let sp := (c.get "start").arrD
    let dp := (c.get "dir").arrD
    let (sx, sy, sz) := (floatArr sp[0]!, floatArr sp[1]!, floatArr sp[2]!)
    let (dx, dy) := (floatArr dp[0]!, floatArr dp[1]!)
    let ms := (c.get "metrics").arrD
    for i in [0:ms.size] do
      let m := Arrows.metrics2d st (dx.get! i) (dy.get! i)
      let got : FloatArray := ⟨#[m.taillength, m.tailwidth, m.shaftlength, m.shaftwidth, m.tiplength, m.tipwidth]⟩
      check s!"arrows2d {name} metrics {i}" (faBitEq got (floatArr ms[i]!))
        (fun _ => s!"got {showFA got} want {showFA (floatArr ms[i]!)}")
    let shapes := Arrows.shapes2d st sx sy sz dx dy
    let meshes := (c.get "meshes").arrD
    check s!"arrows2d {name} polygon count" (shapes.count == meshes.size)
      (fun _ => s!"got {shapes.count} want {meshes.size}")
    let mut worst : Float := 0
    for k in [0:min shapes.count meshes.size] do
      let p := shapes.polygon k
      let w := cols3 meshes[k]!
      let d := LeanPlot.Num.jmax (maxAbsDiff p.xs w.xs) (maxAbsDiff p.ys w.ys)
      worst := LeanPlot.Num.jmax worst d
    check s!"arrows2d {name} polygons" (worst ≤ 1e-4) (fun _ => s!"max diff {worst}")
  -- arrows3d
  for c in (j.get "arrows3d").arrD do
    let name := (c.get "name").string
    let kw := c.get "kw"
    let st : Arrows.Style3D := {
      taillength := fld kw "taillength" 0, tailradius := fld kw "tailradius" 0.15,
      tiplength := fld kw "tiplength" 0.4, tipradius := fld kw "tipradius" 0.15,
      markerscale := (kw.get? "markerscale").map (·.float) }
    -- Makie projects data points to world space (`Point3f`) first; start from there
    let starts := cols3 (c.get "world_start")
    let ends := cols3 (c.get "world_end")
    let pl := Arrows.placements3d st starts ends
    let s32 := Pts3.ofArrays (F32.roundArray starts.xs) (F32.roundArray starts.ys) (F32.roundArray starts.zs)
    let e32 := Pts3.ofArrays (F32.roundArray ends.xs) (F32.roundArray ends.ys) (F32.roundArray ends.zs)
    let k := match st.markerscale with | some m => m | none => Arrows.autoScale3 s32 e32
    check s!"arrows3d {name} arrowscale" (bitEq k (c.get "arrowscale").float)
      (fun _ => s!"got {k} want {(c.get "arrowscale").float}")
    let ms := (c.get "metrics").arrD
    let pick (f : Arrows.Placement → Vec3) (sel : (Arrows.Metrics × Arrows.Placement × Arrows.Placement × Arrows.Placement) → Arrows.Placement) : Pts3 :=
      toPts3 (pl.map fun q => f (sel q))
    check s!"arrows3d {name} world start" (ptsEq (pick (·.pos) (·.2.1)) (cols3 (c.get "world_start")))
    check s!"arrows3d {name} shaft pos" (ptsEq (pick (·.pos) (·.2.2.1)) (cols3 (c.get "shaft_pos")))
      (fun _ => s!"diff {ptsDiff (pick (·.pos) (·.2.2.1)) (cols3 (c.get "shaft_pos"))}")
    check s!"arrows3d {name} tip pos" (ptsEq (pick (·.pos) (·.2.2.2)) (cols3 (c.get "tip_pos")))
      (fun _ => s!"diff {ptsDiff (pick (·.pos) (·.2.2.2)) (cols3 (c.get "tip_pos"))}")
    check s!"arrows3d {name} rot" (ptsEq (pick (·.dir) (·.2.1)) (cols3 (c.get "rot")))
      (fun _ => s!"diff {ptsDiff (pick (·.dir) (·.2.1)) (cols3 (c.get "rot"))}")
    check s!"arrows3d {name} tail scale" (ptsEq (pick (·.scale) (·.2.1)) (cols3 (c.get "tail_scale")))
    check s!"arrows3d {name} shaft scale" (ptsEq (pick (·.scale) (·.2.2.1)) (cols3 (c.get "shaft_scale")))
    check s!"arrows3d {name} tip scale" (ptsEq (pick (·.scale) (·.2.2.2)) (cols3 (c.get "tip_scale")))
    for i in [0:ms.size] do
      let m := pl[i]!.1
      let got : FloatArray := ⟨#[m.taillength, m.tailwidth, m.shaftlength, m.shaftwidth, m.tiplength, m.tipwidth]⟩
      check s!"arrows3d {name} metrics {i}" (faBitEq got (floatArr ms[i]!))
        (fun _ => s!"got {showFA got} want {showFA (floatArr ms[i]!)}")
      let q := Arrows.rotationFromZ pl[i]!.2.1.dir
      let wq := floatArr ((c.get "quats").arrD[i]!)
      let gq : FloatArray := ⟨#[q.x, q.y, q.z, q.w].map F32.r32⟩
      check s!"arrows3d {name} quat {i}" (faApprox 1e-7 gq wq) (fun _ => s!"got {showFA gq} want {showFA wq}")
  -- markers: triangle sets of (position, normal) corners
  for (name, m) in [("cylinder", Arrows.cylinderMarker 32), ("cone", Arrows.coneMarker 32)] do
    let w := (j.get "markers").get name
    let wp := cols3 (w.get "pos")
    let wn := cols3 (w.get "normals")
    let key (p n : Vec3) : String := s!"{p.x.toBits},{p.y.toBits},{p.z.toBits};{n.x.toBits},{n.y.toBits},{n.z.toBits}"
    let wantTris := (Array.range (w.get "faces").arrD.size).map fun f =>
      let pf := ((w.get "faces").arrD[f]!).arrD.map (·.nat)
      let nf := ((w.get "nfaces").arrD[f]!).arrD.map (·.nat)
      "|".intercalate ((List.range 3).map fun c => key (wp.get! pf[c]!) (wn.get! nf[c]!))
    let gotTris := (Array.range m.mesh.numTriangles).map fun t =>
      "|".intercalate ((List.range 3).map fun c =>
        let v := (m.mesh.tri[3 * t + c]!).toNat
        key (m.mesh.pos.get! v) (m.normals.get! v))
    let g := gotTris.qsort (· < ·)
    let wt := wantTris.qsort (· < ·)
    let bad := (List.range (min g.size wt.size)).filter fun i => g[i]! != wt[i]!
    check s!"marker {name}" (g.size == wt.size && bad.isEmpty)
      (fun _ => s!"sizes {g.size} {wt.size}; {bad.length} differ: got {g[bad.headD 0]?} want {wt[bad.headD 0]?}")
  -- Cartan spacing
  for c in (j.get "spacing").arrD do
    let p := cols3 (c.get "pts")
    let s := Arrows.Cartan.spacing p
    let w := (c.get "spacing").float
    check s!"spacing n={p.size}" ((s - w).abs ≤ 1e-12 * w.abs) (fun _ => s!"got {s} want {w}")

/-- Cartan operator scalings on hand-made columns. -/
def cartanSuite : TestM Unit := do
  let c1 : Pts3 := Pts3.ofArrays ⟨#[3, 0]⟩ ⟨#[4, 2]⟩ ⟨#[0, 0]⟩  -- norms 5, 2 → mean 3.5
  let c2 : Pts3 := Pts3.ofArrays ⟨#[1, 1]⟩ ⟨#[0, 0]⟩ ⟨#[0, 0]⟩  -- mean 1
  check "scaledarrows operator" (bitEq (Arrows.Cartan.scaledArrowsLengthscaleOp 7 #[c1, c2]) (7 / 3.5 / 3))
  check "bundle operator" (bitEq (Arrows.Cartan.bundleLengthscaleOp 7 #[c1, c2]) (7 / 1 / 2))
  check "scaledarrows field" (bitEq (Arrows.Cartan.scaledArrowsLengthscale 7 c1) (7 / 3.5 / 3))

end LeanPlotTest.Recipes.ArrowsTest
