import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Contour

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Contour` against `contour.json`: Makie's
`contour` recipe (`zlevels`, `contour_points`, `elements_per_segment`) and raw
binary64 `Contour.contours` lines. Lines are compared bit-exactly as multisets
per level (Julia traces start cells in hash order; each line itself is in
Makie's canonical order on both sides).
-/

namespace LeanPlotTest.Recipes.ContourTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- A bit-exact key for one line of a level. -/
def lineKey (lvl : Nat) (xs ys : FloatArray) : String :=
  let pts := (List.range xs.size).map fun i => s!"{(xs.get! i).toBits}/{(ys.get! i).toBits}"
  s!"{lvl}:" ++ ",".intercalate pts

/-- Sorted keys of traced lines. -/
def keysOf (ls : Contour.Lines) : Array String :=
  (ls.lines.map fun (k, p) => lineKey k p.xs p.ys).qsort (· < ·)

/-- Sorted keys of Makie's flat `contour_points` + `elements_per_segment`. -/
def keysOfFlat (px py : FloatArray) (segs : Array (Nat × Nat)) : Array String := Id.run do
  let mut out := #[]
  let mut off := 0
  for (lvl, cnt) in segs do
    let n := cnt - 1
    out := out.push (lineKey lvl ⟨px.data.extract off (off + n)⟩ ⟨py.data.extract off (off + n)⟩)
    off := off + cnt
  return out.qsort (· < ·)

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "contour.json" | return
  for c in (j.get "cases").arrD do
    let name := (c.get "name").string
    let nx := (c.get "nx").nat
    let ny := (c.get "ny").nat
    let xs := floatArr (c.get "x")
    let ys := floatArr (c.get "y")
    let coords : Contour.Coords :=
      if (c.get "curvilinear").boolean then .curvilinear xs ys else .rect xs ys
    let some g := Grid2.ofFloatArray? nx ny (floatArr (c.get "z"))
      | check s!"{name} grid" false (fun _ => "bad grid size")
    let spec : Levels.LevelSpec := match c.get "spec" with
      | .arr _ => .values (floatArr (c.get "spec"))
      | s => .count s.nat
    let ls := Contour.makieContour coords g spec
    let zl := floatArr (c.get "zlevels")
    let zl' := Levels.contourZLevels spec (Contour.roundGrid g).z
    check s!"{name} zlevels" (faBitEq zl' zl) (fun _ => s!"got {showFA zl'} want {showFA zl}")
    check s!"{name} traced levels" (faBitEq ls.levels (F32.roundArray zl))
    let segs := (c.get "segs").arrD.map fun s => (s.arrD[0]!.nat, s.arrD[1]!.nat)
    check s!"{name} line count" (ls.lines.size == segs.size)
      (fun _ => s!"got {ls.lines.size} want {segs.size}")
    let got := keysOf ls
    let want := keysOfFlat (floatArr (c.get "px")) (floatArr (c.get "py")) segs
    let bad := (List.range (min got.size want.size)).filter fun i => got[i]! != want[i]!
    check s!"{name} makie lines" (got.size == want.size && bad.isEmpty)
      (fun _ => s!"{bad.length} lines differ; first got {got[bad.headD 0]!.take 300} want {want[bad.headD 0]!.take 300}")
    -- flat layout round trip
    let (flat, fsegs) := ls.flatten
    check s!"{name} flatten" (flat.size == (floatArr (c.get "px")).size && fsegs.size == segs.size)
    -- colour range and per-level colours (viridis)
    let (zlo, zhi) := Levels.dataRange (Contour.roundGrid g).z
    let (clo, chi) := Contour.colorRange zlo zhi
    let wcr := floatArr (c.get "colorrange")
    check s!"{name} colorrange" (bitEq clo (wcr.get! 0) && bitEq chi (wcr.get! 1))
      (fun _ => s!"got {clo} {chi} want {showFA wcr}")
    let f32Levels := match spec with | .count _ => true | .values _ => false
    let lcs := Contour.levelColors LeanPlot.Colormap.viridis zl clo chi f32Levels
    let wlc := (c.get "level_colors").arrD.map (·.floats)
    let lcOk := lcs.size == wlc.size && (Array.range lcs.size).all fun k =>
      floatsBitEq #[lcs[k]!.r, lcs[k]!.g, lcs[k]!.b, lcs[k]!.a] wlc[k]!
    check s!"{name} level colours" lcOk (fun _ => s!"got {repr lcs} want {wlc}")
    -- binary64 Contour.jl
    let raw := (c.get "raw64").arrD
    let ls64 := Contour.contourLines coords g zl false
    let got64 := keysOf ls64
    let want64 := (raw.map fun r => lineKey (r.get "level").nat (floatArr (r.get "x")) (floatArr (r.get "y"))).qsort (· < ·)
    let bad64 := (List.range (min got64.size want64.size)).filter fun i => got64[i]! != want64[i]!
    check s!"{name} raw64 lines" (got64.size == want64.size && bad64.isEmpty)
      (fun _ => s!"sizes {got64.size} {want64.size}, {bad64.length} differ")

/-- `contour3d` lifts every point to its level. -/
def liftSuite : TestM Unit := do
  let xs := LeanPlot.Num.range (-1) 1 9
  let g := Grid2.sample (fun x y => x * x + y * y) xs xs
  let ls := Contour.makieContour (.rect xs xs) g (.values ⟨#[0.25, 0.5]⟩)
  let (p3, segs) := ls.flatten3d
  let (p2, _) := ls.flatten
  let ok := (List.range p3.size).all fun i =>
    let v := p3.get! i
    if (p2.xs.get! i).isNaN then v.z.isNaN else bitEq v.x (p2.xs.get! i) && (bitEq v.z 0.25 || bitEq v.z 0.5)
  check "contour3d lift" (ok && segs.size == ls.lines.size && p3.size == p2.size)

end LeanPlotTest.Recipes.ContourTest
