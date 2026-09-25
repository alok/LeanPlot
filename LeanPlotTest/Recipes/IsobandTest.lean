import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Isoband
import LeanPlot.Recipes.Algo.Contour

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Isoband` against `isoband.json`:
Isoband.jl rings per band (binary64) and Makie's `contourf` polygons (binary32,
with holes) and colour values. Rings are compared bit-exactly up to the starting
vertex; rings and polygons are compared as multisets (the C++ library emits
them in hash-map order).
-/

namespace LeanPlotTest.Recipes.IsobandTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- Bit key of an open ring, rotated to its smallest starting sequence. -/
def ringKey (xs ys : FloatArray) : String :=
  let n := xs.size
  let pt (i : Nat) : String := s!"{(xs.get! i).toBits}/{(ys.get! i).toBits}"
  let rot (s : Nat) : String := ",".intercalate ((List.range n).map fun t => pt ((s + t) % n))
  -- start at a smallest vertex (Julia `isless` order); ties broken by the whole sequence
  let best := (List.range n).foldl (fun b i =>
    if Contour.islessP (xs.get! i) (ys.get! i) (xs.get! b) (ys.get! b) then i else b) 0
  let cands := (List.range n).filter fun i => xs.get! i == xs.get! best && ys.get! i == ys.get! best
  (cands.map rot).foldl (fun m s => if s < m then s else m) (rot best)

/-- Drop the closing point of a closed ring. -/
def openRing (xs ys : FloatArray) : FloatArray × FloatArray :=
  let n := xs.size
  if n > 1 && xs.get! 0 == xs.get! (n - 1) && ys.get! 0 == ys.get! (n - 1) then
    (⟨xs.data.extract 0 (n - 1)⟩, ⟨ys.data.extract 0 (n - 1)⟩)
  else (xs, ys)

/-- Key of a closed ring. -/
def closedKey (p : Pts2) : String := let (xs, ys) := openRing p.xs p.ys; ringKey xs ys

/-- Key of a polygon with holes and its colour. -/
def polyKey (outer : Pts2) (holes : Array Pts2) (color : Float) : String :=
  let hs := (holes.map closedKey).qsort (· < ·)
  s!"{color.toBits}#" ++ closedKey outer ++ "|" ++ "|".intercalate hs.toList

/-- Two point arrays from a `[xs, ys]` JSON pair. -/
def pts (j : J) : Pts2 := Pts2.ofArrays (floatArr j.arrD[0]!) (floatArr j.arrD[1]!)

/-- Compare two key multisets. -/
def sameKeys (name : String) (got want : Array String) : TestM Unit := do
  let g := got.qsort (· < ·)
  let w := want.qsort (· < ·)
  let bad := (List.range (min g.size w.size)).filter fun i => g[i]! != w[i]!
  check name (g.size == w.size && bad.isEmpty)
    (fun _ => s!"sizes {g.size} {w.size}, {bad.length} differ; got {(g[bad.headD 0]?.getD "").take 200} want {(w[bad.headD 0]?.getD "").take 200}")

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "isoband.json" | return
  for c in (j.get "cases").arrD do
    let name := (c.get "name").string
    let nx := (c.get "nx").nat
    let ny := (c.get "ny").nat
    let xs := floatArr (c.get "x")
    let ys := floatArr (c.get "y")
    let some g := Grid2.ofFloatArray? nx ny (floatArr (c.get "z"))
      | check s!"{name} grid" false (fun _ => "bad grid size")
    let spec : Levels.LevelSpec := match c.get "spec" with
      | .arr _ => .values (floatArr (c.get "spec"))
      | s => .count s.nat
    let cf := Isoband.makieContourf xs ys g spec .normal (c.get "extendlow").boolean (c.get "extendhigh").boolean
    check s!"{name} levels" (faBitEq cf.levels (floatArr (c.get "levels")))
      (fun _ => s!"got {showFA cf.levels} want {showFA (floatArr (c.get "levels"))}")
    check s!"{name} lows" (faBitEq cf.lows (floatArr (c.get "lows")))
    check s!"{name} highs" (faBitEq cf.highs (floatArr (c.get "highs")))
    -- raw rings per band (binary64 isoband on Makie's binary32 inputs)
    let z32 := g.map F32.r32
    let raw := (c.get "raw").arrD
    for b in [0:raw.size] do
      let r := raw[b]!
      let rx := floatArr (r.get "x")
      let ry := floatArr (r.get "y")
      let ids := (r.get "id").arrD.map (·.nat)
      let mut want : Array String := #[]
      let mut start := 0
      for k in [0:ids.size + 1] do
        if k == ids.size || (k > start && ids[k]! != ids[start]!) then
          if k > start then
            want := want.push (ringKey ⟨rx.data.extract start k⟩ ⟨ry.data.extract start k⟩)
          start := k
      let rings := Isoband.isobandRings xs ys z32 (cf.lows.get! b) (cf.highs.get! b)
      sameKeys s!"{name} band {b} rings" (rings.map fun p => ringKey p.xs p.ys) want
    -- Makie polygons with holes and colours
    let colors := floatArr (c.get "colors")
    let want := (Array.range (c.get "polys").arrD.size).map fun k =>
      let p := (c.get "polys").arrD[k]!
      polyKey (pts (p.get "outer")) ((p.get "holes").arrD.map pts) (colors.get! k)
    let got := (Array.range cf.polys.size).map fun k =>
      let p := cf.polys[k]!
      polyKey p.outer p.holes (cf.colors.get! k)
    sameKeys s!"{name} polygons" got want
    -- final polygon colours (banded viridis), matched through the polygon keys
    let bc := Isoband.bandColoring LeanPlot.Colormap.viridis cf.levels (c.get "extendlow").boolean (c.get "extendhigh").boolean
    let mine := Isoband.polygonColors bc cf
    let rgbaKey (c : LeanPlot.RGBA) : String := s!"{c.r.toBits}/{c.g.toBits}/{c.b.toBits}/{c.a.toBits}"
    let wantC := (Array.range (c.get "polys").arrD.size).map fun k =>
      let q := (c.get "rgba").arrD[k]!.floats
      want[k]! ++ "@" ++ rgbaKey ⟨q[0]!, q[1]!, q[2]!, q[3]!⟩
    let gotC := (Array.range cf.polys.size).map fun k => got[k]! ++ "@" ++ rgbaKey mine[k]!
    sameKeys s!"{name} polygon colours" gotC wantC

end LeanPlotTest.Recipes.IsobandTest
