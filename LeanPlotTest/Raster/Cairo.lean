import LeanPlotTest.Raster.Util

/-!
Parity with Cairo, the rasteriser behind CairoMakie and so the Makie oracle.

`cairo/cairo_ref.jl` draws the same geometry with Cairo (through the
`Cairo.jl` that CairoMakie loads) and writes PNGs with libpng. This suite
decodes those PNGs with our own decoder, a second codec check on third-party
output, and compares them with our renders. Exact pixel equality is not
expected: Cairo snaps to 24.8 fixed point, supersamples coverage and flattens
arcs with a net area deficit. The comparison therefore checks:

* mean |Δ| per channel ≤ 0.5 (of 255),
* at most 1 % of channels differ by more than 16,
* painted "ink" (∑ darkness of the green channel) within 0.5 %.

Measured on the committed references: mean |Δ| ≤ 0.13, and the largest
single difference is 25, on the circle rim, where Cairo's flattening is
0.13 % short of πr² and ours is within 0.001 %.

The reference PNGs are looked up relative to the working directory (the
repository root under `lake test`). When they are missing the suite reports
itself skipped instead of failing.
-/

namespace LeanPlotTest.Raster.CairoTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Reference directory, relative to the repository root. -/
def refDir : System.FilePath := "LeanPlotTest" / "Raster" / "cairo"

/-- The scenes of `cairo_ref.jl`, one draw-op list each (200 × 150, white). -/
def scenes : List (String × Array DrawOp) :=
  let star : Path := Id.run do
    let mut p : Path := {}
    for i in [0:5] do
      let t := -3.141592653589793 / 2.0 + 4.0 * 3.141592653589793 * i.toFloat / 5.0
      let x := 100.0 + 60.0 * Float.cos t; let y := 78.0 + 60.0 * Float.sin t
      p := if i == 0 then p.moveTo x y else p.lineTo x y
    return p.close
  [("circle", #[.path (circlePath 100.3 75.7 40.0) (some { color := .black }) none none]),
   ("miter", #[.path (polyline [(20.3, 120.1), (60.7, 30.2), (110.2, 110.9), (180.5, 40.4)]) none
       (some { color := .black, width := 6, join := .miter, miterLimit := 4 }) none]),
   ("round", #[.path (polyline [(30, 40), (170, 60), (60, 120)]) none
       (some { color := ⟨0.8, 0.1, 0.1, 1⟩, width := 11, cap := .round, join := .round }) none]),
   ("dash", #[.path (polyline [(10.5, 75.25), (190.5, 75.25)]) none
       (some { color := .black, width := 2, dash := #[10, 5] }) none]),
   ("thin", (List.range 4).toArray.map fun k =>
      let kf := (k + 1).toFloat
      .path (polyline [(10 + 40 * kf, 10), (40 + 40 * kf, 140)]) none
        (some { color := .black, width := #[0.5, 1.0, 1.5, 2.0][k]! }) none),
   ("evenodd", #[.path star (some { color := ⟨0.1, 0.3, 0.9, 0.6⟩, rule := .evenOdd }) none none]),
   ("bezier", #[.path (((({} : Path).moveTo 20 130).cubicTo 40 10 160 10 180 130).close)
       (some { color := ⟨0.2, 0.6, 0.2, 1⟩ }) none none,
     .path ((({} : Path).moveTo 20 20).cubicTo 60 140 140 (-40) 180 80) none
       (some { color := .black, width := 3 }) none])]

/-- Compare one scene with its reference. -/
def compare (name : String) (ops : Array DrawOp) : T Unit := do
  let path := refDir / (name ++ ".png")
  if !(← path.pathExists) then
    IO.println s!"  (skipped cairo parity {name}: {path} not found; run from the repository root)"
    return
  let file ← IO.FS.readBinFile path
  match PNG.decodeRGBA file with
  | .error e => check s!"decode cairo {name}" false e
  | .ok (w, h, ref) =>
    check s!"cairo {name} size" (w == 200 && h == 150)
    let cv := paint 200 150 ops
    let mut sum := 0; let mut big := 0; let mut maxd := 0
    let mut inkL := 0.0; let mut inkC := 0.0
    for i in [0:200*150] do
      for c in [0:3] do
        let a := (cv.data.get! (4*i+c)).toNat; let b := (ref.get! (4*i+c)).toNat
        let d := if a ≥ b then a - b else b - a
        sum := sum + d; maxd := max maxd d
        if d > 16 then big := big + 1
      inkL := inkL + (255.0 - (cv.data.get! (4*i+1)).toFloat) / 255.0
      inkC := inkC + (255.0 - (ref.get! (4*i+1)).toFloat) / 255.0
    let mean := sum.toFloat / (3.0 * 200.0 * 150.0)
    let frac := big.toFloat / (3.0 * 200.0 * 150.0)
    IO.println s!"  cairo {name}: mean|Δ| {mean}, max|Δ| {maxd}, >16: {big}, ink {inkL} vs {inkC}"
    check s!"cairo {name} mean |Δ| ≤ 0.5" (mean ≤ 0.5) s!"{mean}"
    check s!"cairo {name} outliers ≤ 1%" (frac ≤ 0.01) s!"{frac}"
    check s!"cairo {name} ink within 0.5%" ((inkL - inkC).abs ≤ 0.005 * inkC) s!"{inkL} vs {inkC}"

/-- All Cairo parity checks. -/
def tests : T Unit := do
  for (name, ops) in scenes do compare name ops

/-- Run the Cairo parity suite. -/
def run : IO (Nat × Nat) := runSuite "cairo parity" tests

end LeanPlotTest.Raster.CairoTests
