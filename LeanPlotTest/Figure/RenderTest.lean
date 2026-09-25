import LeanPlotTest.Figure.LayoutTest

/-!
Render tests:

* **Makie parity**: our raster output for the oracle figures against CairoMakie's own
  renders (`LeanPlotTest/oracle/figure/ref/*.png`, `save(...; px_per_unit = 1)`), decoded
  with the Lean PNG decoder: mean per-pixel difference (max over channels, composited over
  white) at most 1/255, and at most 0.5 % of the pixels off by more than 64/255 (glyph
  hinting and triangle-edge antialiasing account for the rest);
* **SVG goldens**: byte-exact deterministic SVG output of a few figures
  (`LeanPlotTest/Figure/golden/*.svg`; set `LEANPLOT_UPDATE_GOLDENS=1` to rewrite them);
* **structure**: every figure renders to a scene of the figure size with ops in Makie's
  z-order (background first, spines after plots).
-/

namespace LeanPlotTest.Figure

open LeanPlot LeanPlotTest.Core

/-- Directory of the CairoMakie reference renders. -/
def refDir : System.FilePath := oracleDir / "ref"

/-- Directory of the SVG goldens. -/
def goldenDir : System.FilePath := "LeanPlotTest" / "Figure" / "golden"

/-- Figures with a committed CairoMakie reference render. -/
def parityNames : Array String :=
  #["basic", "empty", "limits", "heatmap_colorbar", "legend", "logy", "bandpoly", "axis3", "markers", "heatmap_image",
    "label_log_reversed", "hvlines_aspect", "legend_horizontal"]

/-- Figures with a committed SVG golden. -/
def svgNames : Array String := #["basic", "legend", "heatmap_colorbar", "axis3"]

/-- A channel composited over white (straight alpha). -/
@[inline] def overWhite (c a : UInt8) : Float :=
  let al := a.toNat.toFloat / 255
  c.toNat.toFloat * al + 255 * (1 - al)

/-- `(sum of per-pixel max channel difference, count of pixels off by more than `thr`)`. -/
def diffStats (a b : ByteArray) (n : Nat) (thr : Float) : Float × Nat :=
  let rec go (i : Nat) (sum : Float) (bad : Nat) : Float × Nat :=
    if i < n then
      let k := 4 * i
      let d (c : Nat) : Float :=
        (overWhite (a.get! (k + c)) (a.get! (k + 3)) - overWhite (b.get! (k + c)) (b.get! (k + 3))).abs
      let m := max (d 0) (max (d 1) (d 2))
      go (i + 1) (sum + m) (if m > thr then bad + 1 else bad)
    else (sum, bad)
  termination_by n - i
  go 0 0 0

/-- Compare one figure with CairoMakie's render. -/
def checkParity (name : String) (f : Figure) : TestM Unit := do
  let p := refDir / (name ++ ".png")
  if !(← p.pathExists) then
    check s!"parity {name}: reference" false (fun _ => s!"missing {p}")
    return
  let bytes ← IO.FS.readBinFile p
  match PNG.decodeRGBA bytes with
  | .error e => check s!"parity {name}: decode" false (fun _ => e)
  | .ok (w, h, ref) =>
    let sc := f.toScene
    check s!"parity {name}: size" (w == sc.width && h == sc.height) fun _ => s!"{w}x{h} vs {sc.width}x{sc.height}"
    if w == sc.width && h == sc.height then
      let cv := sc.toCanvas
      let n := w * h
      let (sum, bad) := diffStats cv.data ref n 64
      let mean := sum / n.toFloat
      let frac := bad.toFloat / n.toFloat
      check s!"parity {name}: mean |Δ| {mean} ≤ 1" (mean ≤ 1.0)
      check s!"parity {name}: {bad} px off by > 64" (frac ≤ 0.005)

/-- Compare (or rewrite) one SVG golden. -/
def checkSvg (name : String) (f : Figure) : TestM Unit := do
  let svg := f.toSVG
  let p := goldenDir / (name ++ ".svg")
  if (← IO.getEnv "LEANPLOT_UPDATE_GOLDENS").isSome then
    IO.FS.createDirAll goldenDir
    IO.FS.writeFile p svg
    check s!"svg {name}: written" true
  else if !(← p.pathExists) then
    check s!"svg {name}: golden" false (fun _ => s!"missing {p} (LEANPLOT_UPDATE_GOLDENS=1 writes it)")
  else
    let want ← IO.FS.readFile p
    check s!"svg {name}: byte-identical" (svg == want) fun _ => s!"{svg.length} vs {want.length} bytes"
    check s!"svg {name}: deterministic" (f.toSVG == svg)

/-- The index of the first op matching `p`. -/
def firstIdx (ops : Array DrawOp) (p : DrawOp → Bool) : Option Nat :=
  (Array.range ops.size).find? fun i => p ops[i]!

/-- Structural checks on the lowered scene of the basic figure. -/
def checkStructure : TestM Unit := do
  let sc := basic.toScene
  check "structure: size" (sc.width == 600 && sc.height == 450)
  -- op 0: the white axis background covering the viewport (74, 36, 510, 355)
  match (sc.ops[0]? : Option DrawOp) with
  | some (DrawOp.path p (some f) none none) =>
    check "structure: background first" (f.color == RGBA.white && p.coords.size == 8 && p.coords[0]! == 74 && p.coords[1]! == 36)
  | _ => check "structure: background first" false
  -- the plot lines are clipped to the viewport and come after the grid, before the spines
  let isPlot (op : DrawOp) : Bool := match op with
    | .path _ none (some s) (some _) => s.width == 1.5
    | _ => false
  let isSpine (op : DrawOp) : Bool := match op with
    | .path _ none (some s) none => s.width == 1 && s.color == RGBA.black
    | _ => false
  match firstIdx sc.ops isPlot, firstIdx sc.ops isSpine with
  | some i, some j => check "structure: plots before ticks and spines" (i < j)
  | _, _ => check "structure: plot and spine ops present" false
  -- every spec lowers to a scene of its size, all ops finite
  for (name, f) in specs do
    let s := f.toScene
    check s!"structure {name}: dims" (s.width == f.size.1 && s.height == f.size.2)
    check s!"structure {name}: nonempty" (s.ops.size > 5)

/-- The render suite. -/
def renderSuite : TestM Unit := do
  checkStructure
  for n in parityNames do
    if let some (_, f) := specs.find? (·.1 == n) then checkParity n f
  for n in svgNames do
    if let some (_, f) := specs.find? (·.1 == n) then checkSvg n f

end LeanPlotTest.Figure
