import LeanPlotTest.Core.Harness
import LeanPlot.Backend.SVG

/-!
SVG writer: byte-stable goldens for a small hand-built scene exercising every
`DrawOp` (paths with fills, strokes, dashes, curves, NaN breaks; segments with
per-segment colours; flat and gradient triangles; images with a stub PNG
encoder and the per-pixel fallback; text with escaping and rotation; clip
deduplication), plus unit tests of `base64`, `escape` and path data.

Set `LEANPLOT_UPDATE_GOLDEN=1` to (re)write the golden files.
-/

namespace LeanPlotTest.Core.SVGTest

open LeanPlot LeanPlot.Backend

/-- A deterministic stand-in for the PNG encoder: `"PNG" w h` then the pixels. -/
def fakePNG (w h : Nat) (rgba : ByteArray) : ByteArray :=
  ((ByteArray.empty.push 80).push 78).push 71 |>.push w.toUInt8 |>.push h.toUInt8 |> (· ++ rgba)

/-- The golden scene. -/
def scene : Scene :=
  let clipA : Rect := ⟨10, 10, 180, 130⟩
  let clipB : Rect := ⟨0, 0, 100.0005, 80⟩
  let box := Path.rect ⟨20, 20, 60, 40⟩
  let line : Path := ((((({} : Path).moveTo 20 100).lineTo 60 120).lineTo Num.nan 0).lineTo 100 90).lineTo 140 130
  let curve : Path := (((({} : Path).moveTo 150 20).quadTo 180 10 190 40).cubicTo 170 60 160 60 150 40).close
  let segXs : FloatArray := ⟨#[0, 50, 50, 90, 90, 95]⟩
  let segYs : FloatArray := ⟨#[70, 70, 70, 60, 60, 20]⟩
  let segRGBA : ByteArray := ⟨#[255, 0, 0, 255, 255, 0, 0, 255, 0, 128, 255, 128]⟩
  let triXs : FloatArray := ⟨#[110, 150, 130, 170]⟩
  let triYs : FloatArray := ⟨#[140, 140, 110, 110]⟩
  let triRGBA : ByteArray := ⟨#[255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 255, 255, 0, 255]⟩
  let triIdx : ByteArray := ⟨#[0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0]⟩
  let img : ByteArray := ⟨#[0, 0, 0, 255, 255, 255, 255, 255, 255, 0, 0, 128, 0, 0, 255, 0]⟩
  { width := 200, height := 150, background := RGBA.white, ops := #[
    .path box (some { color := ⟨0.2, 0.4, 0.8, 1⟩ }) (some { color := RGBA.black, width := 2, join := .round }) (some clipA),
    .path line none (some { color := wongColors[1]!, width := 1.5, cap := .round, dash := #[4, 2], dashOffset := 1 }) (some clipA),
    .path curve (some { color := ⟨0, 0.6, 0.3, 0.5⟩, rule := .evenOdd }) none none,
    .segments segXs segYs segRGBA 3 .square (some clipB),
    .segments segXs segYs segRGBA 1 .butt (some ⟨0, 0, 100.0004, 80⟩),
    .triangles triXs triYs triRGBA triIdx none,
    .image 2 2 img ⟨150, 100, 40, 40⟩ .nearest none,
    .text 100 145 "a<b & \"c\"" { size := 10, halign := .center, valign := .bottom } none,
    .text 30 75 "y label" { size := 12, rotation := Num.pi / 2, color := ⟨0, 0, 0, 0.8⟩, bold := true, halign := .center } (some clipA)
  ] }

/-- Compare against a golden file (or write it when updating). -/
def golden (name : String) (got : String) : TestM Unit := do
  let p := oracleDir / name
  if (← IO.getEnv "LEANPLOT_UPDATE_GOLDEN") == some "1" then
    IO.FS.writeFile p got
    check s!"golden {name} (written)" true
  else if !(← p.pathExists) then
    check s!"golden {name}" false fun _ => s!"missing {p}; run with LEANPLOT_UPDATE_GOLDEN=1"
  else
    let want ← IO.FS.readFile p
    let firstDiff := (got.toList.zip want.toList).findIdx? fun (a, b) => a != b
    check s!"golden {name}" (got == want) fun _ =>
      s!"sizes {got.length}/{want.length}, first difference at char {firstDiff.getD (min got.length want.length)}"

/-- Unit tests and goldens. -/
def suite : TestM Unit := do
  check "base64" (SVG.base64 "Man".toUTF8 == "TWFu" && SVG.base64 "Ma".toUTF8 == "TWE=" &&
    SVG.base64 "M".toUTF8 == "TQ==" && SVG.base64 .empty == "") fun _ => SVG.base64 "Ma".toUTF8
  check "escape" (SVG.escape "a<b>&\"'" == "a&lt;b&gt;&amp;&quot;&apos;") fun _ => SVG.escape "a<b>&\"'"
  let ctx : SVG.Ctx := { opts := {}, encodePNG := none, svgText := SVG.defaultText (Num.fmtFixed 3) }
  let d := SVG.pathData ctx (((({} : Path).moveTo 0 0).lineTo Num.nan 1).lineTo 2.5 (-0.0004)).close
  check "pathData NaN break" (d == "M0 0M2.5 0Z") fun _ => d
  let flat := scene.toSVG (encodePNG := some fakePNG)
  golden "svg_scene_flat.golden" flat
  let grad := scene.toSVG (encodePNG := some fakePNG) (opts := { triangleMode := .gradient, xmlDeclaration := true })
  golden "svg_scene_gradient.golden" grad
  let noPng := scene.toSVG
  golden "svg_scene_nopng.golden" noPng
  -- determinism: rendering twice gives identical bytes
  check "deterministic" (scene.toSVG (encodePNG := some fakePNG) == flat) fun _ => "differs"
  -- clip deduplication is by formatted rectangle: 100.0005 and 100.0004 both print as 100
  check "clip paths deduplicated" ((flat.splitOn "<clipPath").length - 1 == 2) fun _ => s!"{(flat.splitOn "<clipPath").length - 1}"
  -- glyph-outline text hook: a stand-in "font" drawing each character as a
  -- 0.5em-wide box advancing by 0.6em from the anchor
  let boxes (st : TextStyle) (s : String) (x y : Float) : Path :=
    (List.range s.length).foldl (init := {}) fun p i =>
      let x0 := x + i.toFloat * 0.6 * st.size
      let y0 := y - 0.7 * st.size
      let x1 := x0 + 0.5 * st.size
      ((((p.moveTo x0 y0).lineTo x1 y0).lineTo x1 y).lineTo x0 y).close
  let glyph := SVG.glyphText boxes
  let g1 := glyph { size := 10, color := ⟨1, 0, 0, 0.5⟩ } "ab" 1.25 20
  check "glyphText" (g1 == "<path d=\"M1.25 13L6.25 13L6.25 20L1.25 20ZM7.25 13L12.25 13L12.25 20L7.25 20Z\" fill=\"#ff0000\" fill-opacity=\"0.5\"/>")
    fun _ => g1
  check "glyphText empty" (glyph {} "" 0 0 == "") fun _ => glyph {} "" 0 0
  let withGlyphs := scene.toSVG (encodePNG := some fakePNG) (svgText := some glyph)
  check "glyphText replaces <text>" ((withGlyphs.splitOn "<text").length == 1 &&
      (withGlyphs.splitOn "fill-opacity=\"0.8\"").length == 2)
    fun _ => withGlyphs

end LeanPlotTest.Core.SVGTest
