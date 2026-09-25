import LeanPlotTest.Font.Harness

/-!
Golden SVG: a sample sheet of text rendered as filled `<path>` outlines (Latin, Greek, math
with DejaVu fallback, tick labels with U+2212, bold, alignment anchors, rotation, rich
sub/superscripts, multi-line, a missing glyph). The file is compared byte for byte with
`LeanPlotTest/Font/golden/text_sample.svg`; set `LEANPLOT_UPDATE_GOLDEN=1` to rewrite it.
The actual output of a failing run is written to `.lake/font-test-out/`.
-/

namespace LeanPlotTest.Font.Golden

open LeanPlot LeanPlot.Font

/-- A filled path element. -/
def pathEl (p : Path) (fill : String := "#000") : String :=
  s!"<path d=\"{pathD p}\" fill=\"{fill}\"/>\n"

/-- A small crosshair marking an anchor. -/
def cross (x y : Float) : String :=
  s!"<path d=\"M{fmt (x - 4)} {fmt y}H{fmt (x + 4)}M{fmt x} {fmt (y - 4)}V{fmt (y + 4)}\" stroke=\"#d33\" stroke-width=\"0.5\" fill=\"none\"/>\n"

/-- An outlined rectangle (text bounds). -/
def rectEl (r : Rect) (stroke : String) : String :=
  s!"<rect x=\"{fmt r.x}\" y=\"{fmt r.y}\" width=\"{fmt r.w}\" height=\"{fmt r.h}\" fill=\"none\" stroke=\"{stroke}\" stroke-width=\"0.5\"/>\n"

/-- The sample sheet. -/
def sample : String := Id.run do
  let w := 640
  let h := 400
  let mut out := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{w}\" height=\"{h}\" viewBox=\"0 0 {w} {h}\">\n"
  out := out ++ s!"<rect width=\"{w}\" height=\"{h}\" fill=\"#fff\"/>\n"
  let body : TextStyle := { size := 18 }
  out := out ++ pathEl (textPath body "The quick brown fox jumps over the lazy dog. 0123456789" 20 36)
  out := out ++ pathEl (textPath body "αβγδεζηθικλμνξοπρστυφχψω ΑΒΓΔΘΛΞΠΣΦΨΩ ϑϕϵ" 20 64)
  out := out ++ pathEl (textPath body "∂ₖ ϵ¹ ∇⋅F = ∑ᵢ ∂ᵢFⁱ, ∫ f dx ≈ ∞, v₁₂ ∧ w₃ ⊗ e¹²³, ⟨a, b⟩ ≤ ‖x‖" 20 92)
  out := out ++ pathEl (textPath body "ℝ³ → ℂ, x ∈ ∅ ∪ 𝕂, θ ≥ 45°, a′ ″, 2π × 10⁻³ ⇒ ⊕ ⋆ √2" 20 120)
  out := out ++ pathEl (textPath { size := 14, color := RGBA.black } "−1.5    −1.0    −0.5    0.0    0.5    1.0    1.5" 20 146)
  out := out ++ pathEl (textPath { size := 22, bold := true, halign := .center } "Bold Title (Heros Bold)" 320 180)
  -- alignment anchors
  let names := [("left/top", HAlign.left, VAlign.top), ("center/middle", .center, .middle),
                ("right/bottom", .right, .bottom), ("left/baseline", .left, .baseline)]
  let mut i := 0
  for (s, ha, va) in names do
    let x := 110 + 140 * Float.ofNat i
    let y := 230.0
    let st : TextStyle := { size := 13, halign := ha, valign := va }
    out := out ++ rectEl (textBounds st s x y) "#9bd"
    out := out ++ pathEl (textPath st s x y)
    out := out ++ cross x y
    i := i + 1
  -- rotated labels
  let yl : TextStyle := { size := 14, halign := .center, valign := .bottom, rotation := pi / 2 }
  out := out ++ rectEl (textBounds yl "y label (rotated)" 30 320) "#9bd"
  out := out ++ pathEl (textPath yl "y label (rotated)" 30 320)
  out := out ++ cross 30 320
  let dl : TextStyle := { size := 13, rotation := pi / 6 }
  out := out ++ rectEl (textBounds dl "tick 30°" 90 360) "#9bd"
  out := out ++ pathEl (textPath dl "tick 30°" 90 360)
  out := out ++ cross 90 360
  -- rich text and multi-line
  out := out ++ pathEl (richPath { size := 22 } (.cat [.text "x", .subsup (.text "i") (.text "2"), .text " + e",
    .sup (.text "iπq"), .text " = y", .sub (.text "q")]) 180 300)
  let ml : TextStyle := { size := 14, halign := .center, valign := .top }
  out := out ++ rectEl (textBounds ml "multi-line\ncentred text" 470 280) "#9bd"
  out := out ++ pathEl (textPath ml "multi-line\ncentred text" 470 280) "#236"
  out := out ++ cross 470 280
  out := out ++ pathEl (textPath { size := 14 } "missing glyph: \uE000" 180 360) "#555"
  out := out ++ "</svg>\n"
  return out

/-- Compare with (or update) the golden file. -/
def run : IO (Nat × Nat) := do
  let actual := sample
  let golden := goldenPath "text_sample.svg"
  let update := (← IO.getEnv "LEANPLOT_UPDATE_GOLDEN").isSome
  if update || !(← golden.pathExists) then
    IO.FS.createDirAll (golden.parent.getD ".")
    IO.FS.writeFile golden actual
    IO.println s!"  font/golden: wrote {golden}"
    return (1, 0)
  let expected ← IO.FS.readFile golden
  let t : Tally := {}
  let t := t.check (expected == actual) s!"{golden} differs (actual in .lake/font-test-out/text_sample.svg)"
  if expected != actual then
    IO.FS.createDirAll ".lake/font-test-out"
    IO.FS.writeFile ".lake/font-test-out/text_sample.svg" actual
  t.report "golden"

end LeanPlotTest.Font.Golden
