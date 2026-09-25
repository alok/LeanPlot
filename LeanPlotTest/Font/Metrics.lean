import LeanPlotTest.Font.Harness

/-!
Embedded metrics against fontTools (`LeanPlotTest/Font/data/metrics.json`, written by
`scripts/font/gen.py` straight from the source fonts): face metrics, character → face/glyph
placement (Heros first, DejaVu fallback), advances, control boxes, contour and verb counts,
`.notdef`, kerning pairs.
-/

namespace LeanPlotTest.Font.Metrics

open LeanPlot LeanPlot.Font

/-- Number of `moveTo` verbs (contours) in glyph `g`. -/
def contours (f : Face) (g : Nat) : Nat :=
  go (f.verbStart[g]!) (f.verbStart[g + 1]!) 0
where
  go (i stop acc : Nat) : Nat :=
    if i < stop then go (i + 1) stop (if f.verbs.get! i == 0 then acc + 1 else acc) else acc
  termination_by stop - i

/-- Check one face against its JSON record. -/
def checkFace (t : Tally) (key : String) (f : Face) (j : Json) : Tally := Id.run do
  let mut t := t
  t := t.check (f.name == (j.get "name").str!) s!"{key}: name {f.name}"
  t := t.check (f.unitsPerEm == (j.get "unitsPerEm").num!) s!"{key}: unitsPerEm"
  t := t.check (f.ascender == (j.get "ascender").num!) s!"{key}: ascender"
  t := t.check (f.descender == (j.get "descender").num!) s!"{key}: descender"
  t := t.check (f.lineGap == (j.get "lineGap").num!) s!"{key}: lineGap"
  t := t.check (Float.ofNat f.numGlyphs == (j.get "nGlyphs").num!) s!"{key}: nGlyphs {f.numGlyphs}"
  let nd := j.get "notdef"
  t := t.check (f.glyphAdvance 0 == (nd.get "advance").num!) s!"{key}: .notdef advance"
  t := t.check (Float.ofNat (contours f 0) == (nd.get "contours").num!) s!"{key}: .notdef contours"
  for (cpStr, g) in (j.get "glyphs").obj! do
    let cp := cpStr.toNat!
    let gid := f.glyphIndex cp
    let ctx := s!"{key} U+{String.ofList (Nat.toDigits 16 cp)}"
    t := t.check' (Float.ofNat gid == (g.get "gid").num!) fun _ => s!"{ctx}: gid {gid}"
    t := t.check' (f.glyphAdvance gid == (g.get "advance").num!) fun _ =>
      s!"{ctx}: advance {f.glyphAdvance gid} vs {(g.get "advance").num!}"
    let (x0, y0, x1, y1) := f.glyphBox gid
    let cb := g.get "cbox"
    t := t.check' (x0 == (cb.idx 0).num! && y0 == (cb.idx 1).num! && x1 == (cb.idx 2).num! &&
        y1 == (cb.idx 3).num!) fun _ => s!"{ctx}: cbox ({x0}, {y0}, {x1}, {y1})"
    t := t.check' (Float.ofNat (contours f gid) == (g.get "contours").num!) fun _ => s!"{ctx}: contours"
    t := t.check' (Float.ofNat (f.verbStart[gid + 1]! - f.verbStart[gid]!) == (g.get "verbs").num!)
      fun _ => s!"{ctx}: verb count"
  let kern := (j.get "kern").arr!
  t := t.check (kern.size == f.kernKeys.size) s!"{key}: kerning pair count {f.kernKeys.size}"
  for k in kern do
    let l := (k.idx 0).num!.toUInt64.toNat
    let r := (k.idx 1).num!.toUInt64.toNat
    t := t.check (f.kerning l r == (k.idx 2).num!) s!"{key}: kern {l} {r}"
  return t

/-- Placement: every embedded codepoint resolves through `Font.regular`/`Font.bold` to the
face the generator put it in; the generator's "missing" list resolves to `.notdef`. -/
def checkPlacement (t : Tally) (j : Json) : Tally := Id.run do
  let mut t := t
  for (cpStr, p) in (j.get "placement").obj! do
    let cp := cpStr.toNat!
    let want := if p.str! == "primary" then 0 else 1
    for font in [Font.regular, Font.bold] do
      let code := font.resolve cp
      t := t.check' (code != 0 && code % 2 == want) fun _ => s!"placement U+{String.ofList (Nat.toDigits 16 cp)}: code {code}"
  for m in (j.get "missing").arr! do
    let cp := m.num!.toUInt64.toNat
    t := t.check (Font.regular.resolve cp == 0) s!"missing U+{String.ofList (Nat.toDigits 16 cp)} resolved"
  return t

/-- Run the metrics checks. -/
def run : IO (Nat × Nat) := do
  let j ← Json.readFile (dataPath "metrics.json")
  let faces := j.get "faces"
  let mut t : Tally := {}
  t := checkFace t "HerosRegular" herosRegular (faces.get "HerosRegular")
  t := checkFace t "HerosBold" herosBold (faces.get "HerosBold")
  t := checkFace t "DejaVuSans" dejaVuSans (faces.get "DejaVuSans")
  t := checkPlacement t j
  -- sanity on the charset: the characters the Grassmann/Cartan labels use are all embedded
  let needed := "∞∅∂∇ϵ∧∨⋅×⟨⟩⊗⊕⋆≈≤≥∑∫√πθφω−°′″·₀₁₂₃₄₅₆₇₈₉⁰¹²³⁴⁵⁶⁷⁸⁹ₐₑᵢⱼₖ→←↑↓⇒⇔∈∉⊂⊆∪∩ℝℂℤ𝕂𝟙★☉☾⟂"
  for c in needed.toList do
    t := t.check (Font.regular.resolve c.toNat != 0) s!"charset: {c} not embedded"
  t.report "metrics"

end LeanPlotTest.Font.Metrics
