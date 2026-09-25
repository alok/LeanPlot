import LeanPlotTest.Font.Harness

/-!
Layout against the Makie oracle (`LeanPlotTest/Font/data/makie_layout.json`, written by
`scripts/font/oracle.jl` from `Makie.glyph_collection` / `Makie.unchecked_boundingbox` of
Makie 0.24 with its bundled fonts), plus unit checks of the behaviour Makie does not cover
(missing glyphs, control characters, rich text, measurement, rotated bounds).
-/

namespace LeanPlotTest.Font.Layout

open LeanPlot LeanPlot.Font

/-- Parse Makie's alignment names. -/
def halignOf : String → HAlign
  | "center" => .center | "right" => .right | _ => .left

/-- Parse Makie's alignment names (`center` is our `middle`). -/
def valignOf : String → VAlign
  | "top" => .top | "bottom" => .bottom | "center" => .middle | _ => .baseline

/-- Compare face metrics and per-character extents with FreeType's (via Makie). -/
def checkExtents (t : Tally) (j : Json) : Tally := Id.run do
  let mut t := t
  for (key, f) in [("HerosRegular", herosRegular), ("HerosBold", herosBold), ("DejaVuSans", dejaVuSans)] do
    let r := (j.get "faces").get key
    t := t.check (f.ascender == (r.get "ascender").num! && f.descender == (r.get "descender").num! &&
      f.height == (r.get "height").num! && f.unitsPerEm == (r.get "unitsPerEm").num!)
      s!"{key}: FreeType face metrics"
  for e in (j.get "extents").arr! do
    let cp := (e.get "cp").num!.toUInt64.toNat
    let code := Font.regular.resolve cp
    let f := Font.regular.faceOf code
    let wantFace := if (e.get "face").str! == "DejaVuSans" then 1 else 0
    let ch := (e.get "char").str!
    t := t.check (code % 2 == wantFace && code != 0) s!"extent {ch}: face"
    let em := f.unitsPerEm
    let tol := 1e-6
    t := t.check (near (f.glyphAdvance (code / 2) / em) (e.get "hadvance").num! tol) s!"extent {ch}: hadvance"
    t := t.check (near (f.ascender / em) (e.get "ascender").num! tol &&
      near (f.descender / em) (e.get "descender").num! tol) s!"extent {ch}: ascender/descender"
    let (x0, y0, x1, y1) := f.glyphBox (code / 2)
    let ink := e.get "ink"
    t := t.check' (near (x0 / em) (ink.idx 0).num! tol && near (y0 / em) (ink.idx 1).num! tol &&
        near (x1 / em) (ink.idx 2).num! tol && near (y1 / em) (ink.idx 3).num! tol) fun _ =>
      s!"extent {ch}: ink box ({x0}, {y0}, {x1}, {y1}) vs {(ink.idx 0).num!} {(ink.idx 1).num!} {(ink.idx 2).num!} {(ink.idx 3).num!}"
  return t

/-- Compare one `glyph_collection` case: every character's origin (Makie keeps `'\n'` as an
invisible glyph; ours are the run's `breaks`), its face, and the text bounding box. -/
def checkCase (t : Tally) (c : Json) : Tally := Id.run do
  let mut t := t
  let text := (c.get "text").str!
  let size := (c.get "size").num!
  let font := if (c.get "bold") matches .bool true then Font.bold else Font.regular
  let just := c.get "justification"
  let opts : LayoutOptions := {
    lineHeight := (c.get "lineheight").num!
    justification := if just.isNull then none else some just.num! }
  let ha := (c.get "halign").str!
  let va := (c.get "valign").str!
  let run := layout font text (halignOf ha) (valignOf va) opts
  let ctx := s!"{repr text} {ha}/{va}"
  let tol := 2e-4 * size  -- Makie works in Float32
  let mut gi := 0
  let mut bi := 0
  for g in (c.get "glyphs").arr! do
    let x := (g.get "x").num!
    let y := (g.get "y").num!
    if (g.get "char").str! == "\n" then
      let ok := 3 * bi + 2 < run.breaks.size &&
        near (size * run.breaks[3 * bi]!) x tol && near (size * run.breaks[3 * bi + 1]!) y tol
      t := t.check' ok fun _ => s!"{ctx}: newline {bi} at ({x}, {y}), ours " ++
        s!"({size * run.breaks[3 * bi]!}, {size * run.breaks[3 * bi + 1]!})"
      bi := bi + 1
    else
      if h : gi < run.codes.size then
        let code := run.codes[gi]
        let wantFace := if (g.get "face").str! == "DejaVuSans" then 1 else 0
        t := t.check' (code % 2 == wantFace) fun _ => s!"{ctx}: glyph {gi} face"
        t := t.check' (near (size * run.xs[gi]!) x tol && near (size * run.ys[gi]!) y tol) fun _ =>
          s!"{ctx}: glyph {gi} ({(g.get "char").str!}) at ({x}, {y}), ours ({size * run.xs[gi]!}, {size * run.ys[gi]!})"
        t := t.check' (near (run.font.advanceEm code) (g.get "hadvance").num! 1e-6) fun _ =>
          s!"{ctx}: glyph {gi} hadvance"
      else t := t.check false s!"{ctx}: missing glyph {gi}"
      gi := gi + 1
  t := t.check (gi == run.codes.size && 3 * bi == run.breaks.size) s!"{ctx}: glyph/break counts"
  let bb := c.get "bbox"
  unless bb.isNull do
    let e := run.logicalBox
    let tolB := 1e-3
    t := t.check' (near (size * e.left) (bb.idx 0).num! tolB && near (size * e.bottom) (bb.idx 1).num! tolB &&
        near (size * e.right) (bb.idx 2).num! tolB && near (size * e.top) (bb.idx 3).num! tolB) fun _ =>
      s!"{ctx}: bbox ({size * e.left}, {size * e.bottom}, {size * e.right}, {size * e.top}) vs " ++
      s!"({(bb.idx 0).num!}, {(bb.idx 1).num!}, {(bb.idx 2).num!}, {(bb.idx 3).num!})"
  return t

/-- Behaviour outside the oracle. -/
def checkUnits (t : Tally) : Tally := Id.run do
  let mut t := t
  let em (c : Char) : Float := Font.regular.advanceEm (Font.regular.resolve c.toNat)
  -- empty text
  let r0 := layout Font.regular ""
  t := t.check (r0.codes.isEmpty && r0.lines == 0 && r0.logicalBox == {} && r0.inkBox == {}) "empty layout"
  t := t.check ((textPath {} "" 0 0).verbs.isEmpty) "empty textPath"
  -- missing glyph → primary .notdef box with its advance
  let rm := layout Font.regular "a\uE000b"
  t := t.check (rm.codes.size == 3 && rm.codes[1]! == 0) "missing glyph is .notdef"
  t := t.check (near rm.xs[2]! (em 'a' + 0.49) 1e-12) "missing glyph advance"
  -- tab = space; CR and other control characters dropped
  let rt := layout Font.regular "a\tb\rc\u0007"
  t := t.check (rt.codes.size == 4 && near rt.xs[2]! (em 'a' + em ' ') 1e-12) "tab and control chars"
  -- plain run positions
  let r := layout Font.regular "AV"
  t := t.check (r.xs[0]! == 0 && near r.xs[1]! 0.667 1e-12 && near r.width 1.334 1e-12) "AV advances"
  -- kerning has no data in the embedded faces: identical with and without
  let rk := layout Font.regular "AVAWAY" .left .baseline { kerning := false }
  let rk' := layout Font.regular "AVAWAY"
  t := t.check (rk.xs.toList == rk'.xs.toList) "kerning is a no-op for Heros"
  -- measure
  let m := measure { size := 10 } "x"
  t := t.check (near m.advance 5 1e-9 && near m.ascent 9.47 1e-9 && near m.descent 2.18 1e-9 &&
    near m.height 11.65 1e-9 && m.lines == 1) s!"measure x: {repr m}"
  let m2 := measure { size := 10, valign := .top } "a\nb"
  t := t.check (near m2.height (9.47 + 11.65 + 2.18) 1e-9 && m2.lines == 2 &&
    near m2.ascent 9.47 1e-9 && near m2.descent 2.18 1e-9) s!"measure 2 lines: {repr m2}"
  t := t.check (near m2.logical.y 0 1e-9 && near m2.logical.h m2.height 1e-9) "top-aligned logical box"
  -- device bounds
  let st : TextStyle := { size := 12, halign := .center, valign := .middle }
  let b0 := textBounds st "Label" 100 100
  let lr := (layoutStyled st "Label").logicalBox.toRect 12
  t := t.check (near b0.x (100 + lr.x) 1e-9 && near b0.y (100 + lr.y) 1e-9 && near b0.w lr.w 1e-9 &&
    near b0.h lr.h 1e-9) "textBounds (rotation 0) = logical box"
  let b90 := textBounds { st with rotation := pi / 2 } "Label" 100 100
  t := t.check (near b90.w b0.h 1e-9 && near b90.h b0.w 1e-9 && near (b90.x + b90.w / 2) 100 1e-6 &&
    near (b90.y + b90.h / 2) 100 1e-6) s!"textBounds rotated: {repr b90}"
  -- rotation direction: reading bottom to top, the end of the text is above the anchor
  let pr := textPath { size := 10, rotation := pi / 2 } "ab" 0 0
  let ymin := pr.coords.foldl (init := ((0 : Float), (0 : Nat))) fun (m, i) v =>
    (if i % 2 == 1 && v < m then v else m, i + 1)
  t := t.check (ymin.1 < -5) "rotation π/2 draws upwards"
  -- rich text
  let rr := layoutRich Font.regular (.cat [.text "x", .sub (.text "1"), .sup (.text "2")])
  t := t.check (rr.codes.size == 3 && near rr.scales[1]! 0.66 1e-12 && near rr.ys[1]! (-0.25) 1e-12 &&
    near rr.ys[2]! 0.4 1e-12 && near rr.xs[1]! (em 'x') 1e-12 &&
    near rr.xs[2]! (em 'x' + 0.66 * em '1') 1e-12) "rich sub/sup"
  let rs := layoutRich Font.regular (.cat [.text "x", .subsup (.text "i") (.text "22"), .text "y"])
  t := t.check (rs.codes.size == 5 && near rs.xs[1]! rs.xs[2]! 1e-12 &&
    near rs.xs[4]! (em 'x' + 0.66 * 2 * em '2') 1e-12) "rich subsup stacks and advances by the wider"
  t := t.check (Rich.toPlain (.cat [.text "a", .sub (.text "b")]) == "ab") "Rich.toPlain"
  -- Unicode script helpers
  t := t.check (toSubscript? "12" == some "₁₂" && toSuperscript? "-1" == some "⁻¹" &&
    toSubscript? "q" == none && toSuperscript? "ij" == some "ⁱʲ") "unicode script maps"
  t := t.check (match Rich.subscript "ij" with | .text s => s == "ᵢⱼ" | _ => false) "Rich.subscript unicode"
  t := t.check (match Rich.superscript "q" with | .sup _ => true | _ => false) "Rich.superscript fallback"
  -- every subscript/superscript target is embedded
  for c in "0123456789+-=()aehijklmnoprstuvxβγρφχ".toList do
    t := t.check ((subscriptChar? c).all (fun s => Font.regular.resolve s.toNat != 0)) s!"subscript {c} embedded"
  for c in "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUVWαβγδεθιφχ".toList do
    t := t.check ((superscriptChar? c).all (fun s => Font.regular.resolve s.toNat != 0)) s!"superscript {c} embedded"
  -- lowering text ops to paths
  let st2 : TextStyle := { size := 16, color := ⟨1, 0, 0, 1⟩, halign := .right }
  let clip : Rect := { x := 0, y := 0, w := 50, h := 50 }
  let sc : Scene := { width := 100, height := 100, ops := #[.text 10 20 "Tx" st2 (some clip), .path {} none none none] }
  let lo := sc.lowerText
  t := t.check (match lo.ops[0]! with
    | .path p (some f) none (some c) =>
      p.coords.toList == (textPath st2 "Tx" 10 20).coords.toList && p.verbs == (textPath st2 "Tx" 10 20).verbs &&
        f.color == st2.color && f.rule == .nonzero && c == clip
    | _ => false) "Scene.lowerText turns text into a filled path"
  t := t.check (lo.ops.size == 2 && (match lo.ops[1]! with | .path _ none none none => true | _ => false))
    "Scene.lowerText keeps other ops"
  -- bold differs from regular and uses the bold face
  t := t.check ((layout Font.bold "m").width > (layout Font.regular "m").width) "bold m is wider"
  return t

/-- Run the layout checks. -/
def run : IO (Nat × Nat) := do
  let j ← Json.readFile (dataPath "makie_layout.json")
  let mut t : Tally := {}
  t := checkExtents t j
  for c in (j.get "cases").arr! do
    t := checkCase t c
  t := checkUnits t
  t.report "layout"

end LeanPlotTest.Font.Layout
