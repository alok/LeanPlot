import LeanPlot.Font.Faces

/-!
# Text layout and text-to-path

A port of Makie's `glyph_collection` (Makie 0.24 `src/layouting/text_layouting.jl`):

* every character is looked up in the primary face, then in the fallback face
  (`find_font_for_char`); characters in neither are drawn as the primary face's `.notdef`
  box (Makie would raise an error instead);
* glyphs advance left to right by their `hadvance` (plus pair kerning within one face, which
  Makie does not apply; the embedded faces have no kerning pairs, so the result is the same);
* `'\n'` breaks lines; line `k` sits `lineheight_k` below line `k-1`, where a line's height
  is the largest `face.height / unitsPerEm * lineHeight` among its characters (including the
  `'\n'` that ends it, which uses the primary face, as in Makie);
* lines are justified within the widest line (`justification`, default: follow `halign`) and
  the block is aligned: `left/center/right` against the widest line, `top` against the
  largest ascender of the first line, `bottom` against the smallest descender of the last
  line, `middle` halfway between, and `baseline` puts the *last* line's baseline on the anchor
  (identical to the first line's for single-line text).

Layout happens in *em units* (font size 1), y up, with the anchor at the origin. `textPath`
then scales by `TextStyle.size`, rotates by `TextStyle.rotation` (counter-clockwise on
screen) and flips into y-down device space.

`'\t'` is laid out as a space; `'\r'` and other control characters are dropped.

Rich text (`Rich`) adds Makie's `subscript`/`superscript`/`subsup` spans (scale 0.66, baseline
−0.25 em / +0.4 em of the enclosing size). Unicode sub/superscript characters (`x₁`, `v¹²`,
`eᵢ`) need no rich text: they are ordinary embedded glyphs.
-/

namespace LeanPlot.Font

/-- Layout options that `TextStyle` does not carry. -/
structure LayoutOptions where
  /-- Apply pair kerning between consecutive glyphs of the same face. -/
  kerning : Bool := true
  /-- Makie's `lineheight` factor, relative to each face's `height / unitsPerEm`. -/
  lineHeight : Float := 1.0
  /-- Justification of lines within the block, `0` left … `1` right; `none` follows the
  horizontal alignment (Makie's `justification = automatic`). -/
  justification : Option Float := none
  deriving Inhabited, Repr

namespace Font

/-- Face-tagged glyph code of codepoint `cp`: `2 * gid` for the primary face, `2 * gid + 1`
for the fallback face, and `0` (the primary `.notdef` box) when neither face maps it. -/
@[inline] def resolve (f : Font) (cp : Nat) : Nat :=
  let g := f.primary.glyphIndex cp
  if g != 0 then 2 * g
  else
    let g := f.fallback.glyphIndex cp
    if g != 0 then 2 * g + 1 else 0

/-- The face a glyph code refers to. -/
@[inline] def faceOf (f : Font) (code : Nat) : Face :=
  if code % 2 == 0 then f.primary else f.fallback

/-- Advance of a glyph code in em. -/
@[inline] def advanceEm (f : Font) (code : Nat) : Float :=
  let face := f.faceOf code
  face.advance[code / 2]! / face.unitsPerEm

end Font

/-- Horizontal alignment as a fraction of the block width (Makie's `halign2num`). -/
@[inline] def halignFrac : HAlign → Float
  | .left => 0 | .center => 0.5 | .right => 1

/-- `Float` infinity, used as the neutral element of max/min folds. -/
@[inline] def inf : Float := 1.0 / 0.0

/-! ## Shaping: characters → glyphs with pen positions and line metrics -/

/-- Accumulates shaped glyphs and finished lines (em units, before alignment). -/
structure Builder where
  /-- Face-tagged glyph codes (see `Font.resolve`). -/
  codes : Array Nat := #[]
  /-- Pen x of each glyph within its line. -/
  penX : FloatArray := .empty
  /-- Baseline offset of each glyph within its line (rich text sub/superscripts). -/
  yOff : FloatArray := .empty
  /-- Size of each glyph relative to the font size. -/
  scale : FloatArray := .empty
  /-- First glyph of each finished line. -/
  lineStart : Array Nat := #[]
  /-- Width of each finished line. -/
  lineWidth : FloatArray := .empty
  /-- Largest `yOff + ascender` of each finished line. -/
  lineAsc : FloatArray := .empty
  /-- Smallest `yOff + descender` of each finished line. -/
  lineDesc : FloatArray := .empty
  /-- Line height of each finished line. -/
  lineHeight : FloatArray := .empty
  /-- Current pen x. -/
  x : Float := 0
  /-- First glyph of the current line. -/
  curStart : Nat := 0
  /-- Running maximum ascender of the current line. -/
  curAsc : Float := -inf
  /-- Running minimum descender of the current line. -/
  curDesc : Float := inf
  /-- Running maximum line height of the current line. -/
  curLH : Float := 0
  /-- Characters (glyphs and the terminating newline) in the current line. -/
  curChars : Nat := 0
  /-- Code of the previous glyph plus one, `0` when there is none (kerning). -/
  prev : Nat := 0
  /-- Line breaks: `pen x, line index, baseline offset, scale` per `'\n'`. -/
  breaks : FloatArray := .empty

namespace Builder

/-- Fold a face's vertical metrics (at `yoff`, `scale`) into the current line. -/
@[inline] def foldMetrics (b : Builder) (face : Face) (lhFactor yoff scale : Float) : Builder :=
  let k := scale / face.unitsPerEm
  let a := yoff + face.ascender * k
  let d := yoff + face.descender * k
  let h := face.height * k * lhFactor
  { b with
    curAsc := if a > b.curAsc then a else b.curAsc
    curDesc := if d < b.curDesc then d else b.curDesc
    curLH := if h > b.curLH then h else b.curLH }

/-- Append one glyph (face-tagged code) at the pen, then advance the pen. -/
@[inline] def pushGlyph (b : Builder) (font : Font) (opts : LayoutOptions) (code : Nat)
    (yoff scale : Float) : Builder :=
  let face := font.faceOf code
  let g := code / 2
  let x :=
    if opts.kerning && b.prev != 0 && (b.prev - 1) % 2 == code % 2 then
      b.x + face.kerning ((b.prev - 1) / 2) g / face.unitsPerEm * scale
    else b.x
  let b := b.foldMetrics face opts.lineHeight yoff scale
  { b with
    codes := b.codes.push code
    penX := b.penX.push x
    yOff := b.yOff.push yoff
    scale := b.scale.push scale
    x := x + face.advance[g]! / face.unitsPerEm * scale
    curChars := b.curChars + 1
    prev := code + 1 }

/-- Close the current line with the given width and start a new one. -/
@[inline] def closeLine (b : Builder) (width : Float) : Builder :=
  { b with
    lineStart := b.lineStart.push b.curStart
    lineWidth := b.lineWidth.push width
    lineAsc := b.lineAsc.push b.curAsc
    lineDesc := b.lineDesc.push b.curDesc
    lineHeight := b.lineHeight.push b.curLH
    x := 0, curStart := b.codes.size, curAsc := -inf, curDesc := inf, curLH := 0
    curChars := 0, prev := 0 }

/-- A `'\n'`: it belongs to the line it ends and carries the primary face's metrics (Makie
looks `'\n'` up in the requested font). A line holding only the newline is as wide as the
`.notdef` advance, as in Makie. -/
def newline (b : Builder) (font : Font) (opts : LayoutOptions) (yoff scale : Float) : Builder :=
  let b := b.foldMetrics font.primary opts.lineHeight yoff scale
  let b := { b with
    breaks := (((b.breaks.push b.x).push (Float.ofNat b.lineStart.size)).push yoff).push scale }
  let width := if b.curChars == 0 then font.advanceEm 0 * scale else b.x
  b.closeLine width

/-- Shape one character. -/
@[inline] def pushChar (b : Builder) (font : Font) (opts : LayoutOptions) (c : Char)
    (yoff scale : Float) : Builder :=
  let cp := c.toNat
  if cp == 10 then b.newline font opts yoff scale
  else if cp == 9 then b.pushGlyph font opts (font.resolve 32) yoff scale
  else if cp < 32 || (cp ≥ 127 && cp < 160) then b
  else b.pushGlyph font opts (font.resolve cp) yoff scale

/-- Shape every character of `s`. -/
def pushString (b : Builder) (font : Font) (opts : LayoutOptions) (s : String)
    (yoff scale : Float) : Builder :=
  go s.utf8ByteSize 0 b
where
  go : Nat → String.Pos.Raw → Builder → Builder
    | 0, _, b => b
    | fuel + 1, p, b =>
      if p.byteIdx < s.utf8ByteSize then
        go fuel (p.next s) (b.pushChar font opts (p.get s) yoff scale)
      else b

/-- Close the last line. A trailing empty line (text ending in `'\n'`) takes the primary
face's metrics, like Makie's `metric_char`. -/
def finish (b : Builder) (font : Font) (opts : LayoutOptions) : Builder :=
  if b.curChars == 0 then
    if b.lineStart.isEmpty then b
    else (b.foldMetrics font.primary opts.lineHeight 0 1).closeLine 0
  else b.closeLine b.x

end Builder

/-! ## Alignment -/

/-- A shaped and aligned glyph run in em units (font size 1), y up, with the alignment
anchor at the origin. Structure of arrays, one entry per glyph. -/
structure GlyphRun where
  /-- The font the codes refer to. -/
  font : Font
  /-- Face-tagged glyph codes (see `Font.resolve`). -/
  codes : Array Nat := #[]
  /-- Glyph origin x. -/
  xs : FloatArray := .empty
  /-- Glyph origin y (baseline). -/
  ys : FloatArray := .empty
  /-- Glyph size relative to the font size. -/
  scales : FloatArray := .empty
  /-- Number of lines. -/
  lines : Nat := 0
  /-- Width of the widest line. -/
  width : Float := 0
  /-- Top of the block: largest ascender of the first line (after alignment). -/
  top : Float := 0
  /-- Bottom of the block: smallest descender of the last line (after alignment). -/
  bottom : Float := 0
  /-- Left edge of the block (after alignment). -/
  left : Float := 0
  /-- Baseline of the first line (after alignment). -/
  firstBaseline : Float := 0
  /-- Baseline of the last line (after alignment). -/
  lastBaseline : Float := 0
  /-- Line breaks as `x y scale` triples (after alignment): Makie keeps each `'\n'` in the
  glyph collection as an invisible glyph of the primary face, which counts towards the text
  bounding box (see `logicalBox`). -/
  breaks : FloatArray := .empty
  deriving Inhabited

/-- Align the lines of a finished builder. -/
def Builder.align (b : Builder) (font : Font) (opts : LayoutOptions) (halign : HAlign)
    (valign : VAlign) : GlyphRun :=
  let n := b.lineWidth.size
  if n == 0 then { font } else
  let maxW := maxWidth 0 n 0
  -- baselines: y₀ = 0, y_k = y_{k-1} - lineHeight_k
  let baselines := baselinesFrom 1 n 0 (FloatArray.emptyWithCapacity n |>.push 0)
  let yLast := baselines[n - 1]!
  let top := b.lineAsc[0]!
  let bottom := yLast + b.lineDesc[n - 1]!
  let shiftY := match valign with
    | .top => top
    | .bottom => bottom
    | .middle => 0.5 * (top + bottom)
    | .baseline => yLast
  let h := halignFrac halign
  let j := opts.justification.getD h
  let m := b.codes.size
  let (xs, ys) := place maxW h j baselines shiftY 0 0 m
    (FloatArray.emptyWithCapacity m) (FloatArray.emptyWithCapacity m)
  let breaks := placeBreaks maxW h j baselines shiftY 0 (b.breaks.size / 4) .empty
  { font, codes := b.codes, xs, ys, scales := b.scale, lines := n, width := maxW, breaks
    top := top - shiftY, bottom := bottom - shiftY, left := -h * maxW
    firstBaseline := -shiftY, lastBaseline := yLast - shiftY }
where
  maxWidth (i n : Nat) (acc : Float) : Float :=
    if i < n then
      let w := b.lineWidth[i]!
      maxWidth (i + 1) n (if w > acc then w else acc)
    else acc
  termination_by n - i
  baselinesFrom (k n : Nat) (y : Float) (acc : FloatArray) : FloatArray :=
    if k < n then
      let y := y - b.lineHeight[k]!
      baselinesFrom (k + 1) n y (acc.push y)
    else acc
  termination_by n - k
  /-- Place glyph `i` (in line `k`); returns final xs, ys. -/
  place (maxW h j : Float) (baselines : FloatArray) (shiftY : Float) (k i m : Nat)
      (xs ys : FloatArray) : FloatArray × FloatArray :=
    if i < m then
      -- advance k to the line containing glyph i
      let k := lineOf k i
      let dx := (maxW - b.lineWidth[k]!) * j - h * maxW
      let y := baselines[k]! + b.yOff[i]! - shiftY
      place maxW h j baselines shiftY k (i + 1) m (xs.push (b.penX[i]! + dx)) (ys.push y)
    else (xs, ys)
  termination_by m - i
  placeBreaks (maxW h j : Float) (baselines : FloatArray) (shiftY : Float) (i m : Nat)
      (out : FloatArray) : FloatArray :=
    if i < m then
      let k := (b.breaks[4 * i + 1]!).toUInt64.toNat
      let dx := (maxW - b.lineWidth[k]!) * j - h * maxW
      let x := b.breaks[4 * i]! + dx
      let y := baselines[k]! + b.breaks[4 * i + 2]! - shiftY
      placeBreaks maxW h j baselines shiftY (i + 1) m (((out.push x).push y).push b.breaks[4 * i + 3]!)
    else out
  termination_by m - i
  lineOf (k i : Nat) : Nat :=
    if h : k + 1 < b.lineStart.size then
      if b.lineStart[k + 1] ≤ i then lineOf (k + 1) i else k
    else k
  termination_by b.lineStart.size - k

/-- Shape and align a plain string (em units, y up, anchor at the origin). -/
def layout (font : Font) (s : String) (halign : HAlign := .left)
    (valign : VAlign := .baseline) (opts : LayoutOptions := {}) : GlyphRun :=
  (({} : Builder).pushString font opts s 0 1 |>.finish font opts).align font opts halign valign

/-! ## Rich text -/

/-- Rich text: Makie's `rich`/`subscript`/`superscript`/`subsup` without LaTeX. -/
inductive Rich where
  /-- Plain characters (may contain `'\n'`). -/
  | text (s : String)
  /-- Concatenation. -/
  | cat (parts : List Rich)
  /-- Subscript: 0.66 × size, baseline 0.25 × size lower. -/
  | sub (r : Rich)
  /-- Superscript: 0.66 × size, baseline 0.4 × size higher. -/
  | sup (r : Rich)
  /-- Subscript and superscript stacked at the same position (e.g. `xᵢ²` typeset as `x_i^2`). -/
  | subsup (sub sup : Rich)
  /-- A span scaled by `scale` with its baseline shifted by `dy` (in units of the enclosing
  size, up positive). -/
  | span (scale dy : Float) (r : Rich)
  deriving Inhabited

instance : Coe String Rich := ⟨.text⟩

namespace Rich

/-- Shape rich text into a builder at baseline offset `yoff` and size `scale`. -/
def shape (font : Font) (opts : LayoutOptions) : Rich → Float → Float → Builder → Builder
  | .text s, yoff, scale, b => b.pushString font opts s yoff scale
  | .cat [], _, _, b => b
  | .cat (r :: rs), yoff, scale, b => shape font opts (.cat rs) yoff scale (shape font opts r yoff scale b)
  | .sub r, yoff, scale, b => shape font opts r (yoff - 0.25 * scale) (0.66 * scale) b
  | .sup r, yoff, scale, b => shape font opts r (yoff + 0.4 * scale) (0.66 * scale) b
  | .span s dy r, yoff, scale, b => shape font opts r (yoff + dy * scale) (s * scale) b
  | .subsup lo hi, yoff, scale, b =>
    let x0 := b.x
    let b := shape font opts lo (yoff - 0.25 * scale) (0.66 * scale) b
    let x1 := b.x
    let b := shape font opts hi (yoff + 0.4 * scale) (0.66 * scale) { b with x := x0, prev := 0 }
    { b with x := if x1 > b.x then x1 else b.x, prev := 0 }

/-- Plain text of a rich label (sub/superscript markup dropped). -/
def toPlain : Rich → String
  | .text s => s
  | .cat [] => ""
  | .cat (r :: rs) => toPlain r ++ toPlain (.cat rs)
  | .sub r | .sup r | .span _ _ r => toPlain r
  | .subsup a b => toPlain a ++ toPlain b

end Rich

/-- Shape and align rich text (em units, y up, anchor at the origin). -/
def layoutRich (font : Font) (r : Rich) (halign : HAlign := .left)
    (valign : VAlign := .baseline) (opts : LayoutOptions := {}) : GlyphRun :=
  (Rich.shape font opts r 0 1 {} |>.finish font opts).align font opts halign valign

/-! ## Unicode sub/superscript forms -/

/-- Unicode subscript form of a character, if it has one (digits, `+ − = ( )`, and the
letters `a e h i j k l m n o p r s t u v x β γ ρ φ χ`). -/
def subscriptChar? (c : Char) : Option Char :=
  match c with
  | '0' => '₀' | '1' => '₁' | '2' => '₂' | '3' => '₃' | '4' => '₄'
  | '5' => '₅' | '6' => '₆' | '7' => '₇' | '8' => '₈' | '9' => '₉'
  | '+' => '₊' | '-' => '₋' | '−' => '₋' | '=' => '₌' | '(' => '₍' | ')' => '₎'
  | 'a' => 'ₐ' | 'e' => 'ₑ' | 'h' => 'ₕ' | 'i' => 'ᵢ' | 'j' => 'ⱼ' | 'k' => 'ₖ'
  | 'l' => 'ₗ' | 'm' => 'ₘ' | 'n' => 'ₙ' | 'o' => 'ₒ' | 'p' => 'ₚ' | 'r' => 'ᵣ'
  | 's' => 'ₛ' | 't' => 'ₜ' | 'u' => 'ᵤ' | 'v' => 'ᵥ' | 'x' => 'ₓ'
  | 'β' => 'ᵦ' | 'γ' => 'ᵧ' | 'ρ' => 'ᵨ' | 'φ' => 'ᵩ' | 'χ' => 'ᵪ'
  | _ => none

/-- Unicode superscript form of a character, if it has one (digits, `+ − = ( )`, all Latin
lowercase letters except `q`, most capitals, and `α β γ δ ε θ ι φ χ`). -/
def superscriptChar? (c : Char) : Option Char :=
  match c with
  | '0' => '⁰' | '1' => '¹' | '2' => '²' | '3' => '³' | '4' => '⁴'
  | '5' => '⁵' | '6' => '⁶' | '7' => '⁷' | '8' => '⁸' | '9' => '⁹'
  | '+' => '⁺' | '-' => '⁻' | '−' => '⁻' | '=' => '⁼' | '(' => '⁽' | ')' => '⁾'
  | 'a' => 'ᵃ' | 'b' => 'ᵇ' | 'c' => 'ᶜ' | 'd' => 'ᵈ' | 'e' => 'ᵉ' | 'f' => 'ᶠ'
  | 'g' => 'ᵍ' | 'h' => 'ʰ' | 'i' => 'ⁱ' | 'j' => 'ʲ' | 'k' => 'ᵏ' | 'l' => 'ˡ'
  | 'm' => 'ᵐ' | 'n' => 'ⁿ' | 'o' => 'ᵒ' | 'p' => 'ᵖ' | 'r' => 'ʳ' | 's' => 'ˢ'
  | 't' => 'ᵗ' | 'u' => 'ᵘ' | 'v' => 'ᵛ' | 'w' => 'ʷ' | 'x' => 'ˣ' | 'y' => 'ʸ'
  | 'z' => 'ᶻ'
  | 'A' => 'ᴬ' | 'B' => 'ᴮ' | 'D' => 'ᴰ' | 'E' => 'ᴱ' | 'G' => 'ᴳ' | 'H' => 'ᴴ'
  | 'I' => 'ᴵ' | 'J' => 'ᴶ' | 'K' => 'ᴷ' | 'L' => 'ᴸ' | 'M' => 'ᴹ' | 'N' => 'ᴺ'
  | 'O' => 'ᴼ' | 'P' => 'ᴾ' | 'R' => 'ᴿ' | 'T' => 'ᵀ' | 'U' => 'ᵁ' | 'V' => 'ⱽ'
  | 'W' => 'ᵂ'
  | 'α' => 'ᵅ' | 'β' => 'ᵝ' | 'γ' => 'ᵞ' | 'δ' => 'ᵟ' | 'ε' => 'ᵋ' | 'θ' => 'ᶿ'
  | 'ι' => 'ᶥ' | 'φ' => 'ᵠ' | 'χ' => 'ᵡ'
  | _ => none

/-- Map every character through `f`, failing if any character has no image. -/
def mapChars? (f : Char → Option Char) (s : String) : Option String :=
  s.foldl (fun acc c => acc.bind fun t => (f c).map t.push) (some "")

/-- `s` written with Unicode subscript characters, if every character has one. -/
def toSubscript? (s : String) : Option String := mapChars? subscriptChar? s

/-- `s` written with Unicode superscript characters, if every character has one. -/
def toSuperscript? (s : String) : Option String := mapChars? superscriptChar? s

/-- Subscript `s`: Unicode subscript glyphs when available (so the label stays plain text),
otherwise a rich `sub` span. -/
def Rich.subscript (s : String) : Rich :=
  match toSubscript? s with
  | some t => .text t
  | none => .sub (.text s)

/-- Superscript `s`: Unicode superscript glyphs when available, otherwise a rich `sup` span. -/
def Rich.superscript (s : String) : Rich :=
  match toSuperscript? s with
  | some t => .text t
  | none => .sup (.text s)

/-! ## Geometry of a run -/

/-- A box `left bottom right top` in em units, y up. -/
structure EmBox where
  /-- Smallest x. -/
  left : Float := 0
  /-- Smallest y. -/
  bottom : Float := 0
  /-- Largest x. -/
  right : Float := 0
  /-- Largest y. -/
  top : Float := 0
  deriving Inhabited, Repr, BEq

/-- Device-space axis-aligned bounds of an em-space box after scaling by `size`, rotating by
the angle with cosine `c` and sine `s` (counter-clockwise on screen) and anchoring at `(x, y)`
(y down). -/
def EmBox.toDeviceCS (e : EmBox) (size c s x y : Float) : Rect :=
  let px (u v : Float) : Float := x + size * (u * c - v * s)
  let py (u v : Float) : Float := y - size * (u * s + v * c)
  let xa := px e.left e.bottom
  let xb := px e.right e.bottom
  let xc := px e.right e.top
  let xd := px e.left e.top
  let ya := py e.left e.bottom
  let yb := py e.right e.bottom
  let yc := py e.right e.top
  let yd := py e.left e.top
  let x0 := min (min xa xb) (min xc xd)
  let x1 := max (max xa xb) (max xc xd)
  let y0 := min (min ya yb) (min yc yd)
  let y1 := max (max ya yb) (max yc yd)
  { x := x0, y := y0, w := x1 - x0, h := y1 - y0 }

/-- Device-space axis-aligned bounds of an em-space box after scaling by `size`, rotating
by `rotation` (counter-clockwise on screen) and anchoring at `(x, y)` (y down). -/
@[inline] def EmBox.toDevice (e : EmBox) (size rotation x y : Float) : Rect :=
  e.toDeviceCS size (Float.cos rotation) (Float.sin rotation) x y

/-- Convert an unrotated em box to a y-down pixel rectangle relative to the anchor. -/
@[inline] def EmBox.toRect (e : EmBox) (size : Float) : Rect :=
  { x := size * e.left, y := -size * e.top, w := size * (e.right - e.left), h := size * (e.top - e.bottom) }

namespace GlyphRun

/-- Number of glyphs. -/
@[inline] def size (r : GlyphRun) : Nat := r.codes.size

/-- Makie's text bounding box (`unchecked_boundingbox` with
`height_insensitive_boundingbox_with_advance`): the union over glyphs of
`[x, x + advance] × [y + descender, y + ascender]` (scaled per glyph), plus, for every line
break, the single point `(x, y + descender)` (Makie keeps `'\n'` as glyph index 0, whose box
it collapses to its origin corner). A missing character contributes its full `.notdef` box,
since it is drawn. Zero box for empty text. -/
def logicalBox (r : GlyphRun) : EmBox :=
  if r.codes.isEmpty && r.breaks.isEmpty then {} else
  let e := go 0 inf inf (-inf) (-inf)
  withBreaks 0 e.left e.bottom e.right e.top
where
  go (i : Nat) (l b rt t : Float) : EmBox :=
    if i < r.codes.size then
      let code := r.codes[i]!
      let face := r.font.faceOf code
      let k := r.scales[i]! / face.unitsPerEm
      let x := r.xs[i]!
      let y := r.ys[i]!
      let x1 := x + face.advance[code / 2]! * k
      let y0 := y + face.descender * k
      let y1 := y + face.ascender * k
      go (i + 1) (min l x) (min b y0) (max rt x1) (max t y1)
    else { left := l, bottom := b, right := rt, top := t }
  termination_by r.codes.size - i
  withBreaks (i : Nat) (l b rt t : Float) : EmBox :=
    if i + 2 < r.breaks.size then
      let x := r.breaks[i]!
      let y := r.breaks[i + 1]! + r.font.primary.descender * (r.breaks[i + 2]! / r.font.primary.unitsPerEm)
      withBreaks (i + 3) (min l x) (min b y) (max rt x) (max t y)
    else { left := l, bottom := b, right := rt, top := t }
  termination_by r.breaks.size - i

/-- Union of the glyphs' control boxes (the ink extent). Zero box when no glyph has ink. -/
def inkBox (r : GlyphRun) : EmBox :=
  go 0 inf inf (-inf) (-inf)
where
  go (i : Nat) (l b rt t : Float) : EmBox :=
    if i < r.codes.size then
      let code := r.codes[i]!
      let face := r.font.faceOf code
      let g := code / 2
      let k := r.scales[i]! / face.unitsPerEm
      let x0 := face.bbox[4 * g]!
      let y0 := face.bbox[4 * g + 1]!
      let x1 := face.bbox[4 * g + 2]!
      let y1 := face.bbox[4 * g + 3]!
      if x0 == x1 && y0 == y1 then go (i + 1) l b rt t
      else
        let x := r.xs[i]!
        let y := r.ys[i]!
        go (i + 1) (min l (x + x0 * k)) (min b (y + y0 * k)) (max rt (x + x1 * k)) (max t (y + y1 * k))
    else if l > rt then {} else { left := l, bottom := b, right := rt, top := t }
  termination_by r.codes.size - i

/-- Append the run's outlines to `p`, scaled to `size` pixels per em, rotated by `rotation`
(counter-clockwise on screen) and placed with the anchor at device point `(x, y)` (y down). -/
def appendPath (r : GlyphRun) (size rotation x y : Float) (p : Path) : Path :=
  let c := Float.cos rotation
  let s := Float.sin rotation
  glyphs c s 0 p
where
  glyphs (c s : Float) (i : Nat) (p : Path) : Path :=
    if i < r.codes.size then
      let code := r.codes[i]!
      let face := r.font.faceOf code
      let g := code / 2
      let ox := r.xs[i]!
      let oy := r.ys[i]!
      -- device = (x, y) + size · R(θ) · (u, v) with the y flip: X = x + size(u c − v s), Y = y − size(u s + v c)
      let k := size * r.scales[i]! / face.unitsPerEm
      let a := k * c
      let bb := k * s
      let x0 := x + size * (ox * c - oy * s)
      let y0 := y - size * (ox * s + oy * c)
      let v0 := face.verbStart[g]!
      let v1 := face.verbStart[g + 1]!
      let c0 := face.coordStart[g]!
      let c1 := face.coordStart[g + 1]!
      let verbs := face.verbs.copySlice v0 p.verbs p.verbs.size (v1 - v0)
      let coords := points face.coords a bb x0 y0 c0 c1 p.coords
      glyphs c s (i + 1) { verbs, coords }
    else p
  termination_by r.codes.size - i
  points (src : FloatArray) (a b x0 y0 : Float) (j stop : Nat) (out : FloatArray) : FloatArray :=
    if j + 1 < stop then
      let gx := src[j]!
      let gy := src[j + 1]!
      points src a b x0 y0 (j + 2) stop ((out.push (x0 + a * gx - b * gy)).push (y0 - b * gx - a * gy))
    else out
  termination_by stop - j

/-- The run's outlines as a device-space path (see `appendPath`). -/
@[inline] def toPath (r : GlyphRun) (size rotation x y : Float) : Path :=
  r.appendPath size rotation x y {}

end GlyphRun

/-! ## `TextStyle` entry points -/

/-- Lay out `s` with the font, alignment and options implied by a `TextStyle`. -/
@[inline] def layoutStyled (style : TextStyle) (s : String) (opts : LayoutOptions := {}) : GlyphRun :=
  layout (Font.ofStyle style) s style.halign style.valign opts

/-- Device-space outlines of `s` drawn with `style` anchored at `(x, y)`: honours size, bold,
horizontal/vertical alignment and rotation; y down. Fill it with `style.color` (nonzero rule). -/
def textPath (style : TextStyle) (s : String) (x y : Float) : Path :=
  (layoutStyled style s).toPath style.size style.rotation x y

/-- Device-space outlines of rich text drawn with `style` anchored at `(x, y)`. -/
def richPath (style : TextStyle) (r : Rich) (x y : Float) : Path :=
  (layoutRich (Font.ofStyle style) r style.halign style.valign).toPath style.size style.rotation x y

/-- Size of laid-out text in pixels (unrotated). -/
structure TextMetrics where
  /-- Width of the widest line (sum of advances). -/
  advance : Float
  /-- Height of the first line's ascender above its baseline. -/
  ascent : Float
  /-- Depth of the last line's descender below its baseline (positive). -/
  descent : Float
  /-- Distance from the top of the block to its bottom (`ascent + descent + line spacing`). -/
  height : Float
  /-- Number of lines. -/
  lines : Nat
  /-- Makie's text bounding box, relative to the anchor, y down. -/
  logical : Rect
  /-- Ink bounding box (glyph control boxes), relative to the anchor, y down. -/
  ink : Rect
  deriving Inhabited, Repr

/-- Measure `s` drawn with `style` (rotation ignored; see `textBounds`). -/
def measure (style : TextStyle) (s : String) : TextMetrics :=
  let run := layoutStyled style s
  let sz := style.size
  { advance := sz * run.width
    ascent := sz * (run.top - run.firstBaseline)
    descent := sz * (run.lastBaseline - run.bottom)
    height := sz * (run.top - run.bottom)
    lines := run.lines
    logical := run.logicalBox.toRect sz
    ink := run.inkBox.toRect sz }

/-- Device-space bounding box of a run drawn at `size`, `rotation`, anchored at `(x, y)`: the
union of the per-glyph Makie boxes (`[0, advance] × [descender, ascender]`, and the line-break
points of `logicalBox`), each rotated, so it is tight for any rotation (Makie's
`unchecked_boundingbox`). Empty text gives the empty rectangle at `(x, y)`. -/
def GlyphRun.deviceBounds (run : GlyphRun) (size rotation x y : Float) : Rect :=
  if run.codes.isEmpty && run.breaks.isEmpty then { x, y, w := 0, h := 0 } else
  let c := Float.cos rotation
  let s := Float.sin rotation
  let (x0, y0, x1, y1) := go c s 0 inf inf (-inf) (-inf)
  let (x0, y0, x1, y1) := goBreaks c s 0 x0 y0 x1 y1
  { x := x0, y := y0, w := x1 - x0, h := y1 - y0 }
where
  goBreaks (c s : Float) (i : Nat) (x0 y0 x1 y1 : Float) : Float × Float × Float × Float :=
    if i + 2 < run.breaks.size then
      let u := run.breaks[i]!
      let v := run.breaks[i + 1]! + run.font.primary.descender * (run.breaks[i + 2]! / run.font.primary.unitsPerEm)
      let px := x + size * (u * c - v * s)
      let py := y - size * (u * s + v * c)
      goBreaks c s (i + 3) (min x0 px) (min y0 py) (max x1 px) (max y1 py)
    else (x0, y0, x1, y1)
  termination_by run.breaks.size - i
  go (c s : Float) (i : Nat) (x0 y0 x1 y1 : Float) : Float × Float × Float × Float :=
    if i < run.codes.size then
      let code := run.codes[i]!
      let face := run.font.faceOf code
      let k := run.scales[i]! / face.unitsPerEm
      let e : EmBox := { left := run.xs[i]!, right := run.xs[i]! + face.advance[code / 2]! * k
                         bottom := run.ys[i]! + face.descender * k, top := run.ys[i]! + face.ascender * k }
      let r := e.toDeviceCS size c s x y
      go c s (i + 1) (min x0 r.x) (min y0 r.y) (max x1 (r.x + r.w)) (max y1 (r.y + r.h))
    else (x0, y0, x1, y1)
  termination_by run.codes.size - i

/-- Device-space bounding box of `s` drawn with `style` at `(x, y)` (see
`GlyphRun.deviceBounds`). This is what layout should reserve for a label. -/
def textBounds (style : TextStyle) (s : String) (x y : Float) : Rect :=
  (layoutStyled style s).deviceBounds style.size style.rotation x y

/-- Device-space ink bounds of `s` drawn with `style` at `(x, y)`: the rotated ink box. -/
def textInkBounds (style : TextStyle) (s : String) (x y : Float) : Rect :=
  (layoutStyled style s).inkBox.toDevice style.size style.rotation x y

end LeanPlot.Font
