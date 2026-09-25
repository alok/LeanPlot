import LeanPlot.Font
import LeanPlot.Core.Format
import LeanPlot.Figure.Layout
import LeanPlot.Core.Geometry

/-!
# Text for layout

Labels are measured with the embedded fonts exactly as Makie measures them
(`boundingbox(text, :data)`, i.e. the union of the per-glyph boxes
`[x, x + advance] × [y + descender, y + ascender]`), and drawn as `DrawOp.text` (plain
strings) or as filled glyph paths (rich tick labels such as `10³` or `1.5×10⁻⁵`, whose
exponent is a Makie `superscript` span).
-/

namespace LeanPlot.FigText

open LeanPlot LeanPlot.Num

/-- The rich-text form of a tick label (`sup b e` = `rich(b, superscript(e))`). -/
def richOf : TickLabel → Font.Rich
  | .plain s => .text s
  | .sup b e => .cat [.text b, .sup (.text e)]

/-- Lay out a tick label with a text style's font and alignment. -/
def layoutLabel (style : TextStyle) (l : TickLabel) : Font.GlyphRun :=
  match l with
  | .plain s => Font.layoutStyled style s
  | .sup .. => Font.layoutRich (Font.Font.ofStyle style) (richOf l) style.halign style.valign

/-- Device-space bounds (y down) of a label anchored at `(x, y)`. -/
def labelBounds (style : TextStyle) (l : TickLabel) (x y : Float) : Rect :=
  (layoutLabel style l).deviceBounds style.size style.rotation x y

/-- Width and height of a plain string's Makie bounding box drawn with `style`. -/
def stringSize (style : TextStyle) (s : String) : Float × Float :=
  let r := (Font.layoutStyled style s).deviceBounds style.size style.rotation 0 0
  (r.w, r.h)

/-- Union of rectangles (none for an empty list). -/
def unionRects (rs : Array Rect) : Option Rect :=
  rs.foldl (init := none) fun acc r =>
    match acc with
    | none => some r
    | some a => some (Rect.union a r)

/-- A label as a draw op at device point `(x, y)` (y down): `text` for plain strings, a
filled glyph path for rich labels. -/
def labelOp (style : TextStyle) (l : TickLabel) (x y : Float) (clip : Option Rect := none) : DrawOp :=
  match l with
  | .plain s => .text x y s style clip
  | .sup .. =>
    .path ((layoutLabel style l).toPath style.size style.rotation x y)
      (some { color := style.color, rule := .nonzero }) none clip

/-- `true` for labels that draw nothing (Makie `iswhitespace`). -/
def isBlank (s : String) : Bool := s.all Char.isWhitespace

end LeanPlot.FigText
