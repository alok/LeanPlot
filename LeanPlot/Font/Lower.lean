import LeanPlot.Font.Layout

/-!
# Lowering text ops to paths

Backends draw text as filled glyph outlines so that SVG and PNG agree exactly. `lowerText`
rewrites every `DrawOp.text` of a scene into a `DrawOp.path` filled with the text colour
(nonzero rule), keeping its clip; all other ops are unchanged.
-/

namespace LeanPlot

namespace DrawOp

/-- Replace a `text` op by the filled path of its glyph outlines; other ops are unchanged. -/
def lowerText : DrawOp → DrawOp
  | .text x y s style clip =>
    .path (Font.textPath style s x y) (some { color := style.color, rule := .nonzero }) none clip
  | op => op

end DrawOp

namespace Scene

/-- Lower every `text` op to a filled path (see `DrawOp.lowerText`). -/
def lowerText (s : Scene) : Scene := { s with ops := s.ops.map DrawOp.lowerText }

end Scene

end LeanPlot
