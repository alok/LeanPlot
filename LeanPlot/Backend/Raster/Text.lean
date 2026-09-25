import LeanPlot.Backend.Raster.Flatten

/-
Text hook for the raster backend.

Text reaches the raster backend as **glyph outlines**: the font module
(another agent's work) lays out a string with its embedded face and returns a
`Path` in device pixels, with alignment and rotation already applied. The
backend fills that path with the nonzero rule in `style.color`, so raster and
SVG text agree exactly.

Until the font module lands, `TextOutliner.none` returns an empty path and
**no text is drawn** (STUB). The integrator wires the real outliner by
passing it to `Scene.toCanvas (text := …)`, or by replacing
`defaultTextOutliner` once `LeanPlot.Font` is importable from here.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Lays out `s` anchored at `(x, y)` with `style` (size, alignment, rotation)
and returns its glyph outlines as a device-space path. -/
abbrev TextOutliner := TextStyle → String → Float → Float → Path

/-- STUB outliner: produces no geometry, so text is not drawn. -/
def TextOutliner.none : TextOutliner := fun _ _ _ _ => {}

/-- The outliner `Scene.toCanvas` uses by default.
STUB: `TextOutliner.none` until the font module is wired in (integration request). -/
def defaultTextOutliner : TextOutliner := TextOutliner.none

/-- Fill glyph outlines for `s` into the accumulator and resolve them with the
text colour. -/
def textInto {w h : Nat} (outline : TextOutliner) (acc : Accum w h) (cv : Canvas w h) (cl : Clip)
    (style : TextStyle) (s : String) (x y : Float) : Accum w h × Canvas w h :=
  if s.isEmpty || cl.isEmpty then (acc, cv) else
  let p := outline style s x y
  if p.verbs.isEmpty then (acc, cv) else
  let acc := acc.fillPolylines cl (flatten p)
  acc.sweepSolid cv cl .nonzero style.color

/-- Standalone text hook with the signature the integrator asked for
(`TextStyle → String → Float → Float → Canvas → Canvas`). It uses
`defaultTextOutliner`, so it is currently a STUB that draws nothing. It
allocates its own accumulator; inside a scene, `Scene.toCanvas` reuses one. -/
def renderText {w h : Nat} (style : TextStyle) (s : String) (x y : Float) (cv : Canvas w h) : Canvas w h :=
  let p := defaultTextOutliner style s x y
  if p.verbs.isEmpty then cv else
  (textInto defaultTextOutliner (Accum.new w h) cv (Clip.make w h none) style s x y).2

/-- The `path` op that draws `s` as filled glyph outlines (nonzero rule, text
colour, same clip); `none` when the outliner yields no geometry. -/
def lowerTextOp (outline : TextOutliner) (x y : Float) (s : String) (style : TextStyle) (clip : Option Rect) :
    Option DrawOp :=
  if s.isEmpty then none else
  let p := outline style s x y
  if p.verbs.isEmpty then none else
  some (.path p (some { color := style.color, rule := .nonzero }) none clip)

end LeanPlot.Raster

namespace LeanPlot.Scene

/-- Replace every `text` op by a `path` op that fills its glyph outlines, so
any backend that can fill paths draws the text (and SVG and raster output
agree exactly). Text for which `outline` yields no geometry is dropped. Other
ops keep their order. With the font module: `s.lowerText LeanPlot.Font.textPath`. -/
def lowerText (s : Scene) (outline : Raster.TextOutliner) : Scene :=
  { s with ops := s.ops.filterMap fun op =>
      match op with
      | .text x y str style clip => Raster.lowerTextOp outline x y str style clip
      | op => some op }

end LeanPlot.Scene
