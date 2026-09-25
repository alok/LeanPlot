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

end LeanPlot.Raster
