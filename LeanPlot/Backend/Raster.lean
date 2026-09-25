import LeanPlot.Backend.Raster.Canvas
import LeanPlot.Backend.Raster.Coverage
import LeanPlot.Backend.Raster.Flatten
import LeanPlot.Backend.Raster.Stroke
import LeanPlot.Backend.Raster.Segments
import LeanPlot.Backend.Raster.Triangles
import LeanPlot.Backend.Raster.Image
import LeanPlot.Backend.Raster.Text
import LeanPlot.Backend.Raster.Render
import LeanPlot.Backend.Raster.Export
import LeanPlot.Backend.PNG

/-!
# Raster backend

`Scene` → anti-aliased RGBA8 `Canvas` → PNG, with no dependencies.

* `Canvas w h`: straight-alpha RGBA8 with an erased size proof, src-over
  blending (`Canvas.lean`).
* Exact-area coverage accumulation with nonzero and even-odd rules, sparse
  row spans, and antialiased clipping (`Coverage.lean`).
* Bézier flattening at 0.25 px tolerance (`Flatten.lean`).
* Stroker with butt/round/square caps, miter/round/bevel joins, miter limit
  and dashes (`Stroke.lean`).
* `segments` (per-segment colours), `triangles` (seamless Gouraud meshes),
  `image` (nearest/bilinear) and `text` (glyph-outline hook, currently a stub).
* `Scene.toCanvas`, `Scene.toPNG`, `Scene.writePNG`, `Canvas.toPNG`.
-/
