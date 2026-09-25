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
* Bézier flattening at 0.1 px tolerance (Cairo's default), with area
  compensation for chords (`Flatten.lean`).
* Stroker with butt/round/square caps, miter/round/bevel joins, miter limit
  and dashes (`Stroke.lean`).
* `segments` (per-segment colours), `triangles` (seamless Gouraud meshes),
  `image` (nearest/bilinear) and `text` (glyph-outline hook, currently a stub).
* `Scene.toCanvas`, `Scene.toPNG`, `Scene.writePNG`, `Canvas.toPNG`.

Text: pass the font module's outliner, `s.toCanvas LeanPlot.Font.textPath`
(it has exactly the `TextOutliner` type), or lower the text ops to filled
paths first with `(s.lowerText LeanPlot.Font.textPath).toCanvas` (which any
path-filling backend, SVG included, can then draw). The default outliner is a
stub that draws nothing.

Measured on Apple Silicon, v4.35.0-rc3 (`LeanPlotTest/Raster/Perf.lean`,
minimum of 3 runs; the machine was shared, so treat these as upper bounds):

| workload | time |
|---|---|
| 1000×1000, 10⁵-segment polyline stroke (miter / round joins) | 26 / 32 ms |
| 1000×1000, 10⁵-segment polyline stroke, dashed 6/3 | 21 ms |
| 1000×1000, 10⁵-segment random-walk stroke | 38 ms |
| 1000×1000, 10⁵-vertex band fill (area plot) | 11 ms |
| 1000×1000, 10⁵-vertex star (10⁵ overlapping spikes) | 116 ms |
| 800×600 typical plot (231 ops: grid, 5×1000-pt lines, 200 markers, heatmap) | 6.4 ms |
| 1000×1000, 10⁴ markers (fill + outline, one op each) | 76 ms |
| 1000×1000, 79k-triangle Gouraud mesh (200×200 grid) | 81–90 ms |
| 800×600 PNG encode (filters + DEFLATE) | 25 ms, 92 KB (Pillow level 6: 91.7 KB) |

Parity with Cairo (`LeanPlotTest/Raster/Cairo.lean`): mean |Δ| ≤ 0.13/255
per channel on the reference scenes.

Performance rules learned here (see `Raster/Num.lean`): never call
`Nat.toFloat` or `Nat.shiftLeft` in hot code (both hit GMP on this
toolchain); use named `K.*` constants for float literals in branches; never
let a closure capture a structure whose buffer you mutate.
-/
