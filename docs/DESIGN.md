# LeanPlot v0.2 design

LeanPlot is a zero-dependency plotting core for Lean 4. It renders figures to deterministic SVG and
anti-aliased PNG entirely in Lean, with no node, no browser and no VS Code, so plots can be built,
tested and committed from CI. Infoview widgets and Verso docs are optional sub-packages layered on top.
`docs/AUDIT.md` is the detailed survey and plan that this design follows (§8 especially). It includes
the Makie algorithms to port and the measured performance rules.

## Pipeline

```
user API / recipes ──► Figure (grid of Axis2 / Axis3 / Colorbar / Legend / Text blocks)
  ──► layout (sizes, autolimits, ticks, text metrics) ──► Scene = Array DrawOp  (device px)
  ──► backends: SVG writer (String) | Raster canvas (RGBA8) → PNG (DEFLATE)
```

* `LeanPlot/Scene/DrawOp.lean` is the **frozen contract** between the layers above and below.
  Changing it needs a design note here.
* Text is rendered as **glyph outlines** from an embedded font (TeX Gyre Heros, the Makie default
  face, GUST font licence) in *both* backends. Layout metrics are therefore exact, and SVG and PNG
  agree.
* Everything is pure until `Figure.save : Figure → System.FilePath → IO Unit`.

## Package layout

```
lakefile.toml                    package LeanPlot, NO [[require]]
LeanPlot/Scene/DrawOp.lean       IR (done)
LeanPlot/Core/*                  Num (formatting, LinRange), Color, Colormap(+Data), Scale, Ticks,
                                 Geometry (Vec2/3, Mat4, clipping, Axis3 camera), Data (Pts2/Pts3/Grid2/TriMesh)
LeanPlot/Font/*                  embedded glyph outlines + metrics, text → Path
LeanPlot/Backend/SVG.lean        Scene → SVG string (deterministic number formatting)
LeanPlot/Backend/Raster/*        RGBA8 canvas (size proof), AA scanline fill, stroker, image blit
LeanPlot/Backend/PNG.lean        CRC32, Adler32, filters, DEFLATE (LZ77 + Huffman), base64
LeanPlot/Figure/*                Figure, grid layout, Axis2, Axis3, Legend, Colorbar
LeanPlot/Recipes/*               lines, scatter, arrows, streamplot, heatmap, contour(f) (+labels), mesh,
                                 surface, wireframe, poly, text, band, volume (ray cast), volumeslices
LeanPlot/IO.lean                 save by extension (.svg/.png), frame sequences
LeanPlotTest/*                   golden runner (`lake test`): SVG byte goldens, PNG pixel goldens
                                 with tolerance, numeric goldens vs Makie oracle JSON
widgets/                         sub-package: #plot/#figure in the infoview (SVG via ProofWidgets)
docs/                            this file, AUDIT.md, gallery notes
```

## Rules

* Structure-of-arrays with `FloatArray`/`ByteArray` for bulk data. No `Array (Float × Float)` and no
  `Array RGBA` in hot paths.
* Hot loops (rasterization, fractals, streamline integration, marching squares) are tail-recursive
  with explicit Float accumulators. No `for … break` with `let mut` Floats (measured 8× slower).
* Buffers are threaded linearly (unique), so `set!` is in place.
* Size invariants are erased proof fields: `Canvas w h` has `data.size = 4*w*h`, and `Grid2 nx ny`
  has `z.size = nx*ny`.
* Makie parity: where an algorithm is Makie's (ticks, streamplot, contour levels, colormaps,
  autolimits), port it exactly and test it against JSON dumped by the Julia oracle.
* No global notation, no `_root_` defs, no elaboration-time file writes.
