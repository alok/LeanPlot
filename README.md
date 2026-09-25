# LeanPlot

Plotting for Lean 4, written entirely in Lean with no dependencies. A figure is laid out and
rendered to deterministic **SVG** and anti-aliased **PNG** by Lean code alone: no node, no browser,
no Cairo. Layout, ticks, colormaps, fonts and plot recipes follow [Makie](https://docs.makie.org)
and are tested against it. On a suite of reference figures, the solved axis rectangles match
CairoMakie's exactly, and the rendered pixels agree to within 1/255 on average.

> v0.2 is a ground-up rewrite. The previous Recharts/infoview-widget implementation is preserved
> on the [`archive/v0-recharts`](https://github.com/alok/LeanPlot/tree/archive/v0-recharts) branch.

## What's in the box

* **Figures**: a Makie-style grid layout (GridLayoutBase port) with `Axis2`, `Axis3` (Makie's
  camera, 3D frame and ticks), `Legend`, `Colorbar`, labels, and Makie's default theme values.
* **Recipes**: lines (plain and colour-mapped), scatter with marker shapes, band, poly, text,
  heatmap and image, mesh, surface and wireframe, arrows (2D/3D), streamplot, contour and contourf,
  isosurfaces, voxels and volume slices. The algorithms are ports of Makie's, checked against the
  Julia oracle, most of them bit for bit.
* **Core**: exact Wilkinson ticks, scales (log, sqrt, symlog, …), colormaps (viridis, magma,
  inferno, plasma, cividis, turbo, RdBu, …), colour parsing, and Julia `LinRange` semantics.
* **Text**: the TeX Gyre Heros face Makie uses, plus a DejaVu Sans fallback, embedded as glyph
  outlines. Both backends draw text as paths, so SVG and PNG agree and layout metrics are exact.
  Unicode sub/superscripts and math symbols work (`v₁∧v₂ = v₁₂`, `∞ ∅ ∂ ∇`).
* **Backends**: a byte-stable SVG writer, and an anti-aliased coverage rasterizer with a stroker,
  Gouraud-shaded triangles and real DEFLATE PNG encoding.

## Build and test

```bash
lake build
lake test          # 48k checks: fonts vs fontTools, rasterizer vs Cairo, core/figure/recipes vs Makie
```

Toolchain: `leanprover/lean4:v4.35.0-rc3`. Save a figure with `Scene.save "plot.svg"` or `"plot.png"`
(the extension chooses the backend).

## Design

See [`docs/DESIGN.md`](docs/DESIGN.md) for the pipeline (Figure → layout → device-space `DrawOp`
scene → backends) and [`docs/AUDIT.md`](docs/AUDIT.md) for the plan and performance rules.
LeanPlot is co-developed with [Grassmann.lean](https://github.com/alok/Grassmann.jl), a Lean port
of Michael Reed's Grassmann.jl ecosystem, whose Makie plots it reproduces.
