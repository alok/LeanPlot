# TODO for LeanPlot

## Release 0.1.0

- [x] Core library
  - [x] Create `LeanPlot.Components` module
    - [x] `sample` helper (single series)
    - [x] `sampleMany` helper (multi-series)
    - [x] `mkLineChart` builder
  - [x] Integrate new module in `LeanPlot.lean`
  - [x] Rewrite `LeanPlot/Demos/OverlayPlot.lean` to use helpers
  - [x] `lake build` passes
- [ ] Documentation
  - [ ] Fill precise timestamp in `CHANGELOG.md`
  - [ ] Expand `README.md` with usage examples
- [ ] Tooling
  - [ ] Provide Justfile recipes
  - [ ] Set up pre-commit git hook: `lake env lean --run Std.Tactic.Lint` (or similar)

## Post-0.1 Roadmap (updated 2025-05-23:01:39 EST)

### Ergonomics
- [x] Axis & legend labels support (`LeanPlot.Axis`, `mkLineChartWithLabels`)
- [x] Legend component wrapper (`LeanPlot.Legend`) + helper `mkLineChartFull`
- [x] Auto color palette
- [x] `#plot` command alias for `#html` (LeanPlot.PlotCommand)
- [x] Auto domain inference
- [ ] Auto axis labels from binder names (metaprogramming extraction of plotting function parameters)

### More chart types
- [x] Log / linear scale toggle
  - [x] Scale module with linear and logarithmic transforms
  - [x] Demo showing exponential/power law plots with log scales
- [x] Additional chart types
  - [x] Area
  - [x] Bar
  - [x] Scatter
  - [x] Mixed charts (ComposedChart)
- [x] Data transformations (Transform module)
  - [x] Various scale types (log, sqrt, power, symlog)
  - [x] Data normalization and standardization
  - [x] Moving average smoothing
- [x] Advanced plot composition (PlotComposition module)
  - [x] Grid layouts for multiple plots
  - [x] Vertical stacking with shared axes
  - [x] Y-scale normalization across plots
  - [x] Consistent color schemes
- [ ] Interactive domain sliders / zooming

### Grammar-of-graphics core
- [x] `PlotSpec` record & combinators (`withTitle`, `withLegend`, `withAxis`, ...)
- [x] Functional composition DSL (replaced builder pattern)
- [x] Grammar of Graphics module with clean API
- [x] Layer composition via `overlay`/`stack`
- [ ] Faceting support
- [ ] Statistical transformations

### Tooling & docs
- [ ] Justfile recipes
- [ ] Pre-commit lint hook
- [x] CHANGELOG & README refresh

## Next Steps
- [ ] Interactive features (tooltips, zoom, pan)
- [ ] Export to static images (SVG/PNG)
- [ ] Statistical layers (regression lines, confidence intervals)
- [ ] Time series support with date/time axes
- [ ] 3D plotting capabilities
- [ ] Animation support
- [ ] Integrate with Lean's proof visualization needs
- [ ] Heatmaps and contour plots
- [ ] Box plots and violin plots
- [ ] Network/graph visualizations
- [ ] Improved data resampling for overlay with different domains