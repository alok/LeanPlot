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

## Post-0.1 Roadmap (updated 2025-05-23:01:20 EST)

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
- [ ] Additional chart types
  - [x] Area
  - [x] Bar
  - [x] Scatter
- [ ] Interactive domain sliders / zooming

### Grammar-of-graphics core
- [x] `PlotSpec` record & combinators (`withTitle`, `withLegend`, `withAxis`, ...)
- [x] Builder pattern DSL (`PlotBuilder`)
- [x] Grammar of Graphics module with fluent API
- [ ] More sophisticated layer composition
- [ ] Faceting support

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