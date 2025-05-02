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

## Post-0.1 Roadmap (updated 2025-05-02:08:05 UTC)

### Ergonomics
- [x] Axis & legend labels support (`LeanPlot.Axis`, `mkLineChartWithLabels`)
- [x] Legend component wrapper (`LeanPlot.Legend`) + helper `mkLineChartFull`
- [ ] Auto colour palette
- [x] `#plot` command alias for `#html` (LeanPlot.PlotCommand)
- [ ] Auto domain inference

### More chart types
- [ ] Log / linear scale toggle
- [ ] Additional chart types
  - [ ] Area
  - [ ] Bar
  - [ ] Scatter
- [ ] Interactive domain sliders / zooming

### Grammar-of-graphics core
- [ ] `PlotSpec` record & combinators (`withTitle`, `withLegend`, `withAxis`, ...)
- [ ] Forward-application DSL (`|>`)
- [ ] Default instances & renderer

### Tooling & docs
- [ ] Justfile recipes
- [ ] Pre-commit lint hook
- [ ] CHANGELOG & README refresh 