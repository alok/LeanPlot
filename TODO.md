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

## Post-0.1 Roadmap

- [ ] Axis & legend labels support
- [ ] Log / linear scale toggle
- [ ] Additional chart types
  - [ ] Area
  - [ ] Bar
  - [ ] Scatter
- [ ] Interactive domain sliders / zooming
- [ ] `#plot` command macro (syntax sugar)
- [ ] Grammar-of-graphics style configuration record
  - [ ] `#plot` command macro (acts like #eval/#html)
  - [x] `ToFloat` typeclass + default instances (Float, Int, Rat, etc.)
  - [x] Generalise sampling helpers to accept `α` with `[ToFloat α]`
  - [ ] Define `ChartOptions` record (title?, legend?, axis?, tickSize?, colours?)
  - [ ] Provide combinator style DSL:
    - [ ] `withTitle : String → PlotSpec → PlotSpec`
    - [ ] `withLegend : LegendOpts → PlotSpec → PlotSpec`
    - [ ] `withAxis : AxisOpts → PlotSpec → PlotSpec`
  - [ ] Use partial application / function composition to layer styles (Haskell-esque)
  - [ ] Provide infix operator `|>` for forward application in this DSL
  - [ ] Provide default `ChartOptions` via `Default` instance
  - [ ] Update demos to use `#plot` and combinators 