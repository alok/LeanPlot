## [0.1.0] – 2025-05-01:20:15
### Added
- Initial roadmap and design plan for LeanPlot library
- Basic demo overlay of y=x and y=x^2 via Recharts in ProofWidgets

### Planned
- Abstract sampling helpers (`sample`, `sampleMany`)
- Chart builder `mkLineChart`
- Documentation updates 

## [0.1.1] – 2025-05-02:07:41
### Added
- `ToFloat` typeclass providing a uniform conversion **to `Float`** for numeric-like types (`Float`, `Nat`, `Int` instances implemented; `Rat` stubbed).
- Generalised `LeanPlot.Components.sample` and `sampleMany` to work with any codomain that has a `[ToFloat]` instance.

### Changed
- `LeanPlot.Demos.overlay` now relies on the `sampleMany` helper instead of a bespoke sampler.

### Breaking
- Call-sites of `sample` & `sampleMany` must supply functions returning a type with a `[ToFloat]` instance. Existing `Float` code is unaffected. 