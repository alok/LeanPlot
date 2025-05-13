# Changelog

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

## [0.1.2] – 2025-05-02:08:05

### Added

- `LeanPlot.Legend` exposing the Recharts `<Legend>` component.
- `mkLineChartFull` helper that embeds a legend automatically.
- `#plot` command (alias of `#html`) via `LeanPlot.PlotCommand`.
- Demo `LeanPlot.Demos.LinearDemo` now uses `#plot`.

### Changed

- Root `LeanPlot` module imports new `Plot` command.
- Updated TODO roadmap with ergonomics and grammar-of-graphics sections.

### Fixed

- N/A

## [0.1.3] – 2025-05-02:08:12

### Added

- New demo `LeanPlot.Demos.QuadraticDemo` (`y = x²`).
- New demo `LeanPlot.Demos.OverlayDemo` demonstrating multi-series overlay with legend.

### Changed

- `Gallery.md` line-chart checklist now ticks off linear and quadratic demos.

## [0.2.0] – 2025-05-03:09:41

### Added

- Tier-0 ergonomics layer: new module `LeanPlot.API` with zero-config helpers
  - `lineChart` – sample a `Float → β` function on `[0,1]` and render.
  - `scatterChart` – render array of points (first implementation via `mkScatterChart`).
- Core `LeanPlot.Components` now includes bindings for Recharts `ScatterChart`/`Scatter` and helper `mkScatterChart`.
- Library-wide defaults centralised in `LeanPlot.Constants` (`defaultW`, `defaultH`).

### Changed

- Palette helpers now expose `colourFor` for single series selection.

### Fixed

- None.

## [0.2.1] – 2025-05-03:10:25

### Changed

- Introduced `LeanPlot.Core` with the general `Render`, `Layer`, `Plot` abstractions.
- Added low-priority generic `HAdd` instance that overlays any `[ToPlot]` values via `Plot.overlay`.
- Migrated `LeanPlot.Algebra` to these abstractions, deleting duplicated `Render`/`CoeTC` and bespoke `HAdd`.
- `LinePlot` now only provides a `[ToLayer]` instance and inherits `+` overlay behaviour from the core instance.

## [0.2.2] – 2025-05-07:19:00

### Added

- **Warning Banner for Invalid Plot Data**: Charts now display a warning banner if the input data contains `NaN` or `Infinity` values. This helps users identify problematic data that might lead to incorrect or empty plots.
  - New `LeanPlot.WarningBanner` component for displaying HTML-based warnings.
  - New utility functions in `LeanPlot.Utils` (`isInvalidFloat`, `jsonDataHasInvalidFloats`) to detect invalid floating-point numbers in JSON data.
  - Chart generation functions in `LeanPlot.Components` (e.g., `mkLineChart`, `mkScatterChart`) now integrate these checks.
- New demo `LeanPlot.Demos.InvalidDataDemo.lean` to showcase the warning banner with functions producing `NaN`/`Infinity`.

### Changed

- N/A

### Fixed

- Corrected block comment syntax in `LeanPlot/Utils.lean` to resolve parsing errors.
- Adjusted `open` statements in `LeanPlot/Components.lean` to correctly access `jsonDataHasInvalidFloats` from `LeanPlot.Utils`.

## [0.2.3] – 2025-05-08:01:04

### Added

- Support for axis labels in high‐level `PlotSpec` renderer: `AxisSpec.label` is now passed through to Recharts.
- New `PlotSpec.addSeries` combinator to append additional series to an existing plot.

### Changed

- Corrected construction of `AxisProps` in `PlotSpec.render` so that `dataKey?` is wrapped in `some …` as required by the `Option` type.

### Fixed

- N/A

## [0.2.4] – 2025-05-08:15:49

### Added

- **Automatic Axis Labels:** `LeanPlot.Algebra.LinePlot.toHtml` now automatically labels the x-axis as "x". For single-series plots, the y-axis is labelled with the series name. Multi-series overlays keep axes unlabelled to avoid ambiguity.

### Changed

- Added `deriving Inhabited` to `LineSeries` and adjusted `LinePlot` rendering accordingly.

### Fixed

- N/A

## [0.3.0-alpha] - 2025-05-09:22:07

### Changed

- Renamed `SeriesSpec` to `LayerSpec` for clarity and to better reflect its role in a layered grammar of graphics approach.
- Renamed the `RenderSeries` typeclass to `RenderFragment` and its method `renderSeries` to `render`. `SeriesSpec` and `RenderSeries` are kept as deprecated `abbrev`s for backward compatibility.
- Added a dummy `RenderFragment AxisSpec` instance to demonstrate polymorphism of the rendering pipeline.
- Added `PlotSpec.overlay` (alias `stack`) and an `HAdd` instance so two `PlotSpec`s can be combined with the `+` operator.

## [0.3.1] – 2025-05-12:11:24 UTC

### Fixed

- Added `LegendComp` alias in `LeanPlot.Legend` so existing call-sites (`mkLineChartFull`) compile without change.
- Corrected default x-axis domain logic in `line`, `area`, and `lines` constructors to avoid using y-domain heuristic; default is now `[-1,1]` when unspecified, ensuring symmetric sampling for functions like `x²`.

### Changed

- LegendProps now supports `layout`, `verticalAlign`, and `align` optional fields for finer control over legend positioning.  Existing call‐sites remain unchanged as these options default to `none`.  (*)

### Added

- Documentation update in `TODO.md`

## [0.3.2] – 2025-05-12:13:27 UTC

### Fixed

- **Correct x-domain sampling in `Components.sample`:** The default sampling
  interval reverted to `[0,1]`, replacing an erroneous call to `autoDomain`
  which is meant for **y-axis** heuristics.  The previous behaviour caused
  the chart to sample x-values in the range of the function's *output*,
  leading to distorted or empty plots for functions with large magnitude.
  All Tier-0 helpers (`lineChart`, etc.) now behave as documented again.

### Changed

- Removed the silent dependency on `LeanPlot.AutoDomain` in
  `LeanPlot.Components`.  The heuristic is still available for future use
  but no longer misapplied.

## [0.3.3] – 2025-05-13:22:49 UTC

### Added

- **Compile‐time JSON key validation:** new `LeanPlot.AssertKeys` module introducing the `#assert_keys` command.
  This command evaluates a JSON expression at compile time and fails if the specified keys are missing, preventing malformed literal JSON from slipping into generated artifacts.

### Changed

- N/A

### Fixed

- Removed experimental `aliasA` command prototype to avoid syntax errors in `AssertKeys.lean`.