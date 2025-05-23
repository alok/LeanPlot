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

## [0.2.0] – 2025-05-23

### Added
- Log/linear scale support via `LeanPlot.Scale` module
  - `ScaleType` for linear and logarithmic scales with customizable base
  - `ScaleConfig` for configuring x and y axis scales
  - Transform functions for applying scale transformations
  - Demo: `LogScaleDemo.lean` showcasing exponential and power law plots

- Grammar of Graphics DSL via `LeanPlot.GrammarOfGraphics` module
  - Builder pattern for composing plots with fluent API
  - Support for layers with different geometries (Point, Line, Bar, Area)
  - Methods: `withTitle`, `withSize`, `withLegend`, `addLine`, `addPoints`, etc.
  - Scale configuration methods: `logX`, `logY`
  - Demo: `GrammarDemo.lean` showcasing various DSL features

### Documentation
- Added comprehensive doc strings for Scale module
- Added documentation for Grammar of Graphics DSL

## [0.2.1] – 2025-05-03:10:25

### Changed

- Introduced `LeanPlot.Core` with the general `Render`, `Layer`, `Plot` abstractions.
- Added low-priority generic `HAdd` instance that overlays any `[ToPlot]` values via `Plot.overlay`.
- Migrated `LeanPlot.Algebra` to these abstractions, deleting duplicated `Render`/`CoeTC` and bespoke `HAdd`.
- `LinePlot` now only provides a `[ToLayer]` instance and inherits `+` overlay behavior from the core instance.

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
  which is meant for **y-axis** heuristics.  The previous behavior caused
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

## [0.3.4] – 2025-05-19:15:32

### Changed

- Temporarily commented out the `#assert_keys` macro and its elaborator in `LeanPlot.AssertKeys` due to a persistent quasiquotation parsing issue. This ensures project build stability. The feature is not currently used elsewhere and can be revisited.
- Updated `.gitignore` to include `.DS_Store` files.

### Fixed

- Removed an empty, untracked directory `LeanPlot/LeanPlot/Demos/` from the repository structure (no functional change as it was empty and likely already ignored or removed from git cache).

## [0.3.5] – 2025-05-22:07:16

### Added

- **Compile-time JSON Key Assertion Macro (`#assert_keys`)**: Re-introduced and finalized the `#assert_keys` macro in `LeanPlot.JsonExt` (formerly `LeanPlot.AssertKeys`). This macro allows compile-time verification that a given `Json` term contains a specified set of keys.
  - Includes a `Decidable` instance for `LeanPlot.HasKeys` to support compile-time evaluation.
  - Test suite `LeanPlot.Test.JsonKeyCheck` added to `lakefile.toml` and `Justfile` to verify macro functionality.

### Changed

- The module `LeanPlot.AssertKeys` was consolidated into `LeanPlot.JsonExt`.

### Fixed

- Resolved several compilation and linker errors related to the `#assert_keys` macro implementation and its test executable, including incorrect type checks, missing `main` function in tests, and linter warnings.

## [v0.4.1] - 2025-05-23:01:39

### Added
- Support for mixed chart types using ProofWidgets' ComposedChart
- New demo: `MixedChartDemo` showing bars and lines in a single plot

### Changed
- **BREAKING**: Refactored Grammar of Graphics DSL to use functional composition instead of builder pattern
  - Removed `PlotBuilder`, `Aesthetic`, `Geom`, and `Layer` structures
  - New API uses simple functions with partial application
  - More idiomatic Lean code leveraging function composition
- Improved architecture by reducing code duplication across modules

### Fixed
- Mixed chart types now properly render using ComposedChart instead of fallback to LineChart

## [v0.4.0] - 2025-05-23:01:20

## [Unreleased]

### Added
- **Data Transformation Module (`LeanPlot.Transform`)**: New module providing utilities for transforming plot data (2025-01-07)
  - Scale transformations: `linearScale`, `logScale`, `sqrtScale`, `powerScale`, `symlogScale`
  - Data normalization and standardization functions
  - Moving average smoothing
  - Transform functions for applying scales to plot functions
  - Demo: `TransformDemo.lean` showcasing various transformations
- Grammar of Graphics DSL for functional plot composition (2025-05-22:23:51)
- Log scale support for axes (2025-05-22:22:46)
- Comprehensive plotting demos showcase various chart types
- New `PlotSpec.addLine`, `PlotSpec.addScatter`, `PlotSpec.addBar` methods for layer composition (2025-05-23:02:08)

### Changed
- Refactored DSL to use pure functions instead of builder pattern (2025-05-23:00:42)
- Consolidated duplicate plot functions into single definitions (2025-05-23:01:14)
- Improved `PlotSpec.addLine` to properly merge data for mixed charts (2025-05-23:02:08)
  - Now correctly handles data integration when overlaying line on bar charts
  - Converts `Except` results to `Option` for Json number extraction

### Fixed
- Mixed chart data validation errors when combining different plot types (2025-05-23:02:08)

## [0.3.6] – 2025-05-23:09:51

### Added

- **Faceting support** via new `LeanPlot.Faceting` module
  - `facetGrid` helper lays out an `Array PlotSpec` in an HTML grid
  - `facetGridNamed` variant adds per-facet captions
- New demo `LeanPlot.Demos.FacetDemo` showcasing a 2×2 facet grid.

### Changed

- Removed unused variables (`rows`, `forecast`) and stray directories, cleaning up incidental leftovers across the codebase.

### Documentation

- Updated README feature list and demo gallery.
- Marked Faceting as complete in `TODO.md`.