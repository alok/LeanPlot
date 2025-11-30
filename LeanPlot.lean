/-!
# LeanPlot

Interactive plotting for Lean 4.

## Main API

Import `LeanPlot.API` and `LeanPlot.DSL` for the primary plotting functions:
- `plot` - Single function plots
- `plotMany` - Multiple function comparison
- `scatter` - Scatter plots
- `bar` - Bar charts
- `#plot` - Convenient syntax for quick visualization

## Advanced Features

- `LeanPlot.GrammarOfGraphics` - Grammar of Graphics DSL
- `LeanPlot.PlotComposition` - Subplot grids and composition
- `LeanPlot.Transform` - Data transformations (log, sqrt, etc.)
- `LeanPlot.Faceting` - Small multiples layouts
- `LeanPlot.Debug` - PNG export utilities

## Demos

See `LeanPlot.Demos.*` for example usage.
-/

-- Core API (what users should import)
import LeanPlot.API
import LeanPlot.DSL
import LeanPlot.ToFloat

-- Plot specification system
import LeanPlot.Specification
import LeanPlot.Plot
import LeanPlot.Algebra

-- Components layer (Tier-1)
import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.Scale
import LeanPlot.Constants

-- Advanced features
import LeanPlot.GrammarOfGraphics
import LeanPlot.PlotComposition
import LeanPlot.Transform
import LeanPlot.Faceting

-- Utilities
import LeanPlot.Debug
import LeanPlot.Metaprogramming

-- Internal (re-exported for compatibility)
import LeanPlot.Series
import LeanPlot.Core
import LeanPlot.Axis
import LeanPlot.Axes
import LeanPlot.Legend
import LeanPlot.AutoDomain
import LeanPlot.Recharts
import LeanPlot.JsonExt
import LeanPlot.Utils
import LeanPlot.AssertKeys
import LeanPlot.WarningBanner
import LeanPlot.LegacyLayer
