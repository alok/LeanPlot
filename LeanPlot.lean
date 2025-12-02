/-!
# LeanPlot

Interactive plotting for Lean 4.

## Main API

Import `LeanPlot.Graphic` for first-class algebraic graphics:
- `plot f` - Create a function plot
- `scatter pts` - Scatter plot from points
- `bar pts` - Bar chart from points
- Algebraic operators: `+` (overlay), `|||` (horizontal facet), `/` (vertical facet)
- Fluent combinators: `.domain`, `.samples`, `.color`, `.title`, etc.

Import `LeanPlot.API` and `LeanPlot.DSL` for legacy plotting functions:
- `plotMany` - Multiple function comparison
- `#plot` - Convenient syntax for quick visualization

## PNG/SVG Export

Import `LeanPlot.Render.Export` for file export:
- `g.savePNG "path.png"` - Save graphic to PNG
- `g.saveSVG "path.svg"` - Save graphic to SVG

## Advanced Features

- `LeanPlot.GrammarOfGraphics` - Grammar of Graphics DSL
- `LeanPlot.Interactive` - Two-way slider widgets
- `LeanPlot.PlotComposition` - Subplot grids and composition
- `LeanPlot.Transform` - Data transformations (log, sqrt, etc.)
- `LeanPlot.Faceting` - Small multiples layouts

## Demos

See `LeanPlot.Demos.*` for example usage.
-/

-- Core API (what users should import)
import LeanPlot.Graphic  -- First-class algebraic graphics
import LeanPlot.Interactive  -- Two-way slider widgets
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

-- Rendering / Export
import LeanPlot.Render.Bitmap
import LeanPlot.Render.Rasterize
import LeanPlot.Render.PNG.CRC32
import LeanPlot.Render.PNG.Adler32
import LeanPlot.Render.PNG.Encode
import LeanPlot.Render.Export

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
