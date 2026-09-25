import LeanPlot.Core.Num
import LeanPlot.Core.Decimal
import LeanPlot.Core.JuliaExp
import LeanPlot.Core.Range
import LeanPlot.Core.Format
import LeanPlot.Core.Ticks
import LeanPlot.Core.Scale
import LeanPlot.Core.ColorNames
import LeanPlot.Core.Color
import LeanPlot.Core.ColormapData
import LeanPlot.Core.Colormap

/-!
# LeanPlot core

Pure, dependency-free building blocks shared by layout, recipes and backends:

* `LeanPlot.Num`: IEEE helpers, Julia-exact rounding/powers/`exp`, exact decimal
  conversion (Ryu), Julia `range`/`LinRange`/`a:s:b`, the SVG number formatter
  and Makie tick-label formatting.
* `LeanPlot.Ticks`: `PlotUtils.optimize_ticks` (Wilkinson), log/decade/minor ticks.
* `LeanPlot.Scale`: Makie axis scales (identity, log10/log2/ln, sqrt,
  pseudolog10, Symlog10, logit).
* `LeanPlot.RGBA` helpers (hex, names, N0f8/`RGBAf` rounding, blending, RGBA8
  packing), `LeanPlot.wongColors`, `LeanPlot.MakieTheme`.
* `LeanPlot.Colormap`: non-empty LUTs with Makie `interpolated_getindex`,
  `numbers_to_colors`, `resample_cmap`, and 91 built-in `to_colormap` tables.
-/
