import LeanPlot.Core.Num
import LeanPlot.Core.Decimal
import LeanPlot.Core.JuliaExp
import LeanPlot.Core.Range
import LeanPlot.Core.Format
import LeanPlot.Core.Ticks
import LeanPlot.Core.Scale

/-!
# LeanPlot core

Pure, dependency-free building blocks shared by layout, recipes and backends:

* `LeanPlot.Num`: IEEE helpers, Julia-exact rounding/powers/`exp`, exact decimal
  conversion (Ryu), Julia `range`/`LinRange`/`a:s:b`, the SVG number formatter
  and Makie tick-label formatting.
* `LeanPlot.Ticks`: `PlotUtils.optimize_ticks` (Wilkinson), log/decade/minor ticks.
* `LeanPlot.Scale`: Makie axis scales (identity, log10/log2/ln, sqrt,
  pseudolog10, Symlog10, logit).
-/
