import LeanPlot.Scene.Style
import LeanPlot.Scene.Mark
import LeanPlot.Figure.Layout
import LeanPlot.Figure.Theme
import LeanPlot.Figure.Text
import LeanPlot.Figure.Lower
import LeanPlot.Figure.Legend
import LeanPlot.Figure.Axis2
import LeanPlot.Figure.Colorbar
import LeanPlot.Figure.Axis3
import LeanPlot.Figure.Figure

/-!
# Figures

The figure/layout layer of LeanPlot (design: `docs/AUDIT.md` §8.2), reproducing Makie's
default look and layout:

* `LeanPlot.Mark` (`Scene/Mark.lean`, `Scene/Style.lean`): plot primitives in data space
  with Makie's styles (line styles, the default marker map, colour mapping);
* `LeanPlot.Layout`: a port of GridLayoutBase (protrusions, `Auto`/`Fixed`/`Relative`/`Aspect`
  sizes, gaps, `Inside`/`Outside` alignment);
* `LeanPlot.Axis2` (Makie `Axis`: autolimits, scales, ticks, labels, grid, spines, aspect),
  `LeanPlot.Axis3` (camera, panels, grids, frame, ticks, labels, depth-sorted and shaded
  meshes), `LeanPlot.Colorbar`, `Legend` (`LegendBlock`, `AxisLegend`), `LabelBlock`;
* `LeanPlot.Figure`: blocks in a grid → `Scene` (`toScene`, `save`).

All defaults come from Makie 0.24 (dumped by `LeanPlotTest/oracle/figure/figure_oracle.jl`);
the layout is tested against Makie's solved layout numbers.
-/
