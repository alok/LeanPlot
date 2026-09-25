import LeanPlotWidgets.Html
import LeanPlotWidgets.Figure
import LeanPlotWidgets.Plot

/-!
# LeanPlotWidgets

LeanPlot figures in the Lean infoview, through ProofWidgets:

* `#figure e` shows a `Figure`, `Scene`, `Axis2` or `Axis3` (or an `IO` action returning one);
* `#plot f`, `#plot f on a..b using n` draw functions and data as Makie's `lines` would;
* `Figure.toHtml` (and `Scene`/`Axis2`/`Axis3.toHtml`) give the `ProofWidgets.Html` to embed
  in other widgets; `#html fig` works too.

The picture is LeanPlot's own SVG (or PNG) output, the same bytes `Figure.save` writes. See
`docs/WIDGETS.md` in the repository.
-/
