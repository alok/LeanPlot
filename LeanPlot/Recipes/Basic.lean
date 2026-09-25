import LeanPlot.Recipes.Basic.Common
import LeanPlot.Recipes.Basic.Axis2
import LeanPlot.Recipes.Basic.Axis3
import LeanPlot.Recipes.Basic.Figure

/-!
# Basic recipes: the user API

Functional builders over `Axis2`, `Axis3` and `Figure` mirroring Makie's plotting functions
(`lines`, `scatter`, `linesegments`, `band`, `poly`, `text`, `heatmap`, `image`, `mesh`,
`surface`, `wireframe`, `arrows2d`/`arrows3d`, `hlines`/`vlines`, `axislegend`, `limits`)
plus figure placement (`Figure.axis`, `colorbar`, `legend`, `label`) and one-liners
(`LeanPlot.Quick`). Inputs are plain `Core.Data` values, so algorithmic recipes (contours,
streamlines, arrow fields) feed their output straight in: NaN-separated `Pts2` into
`lines`, per-vertex values into `linesColored`, polygons into `polys`.

```lean
open LeanPlot in
#eval (Figure.new |>.axis 1 1 (Axis2.new (title := "sin") |>.linesFn Float.sin (Num.range 0 10 101))).save "sin.svg"
```
-/
