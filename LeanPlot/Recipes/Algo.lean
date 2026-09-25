import LeanPlot.Recipes.Algo.F32
import LeanPlot.Recipes.Algo.Levels
import LeanPlot.Recipes.Algo.Streamplot
import LeanPlot.Recipes.Algo.Contour
import LeanPlot.Recipes.Algo.Isoband
import LeanPlot.Recipes.Algo.Arrows

/-!
# Recipe algorithms

Pure, Figure-independent ports of the Makie recipe algorithms LeanPlot needs to
reproduce Makie/CairoMakie plots. Outputs are `LeanPlot` data types
(`Pts2`, `Pts3`, `TriMesh`, …) or plain structure-of-arrays buffers.

* `LeanPlot.Recipes.Algo.F32`: binary32 emulation and Julia `Float32` ranges.
* `LeanPlot.Recipes.Algo.Levels`: contour/contourf level selection.
* `LeanPlot.Recipes.Algo.Stream`: Makie `streamplot_impl` (2D and 3D).
* `LeanPlot.Recipes.Algo.Contour`: Contour.jl marching squares and Makie `contourlines`.
* `LeanPlot.Recipes.Algo.Isoband`: the `isoband` library (filled bands), Makie
  `_group_polys` and the `contourf` recipe data.
* `LeanPlot.Recipes.Algo.Arrows`: `arrows2d`/`arrows3d` geometry and Cartan's
  `spacing`/`scaledarrows` scaling.
-/
