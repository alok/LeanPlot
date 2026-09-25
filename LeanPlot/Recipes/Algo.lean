import LeanPlot.Recipes.Algo.F32
import LeanPlot.Recipes.Algo.Levels

/-!
# Recipe algorithms

Pure, Figure-independent ports of the Makie recipe algorithms LeanPlot needs to
reproduce Makie/CairoMakie plots. Outputs are `LeanPlot` data types
(`Pts2`, `Pts3`, `TriMesh`, …) or plain structure-of-arrays buffers.

* `LeanPlot.Recipes.Algo.F32`: binary32 emulation and Julia `Float32` ranges.
* `LeanPlot.Recipes.Algo.Levels`: contour/contourf level selection.
-/
