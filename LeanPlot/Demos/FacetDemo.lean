import LeanPlot.Faceting
import LeanPlot.GrammarOfGraphics
import ProofWidgets.Component.HtmlDisplay -- for #html

/-! # LeanPlot.Demos.FacetDemo

Demonstrates the new faceting helper `LeanPlot.Faceting.facetGrid` by laying
out three simple line plots (linear, quadratic, cubic) in a 2×2 grid.
-/

open LeanPlot.Faceting
open LeanPlot.GrammarOfGraphics
open scoped ProofWidgets.Jsx

/-- `facetGrid` example with three elementary functions.  Render with:
```
#html demo
```
-/
def demo : ProofWidgets.Html :=
  let p1 := (plot (fun x : Float => x)).title "y = x" |> PlotBuilder.build
  let p2 := (plot (fun x : Float => x * x)).title "y = x²" |> PlotBuilder.build
  let p3 := (plot (fun x : Float => x * x * x)).title "y = x³" |> PlotBuilder.build
  facetGrid #[p1, p2, p3] (cols := 2)

#html demo
