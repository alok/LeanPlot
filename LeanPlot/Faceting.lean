import LeanPlot.PlotComposition
import LeanPlot.Specification
import ProofWidgets.Component.HtmlDisplay
import Lean.Data.Json

/-! # LeanPlot.Faceting

This module adds **faceting support** – a small but useful subset of the
Grammar-of-Graphics concept whereby multiple small plots are laid out in a
regular grid for side-by-side comparison.

The implementation is intentionally minimal and piggy-backs on the existing
`LeanPlot.PlotComposition.gridLayout` helper.  A more sophisticated version
could integrate automatic grouping of data, shared axis scales, etc.  For now
we expose two convenience wrappers:

* `facetGrid` – accept an `Array PlotSpec` and render them in an `Html` grid;
* `facetGridNamed` – like `facetGrid` but takes `(title, PlotSpec)` pairs and
  renders the title above each subplot.

Both functions delegate the heavy lifting to `PlotComposition.gridLayout`
so that plots remain interactive and benefit from the same validation logic
as standalone charts.
-/

namespace LeanPlot.Faceting

open LeanPlot
open Lean
open ProofWidgets
open scoped ProofWidgets.Jsx -- for JSX syntax

/-- Lay out multiple plots in a grid with the given number of `cols` columns.
Each `PlotSpec` is rendered with `PlotSpec.render`.  The function returns a
single `Html` node that you can embed in the infoview via `#html` or `#plot`.

Example:
```lean
open LeanPlot
open LeanPlot.GrammarOfGraphics
open LeanPlot.Faceting

#html facetGrid #[
  plot (fun x : Float => x) |> PlotBuilder.build,
  plot (fun x => x * x)     |> PlotBuilder.build,
  plot (fun x => x * x * x) |> PlotBuilder.build
] (cols := 2)
```
-/
@[inline] def facetGrid (plots : Array PlotSpec) (cols : Nat := 2) : Html :=
  LeanPlot.PlotComposition.gridLayout plots cols

/-- Like `facetGrid` but each subplot is paired with a `title` string that is
rendered above the individual chart.  Useful when the facets need captions.
-/
@[inline] def facetGridNamed (plots : Array (String × PlotSpec)) (cols : Nat := 2) : Html :=
  let htmlCells := plots.map fun (title, spec) =>
    (<div>
      <h5>{Html.text title}</h5>
      {PlotSpec.render spec}
    </div> : Html)
  -- Re-use grid layout style from `PlotComposition.gridLayout` but inline here
  let gridStyle : Json := Json.mkObj [
    ("display",             Json.str "grid"),
    ("gridTemplateColumns", Json.str s!"repeat({cols}, 1fr)"),
    ("gap",                 Json.str "10px")
  ]
  Html.element "div" #[("style", gridStyle)] htmlCells

end LeanPlot.Faceting
