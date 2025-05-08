import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.Plot
import LeanPlot.Core
import LeanPlot
open LeanPlot.Components LeanPlot.Palette LeanPlot
open Lean ProofWidgets
open scoped ProofWidgets.Jsx

/-! # Operator playground demo – revised

React complained when stacking multiple full Recharts charts in one
panel.  The most reliable demonstration of an *overlay* is therefore to
build **one** chart that already contains every series we want to show.

Below we construct a three-series chart (id, sqr, sqrt) using
`sampleMany`.  Render it with `#plot multiSeries`. -/

namespace LeanPlot.OperatorDemo

private def multiSeries : Html :=
  mkLineChartFull
    (sampleMany #[
      ("id",   fun x => x),
      ("sqr",  fun x => x*x),
      ("sqrt", fun x => Float.sqrt x)])
    (autoColours #["id", "sqr", "sqrt"])

#plot multiSeries
#plot line (fun x => x^3)
end LeanPlot.OperatorDemo
