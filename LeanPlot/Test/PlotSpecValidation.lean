import LeanPlot.Specification
import LeanPlot.Plot
import Lean

/-!
# PlotSpec Validation Tests

This file exercises the *built-in* compile-time validations that now run when
`PlotSpec`s are evaluated for rendering:

* every JSON row must contain all series `dataKey`s plus the x-axis key;
* every series must have a **unique** `name` and `dataKey`.

The positive tests below should compile without errors.  Negative examples that
ought to fail are provided as commented lines – uncommenting them should cause
`lake build` to emit the expected error.
-/

open LeanPlot
open LeanPlot.PlotSpec

namespace LeanPlot.Test.PlotSpecValidation

/-! Positive cases -/

/- Single-series line plot passes validation. -/
#plot (line (fun x : Float => x) (name := "id"))

/- Two distinct series with unique names / keys also pass. -/
#plot (
  lines #[
    ("id",   fun x : Float => x),
    ("sqr",  fun x : Float => x * x),
    ("cube", fun x : Float => x * x * x)
  ]
)

/-! Negative cases (kept **commented** so the test suite still builds)

Duplicate series names / keys

```
#plot (
  line (fun x : Float => x)   (name := "dup") +
  line (fun x : Float => x*x) (name := "dup")
)
```
Expect: `LeanPlot: duplicate series name or dataKey detected`.

Missing key in `chartData`

The following manually-crafted spec omits the `"y"` field – validation should
fail.

```
def badSpec : PlotSpec := {
  chartData := #[json% {x: 1}],
  series    := # [{ name := "y", dataKey := "y", color := "#ff0000" }],
  xAxis     := some { dataKey := "x" },
  legend    := false
}
#plot badSpec
```
Expect: `LeanPlot: chartData is missing required keys`.
-/

end LeanPlot.Test.PlotSpecValidation
