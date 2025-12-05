import LeanPlot.API
import LeanPlot.DSL

/-! # Invalid Data Demo

This demo shows how LeanPlot handles functions that produce invalid data
(NaN, Infinity) at certain points.
-/

open LeanPlot.API

-- Try to plot tan(x) which has infinities near π/2
-- Note: The plot may show warnings about invalid values
#plot (fun x => Float.tan x) using 200

-- Try to plot 1/x which has an infinity at 0
#plot (fun x => 1/x) using 100

-- Function that returns NaN at x=0
/-- Function that produces NaN at x=0 for testing -/
def funcWithNaN (x : Float) : Float :=
  if x == 0.0 then 0.0/0.0 else Float.sin x

-- Plot the function with NaN
-- LeanPlot should display a warning about invalid values
#plot funcWithNaN using 100
