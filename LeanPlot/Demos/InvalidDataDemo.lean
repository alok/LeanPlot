import LeanPlot
import LeanPlot.Demos.Lib

open LeanPlot

-- Try to plot tan(x) which has infinities
#plot (
  lineChart (Float.tan) xsteps 200 domain=(-Float.pi, Float.pi)
    |> Plot.title "Tangent Function (with Infinities)"
    |> Plot.xLabel "x"
    |> Plot.yLabel "tan(x)"
)

-- Try to plot 1/x which has an infinity at 0
#plot (
  lineChart (fun x => 1/x) xsteps 100 domain=(-2, 2)
    |> Plot.title "Inverse Function (1/x)"
    |> Plot.xLabel "x"
    |> Plot.yLabel "1/x"
)

-- Try to plot 0/0 (NaN)
-- For this, we might need a scatter plot of explicit points if lineChart expects a function
-- Or a function that can return NaN for certain inputs.
-- Let's try a function that returns NaN at a specific point.
def funcWithNaN (x : Float) : Float :=
  if x == 0.0 then 0.0/0.0 else Float.sin x

#plot (
  lineChart funcWithNaN xsteps 100 domain=(-Float.pi, Float.pi)
    |> Plot.title "Function with NaN"
    |> Plot.xLabel "x"
    |> Plot.yLabel "f(x)"
)
