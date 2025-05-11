import LeanPlot
import LeanPlot.Utils -- For Float.pi if not already available

open LeanPlot

-- Test Case 1: Simple Line Plot (y = x^2)
def parabolaSpec : PlotSpec :=
  line (fun x => x*x) (name := "y=x²")

#plot parabolaSpec

-- Test Case 2: Simple Scatter Plot
def scatterPointsSpec : PlotSpec :=
  scatter #[ (0,0), (1,1), (2,4), (3,9) ] (name := "Simple Points")

#plot scatterPointsSpec

-- Test Case 3: Customized Line Plot (y = sin x)
def sineWaveSpec : PlotSpec :=
  let pi := LeanPlot.Utils.Float.pi -- Assuming pi is in LeanPlot.Utils or accessible
  (line (fun x => Float.sin x) (name := "sin(x)") (domainOpt := some (0, 2 * pi)) (color := some "#0074D9"))
    |> PlotSpec.withTitle "Sine Wave"
    |> PlotSpec.withXLabel "Angle (radians)"
    |> PlotSpec.withYLabel "Value"
    |> fun spec => { spec with series := spec.series.map (fun s => { s with dot := some false }) } -- No dots
    |> PlotSpec.withLegend false -- Explicitly hide legend

#plot sineWaveSpec

-- Test Case 4: Line plot with custom domain and y-axis label only
def customLineSpec : PlotSpec :=
  (line (fun x => 0.5 * x - 1) (name := "linear") (domainOpt := some (-5, 5)))
    |> PlotSpec.withYLabel "Transformed Value"

#plot customLineSpec

-- Test Case 5: Scatter plot with a title
def titledScatterSpec : PlotSpec :=
  (scatter #[(-1, 2), (0, 1), (1, 2), (2, 5)] (name := "V-Shape"))
    |> PlotSpec.withTitle "Scatter with Title"

#plot titledScatterSpec
