import LeanPlot.GrammarOfGraphics
import LeanPlot.Core
import LeanPlot.Components
import LeanPlot.API
import ProofWidgets.Data.Html

open LeanPlot.GrammarOfGraphics
open LeanPlot
open Lean
open ProofWidgets  -- Add this for Html

namespace LeanPlot.Demos

/-- Demo: Simple line plot using the Grammar of Graphics DSL -/
def grammarLineDemo : Html :=
  let pb := plot (fun x => x * x)
    |> fun p => PlotBuilder.withTitle p "Quadratic Function"
    |> fun p => PlotBuilder.withSize p 500 400
    |> fun p => PlotBuilder.withLegend p true
  Render.render pb.build

/-- Demo: Combining multiple layers -/
def grammarMultiLayerDemo : Html :=
  -- Sample data for different functions
  let linearData := Components.sample (fun x => x) 100
  let quadraticData := Components.sample (fun x => x * x) 100
  let cubicData := Components.sample (fun x => x * x * x) 100

  let pb := PlotBuilder.new
    |> fun p => PlotBuilder.withTitle p "Multiple Functions"
    |> fun p => PlotBuilder.withSize p 600 400
    |> fun p => PlotBuilder.addLine p linearData "linear"
    |> fun p => PlotBuilder.addLine p quadraticData "quadratic"
    |> fun p => PlotBuilder.addLine p cubicData "cubic"
    |> fun p => PlotBuilder.withLegend p true

  Render.render pb.build

/-- Demo: Logarithmic scale -/
def grammarLogScaleDemo : Html :=
  let expData := Components.sample Float.exp 100 (some (0, 5))

  let pb := PlotBuilder.new
    |> fun p => PlotBuilder.withTitle p "Exponential Growth (Log Y-Scale)"
    |> fun p => PlotBuilder.addLine p expData "exp(x)"
    |> fun p => PlotBuilder.logY p 10.0
    |> fun p => PlotBuilder.withSize p 500 400

  Render.render pb.build

/-- Demo: Mixed geometries -/
def grammarMixedGeomDemo : Html :=
  -- Generate some sample points
  let points : Array (Float × Float) :=
    (List.range 10).toArray.map fun i =>
      let x := i.toFloat / 2
      (x, Float.sin (x * Float.pi / 5))

  let pointData := API.xyArrayToJson points
  let lineData := Components.sample (fun x => Float.sin (x * Float.pi / 5)) 100 (some (0, 4.5))

  let pb := PlotBuilder.new
    |> fun p => PlotBuilder.withTitle p "Sine Wave with Points"
    |> fun p => PlotBuilder.addLine p lineData "sine"
    |> fun p => PlotBuilder.addPoints p pointData "samples"
    |> fun p => PlotBuilder.withSize p 600 400

  Render.render pb.build

#html grammarLineDemo
#html grammarMultiLayerDemo
#html grammarLogScaleDemo
#html grammarMixedGeomDemo

end LeanPlot.Demos
