import LeanPlot.Components
import LeanPlot.Scale
import LeanPlot.Palette

open Lean ProofWidgets Recharts LeanPlot.Components LeanPlot.Scale
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/-- Sample exponential growth function to showcase log scale -/
private def exponentialData (steps : Nat := 100) : Array Json :=
  (List.range (steps + 1)).toArray.map (fun i =>
    let x : Float := i.toFloat / 10.0  -- x from 0 to 10
    let y := Float.exp x  -- e^x for exponential growth
    json% {x: $(toJson x), y: $(toJson y)})

/-- Sample power law data for log-log plot -/
private def powerLawData (steps : Nat := 100) : Array Json :=
  (List.range steps).toArray.map (fun i =>
    let x : Float := (i + 1).toFloat  -- x from 1 to 100
    let y := x ^ 2.5  -- Power law: y = x^2.5
    json% {x: $(toJson x), y: $(toJson y)})

/-- Demo: Exponential growth on linear scale -/
def exponentialDemo : Html :=
  let data := sampleMany #[("exp", Float.exp)] 100 0 5
  mkLineChart data #[("exp", "#1f77b4")] 400 400

/-- Transform data for log scale display -/
private def transformDataForLogScale (data : Array Json) (scaleConfig : ScaleConfig) : Array Json :=
  data.map fun obj =>
    match obj with
    | Json.obj _ =>
      let xVal := match obj.getObjVal? "x" with
        | .ok (Json.num n) => n.toFloat
        | _ => 1.0
      let yVal := match obj.getObjVal? "y" with
        | .ok (Json.num n) => n.toFloat
        | _ => 1.0
      let xTransformed := transform scaleConfig.xScale xVal
      let yTransformed := transform scaleConfig.yScale yVal
      json% {x: $(toJson xTransformed), y: $(toJson yTransformed)}
    | _ => obj

/-- Demo: Exponential growth on log scale -/
def exponentialLogScale : Html :=
  let data := exponentialData
  let scaleConfig : ScaleConfig := { xScale := ScaleType.Linear, yScale := ScaleType.Logarithmic }
  let transformedData := transformDataForLogScale data scaleConfig
  mkLineChart transformedData #[("y", "#ff7f0e")] 400 400

/-- Demo: Power law on log-log scale appears linear -/
def powerLawLogLogScale : Html :=
  let data := powerLawData
  let scaleConfig : ScaleConfig := {
    xScale := ScaleType.Logarithmic,
    yScale := ScaleType.Logarithmic
  }
  let transformedData := transformDataForLogScale data scaleConfig
  mkLineChart transformedData #[("y", "#2ca02c")] 400 400

#html exponentialDemo
#html exponentialLogScale
#html powerLawLogLogScale

end LeanPlot.Demos
