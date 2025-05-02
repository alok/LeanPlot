import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts

/-! # LeanPlot core components

A tiny layer above ProofWidgets4 that lets us sample Lean
functions and visualise the result in Recharts with one call.
-/

open Lean ProofWidgets Recharts
open scoped ProofWidgets.Jsx

namespace LeanPlot.Components

/-- Sample one real-valued function uniformly on `[min,max]`. -/
@[inline] def sample (f : Float → Float) (steps : Nat := 200) (min : Float := 0) (max : Float := 1) : Array Json :=
  (List.range (steps.succ)).toArray.map fun i =>
    let x : Float := min + (max - min) * i.toFloat / steps.toFloat
    json% {x: $(toJson x), y: $(toJson (f x))}

/--
Sample several functions whose outputs will be stored under the given series names.
-/
@[inline] def sampleMany (fns : Array (String × (Float → Float))) (steps : Nat := 200) (min : Float := 0) (max : Float := 1) : Array Json :=
  (List.range (steps.succ)).toArray.map fun i =>
    let x : Float := min + (max - min) * i.toFloat / steps.toFloat
    let pairs : Array (String × Json) := Id.run do
      let mut arr := #[("x", toJson x)]
      for (name, f) in fns do
        arr := arr.push (name, toJson (f x))
      pure arr
    Json.mkObj pairs.toList

/--
Turn an array of JSON rows into a Recharts line chart.

`seriesStrokes` supplies a colour for each series; its order must match the order in `fns` used to create the data.
-/
@[inline] def mkLineChart (data : Array Json) (seriesStrokes : Array (String × String)) (w h : Nat := 400) : Html :=
  <LineChart width={w} height={h} data={data}>
    <XAxis dataKey?="x" />
    <YAxis />
    {... seriesStrokes.map (fun (name, colour) =>
      <Line type={LineType.monotone} dataKey={Json.str name} stroke={colour} dot?={some false} />)}
  </LineChart>

end LeanPlot.Components
