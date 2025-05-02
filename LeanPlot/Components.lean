import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts
import LeanPlot.ToFloat
import LeanPlot.Axis
import LeanPlot.Legend

/-! # LeanPlot core components

A tiny layer above ProofWidgets4 that lets us sample Lean
functions and visualise the result in Recharts with one call.
-/

open Lean ProofWidgets
open ProofWidgets.Recharts (LineChart Line LineType)
open LeanPlot.Axis
open LeanPlot.Legend (Legend)
open scoped ProofWidgets.Jsx

namespace LeanPlot.Components

/-- Uniformly sample a function `f : Float → β` on the interval `[min,max]`.
`β` is required to have a `[ToFloat β]` instance so that values can be
converted to a JavaScript-friendly `Float` for serialisation. -/
@[inline] def sample {β} [ToFloat β]
  (f : Float → β) (steps : Nat := 200) (min : Float := 0) (max : Float := 1) : Array Json :=
  (List.range (steps.succ)).toArray.map fun i =>
    let x : Float := min + (max - min) * i.toFloat / steps.toFloat
    let y : β := f x
    json% {x: $(toJson x), y: $(toJson (toFloat y))}

/--
Sample several functions whose outputs will be stored under the given series names.
Each function must return a type with a `[ToFloat]` instance so that the
value can be serialised.
-/
@[inline] def sampleMany {β} [ToFloat β]
  (fns : Array (String × (Float → β))) (steps : Nat := 200) (min : Float := 0) (max : Float := 1) : Array Json :=
  (List.range (steps.succ)).toArray.map fun i =>
    let x : Float := min + (max - min) * i.toFloat / steps.toFloat
    let pairs : Array (String × Json) := Id.run do
      let mut arr := #[("x", toJson x)]
      for (name, f) in fns do
        arr := arr.push (name, toJson (toFloat (f x)))
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

/-- Like `mkLineChart` but allows setting axis labels. -/
@[inline] def mkLineChartWithLabels (data : Array Json)
    (seriesStrokes : Array (String × String))
    (xLabel? : Option String := none) (yLabel? : Option String := none)
    (w h : Nat := 400) : Html :=
  <LineChart width={w} height={h} data={data}>
    <XAxis dataKey?="x" label?={xLabel?} />
    <YAxis label?={yLabel?} />
    {... seriesStrokes.map (fun (name, colour) =>
      <Line type={LineType.monotone} dataKey={Json.str name} stroke={colour} dot?={some false} />)}
  </LineChart>

/--
`mkLineChartFull` extends `mkLineChartWithLabels` by also including a
Recharts `<Legend>` block so that each series is labelled with the name
provided to `sampleMany`.  This is optional but useful for multi-series
plots.
-/
@[inline] def mkLineChartFull (data : Array Json)
    (seriesStrokes : Array (String × String))
    (xLabel? : Option String := none) (yLabel? : Option String := none)
    (w h : Nat := 400) : Html :=
  <LineChart width={w} height={h} data={data}>
    <XAxis dataKey?="x" label?={xLabel?} />
    <YAxis label?={yLabel?} />
    <Legend />
    {...
      seriesStrokes.map (fun (name, colour) =>
        <Line type={LineType.monotone} dataKey={Json.str name} stroke={colour} dot?={some false} />)}
  </LineChart>

/-- Convenience wrapper of `sample` that fixes the interval to `[0,1]` – the
most common case in our demos.  Use this to avoid repeating `0 1` at call
sites. -/
@[inline] def sample01 {β} [ToFloat β]
  (f : Float → β) (steps : Nat := 200) : Array Json :=
  sample f steps 0 1

/-- Like `sampleMany` but with the domain fixed to `[0,1]`. -/
@[inline] def sampleMany01 {β} [ToFloat β]
  (fns : Array (String × (Float → β))) (steps : Nat := 200) : Array Json :=
  sampleMany fns steps 0 1

end LeanPlot.Components
