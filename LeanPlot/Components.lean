import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts
import LeanPlot.ToFloat
import LeanPlot.Axis
import LeanPlot.Legend
import LeanPlot.Palette
import LeanPlot.Utils
import LeanPlot.WarningBanner
import LeanPlot.AutoDomain

/-! # LeanPlot core components

A tiny layer above ProofWidgets4 that lets us sample Lean
functions and visualise the result in Recharts with one call.
-/

open Lean ProofWidgets
open ProofWidgets.Recharts (LineChart Line LineType)
open LeanPlot.Axis
open LeanPlot.Legend (LegendComp)
open LeanPlot
open LeanPlot.Utils
open scoped ProofWidgets.Jsx
open LeanPlot.AutoDomain

namespace LeanPlot.Components

/-- Uniformly sample a function `f : Float → β` on the interval `[min,max]` or an auto-detected domain.
`β` is required to have a `[ToFloat β]` instance so that values can be
converted to a JavaScript-friendly `Float` for serialisation. -/
@[inline] def sample {β} [ToFloat β]
  (f : Float → β) (steps : Nat := 200) (domainOpt : Option (Float × Float) := none) : Array Json :=
  let (minVal, maxVal) : Float × Float := match domainOpt with
    | some (minD, maxD) => (minD, maxD)
    | none =>
      if steps == 0 then (0.0, 1.0) -- Default for zero steps if autoDomain isn't ideal
      else autoDomain f steps
  if steps == 0 then
    #[] -- Return empty array if steps is zero
  else
    (List.range (steps.succ)).toArray.map fun i =>
      let x : Float := minVal + (maxVal - minVal) * i.toFloat / steps.toFloat
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

`seriesStrokes` supplies a color for each series; its order must match the order in `fns` used to create the data.
-/
@[inline] def mkLineChart (data : Array Json) (seriesStrokes : Array (String × String)) (w h : Nat := 400) : Html :=
  let chartHtml :=
    <LineChart width={w} height={h} data={data}>
      <XAxis dataKey?="x" />
      <YAxis />
      {... seriesStrokes.map (fun (name, color) =>
        <Line type={LineType.monotone} dataKey={Json.str name} stroke={color} dot?={some false} />)}
    </LineChart>

  let keysToCheck := seriesStrokes.map (fun (name, _) => name) |>.push "x"
  if jsonDataHasInvalidFloats data keysToCheck then
    let warningProps : WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := WarningBanner warningProps
    .element "div" #[] #[warningHtml, chartHtml]
  else
    chartHtml

/-- Like `mkLineChart` but allows setting axis labels. -/
@[inline] def mkLineChartWithLabels (data : Array Json)
    (seriesStrokes : Array (String × String))
    (xLabel? : Option String := none) (yLabel? : Option String := none)
    (w h : Nat := 400) : Html :=
  let chartHtml :=
    <LineChart width={w} height={h} data={data}>
      <XAxis dataKey?="x" label?={xLabel?} />
      <YAxis label?={yLabel?} />
      {... seriesStrokes.map (fun (name, color) =>
        <Line type={LineType.monotone} dataKey={Json.str name} stroke={color} dot?={some false} />)}
    </LineChart>

  let keysToCheck := seriesStrokes.map (fun (name, _) => name) |>.push "x"
  if jsonDataHasInvalidFloats data keysToCheck then
    let warningProps : WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := WarningBanner warningProps
    .element "div" #[] #[warningHtml, chartHtml]
  else
    chartHtml

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
  let chartHtml :=
    <LineChart width={w} height={h} data={data}>
      <XAxis dataKey?="x" label?={xLabel?} />
      <YAxis label?={yLabel?} />
      <LegendComp />
      {...
        seriesStrokes.map (fun (name, color) =>
          <Line type={LineType.monotone} dataKey={Json.str name} stroke={color} dot?={some false} />)}
    </LineChart>

  let keysToCheck := seriesStrokes.map (fun (name, _) => name) |>.push "x"
  if jsonDataHasInvalidFloats data keysToCheck then
    let warningProps : WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := WarningBanner warningProps
    .element "div" #[] #[warningHtml, chartHtml]
  else
    chartHtml

/-- Props for a Recharts `<ScatterChart>` wrapped in Lean.  We intentionally
keep this minimal, exposing only what the Tier-0 helpers require. -/
structure ScatterChartProps where
  /-- Width of the SVG container in pixels. -/
  width  : Nat
  /-- Height of the SVG container in pixels. -/
  height : Nat
  /-- Array of JSON rows each containing at least `x` and `y` fields. -/
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts `<ScatterChart>`.
We delegate to the same JavaScript bundle used by the other Recharts
components that ship with ProofWidgets. -/
@[inline] def ScatterChart : ProofWidgets.Component ScatterChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "ScatterChart"

/-- Props for a Recharts `<Scatter>` series. -/
structure ScatterProps where
  /-- Which field of the JSON row encodes the y-value to plot. Defaults to
  `"y"`. -/
  dataKey : Json := Json.str "y"
  /-- CSS color for the scatter points. -/
  fill    : String
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts `<Scatter>`. -/
@[inline] def Scatter : ProofWidgets.Component ScatterProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "Scatter"

/--
Turn an array of JSON rows into a Recharts scatter chart containing a
single series named `y`.  The helper mirrors `mkLineChart` but renders
dots instead of a line.  The point color is supplied via `fillColor`.
-/
@[inline] def mkScatterChart (data : Array Json) (fillColor : String)
    (w h : Nat := 400) : Html :=
  let chartHtml :=
    <ScatterChart width={w} height={h} data={data}>
      <XAxis dataKey?="x" />
      <YAxis />
      <Scatter dataKey={Json.str "y"} fill={fillColor} />
    </ScatterChart>

  let keysToCheck := #["x", "y"]
  if jsonDataHasInvalidFloats data keysToCheck then
    let warningProps : WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := WarningBanner warningProps
    .element "div" #[] #[warningHtml, chartHtml]
  else
    chartHtml

/-! ## New chart types: Area and Bar

We extend the Thin Lean wrappers to cover Recharts `<AreaChart>`/`<Area>` and
`<BarChart>`/`<Bar>` so that higher-level helpers can support additional chart
types without depending on upstream ProofWidgets releases.  Only a **minimal**
set of props is exposed for now. -/

/-- Props for a Recharts `<AreaChart>`. -/
structure AreaChartProps where
  /-- Width of the SVG container in pixels. -/
  width  : Nat
  /-- Height of the SVG container in pixels. -/
  height : Nat
  /-- Dataset array. -/
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts `<AreaChart>`. -/
@[inline] def AreaChart : ProofWidgets.Component AreaChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "AreaChart"

/-- Props for a Recharts `<Area>` series.  We expose the usual `dataKey`,
`fill` and `stroke` colours.  Additional Recharts props can be added later. -/
structure AreaProps where
  dataKey : Json := Json.str "y"
  /-- Fill colour of the area. -/
  fill    : String
  /-- Stroke colour of the area border.  Defaults to the same as `fill`. -/
  stroke  : String := ""
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts `<Area>`. -/
@[inline] def Area : ProofWidgets.Component AreaProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "Area"

/-- Props for a Recharts `<BarChart>`. -/
structure BarChartProps where
  width  : Nat
  height : Nat
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts `<BarChart>`. -/
@[inline] def BarChart : ProofWidgets.Component BarChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "BarChart"

/-- Props for a Recharts `<Bar>` series. -/
structure BarProps where
  dataKey : Json := Json.str "y"
  fill    : String
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts `<Bar>`. -/
@[inline] def Bar : ProofWidgets.Component BarProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "Bar"

end LeanPlot.Components
