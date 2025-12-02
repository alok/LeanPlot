import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts
import LeanPlot.ToFloat
import LeanPlot.Axis
import LeanPlot.Legend
import LeanPlot.Palette
import LeanPlot.Utils
import LeanPlot.WarningBanner
import LeanPlot.AutoDomain
import LeanPlot.Metaprogramming

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
open LeanPlot.Metaprogramming
open scoped ProofWidgets.Jsx
open LeanPlot.AutoDomain

namespace LeanPlot.Components

/-- Uniformly sample a function {lit}`f : Float → β` on the interval {lit}`[min,max]` or an auto-detected domain.
{lit}`β` is required to have a {lit}`[ToFloat β]` instance so that values can be
converted to a JavaScript-friendly {lean}`Float` for serialisation. -/
@[inline] def sample {β} [ToFloat β]
  (f : Float → β) (steps : Nat := 200) (domainOpt : Option (Float × Float) := none) : Array Json :=
  -- Decide on the **x‐domain** to sample.
  -- • If the caller supplies an explicit `domainOpt` we honour it.
  -- • Otherwise we fall back to the canonical unit interval `[0,1]` used
  --   by Tier-0 helpers.  The earlier implementation mistakenly delegated
  --   to `autoDomain`, which is intended for **y-axis** heuristics and
  --   therefore produced misleading samples when the codomain of `f`
  --   had large magnitude.
  let (minVal, maxVal) : Float × Float := match domainOpt with
    | some (minD, maxD) => (minD, maxD)
    | none => (0.0, 1.0)
  if steps == 0 then
    #[] -- Return empty array if steps is zero
  else
    (List.range (steps.succ)).toArray.map fun i =>
      let x : Float := minVal + (maxVal - minVal) * i.toFloat / steps.toFloat
      let y : β := f x
      json% {x: $(toJson x), y: $(toJson (toFloat y))}

/--
Sample several functions whose outputs will be stored under the given series names.
Each function must return a type with a {lit}`[ToFloat]` instance so that the
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

{lit}`seriesStrokes` supplies a color for each series; its order must match the order in {lit}`fns` used to create the data.
-/
@[inline] def mkLineChart (data : Array Json) (seriesStrokes : Array (String × String)) (w h : Nat := 400) : Html :=
  let chartHtml :=
    <LineChart width={w} height={h} data={data}>
      <LeanPlot.Axis.XAxis dataKey?="x" />
      <LeanPlot.Axis.YAxis />
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

/-- Like {name}`mkLineChart` but allows setting axis labels. -/
@[inline] def mkLineChartWithLabels (data : Array Json)
    (seriesStrokes : Array (String × String))
    (xLabel? : Option String := none) (yLabel? : Option String := none)
    (w h : Nat := 400) : Html :=
  let xLabelJson? : Option Json := xLabel?.map fun l => (json% $l)
  let yLabelJson? : Option Json := yLabel?.map fun l =>
    json% { value: $l, angle: -90, position: "left" }

  let chartHtml : Html :=
    <LineChart width={w} height={h} data={data}>
      <LeanPlot.Axis.XAxis dataKey?="x" label?={xLabelJson?} />
      <LeanPlot.Axis.YAxis label?={yLabelJson?} />
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
{name}`mkLineChartFull` extends {name}`mkLineChartWithLabels` by also including a
Recharts {lit}`<Legend>` block so that each series is labelled with the name
provided to {name}`sampleMany`.  This is optional but useful for multi-series
plots.
-/
@[inline] def mkLineChartFull (data : Array Json)
    (seriesStrokes : Array (String × String))
    (xLabel? : Option String := none) (yLabel? : Option String := none)
    (w h : Nat := 400) : Html :=
  let xLabelJson? : Option Json := xLabel?.map Json.str
  let yLabelJson? : Option Json := yLabel?.map fun l =>
    Json.mkObj [
      ("value", Json.str l),
      ("angle", toJson (-90.0)),
      ("position", Json.str "left")
    ]

  let chartHtml :=
    <LineChart width={w} height={h} data={data}>
      <LeanPlot.Axis.XAxis dataKey?="x" label?={xLabelJson?} />
      <LeanPlot.Axis.YAxis label?={yLabelJson?} />
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

/-- Props for a Recharts {lit}`<ScatterChart>` wrapped in Lean.  We intentionally
keep this minimal, exposing only what the Tier-0 helpers require. -/
structure ScatterChartProps where
  /-- Width of the SVG container in pixels. -/
  width  : Nat
  /-- Height of the SVG container in pixels. -/
  height : Nat
  /-- Array of JSON rows each containing at least {lit}`x` and {lit}`y` fields. -/
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<ScatterChart>`.
We delegate to the same JavaScript bundle used by the other Recharts
components that ship with ProofWidgets. -/
@[inline] def ScatterChart : ProofWidgets.Component ScatterChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "ScatterChart"

/-- Props for a Recharts {lit}`<Scatter>` series. -/
structure ScatterProps where
  /-- Which field of the JSON row encodes the y-value to plot. Defaults to
  {lit}`"y"`. -/
  dataKey : Json := Json.str "y"
  /-- CSS color for the scatter points. -/
  fill    : String
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<Scatter>`. -/
@[inline] def Scatter : ProofWidgets.Component ScatterProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "Scatter"

/--
Turn an array of JSON rows into a Recharts scatter chart containing a
single series named {lit}`y`.  The helper mirrors {name}`mkLineChart` but renders
dots instead of a line.  The point color is supplied via {lit}`fillColor`.
-/
@[inline] def mkScatterChart (data : Array Json) (fillColor : String)
    (w h : Nat := 400) : Html :=
  let chartHtml :=
    <ScatterChart width={w} height={h} data={data}>
      <LeanPlot.Axis.XAxis dataKey?="x" />
      <LeanPlot.Axis.YAxis />
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

/-- Props for a Recharts {lit}`<AreaChart>`. -/
structure AreaChartProps where
  /-- Width of the SVG container in pixels. -/
  width  : Nat
  /-- Height of the SVG container in pixels. -/
  height : Nat
  /-- Dataset array. -/
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<AreaChart>`. -/
@[inline] def AreaChart : ProofWidgets.Component AreaChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "AreaChart"

/-- Props for a Recharts {lit}`<Area>` series.  We expose the usual {lit}`dataKey`,
{lit}`fill` and {lit}`stroke` colors.  Additional Recharts props can be added later. -/
structure AreaProps where
  /-- Which field of the JSON row encodes the y-value to plot. Defaults to
  {lit}`"y"`. -/
  dataKey : Json := Json.str "y"
  /-- Fill color of the area. -/
  fill    : String
  /-- Stroke color of the area border.  Defaults to the same as {lit}`fill`. -/
  stroke  : String := ""
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<Area>`. -/
@[inline] def Area : ProofWidgets.Component AreaProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "Area"

/-- Props for a Recharts {lit}`<BarChart>`. -/
structure BarChartProps where
  /-- Width of the SVG container in pixels. -/
  width  : Nat
  /-- Height of the SVG container in pixels. -/
  height : Nat
  /-- Dataset array. -/
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<BarChart>`. -/
@[inline] def BarChart : ProofWidgets.Component BarChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "BarChart"

/-- Props for a Recharts {lit}`<Bar>` series. -/
structure BarProps where
  /-- Which field of the JSON row encodes the y-value to plot. Defaults to
  {lit}`"y"`. -/
  dataKey : Json := Json.str "y"
  /-- CSS color for the bars. -/
  fill    : String
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<Bar>`. -/
@[inline] def Bar : ProofWidgets.Component BarProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "Bar"

/-- Props for a Recharts {lit}`<ComposedChart>` for mixed chart types. -/
structure ComposedChartProps where
  /-- Width of the SVG container in pixels. -/
  width  : Nat
  /-- Height of the SVG container in pixels. -/
  height : Nat
  /-- Dataset array. -/
  data   : Array Json
  deriving FromJson, ToJson

/-- Lean wrapper for Recharts {lit}`<ComposedChart>` which supports mixing different chart types. -/
@[inline] def ComposedChart : ProofWidgets.Component ComposedChartProps where
  javascript := ProofWidgets.Recharts.Recharts.javascript
  «export»   := "ComposedChart"

/--
Turn an array of JSON rows into a Recharts **bar chart** containing a single
series named {lit}`y`.  The helper mirrors {name}`mkScatterChart` but renders bars instead
of dots.  The bar color is supplied via {lit}`fillColor`.
-/
@[inline] def mkBarChart (data : Array Json) (fillColor : String)
    (w h : Nat := 400) : Html :=
  let chartHtml :=
    <BarChart width={w} height={h} data={data}>
      <LeanPlot.Axis.XAxis dataKey?="x" />
      <LeanPlot.Axis.YAxis />
      <Bar dataKey={Json.str "y"} fill={fillColor} />
    </BarChart>

  let keysToCheck := #["x", "y"]
  if jsonDataHasInvalidFloats data keysToCheck then
    let warningProps : WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := WarningBanner warningProps
    .element "div" #[] #[warningHtml, chartHtml]
  else
    chartHtml

/-! ## Simple Plotting (Zero-Configuration Plots) -/

/-- 🎯 Plot a function with automatic everything. Just works!

Examples:
- {lit}`plotSimple (fun t => t^2)` - Simple quadratic plot
- {lit}`plotSimple (fun x => x + 1)` - Linear function
- {lit}`plotSimple (fun i => i * 2)` - Linear scaling

You never have to think about axis labels again!
-/
def plotSimple {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)
    (domain : Option (Float × Float) := none) (w h : Nat := 400) : Html :=
  -- Sample the function
  let data := sample f steps domain

  -- Generate axis labels (this would use metaprogramming in full implementation)
  -- For now, provide sensible defaults that work with the common patterns
  let xLabel := "x"
  let yLabel := "f(x)"
  let seriesStrokes := #[("y", "#2563eb")]  -- Nice blue color

  mkLineChartWithLabels data seriesStrokes (some xLabel) (some yLabel) w h

/-- Plot multiple functions with automatic labeling.
    Just pass your functions and get a beautiful multi-line plot!

Examples:
- {lit}`plotManySimple #[("sin", fun t => Float.sin t), ("cos", fun t => Float.cos t)]`
- {lit}`plotManySimple #[("linear", fun x => x), ("quadratic", fun x => x^2)]`

Everything is automatic - colors, labels, legend!
-/
def plotManySimple {β} [ToFloat β] (fns : Array (String × (Float → β)))
    (steps : Nat := 200) (domain : Float × Float := (0.0, 1.0))
    (w h : Nat := 400) : Html :=
  -- Sample all functions
  let data := sampleMany fns steps domain.1 domain.2

  -- Generate axis labels
  let xLabel := "x"  -- Could be enhanced with metaprogramming
  let yLabel := "y"  -- Could be enhanced with metaprogramming

  -- Auto-generate colors for each series
  let colors := #["#2563eb", "#dc2626", "#16a34a", "#ca8a04", "#7c3aed", "#db2777"]
  let seriesStrokes := fns.mapIdx fun i (name, _) =>
    let color := colors.getD (i % colors.size) "#64748b"
    (name, color)

  mkLineChartFull data seriesStrokes (some xLabel) (some yLabel) w h

/-- Scatter plot with automatic labeling. -/
def scatterSimple {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)
    (domain : Option (Float × Float) := none) (w h : Nat := 400) : Html :=
  let data := sample f steps domain
  mkScatterChart data "#dc2626" w h  -- Nice red color

/-- Bar chart with automatic labeling. -/
def barSimple {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)
    (domain : Option (Float × Float) := none) (w h : Nat := 400) : Html :=
  let data := sample f steps domain
  mkBarChart data "#16a34a" w h  -- Nice green color

/-- Enhanced line chart builder with automatic axis label generation.
    This function demonstrates how metaprogramming could be used to automatically
    extract parameter names from function expressions for axis labeling. -/
def mkLineChartWithAutoLabels (data : Array Json)
    (seriesStrokes : Array (String × String))
    (xDataKey : String := "x") (yDataKey : String := "y")
    (w h : Nat := 400) : Html :=
  -- For now, use the data keys as labels. In a full implementation,
  -- this would use metaprogramming to extract parameter names from function expressions
  let xLabel : Option String := some xDataKey
  let yLabel : Option String := some yDataKey
  mkLineChartWithLabels data seriesStrokes xLabel yLabel w h

end LeanPlot.Components
