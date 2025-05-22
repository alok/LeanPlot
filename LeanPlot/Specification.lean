import LeanPlot.Constants
import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.ToFloat
import LeanPlot.Axis -- For AxisProps used in Recharts
import LeanPlot.API -- For xyArrayToJson
import LeanPlot.Utils -- For jsonDataHasInvalidFloats
import LeanPlot.WarningBanner -- For WarningBanner
import LeanPlot.Core -- For LeanPlot.Render instance
import LeanPlot.Legend -- Add import for Legend
import ProofWidgets.Component.Recharts
import ProofWidgets.Data.Html -- Explicit import for Html.empty and Html.text
import Lean.Server -- For HtmlEval
-- import ProofWidgets.Data.Legend

open Lean ProofWidgets ProofWidgets.Recharts LeanPlot.Constants
open LeanPlot -- For WarningBannerProps, WarningBanner, Render
open LeanPlot.Utils -- For jsonDataHasInvalidFloats
open LeanPlot.Legend (Legend) -- Open Legend for direct use
open LeanPlot.Components (BarChart AreaChart)
open scoped ProofWidgets.Jsx -- This enables JSX syntax
namespace LeanPlot

/-! # LeanPlot Specification Language
Defines data types for plot specifications (series, axes, styling etc.) that
can be lowered to different concrete back-ends (Recharts, VegaLite etc.).
-/

/--
Specification of a chart axis.

Note that Recharts expects the *domain* as a JSON array `[lo, hi]`, so we
store it as `Array Json` ready for serialisation rather than a tuple of
`Float`s.  When the domain is left as `none`, Recharts will automatically pick
an appropriate range based on the data.
-/
structure AxisSpec where
  /-- Human-readable label shown alongside the axis. -/
  label    : Option String := none
  /-- Field in the chart-level JSON rows that provides the coordinate for this axis. -/
  dataKey  : String
  /-- Axis interpretation, e.g. `"number"` or `"category"`. -/
  type     : Option String := none
  /-- Explicit numeric domain given as `[lo, hi]`. -/
  domain   : Option (Array Json) := none
  deriving ToJson, FromJson, Inhabited

/--
Specification of an individual series/layer within the chart.  This is intentionally
minimal for now — fields are added on demand by the higher-level helpers.
-/
structure LayerSpec where
  /-- Name of the series (appears in legends and tooltips). -/
  name     : String
  /-- Which field of the chart-level data rows to plot for this series. -/
  dataKey  : String
  /-- CSS color (e.g. `"#ff0000"`) used to render the series. -/
  color    : String
  /-- The kind of Recharts series to render, such as `"line"` or `"scatter"`. -/
  type     : String := "line"
  /-- Whether to render the point markers (`<Line dot={…}/>`).  `none` falls back to the Recharts default. -/
  dot      : Option Bool := none
  deriving ToJson, FromJson, Inhabited

/-- Deprecated: Use `LayerSpec` instead. -/
abbrev SeriesSpec := LayerSpec

/-- The specification for a plot. -/
structure PlotSpec where
  /-- The data for the chart. -/
  chartData : Array Json := #[]
  /-- The series in the chart. -/
  series    : Array LayerSpec := #[]
  /-- Default x-axis specification. -/
  xAxis     : Option AxisSpec := some { dataKey := "x" }
  /-- Default y-axis specification. -/
  yAxis     : Option AxisSpec := some { dataKey := "y" }
  /-- The title of the plot. -/
  title     : Option String := none
  /-- The width of the plot. -/
  width     : Nat := defaultW
  /-- The height of the plot. -/
  height    : Nat := defaultH
  /-- Whether to show the legend. -/
  legend    : Bool := true
  deriving Inhabited

-- Basic constructor functions

/-- Construct a line plot from a function. -/
@[inline]
def line {β} [ToFloat β]
  (fn : Float → β) (name : String := "y") (steps : Nat := 200)
  (domainOpt : Option (Float × Float) := none)
  (color : Option String := none) : PlotSpec :=
  let (minVal, maxVal) : Float × Float :=
    match domainOpt with
    | some d => d
    | none   => (-1.0, 1.0)

  let data : Array Json :=
    if steps == 0 then #[] else
      (List.range (steps.succ)).toArray.map fun i =>
        let x : Float := minVal + (maxVal - minVal) * i.toFloat / steps.toFloat
        let y : β := fn x
        Json.mkObj [
          ("x", toJson x),
          (name, toJson (toFloat y))
        ]

  let seriesColor := color.getD (LeanPlot.Palette.colorFromNat 0)
  {
    chartData := data,
    series := #[{
      name      := name,
      dataKey   := name,
      color     := seriesColor,
      type      := "line"
    }],
    xAxis     := some { dataKey := "x", label := some "x" },
    yAxis     := some { dataKey := name, label := some name },
    legend    := true
  }

/-- Construct a scatter plot from an array of points. -/
@[inline]
def scatter (points : Array (Float × Float)) (name : String := "y")
  (color : Option String := none) : PlotSpec :=
  let data := LeanPlot.API.xyArrayToJson points
  let seriesColor := color.getD (LeanPlot.Palette.colorFromNat 0)
  {
    chartData := data,
    series := #[{
      name      := name,
      dataKey   := "y", -- `xyArrayToJson` produces {x:_, y:_}
      color     := seriesColor,
      type      := "scatter"
      -- dot is not applicable for scatter
    }],
    xAxis     := some { dataKey := "x", label := some "x" }, -- Default x-axis label
    yAxis     := some { dataKey := "y", label := some name },
    legend    := !(name == "y")
  }

/-- Construct a bar chart from an array of points.
Each tuple encodes an `(x,y)` pair which will be converted to the
`{x := _, y := _}` JSON objects expected by Recharts. -/
@[inline]
def bar (points : Array (Float × Float)) (name : String := "y")
  (color : Option String := none) : PlotSpec :=
  let data := LeanPlot.API.xyArrayToJson points
  let seriesColor := color.getD (LeanPlot.Palette.colorFromNat 0)
  {
    chartData := data,
    series := #[{
      name      := name,
      dataKey   := "y", -- `xyArrayToJson` produces objects with `y` by default
      color     := seriesColor,
      type      := "bar"
    }],
    xAxis     := some { dataKey := "x", label := some "x" },
    yAxis     := some { dataKey := "y", label := some name },
    legend    := !(name == "y")
  }

/-- Construct an area chart from a function. -/
@[inline]
def area {β} [ToFloat β]
  (fn : Float → β) (name : String := "y") (steps : Nat := 200)
  (domainOpt : Option (Float × Float) := none)
  (color : Option String := none) : PlotSpec :=
  let (minVal, maxVal) : Float × Float :=
    match domainOpt with
    | some d => d
    | none   => (-1.0, 1.0)

  let data : Array Json :=
    if steps == 0 then #[] else
      (List.range (steps.succ)).toArray.map fun i =>
        let x : Float := minVal + (maxVal - minVal) * i.toFloat / steps.toFloat
        let y : β := fn x
        Json.mkObj [
          ("x", toJson x),
          (name, toJson (toFloat y))
        ]

  let seriesColor := color.getD (LeanPlot.Palette.colorFromNat 0)
  {
    chartData := data,
    series := #[{
      name      := name,
      dataKey   := name,
      color     := seriesColor,
      type      := "area"
    }],
    xAxis     := some { dataKey := "x", label := some "x" },
    yAxis     := some { dataKey := name, label := some name },
    legend    := true
  }

/-- Construct a multi-line chart from several functions sampled on a common domain.
Each `(name, fn)` pair becomes its own series. The colors are automatically
assigned from the default palette unless `colors?` is provided. -/
@[inline]
def lines {β} [Inhabited β] [ToFloat β]
  (fns : Array (String × (Float → β)))
  (steps : Nat := 200)
  (domainOpt : Option (Float × Float) := none)
  (colors? : Option (Array String) := none) : PlotSpec :=
  let (minVal, maxVal) : Float × Float :=
    match domainOpt with
    | some d => d
    | none   => (-1.0, 1.0)
  let data : Array Json :=
    if steps == 0 then #[] else
      LeanPlot.Components.sampleMany fns steps minVal maxVal
  -- Determine colors
  let chosenColors : Array String :=
    match colors? with
    | some cs =>
      if cs.size >= fns.size then cs
      else
        cs ++ (List.range (fns.size - cs.size)).toArray.map (fun i => LeanPlot.Palette.colorFromNat (cs.size + i))
    | none => (List.range fns.size).toArray.map LeanPlot.Palette.colorFromNat
  let seriesArr : Array LayerSpec :=
    (List.range fns.size).toArray.map fun idx =>
      let (name, _) := fns[idx]!; {
        name      := name,
        dataKey   := name,
        color     := chosenColors[idx]!,
        type      := "line"
      : LayerSpec }
  {
    chartData := data,
    series    := seriesArr,
    xAxis     := some { dataKey := "x", label := some "x" },
    yAxis     := none, -- let Recharts auto-label or user can set later
    legend    := true
  }

-- Combinators

namespace PlotSpec

/-- Set the title of the plot. -/
@[inline]
def withTitle (spec : PlotSpec) (t : String) : PlotSpec :=
  { spec with title := some t }

/-- Set the width of the plot. -/
@[inline]
def withWidth (spec : PlotSpec) (w : Nat) : PlotSpec :=
  { spec with width := w }

/-- Set the height of the plot. -/
@[inline]
def withHeight (spec : PlotSpec) (h : Nat) : PlotSpec :=
  { spec with height := h }

/-- Set the width and height of the plot. -/
@[inline]
def withSize (spec : PlotSpec) (w h : Nat) : PlotSpec :=
  { spec with width := w, height := h }

/-- Set the x-axis label of the plot. -/
@[inline]
def withXLabel (spec : PlotSpec) (label : String) : PlotSpec :=
  match spec.xAxis with
  | some xAxisSpec => { spec with xAxis := some { xAxisSpec with label := some label } }
  | none           => { spec with xAxis := some { label := some label, dataKey := "x" } }

/-- Set the y-axis label of the plot. -/
@[inline]
def withYLabel (spec : PlotSpec) (label : String) : PlotSpec :=
  match spec.yAxis with
  | some yAxisSpec => { spec with yAxis := some { yAxisSpec with label := some label } }
  | none           => { spec with yAxis := some { label := some label, dataKey := "y" } }
  -- Note: dataKey for yAxis might need to be smarter if multiple series exist

/-- Show or hide the legend. -/
@[inline]
def withLegend (spec : PlotSpec) (shouldShow : Bool) : PlotSpec :=
  { spec with legend := shouldShow }

/-- Set the x-axis domain. -/
@[inline]
def withXDomain (spec : PlotSpec) (min max : Float) : PlotSpec :=
  let newDomain := #[toJson min, toJson max]
  match spec.xAxis with
  | some xAxisSpec => { spec with xAxis := some { xAxisSpec with domain := some newDomain } }
  | none           => { spec with xAxis := some { dataKey := "x", domain := some newDomain } }

/-- Set the y-axis domain. -/
@[inline]
def withYDomain (spec : PlotSpec) (min max : Float) : PlotSpec :=
  let newDomain := #[toJson min, toJson max]
  match spec.yAxis with
  | some yAxisSpec => { spec with yAxis := some { yAxisSpec with domain := some newDomain } }
  | none           => { spec with yAxis := some { dataKey := "y", domain := some newDomain } }

/-- Append a new series to the plot. The caller must ensure that `spec.chartData` already provides the data for `series.dataKey`. -/
@[inline]
def addSeries (spec : PlotSpec) (series : LayerSpec) : PlotSpec :=
  { spec with series := spec.series.push series, legend := true }

/-- Overlay two `PlotSpec`s by concatenating their `series` arrays and combining metadata.
    NOTE: This assumes both specs refer to the *same* `chartData` (same x‐values).
    If the datasets differ the function keeps `p.chartData` and discards `q.chartData`.
    Axis specs prefer the first non-`none` value encountered.  Width/height take the max. -/
@[inline] def overlay (p q : PlotSpec) : PlotSpec :=
  { chartData := p.chartData ++ q.chartData,
    series    := p.series ++ q.series,
    xAxis     := match p.xAxis with | some x => some x | none => q.xAxis,
    yAxis     := match p.yAxis with | some y => some y | none => q.yAxis,
    title     := none,
    width     := max p.width q.width,
    height    := max p.height q.height,
    legend    := true }

/-- Synonym for `overlay` inspired by grammar-of-graphics "stacking". -/
@[inline] def stack := overlay

instance : HAdd PlotSpec PlotSpec PlotSpec where
  hAdd := overlay

-- Renderer Typeclass

/-- A typeclass for rendering a layer specification into HTML. -/
class RenderFragment (α : Type) where
  /-- Renders a layer specification into HTML.
      `layerSpec` is the specification for the individual layer.
      `allChartData` is the complete dataset for the chart, passed in case the renderer needs it. -/
  render (layerSpec : α) (allChartData : Array Json) : Html

/-- Deprecated: Use `RenderFragment` instead. -/
abbrev RenderSeries (α : Type) := RenderFragment α

-- Default instance for `LayerSpec` dispatching on its `type` field.
instance : RenderFragment LayerSpec where
  render (s : LayerSpec) (_allChartData : Array Json) : Html :=
    if s.type == "line" then
      (<Line type={LineType.monotone} dataKey={toJson s.dataKey} stroke={s.color} dot?={some (s.dot.getD false)} /> : Html)
    else if s.type == "scatter" then
      let scatterProps : LeanPlot.Components.ScatterProps := { dataKey := toJson s.dataKey, fill := s.color }
      (<LeanPlot.Components.Scatter {...scatterProps} /> : Html)
    else if s.type == "area" then
      let areaProps : LeanPlot.Components.AreaProps := { dataKey := toJson s.dataKey, fill := s.color, stroke := s.color }
      (<LeanPlot.Components.Area {...areaProps} /> : Html)
    else if s.type == "bar" then
      let barProps : LeanPlot.Components.BarProps := { dataKey := toJson s.dataKey, fill := s.color }
      (<LeanPlot.Components.Bar {...barProps} /> : Html)
    else
      (Html.text s!"Unsupported series type: {s.type}" : Html)

instance : RenderFragment AxisSpec where
  render (_ax : AxisSpec) (_allChartData : Array Json) : Html := (Html.text "AxisSpec Fragment (dummy)" : Html)

/-- Render the plot according to its `PlotSpec`. -/
@[inline] def render (spec : PlotSpec) : Html :=
  let chartComponents := spec.series.map fun s =>
    RenderFragment.render s spec.chartData
  let xAxisHtml := match spec.xAxis with
    | some ax =>
      let label? : Option Json := ax.label.map Json.str
      let axProps : LeanPlot.Axis.AxisProps := {
        dataKey? := some (toJson ax.dataKey),
        domain? := ax.domain,
        label? := label?,
        type := .number
      }
      (<LeanPlot.Axis.XAxis {...axProps} /> : Html)
    | none => (Html.text "" : Html)

  -- Rotate the Y-axis label by –90 ° and place it to the left of the tick labels
  let yAxisHtml := match spec.yAxis with
    | some ax =>
      let labelJson? : Option Json := ax.label.map fun l =>
        Json.mkObj [
          ("value", Json.str l),
          ("angle", toJson (-90.0)),
          ("position", Json.str "left")
        ]
      let axProps : LeanPlot.Axis.AxisProps := {
        dataKey? := some (toJson ax.dataKey),
        domain? := ax.domain,
        label? := labelJson?,
        type := .number
      }
      (<LeanPlot.Axis.YAxis {...axProps} /> : Html)
    | none => (Html.text "" : Html)

  let legendHtml := if spec.legend then (<Legend /> : Html) else (Html.text "" : Html)

  /-
  Choose an appropriate Recharts *chart container* depending on the
  kinds of series contained in `spec.series`.

  • If **all** layers are of type `"bar"` we use `<BarChart>`.
  • Else if all layers are of type `"area"` we use `<AreaChart>`.
  • Otherwise we fall back to `<LineChart>` which handles lines and
    scatters fine.

  NOTE: A more complete solution would select `ComposedChart` when we
  eventually add a Lean wrapper, but this heuristic gets all current
  demos working (simple bar charts now show their bars).
  -/

  let allAre (t : String) : Bool := spec.series.all (fun s => s.type == t)

  let mainChartComponent : Html :=
    if allAre "bar" then
      (<BarChart width={spec.width} height={spec.height} data={spec.chartData}>
        {xAxisHtml}
        {yAxisHtml}
        {legendHtml}
        {... chartComponents}
      </BarChart> : Html)
    else if allAre "area" then
      (<AreaChart width={spec.width} height={spec.height} data={spec.chartData}>
        {xAxisHtml}
        {yAxisHtml}
        {legendHtml}
        {... chartComponents}
      </AreaChart> : Html)
    else
      (<LineChart width={spec.width} height={spec.height} data={spec.chartData}>
        {xAxisHtml}
        {yAxisHtml}
        {legendHtml}
        {... chartComponents}
      </LineChart> : Html)

  let finalHtml := match spec.title with
    | some t => (<div><h4>{Html.text t}</h4>{mainChartComponent}</div> : Html)
    | none => mainChartComponent
  let keysToCheck := spec.series.map (fun s => s.dataKey) |>.push "x"
  if LeanPlot.Utils.jsonDataHasInvalidFloats spec.chartData keysToCheck then
    let warningProps : LeanPlot.WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := LeanPlot.WarningBanner warningProps
    (.element "div" #[] #[warningHtml, finalHtml] : Html)
  else
    finalHtml

instance : Render PlotSpec where
  render := render

instance : HtmlEval PlotSpec where
  eval spec := pure (render spec)

end PlotSpec

universe u

instance {β : Type u} [Inhabited β] : Inhabited (String × (Float → β)) where
  default := ("", fun _ => default)

end LeanPlot
