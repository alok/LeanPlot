import LeanPlot.Constants
import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.ToFloat
import LeanPlot.Axis -- For AxisProps used in Recharts
import LeanPlot.API -- For xyArrayToJson
import LeanPlot.Utils -- For jsonDataHasInvalidFloats
import LeanPlot.WarningBanner -- For WarningBanner
import LeanPlot.Core -- For LeanPlot.Render instance
import ProofWidgets.Component.Recharts
import ProofWidgets.Data.Html -- Explicit import for Html.empty and Html.text
import Lean.Server -- For HtmlEval
-- import ProofWidgets.Data.Legend

open Lean ProofWidgets ProofWidgets.Recharts LeanPlot.Constants
open LeanPlot -- For WarningBannerProps, WarningBanner, Render
open LeanPlot.Utils -- For jsonDataHasInvalidFloats
open scoped ProofWidgets.Jsx -- This enables JSX syntax
namespace LeanPlot

structure AxisSpec where
  label    : Option String := none
  dataKey  : String := "x" -- Default for x-axis, override for y-axis
  -- domain   : Option (Json × Json) := none -- Recharts takes Array Json for domain
  domain   : Option (Array Json) := none
  -- Potentially more options like 'type' (category, number), ticks, etc.
  deriving Inhabited, ToJson, FromJson

structure SeriesSpec where
  /-- The name of the series, used for legend and default y-axis label. -/
  name     : String
  /-- The key in chartData for this series' y-values. -/
  dataKey  : String := name
  /-- The color of the series. -/
  color    : String
  /-- The type of the series, e.g., "line", "scatter". -/
  type     : String -- e.g., "line", "scatter"
  /-- Whether to show dots for a line series. `none` means default (true for line, not applicable for scatter). -/
  dot      : Option Bool := none
  deriving Inhabited, ToJson, FromJson

/-- The specification for a plot. -/
structure PlotSpec where
  /-- The data for the chart. -/
  chartData : Array Json := #[]
  /-- The series in the chart. -/
  series    : Array SeriesSpec := #[]
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
  let data := LeanPlot.Components.sample fn steps (domainOpt := domainOpt)
  let seriesColor := color.getD (LeanPlot.Palette.colourFor 0)
  {
    chartData := data,
    series := #[{
      name      := name,
      dataKey   := "y", -- `sample` produces {x:_, y:_} items
      color     := seriesColor,
      type      := "line"
      -- dot defaults to none -> true for line
    }],
    xAxis     := some { dataKey := "x", label := some "x" }, -- Default x-axis label
    yAxis     := some { dataKey := "y", label := some name },
    legend    := !(name == "y") -- Hide legend if name is default "y" for single series plot
  }

/-- Construct a scatter plot from an array of points. -/
@[inline]
def scatter (points : Array (Float × Float)) (name : String := "y")
  (color : Option String := none) : PlotSpec :=
  let data := LeanPlot.API.xyArrayToJson points
  let seriesColor := color.getD (LeanPlot.Palette.colourFor 0)
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

-- TODO: Add more combinators: addSeries, etc.

-- Renderer
-- This is a complex part, converting the spec to Recharts JSX
-- For now, we can adapt the existing mkLineChartFull or similar logic

/-- Render the plot. -/
@[inline]
def render (spec : PlotSpec) : Html :=
  let chartComponents := spec.series.map fun s =>
    if s.type == "line" then
      let dotProp := s.dot.getD true -- Default to true if `none`
      (<Line type={LineType.monotone} dataKey={s.dataKey} stroke={s.color} dot?={some dotProp} /> : Html)
    else if s.type == "scatter" then
      let scatterProps : LeanPlot.Components.ScatterProps := { dataKey := toJson s.dataKey, fill := s.color }
      (<LeanPlot.Components.Scatter {...scatterProps} /> : Html)
    else
      (Html.text s!"Unsupported series type: {s.type}" : Html)

  let xAxisHtml := match spec.xAxis with
    | some ax =>
      let axProps : LeanPlot.Axis.AxisProps := { dataKey? := toJson ax.dataKey, label? := ax.label, domain? := ax.domain, type := .number }
      (<LeanPlot.Axis.XAxis {...axProps} /> : Html)
    | none => (Html.text "" : Html)

  let yAxisHtml := match spec.yAxis with
    | some ax =>
      let axProps : LeanPlot.Axis.AxisProps := { dataKey? := toJson ax.dataKey, label? := ax.label, domain? := ax.domain, type := .number }
      (<LeanPlot.Axis.YAxis {...axProps} /> : Html)
    | none => (Html.text "" : Html)

  let legendHtml := if spec.legend then (Html.text "" : Html) else (Html.text "" : Html) -- Temporarily removed Legend
  -- Temporarily remove legend until a Legend component is available/created
  -- let legendHtml := (Html.text "" : Html)

  let mainChartComponent :=
    -- Always use LineChart as the main container, it can host Scatter series too.
    (<LineChart width={spec.width} height={spec.height} data={spec.chartData}>
      {xAxisHtml}
      {yAxisHtml}
      {legendHtml}
      {Html.element "Fragment" #[] chartComponents}
    </LineChart> : Html)

  let finalHtml := match spec.title with
    | some t =>
      (<div><h4>{Html.text t}</h4>{mainChartComponent}</div> : Html)
    | none => mainChartComponent

  let keysToCheck := spec.series.map (fun s => s.dataKey) |>.push "x"
  if jsonDataHasInvalidFloats spec.chartData keysToCheck then
    let warningProps : WarningBannerProps := { message := "Plot data contains invalid values (NaN/Infinity) and may not render correctly." }
    let warningHtml := WarningBanner warningProps
    (.element "div" #[] #[warningHtml, finalHtml] : Html)
  else
    finalHtml

/-- Render the plot. -/
instance : Render PlotSpec where -- Render is from LeanPlot.Core, brought in by 'open LeanPlot'
  render := render

/-- Allow PlotSpec to be evaluated by #plot command. -/
instance : HtmlEval PlotSpec where
  eval spec := do
    -- Since render is pure, we just return it directly.
    -- If render needed IO/Rpc actions, they would happen here.
    return render spec

end PlotSpec

end LeanPlot
