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
  /-- CSS colour (e.g. `"#ff0000"`) used to render the series. -/
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
  let data := LeanPlot.Components.sample fn steps (domainOpt := domainOpt)
  let seriesColor := color.getD (LeanPlot.Palette.colorFromNat 0)
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
  { chartData := p.chartData,
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

-- Default instance for LayerSpec using its `type` field.
-- This moves the old if/else logic into the typeclass instance.
instance : RenderFragment LayerSpec where
  render (s : LayerSpec) (_allChartData : Array Json) : Html := -- _allChartData often unused here
    if s.type == "line" then
      let dotProp := s.dot.getD true -- Default to true if `none`
      (<Line type={LineType.monotone} dataKey={toJson s.dataKey} stroke={s.color} dot?={some dotProp} /> : Html)
    else if s.type == "scatter" then
      let scatterProps : LeanPlot.Components.ScatterProps := { dataKey := toJson s.dataKey, fill := s.color }
      (<LeanPlot.Components.Scatter {...scatterProps} /> : Html)
    else
      (Html.text s!"Unsupported series type: {s.type}" : Html)

/-- Instance to demonstrate polymorphism. Does not render a meaningful axis yet. -/
instance : RenderFragment AxisSpec where
  render (_ax : AxisSpec) (_allChartData : Array Json) : Html := (Html.text "AxisSpec Fragment (dummy)" : Html)

-- Main Plot Renderer
-- This is a complex part, converting the spec to Recharts JSX
-- For now, we can adapt the existing mkLineChartFull or similar logic

/-- Render the plot. -/
@[inline]
def render (spec : PlotSpec) : Html :=
  let chartComponents := spec.series.map fun s =>
    RenderFragment.render s spec.chartData -- Use the typeclass instance

  let xAxisHtml := match spec.xAxis with
    | some ax =>
      let axProps : LeanPlot.Axis.AxisProps := { dataKey? := some (toJson ax.dataKey), domain? := ax.domain, label? := ax.label, type := .number }
      (<LeanPlot.Axis.XAxis {...axProps} /> : Html)
    | none => (Html.text "" : Html)

  let yAxisHtml := match spec.yAxis with
    | some ax =>
      let axProps : LeanPlot.Axis.AxisProps := { dataKey? := some (toJson ax.dataKey), domain? := ax.domain, label? := ax.label, type := .number }
      (<LeanPlot.Axis.YAxis {...axProps} /> : Html)
    | none => (Html.text "" : Html)

  let legendHtml := if spec.legend then (<Legend /> : Html) else (Html.text "" : Html)

  let mainChartComponent :=
    -- Always use LineChart as the main container, it can host Scatter series too.
    (<LineChart width={spec.width} height={spec.height} data={spec.chartData}>
      {xAxisHtml}
      {yAxisHtml}
      {legendHtml}
      {... chartComponents}
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
