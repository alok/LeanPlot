import Lean
import LeanPlot.Specification
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts
import LeanPlot.Components

/-!
# LeanPlot.Series

Foundational data types for a *type-safe* representation of heterogeneous
series/layers in `PlotSpec`.  This is the first step towards the dependent
Σ-type design outlined in `llms.txt` (2025-05-06 & 2025-05-12 entries).

At the moment these types are **not** yet integrated with the existing
rendering pipeline; they merely establish the core enum and detail records so
that subsequent commits can build conversion helpers and renderer
instances incrementally without a giant diff.
-/

namespace LeanPlot

/-- Enumeration of the *kind* of chart series/layer we can render.  This will
replace the current runtime `type : String` field in `LayerSpec` with a
compile-time safe alternative. -/
inductive SeriesKind where
  /-- Line chart series type for continuous data. -/
  | line
  /-- Scatter plot series type for discrete points. -/
  | scatter
  /-- Bar chart series type for categorical comparisons. -/
  | bar
  /-- Area chart series type for filled regions. -/
  | area
  deriving Repr, BEq, DecidableEq, Inhabited

/-- Convert SeriesKind to its string representation. -/
instance : ToString SeriesKind where
  toString
    | .line    => "line"
    | .scatter => "scatter"
    | .bar     => "bar"
    | .area    => "area"

/-- Detail options specific to **line** series.  Mirrors the subset of
Recharts `<Line>` props that Tier-0 and Tier-1 helpers currently expose. -/
structure LineSeriesDetails where
  /-- Stroke colour in CSS hex/rgb(a)/named format. -/
  color : String
  /-- Whether to draw dot markers at each sample point.  Defaults to `false`
  (the sensible default for function plots). -/
  dot   : Bool := false
  deriving Repr, Inhabited

/-- Detail options specific to **scatter** series. -/
structure ScatterSeriesDetails where
  /-- Fill colour for the scatter points. -/
  color : String
  /-- SVG/HTML shape identifier recognised by Recharts (`"circle"`,
  `"square"`, …).  We default to Recharts' own default by using an empty
  string here. -/
  shape : String := ""
  deriving Repr, Inhabited

/-- Detail options specific to **bar** series. -/
structure BarSeriesDetails where
  /-- Fill colour for the bars. -/
  color : String
  deriving Repr, Inhabited

/-- Detail options specific to **area** series. -/
structure AreaSeriesDetails where
  /-- Fill colour for the area interior. -/
  fill  : String
  /-- Stroke colour for the boundary line.  Falls back to `fill` if left
  empty. -/
  stroke : String := ""
  deriving Repr, Inhabited

/-- A dependent (Σ-type) wrapper pairing a `SeriesKind` with its corresponding
*detail* record.  This design provides *compile-time* guarantees that the
`details` field matches the declared `kind` without encoding the structure as
an unwieldy mutual inductive. -/
inductive SeriesDetails : SeriesKind → Type where
  /-- Line series with associated styling details. -/
  | line    (d : LineSeriesDetails)    : SeriesDetails SeriesKind.line
  /-- Scatter series with associated styling details. -/
  | scatter (d : ScatterSeriesDetails) : SeriesDetails SeriesKind.scatter
  /-- Bar series with associated styling details. -/
  | bar     (d : BarSeriesDetails)     : SeriesDetails SeriesKind.bar
  /-- Area series with associated styling details. -/
  | area    (d : AreaSeriesDetails)    : SeriesDetails SeriesKind.area

/-- Sigma-encoded series specification combining the *kind*, *name*, *dataKey*
(which JSON field to read from `chartData`), **and** the kind-specific detail
record.  The phantom type parameter `k` keeps Lean's type checker aware of the
relationship between `kind` and `details`. -/
structure SeriesDSpec (k : SeriesKind) where
  /-- Disambiguating name shown in legends/tooltips. -/
  name     : String
  /-- Which key of the chart-level JSON row provides the *y* value for this
  series. -/
  dataKey  : String
  /-- Kind-specific styling & behaviour options. -/
  details  : SeriesDetails k

namespace LeanPlot.SeriesKind

/-- Map a `SeriesKind` to its corresponding detail type. -/
@[reducible] def toDetailType : SeriesKind → Type
  | SeriesKind.line    => LineSeriesDetails
  | SeriesKind.scatter => ScatterSeriesDetails
  | SeriesKind.bar     => BarSeriesDetails
  | SeriesKind.area    => AreaSeriesDetails

/-- Parse a string representation into a `SeriesKind`. -/
def fromString? (s : String) : Option SeriesKind :=
  match s with
  | "line"    => some .line
  | "scatter" => some .scatter
  | "bar"     => some .bar
  | "area"    => some .area
  | _         => none

end LeanPlot.SeriesKind

namespace LeanPlot

/-- Existential wrapper for series specifications with different kinds. This
allows us to put heterogeneous series into arrays. -/
structure SeriesDSpecPacked where
  /-- The kind of series. -/
  kind : SeriesKind
  /-- The wrapped specification. -/
  spec : SeriesDSpec kind

/-- Convert from the legacy string-based `LayerSpec` to the new dependent
`SeriesDSpecPacked`.  Returns `none` if the `type` field is unrecognised. -/
def LayerSpec.toSeriesDSpec? (layer : LayerSpec) : Option SeriesDSpecPacked :=
  SeriesKind.fromString? layer.type |>.map fun kind =>
    let details : SeriesDetails kind :=
      match kind with
      | .line    => .line    ⟨layer.color, layer.dot.getD false⟩
      | .scatter => .scatter ⟨layer.color, ""⟩  -- default shape
      | .bar     => .bar     ⟨layer.color⟩
      | .area    => .area    ⟨layer.color, layer.color⟩
    { kind := kind
      spec := ⟨layer.name, layer.dataKey, details⟩ }

/-- Convert from the new dependent `SeriesDSpecPacked` back to the legacy
`LayerSpec` for compatibility with existing code. -/
def SeriesDSpecPacked.toLayerSpec (packed : SeriesDSpecPacked) : LayerSpec :=
  let ⟨kind, spec⟩ := packed
  let (color, dot) : String × Option Bool :=
    match spec.details with
    | .line    d => (d.color, some d.dot)
    | .scatter d => (d.color, none)
    | .bar     d => (d.color, none)
    | .area    d => (d.fill, none)
  { name    := spec.name
    dataKey := spec.dataKey
    color   := color
    type    := toString kind
    dot     := dot }

end LeanPlot

-- Rendering support

namespace LeanPlot

open ProofWidgets ProofWidgets.Recharts
open scoped ProofWidgets.Jsx
open Lean (toJson Json)

/-- Render a line series to HTML. -/
def renderLine (_name dataKey : String) (details : LineSeriesDetails) : Html :=
  (<Line type={LineType.monotone} dataKey={toJson dataKey} stroke={details.color} dot?={some details.dot} /> : Html)

/-- Render a scatter series to HTML. -/
def renderScatter (_name dataKey : String) (details : ScatterSeriesDetails) : Html :=
  let scatterProps : LeanPlot.Components.ScatterProps := { dataKey := toJson dataKey, fill := details.color }
  (<LeanPlot.Components.Scatter {...scatterProps} /> : Html)

/-- Render a bar series to HTML. -/
def renderBar (_name dataKey : String) (details : BarSeriesDetails) : Html :=
  let barProps : LeanPlot.Components.BarProps := { dataKey := toJson dataKey, fill := details.color }
  (<LeanPlot.Components.Bar {...barProps} /> : Html)

/-- Render an area series to HTML. -/
def renderArea (_name dataKey : String) (details : AreaSeriesDetails) : Html :=
  let areaProps : LeanPlot.Components.AreaProps := {
    dataKey := toJson dataKey,
    fill := details.fill,
    stroke := if details.stroke.isEmpty then details.fill else details.stroke
  }
  (<LeanPlot.Components.Area {...areaProps} /> : Html)

/-- Type-safe renderer for series based on their kind. -/
def renderSeriesByKind (kind : SeriesKind) (name dataKey : String)
    (details : SeriesDetails kind) : Html :=
  match kind, details with
  | .line,    .line d    => renderLine name dataKey d
  | .scatter, .scatter d => renderScatter name dataKey d
  | .bar,     .bar d     => renderBar name dataKey d
  | .area,    .area d    => renderArea name dataKey d

/-- Render a packed series specification to HTML. -/
def SeriesDSpecPacked.render (packed : SeriesDSpecPacked) : Html :=
  renderSeriesByKind packed.kind packed.spec.name packed.spec.dataKey packed.spec.details

end LeanPlot
