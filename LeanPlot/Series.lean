import Lean
import LeanPlot.LegacyLayer
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts
import LeanPlot.Components

/-!
# LeanPlot.Series

Foundational data types for a *type-safe* representation of heterogeneous
series/layers in `PlotSpec`. This is the core dependent Σ-type design outlined
in `llms.txt` (2025-05-06 & 2025-05-12 entries), and it now backs the main
rendering pipeline.
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

/-- Convert from the legacy string-based `LegacyLayerSpec` to the new dependent
`SeriesDSpecPacked`. Returns `none` if the `type` field is unrecognised. -/
def LegacyLayerSpec.toSeriesDSpec? (layer : LegacyLayerSpec) : Option SeriesDSpecPacked :=
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
`LegacyLayerSpec` for compatibility with existing code. -/
def SeriesDSpecPacked.toLegacyLayerSpec (packed : SeriesDSpecPacked) : LegacyLayerSpec :=
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

namespace SeriesDSpecPacked

-- convenience accessors
/-- Extract the name of a packed series specification -/
@[inline] def name (p : SeriesDSpecPacked) : String := p.spec.name
/-- Extract the data key of a packed series specification -/
@[inline] def dataKey (p : SeriesDSpecPacked) : String := p.spec.dataKey
/-- Extract the series kind of a packed series specification -/
@[inline] def kind' (p : SeriesDSpecPacked) : SeriesKind := p.kind
/-- Extract the color from a packed series specification -/
@[inline] def color (p : SeriesDSpecPacked) : String :=
  match p.kind, p.spec.details with
  | .line,    SeriesDetails.line d    => d.color
  | .scatter, SeriesDetails.scatter d => d.color
  | .bar,     SeriesDetails.bar d     => d.color
  | .area,    SeriesDetails.area d    => d.fill
/-- Extract the dot visibility setting for line series (if applicable) -/
@[inline] def dot? (p : SeriesDSpecPacked) : Option Bool :=
  match p.kind, p.spec.details with
  | .line, SeriesDetails.line d => some d.dot
  | _, _ => none
/-- Convert the series kind to a string representation -/
@[inline] def typeString (p : SeriesDSpecPacked) : String := toString p.kind

-- constructors
/-- Construct a line series (defaults to no dots). -/
@[inline] def mkLine (name dataKey color : String) (dot : Bool := false) : SeriesDSpecPacked :=
  { kind := .line
    spec := { name := name, dataKey := dataKey, details := .line { color := color, dot := dot } } }

/-- Construct a scatter series. -/
@[inline] def mkScatter (name dataKey color : String) (shape : String := "") : SeriesDSpecPacked :=
  { kind := .scatter
    spec := { name := name, dataKey := dataKey, details := .scatter { color := color, shape := shape } } }

/-- Construct a bar series. -/
@[inline] def mkBar (name dataKey color : String) : SeriesDSpecPacked :=
  { kind := .bar
    spec := { name := name, dataKey := dataKey, details := .bar { color := color } } }

/-- Construct an area series. -/
@[inline] def mkArea (name dataKey fill : String) (stroke : String := "") : SeriesDSpecPacked :=
  { kind := .area
    spec := { name := name, dataKey := dataKey, details := .area { fill := fill, stroke := stroke } } }

-- updates
/-- Update the color for any series kind. -/
@[inline] def setColor (p : SeriesDSpecPacked) (color : String) : SeriesDSpecPacked :=
  match p.kind, p.spec.details with
  | .line,    .line d    => { p with spec := { p.spec with details := .line { d with color := color } } }
  | .scatter, .scatter d => { p with spec := { p.spec with details := .scatter { d with color := color } } }
  | .bar,     .bar d     => { p with spec := { p.spec with details := .bar { d with color := color } } }
  | .area,    .area d    =>
      { p with spec := { p.spec with details := .area { d with fill := color, stroke := if d.stroke.isEmpty then "" else d.stroke } } }

end SeriesDSpecPacked

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
