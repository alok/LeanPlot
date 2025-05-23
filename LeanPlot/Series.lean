import Lean

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
  | line
  | scatter
  | bar
  | area
  deriving Repr, BEq, DecidableEq, Inhabited

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
  | line    (d : LineSeriesDetails)    : SeriesDetails SeriesKind.line
  | scatter (d : ScatterSeriesDetails) : SeriesDetails SeriesKind.scatter
  | bar     (d : BarSeriesDetails)     : SeriesDetails SeriesKind.bar
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

end LeanPlot
