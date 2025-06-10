import Lean
import Lean.Data.Json
import ProofWidgets.Component.HtmlDisplay

/-!
# LeanPlot.LegacyLayer

This module contains the legacy string-based layer specification that predates
the dependent-type `SeriesDSpec` design. It's kept in a separate module to
avoid circular dependencies between `Specification` and `Series`.

This type will be gradually phased out in favor of `SeriesDSpecPacked`.
-/

namespace LeanPlot

/-- Legacy pre-dependent series description (string-based). This will be
gradually phased out. New code should use `SeriesDSpecPacked` from
`LeanPlot.Series`. -/
structure LegacyLayerSpec where
  /-- Name of the series (appears in legends and tooltips). -/
  name     : String
  /-- Which field of the chart-level data rows to plot for this series. -/
  dataKey  : String
  /-- CSS color (e.g. `"#ff0000"`) used to render the series. -/
  color    : String
  /-- The kind of Recharts series to render, such as `"line"` or `"scatter"`. -/
  type     : String := "line"
  /-- Whether to render the point markers (`<Line dot={…}/>`). `none` falls back to the Recharts default. -/
  dot      : Option Bool := none
  deriving Lean.ToJson, Lean.FromJson, Inhabited

end LeanPlot
