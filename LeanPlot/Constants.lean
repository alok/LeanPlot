/-! # LeanPlot.Constants

Module providing library-wide default constants such as the default chart
width/height.  Keeping them in one place makes it easy to tweak the look of all
Tier-0 helpers in one go. -/

namespace LeanPlot.Constants

/-- Default width (in pixels) for Tier-0 `lineChart`/`scatterChart` wrappers. -/
@[inline] def defaultW : Nat := 400

/-- Default height (in pixels) for Tier-0 `lineChart`/`scatterChart` wrappers. -/
@[inline] def defaultH : Nat := 300

end LeanPlot.Constants

/-- Archimedes' constant π as a `Float` with enough precision for plotting
purposes.  Defined once here so that demos can reuse it without duplicate
bindings. -/
@[inline] def _root_.Float.pi : Float := 3.14159265358979323846
