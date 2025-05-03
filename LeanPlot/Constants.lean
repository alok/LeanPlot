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
