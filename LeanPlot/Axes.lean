import ProofWidgets.Component.Recharts

/-! # Axis components with label support

This module defines `AxisProps` identical to `ProofWidgets.Recharts.AxisProps` but
adds an optional `label?` field so that callers can set axis labels without
patching the vendored ProofWidgets library.

We then expose `XAxis` and `YAxis` components that delegate to the same JS
exports used by ProofWidgets (`"XAxis"` and `"YAxis"`).
-/

namespace LeanPlot.Axis
open Lean ProofWidgets ProofWidgets.Recharts

/-- Extended props for an axis. The fields match
`ProofWidgets.Recharts.AxisProps` with an extra `label?` string. -/
structure AxisProps where
  dataKey? : Option Json := none
  domain? : Option (Array Json) := none
  allowDataOverflow : Bool := false
  /-- How values along this axis should be interpreted. The Recharts default is
  `category`. -/
  type : AxisType := .number
  /-- Text label for the axis. Appears at the end of the axis by default. -/
  label? : Option String := none
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/XAxis. -/
def XAxis : ProofWidgets.Component AxisProps where
  javascript := Recharts.javascript
  «export» := "XAxis"

/-- See https://recharts.org/en-US/api/YAxis. -/
def YAxis : ProofWidgets.Component AxisProps where
  javascript := Recharts.javascript
  «export» := "YAxis"

end LeanPlot.Axis
