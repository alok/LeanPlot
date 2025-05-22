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
  /-- Text label (or full label specification) for the axis. `Option Json` so
  callers can supply rich objects with rotation/offset metadata. Use
  `Json.str "my-label"` for plain text. -/
  label? : Option Json := none
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
