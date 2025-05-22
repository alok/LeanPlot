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
  /-- Which field of the JSON row contains the coordinate for this axis. When
  `none`, Recharts will attempt to infer it. -/
  dataKey? : Option Json := none
  /-- Optional explicit `[lo, hi]` domain for the axis.  When `none`, Recharts
  determines the range automatically based on the data. -/
  domain? : Option (Array Json) := none
  /-- When `true`, points are allowed to overflow the axis domain instead of
  being clipped.  Defaults to `false` like Recharts. -/
  allowDataOverflow : Bool := false
  /-- How values along this axis should be interpreted. The Recharts default is
  `category`. -/
  type : AxisType := .number
  /-- Text label (or full label specification) for the axis. We expose this
  as `Option Json` rather than `Option String` so that callers can pass a
  rich Recharts label object (e.g. with `angle` / `position` / `dx` fields)
  when they need fine-grained control.  A plain text label can still be
  supplied via `Json.str "my-label"`. -/
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
