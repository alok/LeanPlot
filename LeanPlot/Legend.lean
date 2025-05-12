import ProofWidgets.Component.Recharts



/-! # Legend component wrapper

Extends ProofWidgets Recharts by exposing the `Legend` component that is part
of the Recharts API but not yet surfaced by ProofWidgets.  We only expose a
minimal props record for now – callers can pass an empty structure to accept
Recharts defaults.
-/

namespace LeanPlot.Legend
open Lean ProofWidgets ProofWidgets.Recharts

/-- Props for the Recharts `<Legend>` component.  We start with an empty record
but keep a structure wrapper so we can add fields later without breaking API. -/
structure LegendProps where
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/Legend. -/
@[inline] def Legend : ProofWidgets.Component LegendProps where
  javascript := Recharts.javascript
  «export» := "Legend"

/- Alias so callers can import `(LegendComp)` instead of `(Legend)`, mirroring
   other component naming conventions in this library. -/
@[inline] def LegendComp : ProofWidgets.Component LegendProps := Legend

end LeanPlot.Legend
