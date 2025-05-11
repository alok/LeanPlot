import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.Constants
import LeanPlot.ToFloat
import LeanPlot.AutoDomain
import LeanPlot.Axis

/-! # LeanPlot.API – Tier-0 zero-config helpers

This module exposes high-level chart helpers with sensible defaults so that
users can go from a Lean function to a rendered plot with essentially zero
boiler-plate.  The philosophy is *progressive disclosure*: we begin with a
tiny "Tier-0" surface area (`lineChart`, `scatterChart`) and allow more
customisation via lower-level constructors such as
`LeanPlot.Components.mkLineChart` when needed.

Future iterations will expand the API hierarchy but the Tier-0 helpers are
expected to remain stable. -/

open LeanPlot.Components LeanPlot.Palette LeanPlot.Constants LeanPlot.Axis
open Lean ProofWidgets
open ProofWidgets.Recharts (LineChart Line LineType)
open scoped ProofWidgets.Jsx

namespace LeanPlot.API

/-- Convert an array of `(x,y)` pairs into the JSON row structure expected by
Recharts. -/
@[inline] def xyArrayToJson (pts : Array (Float × Float)) : Array Json :=
  pts.map fun (x, y) => json% {x: $(toJson x), y: $(toJson y)}

/-- **Tier-0 helper:** Render a line chart from an array of points.
This delegates to `LeanPlot.Components.mkLineChart` under the hood and
uses the first color of `defaultPalette` for the point fill.
-/
@[inline] def mkLineChart (data : Array Json) (seriesStrokes : Array (String × String)) (w h : Nat := 400) : Html :=
  <LineChart width={w} height={h} data={data}>
    <XAxis dataKey?="x" />
    <YAxis />
    {... seriesStrokes.map (fun (name, color) =>
      <Line type={LineType.monotone} dataKey={Json.str name} stroke={color} dot?={some false} />)}
  </LineChart>

/-- **Tier-0 helper:** Render a line chart for a single function
`f : Float → β` with zero configuration.  The function is sampled uniformly
on `[0,1]` using `steps` samples (default 200).  The chart is sized
`defaultW × defaultH` and colored using the first entry of
`Palette.defaultPalette`.

Returns a `ProofWidgets.Html` value that can be rendered with `#plot`.  Example:

```lean
#plot LeanPlot.API.lineChart (fun x => x*x) -- y = x²
``` -/
@[inline] def lineChart {β} [ToFloat β]
  (f : Float → β) (steps : Nat := 200)
  (w : Nat := defaultW) (h : Nat := defaultH) : ProofWidgets.Html :=
  let data := LeanPlot.Components.sample f steps (domainOpt := none)
  -- Assign a color for the single series "y" using the default palette.
  let seriesStrokes := LeanPlot.Palette.autoColors #["y"]

  (<LineChart width={w} height={h} data={data}>
    <XAxis dataKey?="x" label?="x" />
    <YAxis dataKey?="y" label?="y" />
    {... seriesStrokes.map (fun (name, color) =>
      <Line type={LineType.monotone} dataKey={Json.str name} stroke={color} dot?={some false} />)}
  </LineChart>)

/-- **Tier-0 helper:** Render a scatter chart from an array of points.
This delegates to `LeanPlot.Components.mkScatterChart` under the hood and
uses the first color of `defaultPalette` for the point fill.
-/
@[inline] def scatterChart (pts : Array (Float × Float))
  (w : Nat := defaultW) (h : Nat := defaultH) : ProofWidgets.Html :=
  let data := xyArrayToJson pts
  LeanPlot.Components.mkScatterChart data (LeanPlot.Palette.colorFromNat 0) w h

end LeanPlot.API
