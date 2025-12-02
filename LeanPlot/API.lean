import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.Constants
import LeanPlot.ToFloat
import LeanPlot.AutoDomain
import LeanPlot.Axis
import LeanPlot.Metaprogramming
import LeanPlot.Plot
import Lean.Data.Json

/-! # LeanPlot.API – Tier-0 zero-config helpers

This module exposes high-level chart helpers with sensible defaults so that
users can go from a Lean function to a rendered plot with essentially zero
boiler-plate.  The philosophy is *progressive disclosure*: we begin with a
tiny "Tier-0" surface area ({lit}`lineChart`, {lit}`scatterChart`) and allow more
customisation via lower-level constructors such as
{name}`LeanPlot.Components.mkLineChart` when needed.

Future iterations will expand the API hierarchy but the Tier-0 helpers are
expected to remain stable. -/

open LeanPlot.Components LeanPlot.Palette LeanPlot.Constants LeanPlot.Axis
open Lean ProofWidgets
open ProofWidgets.Recharts (LineChart Line LineType)
open scoped ProofWidgets.Jsx

namespace LeanPlot.API

/-- Convert an array of {lit}`(x,y)` pairs into the JSON row structure expected by
Recharts. -/
@[inline] def xyArrayToJson (pts : Array (Float × Float)) : Array Json :=
  pts.map fun (x, y) => json% {x: $(toJson x), y: $(toJson y)}

/-- A thin alias forwarding to {name}`LeanPlot.Components.mkLineChart`.  This keeps
{lit}`LeanPlot.API` free of implementation details while preserving the public
signature users rely on. -/
@[inline] def mkLineChart (data : Array Json)
    (seriesStrokes : Array (String × String)) (w h : Nat := 400) : Html :=
  LeanPlot.Components.mkLineChart data seriesStrokes w h

/-- **Tier-0 helper:** Render a line chart for a single function
{lit}`f : Float → β` with zero configuration.  The function is sampled uniformly
on {lit}`[0,1]` using {lit}`steps` samples (default 200).  The chart is sized
{lit}`defaultW × defaultH` and colored using the first entry of
{name}`Palette.defaultPalette`.

Returns a {lean}`ProofWidgets.Html` value that can be rendered with {lit}`#plot`.  Example:

```lean
#plot LeanPlot.API.lineChart (fun x => x*x) -- y = x²
``` -/
@[inline] def lineChart {β} [ToFloat β]
  (f : Float → β) (steps : Nat := 200)
  (w : Nat := defaultW) (h : Nat := defaultH) : ProofWidgets.Html :=
  let data := LeanPlot.Components.sample f steps (domainOpt := none)
  -- Assign a color for the single series "y" using the default palette.
  let seriesStrokes := LeanPlot.Palette.autoColors #["y"]

  let xLabelJson : Json := json% "x"
  let yLabelJson : Json := json% { value: "y", angle: -90, position: "left" }

  (<LineChart width={w} height={h} data={data}>
    <XAxis dataKey?="x" label?={some xLabelJson} />
    <YAxis dataKey?="y" label?={some yLabelJson} />
    {... seriesStrokes.map (fun (name, color) =>
      <Line type={LineType.monotone} dataKey={Json.str name} stroke={color} dot?={some false} />)}
  </LineChart>)

/-- **Tier-0 helper:** Render a scatter chart from an array of points.
This delegates to {name}`LeanPlot.Components.mkScatterChart` under the hood and
uses the first color of {name}`defaultPalette` for the point fill.
-/
@[inline] def scatterChart (pts : Array (Float × Float))
  (w : Nat := defaultW) (h : Nat := defaultH) : ProofWidgets.Html :=
  let data := xyArrayToJson pts
  LeanPlot.Components.mkScatterChart data (LeanPlot.Palette.colorFromNat 0) w h

/-! ## Simple Plotting (Zero-Configuration Plots)

These functions automatically handle everything for you:
- Axis labels from parameter names
- Colors and styling
- Domain detection
- You just provide the function and get a beautiful plot!

These are now the **recommended** way to create plots in LeanPlot.
-/

/-- **Simple line chart** - Just pass your function, get beautiful plot!

Examples:

  * {lit}`#plot plot (fun t => t^2)` - Automatic "time" labels
  * {lit}`#plot plot (fun x => Float.sin x)` - Automatic "x" labels
  * {lit}`#plot plot (fun i => i * 3) (steps := 100)` - Custom sample count

This is the new recommended way to plot functions. Zero configuration needed!
-/
@[inline] def plot {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)
    (domain : Option (Float × Float) := none)
    (w : Nat := defaultW) (h : Nat := defaultH) : Html :=
  LeanPlot.Components.plotSimple f steps domain w h

/-- **Simple multi-function plot** - Multiple functions, automatic everything!

Examples:

  * {lit}`#plot plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]`
  * {lit}`#plot plotMany #[("linear", fun t => t), ("quadratic", fun t => t^2)] (domain := (0.0, 2.0))`

Automatic colors, legend, and labels. Perfect for comparing functions!
-/
@[inline] def plotMany {β} [ToFloat β] (fns : Array (String × (Float → β)))
    (steps : Nat := 200) (domain : Float × Float := (0.0, 1.0))
    (w : Nat := defaultW) (h : Nat := defaultH) : Html :=
  LeanPlot.Components.plotManySimple fns steps domain w h

/-- **Simple scatter plot** - Points with automatic styling!

Examples:

  * {lit}`#plot scatter (fun x => x + Random.rand)` - Show function with noise
  * {lit}`#plot scatter (fun t => Float.sin t) (steps := 50)` - Fewer points
-/
@[inline] def scatter {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)
    (domain : Option (Float × Float) := none)
    (w : Nat := defaultW) (h : Nat := defaultH) : Html :=
  LeanPlot.Components.scatterSimple f steps domain w h

/-- **Simple bar chart** - Bars with automatic styling!

Examples:

  * {lit}`#plot bar (fun i => i^2) (steps := 10)` - Discrete function as bars
  * {lit}`#plot bar (fun x => Float.floor x) (steps := 20)` - Step function
-/
@[inline] def bar {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)
    (domain : Option (Float × Float) := none)
    (w : Nat := defaultW) (h : Nat := defaultH) : Html :=
  LeanPlot.Components.barSimple f steps domain w h

end LeanPlot.API
