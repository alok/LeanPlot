import LeanPlot.Core
import LeanPlot.Components
import LeanPlot.Palette
import LeanPlot.Constants
import LeanPlot.ToFloat

/-! # LeanPlot.Algebra – Towards an *algebra of graphics*

This module introduces **composable** plot values that can be combined using
familiar algebraic operators.  At the moment we only support *line plots*
(`LinePlot`) but the design is intentionally open-ended – additional plot kinds
(e.g. scatter, bar, heat-map) can slot into the same `PlotLike` type-class in
future iterations.

The immediate ergonomic win is the ability to *overlay* several functions with
just `+`, mirroring the experience in libraries like ggplot2 or Vega-Lite:

```
open LeanPlot.Algebra

#plot (line "y"   (fun x ↦ x) +
       line "y²"  (fun x ↦ x*x) +
       line "y³"  (fun x ↦ x*x*x))
```

Behind the scenes we still delegate to the existing Tier-0 helpers from
`LeanPlot.Components` but the user no longer has to juggle series arrays or
color assignments manually. -/

open LeanPlot.Components LeanPlot.Palette LeanPlot.Constants
open Lean ProofWidgets
open scoped ProofWidgets.Jsx

/- ## Line plots −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−-/

/- A single *named* series defined by a Lean function. -/
structure LineSeries where
  name : String
  fn   : Float → Float
  deriving Inhabited

/- A collection of `LineSeries` sampled uniformly on `[lo, hi]`. -/
structure LinePlot where
  series : Array LineSeries
  /- Number of samples when discretising each function.  Higher → smoother. -/
  steps  : Nat := 200
  lo     : Float := 0.0
  hi     : Float := 1.0
  deriving Inhabited

namespace LinePlot

/- Overlay two plots by concatenating their series and widening the domain
bounds if necessary.  We *max* the number of steps so we never lose
resolution. -/
@[inline] def overlay (p q : LinePlot) : LinePlot :=
  { series := p.series ++ q.series,
    steps  := max p.steps q.steps,
    lo     := if p.lo ≤ q.lo then p.lo else q.lo,
    hi     := if p.hi ≥ q.hi then p.hi else q.hi }

/- Sample all series, assign colors automatically, and delegate to the core
`mkLineChartFull` helper. -/
@[inline] def toHtml (p : LinePlot)
    (w : Nat := defaultW) (h : Nat := defaultH) : Html :=
  let fns : Array (String × (Float → Float)) :=
    p.series.map (fun s => (s.name, s.fn))
  let data := LeanPlot.Components.sampleMany fns p.steps p.lo p.hi
  let names := p.series.map (·.name)
  let seriesStrokes := LeanPlot.Palette.autoColors names
  let xLabel? : Option String := some "x"
  let yLabel? : Option String :=
    if p.series.size = 1 then some p.series[0]!.name else none
  LeanPlot.Components.mkLineChartFull data seriesStrokes xLabel? yLabel? w h

end LinePlot

/-- Provide a *builder* for a single line series so that users can create plots
without mentioning `LineSeries`/`LinePlot` explicitly. -/
@[inline] def line (name : String) (f : Float → Float)
    (lo : Float := 0.0) (hi : Float := 1.0) (steps : Nat := 200) : LinePlot :=
  { series := #[⟨name, f⟩], lo := lo, hi := hi, steps := steps }

/- ## Instances −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−-/

instance : Render LinePlot where
  render := LinePlot.toHtml

/- Allow `LinePlot` to participate in generic `Plot` overlays without needing
its own `HAdd` instance. -/
instance : ToLayer LinePlot where
  toLayer lp := { html := LinePlot.toHtml lp }
