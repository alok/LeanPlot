import LeanPlot.ToFloat

/-! # LeanPlot.AutoDomain

Simple heuristic to infer a reasonable y-axis domain for a numeric function
`f : Float → β` when users do not specify explicit bounds.

We sample `N` points of `f` on the interval `[-1,1]`, compute the minimum and
maximum values, then widen the span by 5 % to give the chart a little padding.
This is **very** naïve but works fine for smooth functions without wild
outliers.  Future versions could adopt more sophisticated strategies such as
robust statistics or user-hinted sampling windows.
-/

namespace LeanPlot.AutoDomain
open LeanPlot ToFloat
open Lean

/--
`autoDomain f N` returns a pair `(lo, hi)` such that the values of `f` on
`[-1,1]` are expected to lie inside that interval.  The result widens the
exact min/max by 5 % (`0.05`) so that rendered plots have a small margin
around the data.

This helper is **purely for convenience**; callers remain free to choose their
own explicit axis limits if desired.
-/
@[inline] def autoDomain {β} [ToFloat β] (f : Float → β) (N : Nat := 100) : Float × Float :=
  if h : N = 0 then
    (0, 1) -- degenerate case; shouldn't happen
  else
    -- Convert `N` to a `Nat` ≥ 1.
    let steps := if N = 0 then 1 else N
    let stepCount : Nat := steps
    -- First sample to initialise `min`/`max`.
    let x₀ : Float := -1
    let y₀ : Float := toFloat (f x₀)
    -- Fold over remaining indices.
    let (lo, hi) :=
      (List.range stepCount).foldl (init := (y₀, y₀)) fun (acc : Float × Float) i =>
        let x : Float := -1.0 + 2.0 * (i.toFloat) / (stepCount.toFloat - 1.0)
        let y : Float := toFloat (f x)
        -- Update running min/max manually (Lean's `Float.min` is a value constant, not a function).
        let lo := if y < acc.fst then y else acc.fst
        let hi := if y > acc.snd then y else acc.snd
        (lo, hi)
    let range := hi - lo
    if range == 0 then
      -- Flat function: widen by ±1 to avoid zero-height chart.
      (lo - 1, hi + 1)
    else
      let pad := range * 0.05
      (lo - pad, hi + pad)

end LeanPlot.AutoDomain
