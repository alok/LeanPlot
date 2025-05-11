import LeanPlot.API
open LeanPlot.API
open LeanPlot.Constants
open scoped ProofWidgets.Jsx

/-!
# LeanPlot – Tier-0 Ergonomics *scratchpad*

Live notebook for ad-hoc experiments while developing LeanPlot.  **Not** part
of the released API – feel free to wipe or rewrite at will.
-/

/- ## Quick experiments ----------------------------------------------------- -/

#html lineChart (fun x ↦ x) -- cute way of writing `fun x ↦ x`

#html lineChart (fun x ↦ x * x) (steps := 50)

#html scatterChart #[(0.1,0.4), (0.2,0.25), (0.4,0.16), (0.6,0.36), (0.8,0.64), (1.0,1.0)]

/-!
## Historic roadmap (kept for context)

1. `LeanPlot.Constants` – centralise defaults.
2. `autoDomain` helper.
3. `sample`/`sampleMany` accept `Option` bounds.
4. Tier-0 wrappers `lineChart`/`scatterChart`.
5. Palette convenience.
6. Demo & doc refresh.
7. Feedback banner.
8. Clean-up & deprecations.
-/
