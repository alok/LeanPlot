import LeanPlot.Plot
import LeanPlot.API

/-!
# LeanPlot.DSL – Ultra-simple `#plot` syntax

For now, we just re-export the API functions so that users can write:

```lean
#plot plot (fun x => x^2)
```

The ergonomic direct syntax `#plot (fun x => x^2)` is not yet implemented
due to conflicts with the existing command elaborator.
-/

-- Re-export for convenience
export LeanPlot.API (plot plotMany scatter bar)