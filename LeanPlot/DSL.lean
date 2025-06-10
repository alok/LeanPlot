import LeanPlot.Plot
import LeanPlot.API

/-!
# LeanPlot.DSL – Simple plot syntax

Adds a macro so you can write:
```lean
#plot (fun x => x^2)
#plot (fun t => Float.sin t) using 300
```
-/

open Lean

-- Add macro to handle function arguments
macro (priority := low) "#plot" f:term : command =>
  `(#html LeanPlot.API.plot $f)

macro (priority := low) "#plot" f:term "using" n:num : command =>
  `(#html LeanPlot.API.plot $f (steps := $n))
