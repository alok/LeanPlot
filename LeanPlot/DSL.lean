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

-- Override #plot when the argument is a lambda
macro_rules (kind := LeanPlot.PlotCommand.plotCmd)
  | `(#plot fun $x => $body) =>
      `(#html LeanPlot.API.plot (fun $x => $body))
  | `(#plot fun $x : $ty => $body) =>
      `(#html LeanPlot.API.plot (fun $x : $ty => $body))
  | `(#plot (fun $x => $body)) =>
      `(#html LeanPlot.API.plot (fun $x => $body))
  | `(#plot (fun $x : $ty => $body)) =>
      `(#html LeanPlot.API.plot (fun $x : $ty => $body))

-- Handle the "using" variant
syntax "#plot" "(" "fun" ident (":" term)? "=>" term ")" "using" num : command

macro_rules
  | `(#plot (fun $x => $body) using $n) =>
      `(#html LeanPlot.API.plot (fun $x => $body) (steps := $n))
  | `(#plot (fun $x : $ty => $body) using $n) =>
      `(#html LeanPlot.API.plot (fun $x : $ty => $body) (steps := $n))
