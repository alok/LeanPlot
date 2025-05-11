import LeanPlot.API
import ProofWidgets.Component.HtmlDisplay

/-! # LeanPlot.DSL – tiny surface syntax

This module introduces a convenience `#plot` command that expands to a call to
`LeanPlot.API.lineChart` and renders the result immediately via `#html`.

Usage examples:
```
#plot (fun x => x)            -- default 200 samples on [0,1]
#plot (fun x => x*x) using 50 -- 50 samples
```

This is intentionally *very* small – just enough to demonstrate how
metaprogramming can raise the ergonomics bar.  Future iterations will evolve
into a richer grammar of graphics.
-/

open Lean
open scoped ProofWidgets.Jsx

/-- Syntax: `#plot f` or `#plot f using 123` -/
syntax "#plot" term ("using" num)? : command

macro_rules
  | `(#plot $f:term) =>
      `(#html LeanPlot.API.lineChart $f)
  | `(#plot $f:term using $n:num) =>
      `(#html LeanPlot.API.lineChart $f (steps := $n))
