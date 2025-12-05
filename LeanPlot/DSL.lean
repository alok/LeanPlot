import LeanPlot.Plot
import LeanPlot.API
import LeanPlot.Constants
import Lean
import ProofWidgets.Component.HtmlDisplay

/-!
# LeanPlot.DSL – Ultra-simple `#plot` syntax

This module implements the ergonomic plotting syntax:

```
#plot (fun x => x^2)            -- default 200 samples
#plot (fun t => Float.sin t) using 400
#plot (fun x => x^2) domain=(-2, 2) steps=100  -- named params
```

With doc comments as captions:

```
/-- A damped oscillator -/
#plot (fun t => Float.exp (-t) * Float.sin (5 * t)) using 200
```

## Named Parameters

The `#plot` command supports named parameters for full control:
- `domain=(lo, hi)` : x-axis range (default: 0 to 1)
- `steps=n` : number of sample points (default: 200)
- `size=(w, h)` : chart dimensions in pixels (default: 400×300)

We intercept the #plot command and check if the argument looks like
a function. If so, we wrap it with {name}`LeanPlot.API.plot` automatically.
-/

namespace LeanPlot.DSL

open Lean Elab Command Term
open ProofWidgets
open LeanPlot.PlotCommand (withCaption)
open LeanPlot.Constants

-- Store the original elaborator before removing it
private def originalElabPlotCmd := LeanPlot.PlotCommand.elabPlotCmd

-- Remove the original elaborator
attribute [-command_elab] LeanPlot.PlotCommand.elabPlotCmd

/-- Syntax for `#plot` with explicit sample count: `#plot f using 400` -/
syntax (name := plotCmdUsing) (docComment)? "#plot " term " using " num : command

/-- Syntax for `#plot` with named parameters: `#plot f domain=(-2, 2) steps=100 size=(500, 300)` -/
syntax (name := plotCmdNamed) (docComment)? "#plot " term
    ("domain=" "(" term "," term ")")?
    ("steps=" num)?
    ("size=" "(" num "," num ")")? : command

/-- Parse a term as a Float, handling negative numbers and various formats -/
private def termToFloat (t : TSyntax `term) (default : Float := 0.0) : Float :=
  -- Try natural literal first
  match t.raw.isNatLit? with
  | some n => n.toFloat
  | none =>
    -- Try negative number (like -2)
    if t.raw.isOfKind `Lean.Parser.Term.app then
      -- Check if it's a negation: (- n)
      let args := t.raw.getArgs
      if args.size >= 2 then
        let fn := args[0]!
        let arg := args[1]!
        if fn.isOfKind `Lean.Parser.Term.paren then
          -- Check for prefix negation
          match arg.isNatLit? with
          | some n => - (n.toFloat)
          | none => default
        else if fn.getId == ``Neg.neg || toString fn == "-" then
          match arg.isNatLit? with
          | some n => - (n.toFloat)
          | none => default
        else default
      else default
    else if t.raw.isOfKind `Lean.Parser.Term.negNum then
      -- Direct negNum syntax (rare but possible)
      match t.raw[1]!.isNatLit? with
      | some n => - (n.toFloat)
      | none => default
    else if t.raw.isOfKind `Lean.Parser.Term.paren then
      -- Parenthesized expression - try to extract inner
      let inner := t.raw[1]!
      match inner.isNatLit? with
      | some n => n.toFloat
      | none => default
    else
      -- Try to get it as a string and parse manually
      let s := t.raw.reprint.getD ""
      let trimmed := s.trim
      -- Simple integer parsing fallback
      if trimmed.startsWith "-" then
        match trimmed.drop 1 |>.toNat? with
        | some n => - (n.toFloat)
        | none => default
      else
        match trimmed.toNat? with
        | some n => n.toFloat
        | none => default

/-- Elaborator for the basic `#plot` command. Wraps functions with `LeanPlot.API.plot`. -/
@[command_elab LeanPlot.PlotCommand.plotCmd]
def elabPlotNew : CommandElab := fun stx => do
  -- Extract doc comment and term from syntax
  let (doc?, term) ← match stx with
    | `($doc:docComment #plot $t:term) => pure (some doc, t)
    | `(#plot $t:term) => pure (none, t)
    | _ => throwUnsupportedSyntax
  -- Try to wrap with plot first (but avoid recursion)
  try
    -- Create the wrapped term
    let wrappedStx ← `(LeanPlot.API.plot $term)
    -- Evaluate it directly as Html
    let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
    let ht ← htX
    -- Wrap with caption if doc comment present
    let finalHtml := match doc? with
      | some doc => withCaption doc.getDocString ht
      | none => ht
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash ProofWidgets.HtmlDisplayPanel.javascript)
      (return json% { html: $(← Server.rpcEncode finalHtml) })
      stx
  catch _ =>
    -- If that fails, use the original implementation
    originalElabPlotCmd stx

/-- Elaborator for the `#plot ... using N` syntax with explicit sample count. -/
@[command_elab plotCmdUsing]
def elabPlotUsing : CommandElab := fun stx => do
  -- Extract doc comment, term, and sample count from syntax
  let (doc?, term, n) ← match stx with
    | `($doc:docComment #plot $t:term using $num) => pure (some doc, t, num)
    | `(#plot $t:term using $num) => pure (none, t, num)
    | _ => throwUnsupportedSyntax
  -- Create the wrapped term with steps
  let wrappedStx ← `(LeanPlot.API.plot $term (steps := $n))
  -- Evaluate it directly as Html
  let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
  let ht ← htX
  -- Wrap with caption if doc comment present
  let finalHtml := match doc? with
    | some doc => withCaption doc.getDocString ht
    | none => ht
  liftCoreM <| Widget.savePanelWidgetInfo
    (hash ProofWidgets.HtmlDisplayPanel.javascript)
    (return json% { html: $(← Server.rpcEncode finalHtml) })
    stx

/-- Elaborator for the `#plot` command with named parameters. -/
@[command_elab plotCmdNamed]
def elabPlotNamed : CommandElab := fun stx => do
  -- Extract doc comment, term, and named parameters from syntax
  let (doc?, term, loT?, hiT?, stepsN?, widthN?, heightN?) ← match stx with
    | `($doc:docComment #plot $t:term $[domain=($lo:term, $hi:term)]? $[steps=$n:num]? $[size=($w:num, $h:num)]?) =>
        pure (some doc, t, lo, hi, n, w, h)
    | `(#plot $t:term $[domain=($lo:term, $hi:term)]? $[steps=$n:num]? $[size=($w:num, $h:num)]?) =>
        pure (none, t, lo, hi, n, w, h)
    | _ => throwUnsupportedSyntax

  -- Parse domain if provided
  let domainOpt : Option (Float × Float) := match loT?, hiT? with
    | some lo, some hi => some (termToFloat lo, termToFloat hi)
    | _, _ => none

  -- Parse steps
  let steps := stepsN?.map (·.getNat) |>.getD 200

  -- Parse size
  let width := widthN?.map (·.getNat) |>.getD defaultW
  let height := heightN?.map (·.getNat) |>.getD defaultH

  -- Build the wrapped term based on what's provided
  let wrappedStx ← match domainOpt with
    | some (lo, hi) =>
      let loLit := Syntax.mkNumLit (toString lo)
      let hiLit := Syntax.mkNumLit (toString hi)
      let stepsLit := Syntax.mkNumLit (toString steps)
      let wLit := Syntax.mkNumLit (toString width)
      let hLit := Syntax.mkNumLit (toString height)
      `(LeanPlot.API.plot $term (steps := $stepsLit) (domain := some ($loLit, $hiLit)) (w := $wLit) (h := $hLit))
    | none =>
      let stepsLit := Syntax.mkNumLit (toString steps)
      let wLit := Syntax.mkNumLit (toString width)
      let hLit := Syntax.mkNumLit (toString height)
      `(LeanPlot.API.plot $term (steps := $stepsLit) (w := $wLit) (h := $hLit))

  -- Evaluate it directly as Html
  let htX ← liftTermElabM <| HtmlCommand.evalCommandMHtml <| ← ``(ProofWidgets.HtmlEval.eval $wrappedStx)
  let ht ← htX

  -- Wrap with caption if doc comment present
  let finalHtml := match doc? with
    | some doc => withCaption doc.getDocString ht
    | none => ht

  liftCoreM <| Widget.savePanelWidgetInfo
    (hash ProofWidgets.HtmlDisplayPanel.javascript)
    (return json% { html: $(← Server.rpcEncode finalHtml) })
    stx

end LeanPlot.DSL

-- Re-export for convenience
export LeanPlot.API (plot plotMany scatter bar)
