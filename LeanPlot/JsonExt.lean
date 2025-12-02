import Lean.Data.Json
import Lean.Elab.Command
import Lean.Meta.Reduce
import Lean.Meta.Tactic.Simp -- Though unused now, it was in the plan, keeping it for now.
open Lean

/-! # LeanPlot.JsonExt

Utility coercions so that string literals can be used wherever a `Json` value
is expected, and similarly for `Option String` → `Option Json`.  These instances
are defined **inside** the `LeanPlot` namespace so they do not leak into user
code unless `open LeanPlot` (or `open LeanPlot.JsonExt`) is used.
-/

namespace LeanPlot

/-- Coerce a `String` to a `Json` string node via `Json.str`. This lets us pass
plain string literals to APIs that expect a `Json` value, e.g.

```
label? := some "x"  -- Works because of this instance
```
-/
@[inline] instance : Coe String Json where
  coe s := Json.str s

/-- Coerce `Option String` to `Option Json` by mapping `Json.str` over the
option.  Allows concise code such as `label? := axis.label` where
`axis.label : Option String` but `label?` expects `Option Json`. -/
@[inline] instance : Coe (Option String) (Option Json) where
  coe
    | some s => some (Json.str s)
    | none   => none

end LeanPlot

namespace Lean

/-- Return the top-level object keys of a `Json` value.  For non-object nodes the
function returns the empty array.  This is a tiny helper used by the
compile-time JSON key validation utilities. -/
@[inline] def Json.keys : Json → Array String
  | _ => #[] -- Placeholder (not used by current helpers)

end Lean

namespace LeanPlot

/-- Boolean helper: `jsonHasKeys j req` returns `true` iff **every** string in
`req` is a key in the top-level object `j`.  Non-object JSON values never have
keys, therefore the result is `false` unless `req` is empty. -/
@[inline] def jsonHasKeys (j : Lean.Json) (req : Array String) : Bool :=
  req.all fun k =>
    match j.getObjVal? k with
    | .ok _ => true
    | _     => false

/-- Propositional variant of `jsonHasKeys`.  This is defined so users can write
`decide (HasKeys j req)` in compile-time assertions. -/
@[simp] def HasKeys (j : Lean.Json) (req : Array String) : Prop :=
  jsonHasKeys j req = true

instance (j : Lean.Json) (req : Array String) : Decidable (HasKeys j req) := by
  unfold HasKeys
  infer_instance

end LeanPlot

/-- Macro to assert that a `Json` object has a specific set of keys at compile time.
If the assertion fails, a compile-time error is raised.

Example:
```
def myJson : Json := Json.mkObj [("foo", 1), ("bar", "baz")]
-- This assertion passes:
#assert_keys myJson #["foo", "bar"]

-- This assertion would fail at compile time:
-- #assert_keys myJson #["foo", "qux"]
```
-/
syntax (name := assertKeys) "#assert_keys " term:max ppSpace term:max : command

/-- Elaborator for the `#assert_keys` macro. -/
@[command_elab assertKeys]
def elabAssertKeys : Elab.Command.CommandElab := fun stx => do
  match stx with
  | `(#assert_keys $jsonTerm $keysTerm) => do
    Elab.Command.liftTermElabM do
      let jsonExpr ← Elab.Term.elabTerm jsonTerm none
      let keysExpr ← Elab.Term.elabTerm keysTerm none
      let jsonType ← Meta.inferType jsonExpr
      let keysType ← Meta.inferType keysExpr
      unless (← Meta.isDefEq jsonType (mkConst ``Json)) do
        throwErrorAt jsonTerm "Expected term of type Json, got {jsonType}"

      -- More robust check for Array String type
      let stringTypeExpr := mkConst ``String
      let arrayStringTypeExpected := mkApp (mkConst ``Array) stringTypeExpr
      unless (← Meta.isDefEq keysType arrayStringTypeExpected) do
        -- Fallback check if direct isDefEq fails due to metavariables
        if !(keysType.isAppOfArity ``Array 1 && (← Meta.isDefEq keysType.appArg! stringTypeExpr)) then
          throwErrorAt keysTerm m!"Expected term of type Array String, got {keysType}"

      let hasKeysProp := mkAppN (mkConst ``LeanPlot.HasKeys) #[jsonExpr, keysExpr]
      let decidableInst ← Meta.synthInstance (mkApp (mkConst ``Decidable) hasKeysProp)
      let decidedTerm := mkAppN (mkConst ``decide) #[hasKeysProp, decidableInst]

      let reducedTerm ← Lean.Meta.reduce (skipTypes := false) (skipProofs := false) decidedTerm

      if reducedTerm.isConstOf ``Bool.false then
        throwError "Compile-time key assertion failed: Json does not have all required keys.\\nJSON: {jsonExpr}\\nKeys: {keysExpr}"
      else if reducedTerm.isConstOf ``Bool.true then
        logInfoAt stx m!"Compile-time key assertion succeeded for {jsonExpr} with keys {keysExpr}"
      else
        throwError "Could not reduce 'decide (HasKeys ...)' to a boolean literal (true/false). Reduction result: {reducedTerm}"
  | _ => Lean.Elab.throwUnsupportedSyntax
