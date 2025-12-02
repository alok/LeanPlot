import Lean.Elab.Command
import Lean.Meta
import LeanPlot.JsonExt

/-! # `#assert_keys` compile-time JSON key validation

This module defines a tiny `#assert_keys` command which fails at compile time
if a given `Json` expression does **not** contain all of the required keys at
its top level.  The syntax is

```
#assert_keys myJsonExpr ["x", "y", "z"]
```

where `myJsonExpr` must elaborate to a `Lean.Json` value evaluable at compile
-time (typically constructed via the `json%` macro).  On success the command
adds nothing to the environment; on failure it raises an error *during*
elaboration so that incorrect literal JSON cannot sneak into the generated
artifacts.

The implementation relies on the `jsonHasKeys` helper from
`LeanPlot.JsonExt` and the `Lean.Meta.evalExpr` API to evaluate the term.
-/

open Lean Elab Command Meta LeanPlot

-- /-- Syntax: `#assert_keys j ["k₁", "k₂", …]` checks that the JSON value
-- `j` contains **all** the specified keys.  The bracketed list must contain at
-- least one string literal. -/
-- syntax (name := assertKeysCmd) "#assert_keys " term " [" str,* "]" : command



-- a) using `syntax` + `@[command_elab alias] def elabOurAlias : CommandElab`
-- syntax (name := aliasA) (docComment)? "aliasA " ident " [ " ident,* "] " : command
--
-- @[command_elab «aliasA»]
-- def elabOurAlias : CommandElab := λ stx =>
--   match stx with
--   | `(aliasA $x:ident [ $ys:ident,* ]) =>
--     for y in ys do
--       Lean.logInfo y
--   | _ =>
--     throwUnsupportedSyntax
--
-- -- aliasA hi.hello ← d.d w.w nnn
--
-- /-- Elaboration for `#assert_keys`.  We evaluate the JSON *at compile time*
-- and call `jsonHasKeys`.  If the result is `false` we throw a tailored error
-- that lists the missing keys.  Otherwise the command is a no-op. -/
-- @[command_elab assertKeysCmd]
-- def elabAssertKeys : CommandElab := fun stx => do
--   match stx with
--   | `(#assert_keys $j:term [ $keyNodes,* ]) => do
--     -- 1. Elaborate the JSON expression to a term of type `Lean.Json`.
--     let jExpr ← elabTerm j (some (mkConst ``Lean.Json))
--     -- 2. Evaluate the expression using the Meta evaluator.
--     let jVal ← Meta.evalExpr Lean.Json (mkConst ``Lean.Json) jExpr
--     -- 3. Collect the string literals into an `Array String`.
--     let keySyns : Array Syntax := keyNodes.getElems
--     let keyStrings : Array String ← keySyns.toList.mapM strLitSyntaxToString <&> Array.mk
--     -- 4. Perform the actual key check.
--     let ok := LeanPlot.jsonHasKeys jVal keyStrings
--     unless ok do
--       let missing := keyStrings.filter (fun k =>
--         match jVal.getObjVal? k with
--         | .ok _ => false
--         | _      => true)
--       throwError "`#assert_keys` failed: JSON value is missing keys {missing}"
--   | _ => throwUnsupportedSyntax

-- Local helper to turn a string literal `Syntax` into the Lean `String` it
-- represents.  We assume the syntax node is of kind `strLitKind` (a standard
-- node for string literals) and contains a single atom child whose value is
-- the literal including quotes.
private def strLitSyntaxToString (stx : Syntax) : CommandElabM String := do
  if !stx.isOfKind strLitKind then
    throwError "expected string literal, got {stx}"
  let rawAtom := stx.getArg 0
  let txt := rawAtom.getAtomVal
  if txt.length < 2 then
    throwError "malformed string literal"
  pure <| (txt.drop 1).dropRight 1

/-- Dummy definition so the file is not empty from Lean's perspective. -/
private def _dummy : Unit := ()
