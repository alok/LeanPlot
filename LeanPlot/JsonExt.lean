import Lean.Data.Json
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
