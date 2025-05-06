import Std.Internal.Rat

/-! # `ToFloat` typeclass
--  A light-weight abstraction for types that can be coerced (lossily) to `Float`.
--  This powers the generic sampling helpers in `LeanPlot.Components` so that users
--  are not restricted to returning `Float` values – any numeric-like type with a
--  `[ToFloat]` instance will work out-of-the-box.
-/

/-! # `ToFloat` typeclass

Provides a **light-weight** abstraction for types that can be coerced
(lossily) to `Float`.  This powers the generic sampling helpers in
`LeanPlot.Components` so that users are not restricted to returning `Float`
values – any numeric-like type with a `[ToFloat]` instance will work
out-of-the-box.
-/

namespace LeanPlot

/-- Typeclass for converting a value to a `Float`.  The conversion might be lossy
    (e.g. when the input is `Int` or `Nat`).  The design purpose is **not** to
    provide a formal embedding – merely a convenient bridge for visualisation
    where `Float` is the lingua franca of JS charting libraries. -/
class ToFloat (α : Type u) : Type u where
  /-- Convert a value to a `Float`. -/
  toFloat : α → Float

export ToFloat (toFloat)

/-- Helper function version (allows `toFloat a` instead of `ToFloat.toFloat a`). -/
@[inline] def toFloatFn {α} [ToFloat α] (a : α) : Float :=
  ToFloat.toFloat a

-- Instances -------------------------------------------------------------------

instance instToFloatFloat : ToFloat Float where
  toFloat := id

instance instToFloatNat : ToFloat Nat where
  toFloat := Float.ofNat

instance instToFloatInt : ToFloat Int where
  toFloat := Float.ofInt

/-- `Coe α Float` is a common pattern in the wild; provide an instance for it.
    This is not the same as `instToFloatFloat` because `Coe α Float` is not
    a `Subsingleton`. -/
instance [Coe α Float] : ToFloat α where
  toFloat a := ↑a

/-! ### `Rat` instance

Lean 4 already ships with a *minimal* rational number type at
`Std.Internal.Rat`.  Batteries/Mathlib re-export it, so we can safely provide a
`ToFloat` instance here unconditionally.
The conversion takes the numerator/denominator and performs a floating-point
division.
-/

instance instToFloatRat : ToFloat Std.Internal.Rat where
  toFloat r := (Float.ofInt r.num) / (Float.ofNat r.den)

end LeanPlot
