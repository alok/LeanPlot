/-
Numeric helpers for the raster hot loops.

Two code-generation traps on `v4.35.0-rc3` make naive float code 10–100×
slower. Both were measured with `sample(1)` on the image blit.

1. `Nat.toFloat` is `Float.ofScientific n false 0`. The compiled
   `Float.ofScientific` rebuilds the big-nat literal `2^53` from a decimal
   string with GMP on every call (≈ 400 ns). `natF` converts through `UInt64`
   instead, which is a single C cast.
2. A float literal such as `0.5` is `Float.ofScientific 5 true 1`. It is
   normally hoisted to a closed term. But inside a branch where some `Bool`
   (or `Nat`) is known to equal one of the literal's components (e.g. under
   `if b then …`, where `b = true`), the compiler substitutes that variable
   for the constant. The literal then stops being closed and is rebuilt,
   through the slow path above, on every evaluation. Minimal repro:
   `def f (b : Bool) (x : Float) := if b then x * 0.5 else x` calls
   `Float.ofScientific(5, b, 1)`. Named top-level constants compile to plain
   `double` globals and are immune, so hot code uses the `K.*` constants below
   instead of literals.
-/

namespace LeanPlot.Raster

/-- `Nat → Float` through `UInt64` (one C cast; saturates above 2⁶⁴). See the
module docstring for why hot code must not use `Nat.toFloat`. -/
@[inline] def natF (n : Nat) : Float := n.toUInt64.toFloat

-- Float constants for hot loops (see the module docstring, trap 2).
namespace K
/-- `0` -/ def zero : Float := 0.0
/-- `1` -/ def one : Float := 1.0
/-- `½` -/ def half : Float := 0.5
/-- `¼` -/ def quarter : Float := 0.25
/-- `¾` -/ def c0_75 : Float := 0.75
/-- `1.5` -/ def c1_5 : Float := 1.5
/-- `2` -/ def two : Float := 2.0
/-- `3` -/ def three : Float := 3.0
/-- `4` -/ def four : Float := 4.0
/-- `6` -/ def six : Float := 6.0
/-- `0.1` -/ def tenth : Float := 0.1
/-- `255` -/ def c255 : Float := 255.0
/-- `1024` -/ def c1024 : Float := 1024.0
/-- Opacity above which a blend is a plain store (`0.998`, i.e. < ½ LSB). -/ def opaqueCut : Float := 0.998
/-- `10⁻⁹` -/ def eps9 : Float := 1.0e-9
/-- `10⁻¹²` -/ def eps12 : Float := 1.0e-12
/-- `10¹⁸` -/ def e18 : Float := 1.0e18
/-- `10³⁰⁰` -/ def huge : Float := 1.0e300
/-- `π` -/ def pi : Float := 3.141592653589793
/-- `π/2` -/ def halfPi : Float := 1.5707963267948966
end K

end LeanPlot.Raster
