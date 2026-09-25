/-
Numeric utilities shared by the plotting core.

* IEEE-754 bit-level helpers (`nextFloat`, `truncBits`, exact decomposition into
  `± m · 2^e`, correctly rounded reconstruction).
* Julia-compatible rounding and integer powers. Makie parity (tick positions,
  label precision) depends on reproducing Julia *bit for bit*, including
  `10.0^-2 == 0.010000000000000002` (Julia's compensated power-by-squaring is not
  correctly rounded) and round-half-even.
* `lerp`, `clamp`, NaN-aware extrema.

`muladd` in Julia is fused on AArch64 (where the oracle runs) unless LLVM
shares the product with another use; we use `Float.fma` where Julia's generated
code fuses and a plain `x*y + z` where it does not (checked against the oracle
for every `10.0^z`, `z ∈ [-330, 330]`).
-/

namespace LeanPlot.Num

/-! ## Constants -/

/-- `2⁻⁵²`, Julia `eps(Float64)`. -/
def eps64 : Float := Float.ofBits 0x3CB0000000000000
/-- Largest finite `Float` (`floatmax(Float64)`). -/
def floatMax : Float := Float.ofBits 0x7FEFFFFFFFFFFFFF
/-- Smallest positive normal `Float` (`floatmin(Float64)`). -/
def floatMin : Float := Float.ofBits 0x0010000000000000
/-- `+∞`. -/
def inf : Float := Float.ofBits 0x7FF0000000000000
/-- A quiet NaN. -/
def nan : Float := Float.ofBits 0x7FF8000000000000
/-- `π` rounded to `Float` (Julia `Float64(π)`). -/
def pi : Float := Float.ofBits 0x400921FB54442D18
/-- `floatmin(Float32)` as a `Float`. -/
def floatMin32 : Float := Float.ofBits 0x3810000000000000

/-! ## Constants for hot code

Lean's code generator can fail to hoist a float literal that sits inside a
branch (the literal's `Bool` sign argument gets merged with the branch
condition) and then rebuilds it with `Float.ofScientific` (big-integer
arithmetic) on every call; `Nat` literals ≥ 2³² are likewise re-parsed from a
string on every use. Hot code therefore refers to these top-level constants. -/

/-- `0.0` as a global. -/
def fZero : Float := 0.0
/-- `1.0` as a global. -/
def fOne : Float := 1.0
/-- `-1.0` as a global. -/
def fMinusOne : Float := -1.0
/-- `2.0` as a global. -/
def fTwo : Float := 2.0
/-- `0.5` as a global. -/
def fHalf : Float := 0.5
/-- `10.0` as a global. -/
def fTen : Float := 10.0
/-- `255.0` as a global. -/
def f255 : Float := 255.0
/-- `2^52` as a global `Nat`. -/
@[noinline] def twoPow52Nat : Nat := 4503599627370496
/-- `2^53` as a global `Nat`. -/
@[noinline] def twoPow53Nat : Nat := 9007199254740992

/-! ## Bit-level helpers -/

/-- Sign bit of `x` (true for `-0.0` and negative NaNs). -/
@[inline] def signBit (x : Float) : Bool := (x.toBits >>> 63) == 1

/-- `x` is `-0.0`. -/
@[inline] def isNegZero (x : Float) : Bool := x.toBits == 0x8000000000000000

/-- `|x|` with the sign of `s` (Julia `copysign`). -/
@[inline] def copySign (x s : Float) : Float :=
  Float.ofBits ((x.toBits &&& 0x7FFFFFFFFFFFFFFF) ||| (s.toBits &&& 0x8000000000000000))

/-- Julia `sign`: `±1` for nonzero numbers, `±0` for signed zeros, NaN for NaN. -/
@[inline] def sign (x : Float) : Float :=
  if x > fZero then fOne else if x < fZero then fMinusOne else x

/-- Julia `nextfloat`. -/
def nextFloat (x : Float) : Float :=
  if x.isNaN || x == inf then x
  else if x == 0 then Float.ofBits 1
  else if x > 0 then Float.ofBits (x.toBits + 1)
  else Float.ofBits (x.toBits - 1)

/-- Julia `prevfloat`. -/
def prevFloat (x : Float) : Float := -(nextFloat (-x))

/-- Clear the lowest `nb` bits of the significand (Julia `Base.truncbits`). -/
@[inline] def truncBits (x : Float) (nb : Nat) : Float :=
  Float.ofBits (x.toBits &&& ((0xFFFFFFFFFFFFFFFF : UInt64) <<< nb.toUInt64))

/-- Error-free product: `(p, e)` with `p = fl(a*b)` and `p + e = a*b` exactly
(barring overflow/underflow). Julia `Base.Math.two_mul`. -/
@[inline] def twoMul (a b : Float) : Float × Float :=
  let p := a * b
  (p, Float.fma a b (-p))

/-- Julia `muladd` (fused on the reference platform). -/
@[inline] def mulAdd (a b c : Float) : Float := Float.fma a b c

/-! ## Julia comparisons -/

/-- Julia `max` for floats: NaN-propagating, `max(-0.0, 0.0) = 0.0`. -/
@[inline] def jmax (a b : Float) : Float :=
  if a.isNaN || b.isNaN then nan
  else if a > b then a else if b > a then b
  else if signBit a then b else a

/-- Julia `min` for floats: NaN-propagating, `min(-0.0, 0.0) = -0.0`. -/
@[inline] def jmin (a b : Float) : Float :=
  if a.isNaN || b.isNaN then nan
  else if a < b then a else if b < a then b
  else if signBit a then a else b

/-- Julia `isapprox(x, y; rtol, atol)` (with `nans = false`). -/
def isApprox (x y : Float) (rtol : Float) (atol : Float := 0.0) : Bool :=
  x == y ||
    (x.isFinite && y.isFinite &&
      (x - y).abs ≤ jmax atol (rtol * jmax x.abs y.abs))

/-! ## Exact decomposition and reconstruction -/

/-- A finite binary number `(-1)^neg · man · 2^exp`. -/
structure Dyadic where
  neg : Bool
  man : Nat
  exp : Int
  deriving Repr, Inhabited, BEq

/-- Exact decomposition of a finite `Float` (subnormals included).
For non-finite inputs the result is meaningless. -/
def decompose (x : Float) : Dyadic :=
  let b := x.toBits
  let neg := (b >>> 63) == 1
  let e := ((b >>> 52) &&& 0x7FF).toNat
  let f := (b &&& 0xFFFFFFFFFFFFF).toNat
  if e == 0 then ⟨neg, f, -1074⟩ else ⟨neg, f + twoPow52Nat, (e : Int) - 1075⟩

/-- Exact decomposition of a finite `Float32`. -/
def decompose32 (x : Float32) : Dyadic :=
  let b := x.toBits
  let neg := (b >>> 31) == 1
  let e := ((b >>> 23) &&& 0xFF).toNat
  let f := (b &&& 0x7FFFFF).toNat
  if e == 0 then ⟨neg, f, -149⟩ else ⟨neg, f + 2 ^ 23, (e : Int) - 150⟩

/-- Round `m / 2^s` to the nearest integer, ties to even. -/
def shiftRoundEven (m s : Nat) : Nat :=
  if s == 0 then m else
  let q := m >>> s
  let r := m % (2 ^ s)
  let half := 2 ^ (s - 1)
  if r > half || (r == half && q % 2 == 1) then q + 1 else q

/-- Correctly rounded (ties to even) `Float` nearest to `(-1)^neg · m · 2^e`,
with correct subnormal and overflow handling. -/
def roundToFloat (neg : Bool) (m : Nat) (e : Int) : Float :=
  let signed (x : Float) : Float := if neg then -x else x
  if m == 0 then signed 0.0 else
  -- exponent of the leading bit
  let top : Int := (m.log2 : Int) + e
  if top > 1023 then signed inf else
  -- quantum exponent: normal numbers keep 53 bits, subnormals have quantum 2^-1074
  let q : Int := max (top - 52) (-1074)
  let (mant, qe) : Nat × Int :=
    if e ≥ q then (m <<< (e - q).toNat, q)
    else (shiftRoundEven m (q - e).toNat, q)
  -- `mant < 2^53` normally; rounding may carry into 2^53
  let (mant, qe) := if mant ≥ twoPow53Nat then (mant >>> 1, qe + 1) else (mant, qe)
  if mant < twoPow52Nat then
    -- subnormal (qe = -1074), encoding is the mantissa itself
    signed (Float.ofBits mant.toUInt64)
  else
    let biased := qe + 1075
    if biased ≥ 2047 then signed inf else
    signed (Float.ofBits ((biased.toNat.toUInt64 <<< 52) ||| (mant - twoPow52Nat).toUInt64))

/-- Correctly rounded conversion of an integer to `Float` (exact below `2^53`;
the native conversion is used there, the exact big-integer path above). The
range test is done on `Int64` so that no big `Nat` literal is materialised per
call (the code generator re-parses `Nat` literals ≥ 2³² on every use). -/
def ofIntExact (i : Int) : Float :=
  let j := i.toInt64
  if j.toInt == i && j < 9007199254740992 && j > -9007199254740992 then j.toFloat
  else roundToFloat (i < 0) i.natAbs 0

/-- The integer value of an integral finite `Float` (exact). Non-integral inputs
are truncated toward zero; non-finite inputs give 0. -/
def toIntExact (x : Float) : Int :=
  if !x.isFinite then 0 else
  -- fast path: the native conversion truncates toward zero and is exact here
  if x.abs < 9.0e18 then x.toInt64.toInt else
  let d := decompose x
  let n : Nat := if d.exp ≥ 0 then d.man <<< d.exp.toNat else d.man >>> (-d.exp).toNat
  if d.neg then -(n : Int) else n

/-! ## Julia rounding -/

/-- `2^52`: every float at or above this magnitude is an integer. -/
private def twoPow52 : Float := 4503599627370496.0

/-- Julia `round(x)` (`RoundNearest`: ties to even), keeping the sign of zero. -/
def roundEven (x : Float) : Float :=
  if !x.isFinite || x.abs ≥ twoPow52 then x
  else
    let r := if x ≥ 0 then (x + twoPow52) - twoPow52 else (x - twoPow52) + twoPow52
    copySign r x

/-- Julia `round(Int, x)`. -/
@[inline] def roundInt (x : Float) : Int := toIntExact (roundEven x)
/-- Julia `floor(Int, x)`. -/
@[inline] def floorInt (x : Float) : Int := toIntExact x.floor
/-- Julia `ceil(Int, x)`. -/
@[inline] def ceilInt (x : Float) : Int := toIntExact x.ceil
/-- Julia `trunc(Int, x)`. -/
@[inline] def truncInt (x : Float) : Int := toIntExact x

/-- Julia `Float64(::Int)` (correctly rounded; exact below `2^53`). -/
@[inline] def ofInt (i : Int) : Float := ofIntExact i

/-! ## Julia integer powers -/

/-- Compensated power-by-squaring, Julia `Base.Math.pow_body(x::Float64, n::Integer)`.
Reproduces Julia bit for bit (with `muladd` fused). -/
def powBody (x : Float) (n : Int) : Float :=
  if n == 3 then x * x * x else
  if n == -2 then (let rx := 1.0 / x; rx * rx) else
  let (x, xnlo, n) : Float × Float × Nat :=
    if n < 0 then
      let rx := 1.0 / x
      let xnlo := if x.isFinite then -(Float.fma x rx (-1.0)) * rx else -0.0
      (rx, xnlo, n.natAbs)
    else (x, -0.0, n.toNat)
  let rec loop (fuel : Nat) (x xnlo y ynlo : Float) (n : Nat) : Float :=
    match fuel with
    | 0 => (let err := mulAdd y xnlo (x * ynlo)
            if x.isFinite && err.isFinite then x * y + err else x * y)
    | fuel + 1 =>
      if n > 1 then
        let (y, ynlo) :=
          if n % 2 == 1 then
            let err := mulAdd y xnlo (x * ynlo)
            let (y', ynlo') := twoMul x y
            (y', ynlo' + err)
          else (y, ynlo)
        let err := x * 2 * xnlo
        let (x', xnlo') := twoMul x x
        loop fuel x' (xnlo' + err) y ynlo (n / 2)
      else
        -- Julia writes `ifelse(isfinite(x) & isfinite(err), muladd(x, y, err), x*y)`;
        -- since `x*y` is needed for the other branch, LLVM shares the product and
        -- the final `muladd` is *not* fused in practice. Reproduce that.
        let err := mulAdd y xnlo (x * ynlo)
        if x.isFinite && err.isFinite then x * y + err else x * y
  loop 128 x xnlo 1.0 0.0 n

/-- Julia `x ^ n` for `x::Float64`, `n::Integer`. Exponents outside
`[-2^12, 3·2^13]` fall back to the C `pow` (Julia uses its own `exp`/`log` there). -/
def powInt (x : Float) (n : Int) : Float :=
  if n == 0 then fOne
  else if -4096 ≤ n && n ≤ 24576 then powBody x n
  else Float.pow x (ofInt n)

/-- Julia `10.0 ^ n` for an integer `n`. -/
@[inline] def pow10 (n : Int) : Float := powInt 10.0 n

/-- Julia `x ^ y` for floats: integral `y` in the power-by-squaring range uses
`powBody` (bit-exact); other exponents use the C `pow` (≤ 1 ulp from Julia). -/
def powF (x y : Float) : Float :=
  if x == 1.0 then 1.0
  else if y.isFinite && y == y.round && y.abs ≤ 24576 then
    let n := toIntExact y
    if n == 0 then 1.0 else powInt x n
  else Float.pow x y

/-! ## Interpolation and clamping -/

/-- Linear interpolation `a + (b - a) * t`. -/
@[inline] def lerp (a b t : Float) : Float := a + (b - a) * t

/-- Julia `clamp(x, lo, hi)`: `x > hi ? hi : (x < lo ? lo : x)`; NaN passes through. -/
@[inline] def clamp (x lo hi : Float) : Float :=
  if x > hi then hi else if x < lo then lo else x

/-- `clamp x 0 1`. -/
@[inline] def clamp01 (x : Float) : Float := clamp x fZero fOne

/-- Replace NaN by `d`. -/
@[inline] def nanTo (d x : Float) : Float := if x.isNaN then d else x

/-! ## NaN-aware reductions over `FloatArray` -/

/-- Minimum and maximum of the finite entries (Makie `extrema_nan`), or `none`
when there is no finite entry. -/
def extremaFinite (xs : FloatArray) : Option (Float × Float) :=
  let rec go (i : Nat) (found : Bool) (lo hi : Float) : Option (Float × Float) :=
    if h : i < xs.size then
      let v := xs[i]
      if v.isFinite then
        if found then go (i + 1) true (if v < lo then v else lo) (if v > hi then v else hi)
        else go (i + 1) true v v
      else go (i + 1) found lo hi
    else if found then some (lo, hi) else none
  termination_by xs.size - i
  go 0 false 0.0 0.0

/-- Minimum and maximum ignoring NaN only (infinities count), or `none`. -/
def extremaNaN (xs : FloatArray) : Option (Float × Float) :=
  let rec go (i : Nat) (found : Bool) (lo hi : Float) : Option (Float × Float) :=
    if h : i < xs.size then
      let v := xs[i]
      if !v.isNaN then
        if found then go (i + 1) true (if v < lo then v else lo) (if v > hi then v else hi)
        else go (i + 1) true v v
      else go (i + 1) found lo hi
    else if found then some (lo, hi) else none
  termination_by xs.size - i
  go 0 false 0.0 0.0

/-- Makie `distinct_extrema_nan`: finite extrema, widened by `±0.5` when they
coincide (so a colour range is never degenerate). `(NaN, NaN)` if empty. -/
def distinctExtrema (xs : FloatArray) : Float × Float :=
  match extremaFinite xs with
  | none => (nan, nan)
  | some (lo, hi) => if lo == hi then (lo - fHalf, hi + fHalf) else (lo, hi)

/-- Number of NaN entries. -/
def countNaN (xs : FloatArray) : Nat :=
  let rec go (i acc : Nat) : Nat :=
    if h : i < xs.size then go (i + 1) (if xs[i].isNaN then acc + 1 else acc) else acc
  termination_by xs.size - i
  go 0 0

/-- True when every entry is finite. -/
def allFinite (xs : FloatArray) : Bool :=
  let rec go (i : Nat) : Bool :=
    if h : i < xs.size then (if xs[i].isFinite then go (i + 1) else false) else true
  termination_by xs.size - i
  go 0

/-! ## Integer helpers -/

/-- Julia `top_set_bit`: number of bits needed to represent `n` (`0 ↦ 0`). -/
def topSetBit (n : Nat) : Nat := if n == 0 then 0 else n.log2 + 1

end LeanPlot.Num
