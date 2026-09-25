import LeanPlot.Core.Range

/-!
# Float32 emulation for Makie-exact recipe algorithms

Makie stores converted plot data as `Float32` (`el32convert`) and several recipe
algorithms (streamplot, contour levels, contour tracing) run partly in `Float32`
arithmetic. Lean's bulk storage is `FloatArray` (binary64), so Float32 values are
kept in `Float`s that are exactly representable as binary32, and Float32
arithmetic is emulated by rounding: for `+ - * / sqrt` of two binary32 operands,
the binary64 result rounded to binary32 is the correctly rounded binary32 result
(binary64 has more than `2·24 + 2` significand bits, so double rounding is
innocuous).

This module provides the rounding primitives and a bit-exact port of Julia's
`Float32` ranges (`base/twiceprecision.jl`, Julia 1.13): `range(a, b; length)`
and `range(a; step, length)` build `StepRangeLen{Float32, Float64, Float64}`
values whose elements are `Float32(ref + (i - offset) * step)` computed in
binary64.
-/

namespace LeanPlot.Recipes.Algo.F32

open LeanPlot.Num

/-- Round a binary64 value to the nearest binary32 value (ties to even), returned
as a `Float`. -/
@[inline] def r32 (x : Float) : Float := x.toFloat32.toFloat

/-- `r32 x` when `f32`, else `x`: the result of an arithmetic operation in the
selected precision. -/
@[inline] def rnd (f32 : Bool) (x : Float) : Float := if f32 then r32 x else x

/-- Largest finite binary32 value, `floatmax(Float32)`. -/
def floatMax32 : Float := Float.ofBits 0x47EFFFFFE0000000

/-- `maxintfloat(Float32) = 2^24`. -/
def maxIntFloat32 : Float := 16777216.0

/-- Julia `nextfloat(x::Float32)` for a binary32 value held in a `Float`. -/
def nextFloat32 (x : Float) : Float :=
  if x.isNaN || x == inf then x
  else
    let b := x.toFloat32.toBits
    if x == 0 then (Float32.ofBits 1).toFloat
    else if x > 0 then (Float32.ofBits (b + 1)).toFloat
    else (Float32.ofBits (b - 1)).toFloat

/-- Julia `prevfloat(x::Float32)`. -/
def prevFloat32 (x : Float) : Float := -(nextFloat32 (-x))

/-- Julia `Base.truncbits(x::Float32, nb)`: clear the lowest `nb` significand bits. -/
def truncBits32 (x : Float) (nb : Nat) : Float :=
  let b := x.toFloat32.toBits
  (Float32.ofBits (b &&& ((0xFFFFFFFF : UInt32) <<< nb.toUInt32))).toFloat

/-- Julia `nbitslen(Float32, len, offset) = min(cld(24, 2), nbitslen(len, offset))`. -/
def nbitslen32 (len : Nat) (offset : Int) : Nat :=
  let raw : Nat :=
    if len < 2 then 0
    else topSetBit ((max (offset - 1) ((len : Int) - offset) - 1).toNat) + 1
  min 12 raw

/-- Julia `add12(x, y)` in binary32. -/
@[inline] def add12 (x y : Float) : Float × Float :=
  let canon (big little : Float) : Float × Float :=
    let h := r32 (big + little)
    (h, r32 (r32 (big - h) + little))
  if y.abs > x.abs then canon y x else canon x y

/-- Julia `rat(x::Float32)`: continued-fraction rational approximation with
numerator and denominator bounded by `maxintfloat(Float16) = 2048`, evaluated in
binary32 arithmetic. -/
def rat (x : Float) : Int × Int :=
  let m : Float := 2048.0
  let mi : Nat := 2048
  let rec go (fuel : Nat) (y : Float) (a b c d : Int) : Int × Int :=
    match fuel with
    | 0 => (a, b)
    | fuel + 1 =>
      if y.abs ≤ m then
        let f := truncInt y
        let y := r32 (y - Num.ofInt f)
        let (a, c) := (f * a + c, a)
        let (b, d) := (f * b + d, b)
        if !(max a.natAbs b.natAbs ≤ mi) then (c, d)
        else if r32 (r32 (Num.ofInt a) / r32 (Num.ofInt b)) == x then (a, b)
        else go fuel (r32 (fOne / y)) a b c d
      else (a, b)
  go 200 x 1 0 0 1

/-- A Julia `StepRangeLen{Float32, Float64, Float64}`: element `i` (1-based) is
`Float32(ref + (i - offset) * step)` evaluated in binary64. -/
structure SRL32 where
  ref : Float
  step : Float
  len : Nat
  offset : Int
  deriving Repr, Inhabited

/-- Element `i` (1-based). -/
@[inline] def SRL32.get (r : SRL32) (i : Int) : Float :=
  r32 (r.ref + Num.ofInt (i - r.offset) * r.step)

/-- `collect(r)`. -/
def SRL32.collect (r : SRL32) : FloatArray :=
  let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
    if i < r.len then go (i + 1) (acc.push (r.get (i + 1))) else acc
  termination_by r.len - i
  go 0 (FloatArray.emptyWithCapacity r.len)

/-- Julia `Int128 / Int128` (both converted to `Float64`, then divided). -/
@[inline] private def divInt (n d : Int) : Float := Num.ofInt n / Num.ofInt d

/-- `steprangelen_hp(Float32, (ref_n, ref_d), (step_n, step_d), nb, len, offset)`. -/
@[inline] private def hpRatio (refN refD stepN stepD : Int) (len : Nat) (offset : Int) : SRL32 :=
  ⟨divInt refN refD, divInt stepN stepD, len, offset⟩

/-- `_linspace(Float32, start_n, stop_n, len, den)` (rational endpoints). -/
def linspaceRat (startN stopN : Int) (len : Nat) (den : Int) : SRL32 :=
  if len < 2 then
    let s := divInt startN den
    ⟨s, s - divInt stopN den, len, 1⟩
  else if startN == stopN then hpRatio startN den 0 den len 1
  else
    let tmin := Num.ofInt (-startN) / (Num.ofInt stopN - Num.ofInt startN)
    let imin := roundInt (tmin * Num.ofInt (len - 1 : Nat) + 1)
    let imin : Int := max 1 (min imin len)
    let refNum := ((len : Int) - imin) * startN + (imin - 1) * stopN
    let refDen := ((len : Int) - 1) * den
    hpRatio refNum refDen (stopN - startN) refDen len imin

/-- `_linspace(start::Float32, stop::Float32, len)` (general endpoints, `len ≥ 2`). -/
def linspaceFloat (start stop : Float) (len : Nat) : SRL32 :=
  let lenF := Num.ofInt len
  let (Δ, Δfac) : Float × Float :=
    let Δ := r32 (stop - start)
    if !Δ.isFinite then (r32 (r32 (stop / lenF) - r32 (start / lenF)), lenF) else (Δ, fOne)
  let tmin := r32 (r32 (-(r32 (start / Δ))) / Δfac)
  let lenn1 : Int := (len : Int) - 1
  let imin := roundInt (r32 (r32 (tmin * Num.ofInt lenn1) + 1))
  let (imin, ref, step) : Int × Float × Float :=
    if 1 < imin && imin < len then
      let t := Num.ofInt (imin - 1) / Num.ofInt lenn1
      let ref := r32 ((1 - t) * start + t * stop)
      let step := if imin - 1 < (len : Int) - imin then r32 (r32 (ref - start) / Num.ofInt (imin - 1))
                  else r32 (r32 (stop - ref) / Num.ofInt ((len : Int) - imin))
      (imin, ref, step)
    else if imin ≤ 1 then (1, start, r32 (r32 (Δ / Num.ofInt lenn1) * Δfac))
    else (len, stop, r32 (r32 (Δ / Num.ofInt lenn1) * Δfac))
  if len == 2 && !step.isFinite then ⟨start, -start + stop, len, 1⟩
  else
    let m := prevFloat32 floatMax32
    let k := Num.ofInt (max (imin - 1) ((len : Int) - imin))
    let lo := jmax (r32 (r32 (-(r32 (m + ref))) / k)) (r32 (r32 (-m + ref) / k))
    let hi := jmin (r32 (r32 (m - ref) / k)) (r32 (r32 (m + ref) / k))
    let stepHiPre := clamp step lo hi
    let stepHi := truncBits32 stepHiPre (nbitslen32 len imin)
    let (x1Hi, x1Lo) := add12 (r32 (Num.ofInt (1 - imin) * stepHi)) ref
    let (x2Hi, x2Lo) := add12 (r32 (Num.ofInt ((len : Int) - imin) * stepHi)) ref
    let a := r32 (r32 (start - x1Hi) - x1Lo)
    let b := r32 (r32 (stop - x2Hi) - x2Lo)
    let stepLo := r32 (r32 (b - a) / Num.ofInt lenn1)
    let refLo := r32 (a - r32 (Num.ofInt (1 - imin) * stepLo))
    ⟨ref + refLo, stepHi + stepLo, len, imin⟩

/-- Julia `range_start_stop_length(start::Float32, stop::Float32, len)`. -/
def startStopLength (start stop : Float) (len : Nat) : SRL32 :=
  if len < 2 then ⟨start, start - stop, len, 1⟩
  else if start == stop then ⟨start, 0, len, 1⟩
  else
    let (_, startD) := rat start
    let (_, stopD) := rat stop
    let exact : Option SRL32 :=
      if startD != 0 && stopD != 0 then
        let den := TwicePrecision.lcmUnchecked startD stopD
        let denF := r32 (Num.ofInt den)
        if den != 0 && (r32 (denF * start)).abs ≤ maxIntFloat32 && (r32 (denF * stop)).abs ≤ maxIntFloat32 then
          let sn := roundInt (r32 (denF * start))
          let en := roundInt (r32 (denF * stop))
          if r32 (divInt sn den) == start && r32 (divInt en den) == stop then
            some (linspaceRat sn en len den)
          else none
        else none
      else none
    match exact with
    | some r => r
    | none => linspaceFloat start stop len

/-- Julia `floatrange(Float32, start_n, step_n, len, den)`. -/
def floatrange (startN stepN : Int) (len : Nat) (den : Int) : SRL32 :=
  if len < 2 || stepN == 0 then hpRatio startN den stepN den len 1 else
  let imin := roundInt (Num.ofInt (-startN) / Num.ofInt stepN + 1)
  let imin : Int := max 1 (min imin len)
  hpRatio (startN + (imin - 1) * stepN) den stepN den len imin

/-- Julia `range_start_step_length(a::Float32, st::Float32, len)`. -/
def startStepLength (a st : Float) (len : Nat) : SRL32 :=
  let (startN, startD) := rat a
  let (stepN, stepD) := rat st
  let exact : Option SRL32 :=
    if startD != 0 && stepD != 0 && r32 (divInt startN startD) == a && r32 (divInt stepN stepD) == st then
      let den := TwicePrecision.lcmUnchecked startD stepD
      let denF := r32 (Num.ofInt den)
      if (r32 (denF * a)).abs ≤ maxIntFloat32 && (r32 (denF * st)).abs ≤ maxIntFloat32 &&
          Int.tmod den startD == 0 && Int.tmod den stepD == 0 then
        some (floatrange (roundInt (r32 (denF * a))) (roundInt (r32 (denF * st))) len den)
      else none
    else none
  match exact with
  | some r => r
  | none => ⟨a, st, len, 1⟩

/-- Julia `collect(range(a, b; length = n))` for `Float32` endpoints (given as
binary32 values in `Float`s). -/
def range (a b : Float) (n : Nat) : FloatArray := (startStopLength a b n).collect

/-- Julia `collect(range(a; step = s, length = n))` for `Float32` arguments. -/
def rangeStep (a s : Float) (n : Nat) : FloatArray := (startStepLength a s n).collect

/-- Julia `isapprox(x, y)` for two `Float32` values (default `rtol = √eps(Float32)`,
`atol = 0`), evaluated in binary32. -/
def isApprox32 (x y : Float) : Bool :=
  let rtol : Float := r32 (Float.sqrt (Float.ofBits 0x3E80000000000000))  -- √(2⁻²³)
  x == y ||
    (x.isFinite && y.isFinite &&
      r32 (x - y).abs ≤ jmax 0 (r32 (rtol * jmax x.abs y.abs)))

/-- Round every entry to binary32. -/
def roundArray (xs : FloatArray) : FloatArray :=
  let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
    if h : i < xs.size then go (i + 1) (acc.push (r32 xs[i])) else acc
  termination_by xs.size - i
  go 0 (FloatArray.emptyWithCapacity xs.size)

end LeanPlot.Recipes.Algo.F32
