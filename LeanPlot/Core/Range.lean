import LeanPlot.Core.Num

/-
Julia ranges, reproduced bit for bit.

* `linRange a b n`: Julia `LinRange(a, b, n)`, element `j` is `(1-t)*a + t*b`
  with `t = j/(n-1)`.
* `range a b n`: Julia `range(a, b; length = n)` for `Float64`, which builds a
  `StepRangeLen` with `TwicePrecision` reference and step (after trying an
  exact rational representation of the endpoints). This is what Julia code
  such as `range(-π, π, length = 201)` samples at, so ports of Julia examples
  must use it to see the same x values.
* `colon a s b`: Julia `a:s:b` for `Float64` (also `TwicePrecision`).
* `rangeStep a s n`: Julia `range(a; step = s, length = n)`.

The port follows `base/twiceprecision.jl` (Julia 1.13) line by line.
-/

namespace LeanPlot.Num

/-- Julia `LinRange(a, b, n)` collected into a `FloatArray`. For `n = 1` the
single element is `a` (Julia requires `a == b` there). -/
def linRange (a b : Float) (n : Nat) : FloatArray :=
  let d : Float := ofInt (if n ≥ 2 then (n - 1 : Nat) else 1)
  let rec go (j : Nat) (acc : FloatArray) : FloatArray :=
    if j < n then
      let t := (ofInt j) / d
      go (j + 1) (acc.push ((1 - t) * a + t * b))
    else acc
  termination_by n - j
  go 0 (FloatArray.emptyWithCapacity n)

namespace TwicePrecision

/-- Julia `TwicePrecision{Float64}`: an unevaluated sum `hi + lo`. -/
structure TP where
  hi : Float
  lo : Float
  deriving Repr, Inhabited

/-- Julia `canonicalize2(big, little)`. -/
@[inline] def canonicalize2 (big little : Float) : Float × Float :=
  let h := big + little
  (h, (big - h) + little)

/-- Julia `add12(x, y)`. -/
@[inline] def add12 (x y : Float) : Float × Float :=
  if y.abs > x.abs then canonicalize2 y x else canonicalize2 x y

/-- Julia `mul12(x, y)`. -/
@[inline] def mul12 (x y : Float) : Float × Float :=
  let (h, l) := twoMul x y
  if !h.isFinite then (h, h) else (h, l)

/-- Julia `TwicePrecision{Float64}(i::Integer)` (via `splitprec`). -/
def ofInt (i : Int) : TP :=
  let hi := truncBits (Num.ofInt i) 27
  let ihi := toIntExact hi
  let lo := Num.ofInt (i - ihi)
  let (h, l) := canonicalize2 hi lo
  ⟨h, l⟩

/-- Julia `/(x::TwicePrecision, y::TwicePrecision)`. -/
def div (x y : TP) : TP :=
  let hi := x.hi / y.hi
  let (uh, ul) := mul12 hi y.hi
  let lo := ((((x.hi - uh) - ul) + x.lo) - hi * y.lo) / y.hi
  if hi == 0 || !hi.isFinite then ⟨hi, hi⟩
  else let (h, l) := canonicalize2 hi lo; ⟨h, l⟩

/-- Julia `TwicePrecision{Float64}((n, d))` for integers. -/
def ofRatio (n d : Int) : TP := div (ofInt n) ⟨Num.ofInt d, 0⟩

/-- Julia `twiceprecision(val::TwicePrecision, nb)`. -/
def truncate (v : TP) (nb : Nat) : TP :=
  let hi := truncBits v.hi nb
  ⟨hi, (v.hi - hi) + v.lo⟩

/-- A `StepRangeLen{Float64, TwicePrecision, TwicePrecision}`. -/
structure SRL where
  ref : TP
  step : TP
  len : Nat
  offset : Int
  deriving Repr, Inhabited

/-- Julia `unsafe_getindex(r::StepRangeLen{T,<:TwicePrecision,<:TwicePrecision}, i)`
for 1-based `i`. -/
@[inline] def SRL.get (r : SRL) (i : Int) : Float :=
  let u := Num.ofInt (i - r.offset)
  let shiftHi := u * r.step.hi
  let shiftLo := u * r.step.lo
  let (xHi, xLo) := add12 r.ref.hi shiftHi
  xHi + (xLo + (shiftLo + r.ref.lo))

/-- `collect(r)`. -/
def SRL.collect (r : SRL) : FloatArray :=
  let rec go (i : Nat) (acc : FloatArray) : FloatArray :=
    if i < r.len then go (i + 1) (acc.push (r.get (i + 1))) else acc
  termination_by r.len - i
  go 0 (FloatArray.emptyWithCapacity r.len)

/-- Julia `nbitslen(Float64, len, offset)`. -/
def nbitslen (len : Nat) (offset : Int) : Nat :=
  let raw : Nat :=
    if len < 2 then 0
    else topSetBit ((max (offset - 1) ((len : Int) - offset) - 1).toNat) + 1
  min 27 raw

/-- `steprangelen_hp(Float64, (ref_n, ref_d), (step_n, step_d), nb, len, offset)`. -/
def hpRatio (refN refD stepN stepD : Int) (nb len : Nat) (offset : Int) : SRL :=
  ⟨ofRatio refN refD, truncate (ofRatio stepN stepD) nb, len, offset⟩

/-- `steprangelen_hp(Float64, ref, step, nb, len, offset)` with float (pair) data. -/
def hpFloat (ref step : TP) (nb len : Nat) (offset : Int) : SRL :=
  ⟨ref, truncate step nb, len, offset⟩

/-- Julia `rat(x)`: a continued-fraction rational approximation `(num, den)`
with `|num|, |den| ≤ 2^24` (`maxintfloat(Float32)`). -/
def rat (x : Float) : Int × Int :=
  let m : Float := fTwoPow24
  let mi : Int := 16777216
  let rec go (fuel : Nat) (y : Float) (a b c d : Int) : Int × Int :=
    match fuel with
    | 0 => (a, b)
    | fuel + 1 =>
      if y.abs ≤ m then
        let f := truncInt y
        let y := y - Num.ofInt f
        let (a, c) := (f * a + c, a)
        let (b, d) := (f * b + d, b)
        if !(max a.natAbs b.natAbs ≤ mi.natAbs) then (c, d)
        else if Num.ofInt a / Num.ofInt b == x then (a, b)
        else go fuel (fOne / y) a b c d
      else (a, b)
  go 200 x 1 0 0 1

/-- Julia `lcm_unchecked(a, b) = a * div(b, gcd(a, b))`. -/
def lcmUnchecked (a b : Int) : Int :=
  let g : Int := Int.gcd a b
  if g == 0 then 0 else a * Int.tdiv b g

/-- `_linspace(Float64, start_n, stop_n, len, den)` (rational endpoints). -/
def linspaceRat (startN stopN : Int) (len : Nat) (den : Int) : SRL :=
  if startN == stopN then hpRatio startN den 0 den 0 len 1 else
  let tmin := (Num.ofInt (-startN)) / (Num.ofInt stopN - Num.ofInt startN)
  let imin := roundInt (tmin * Num.ofInt (len - 1 : Nat) + 1)
  let imin : Int := max 1 (min imin len)
  let refNum := ((len : Int) - imin) * startN + (imin - 1) * stopN
  let refDen := ((len : Int) - 1) * den
  hpRatio refNum refDen (stopN - startN) refDen (nbitslen len imin) len imin

/-- `_linspace(start::Float64, stop::Float64, len)` (general endpoints, `len ≥ 2`). -/
def linspaceFloat (start stop : Float) (len : Nat) : SRL :=
  let lenF := Num.ofInt len
  let (Δ, Δfac) : Float × Float :=
    let Δ := stop - start
    if !Δ.isFinite then (stop / lenF - start / lenF, lenF) else (Δ, fOne)
  let tmin := -(start / Δ) / Δfac
  let lenn1 : Int := (len : Int) - 1
  let imin := roundInt (tmin * Num.ofInt lenn1 + 1)
  let (imin, ref, step) : Int × Float × Float :=
    if 1 < imin && imin < len then
      let t := Num.ofInt (imin - 1) / Num.ofInt lenn1
      let ref := (1 - t) * start + t * stop
      let step := if imin - 1 < (len : Int) - imin then (ref - start) / Num.ofInt (imin - 1)
                  else (stop - ref) / Num.ofInt ((len : Int) - imin)
      (imin, ref, step)
    else if imin ≤ 1 then (1, start, (Δ / Num.ofInt lenn1) * Δfac)
    else (len, stop, (Δ / Num.ofInt lenn1) * Δfac)
  if len == 2 && !step.isFinite then
    hpFloat ⟨start, 0⟩ ⟨-start, stop⟩ 0 len 1
  else
    let m := prevFloat floatMax
    let k := Num.ofInt (max (imin - 1) ((len : Int) - imin))
    let stepHiPre := clamp step (jmax (-(m + ref) / k) ((-m + ref) / k)) (jmin ((m - ref) / k) ((m + ref) / k))
    let nb := nbitslen len imin
    let stepHi := truncBits stepHiPre nb
    let (x1Hi, x1Lo) := add12 (Num.ofInt (1 - imin) * stepHi) ref
    let (x2Hi, x2Lo) := add12 (Num.ofInt ((len : Int) - imin) * stepHi) ref
    let a := (start - x1Hi) - x1Lo
    let b := (stop - x2Hi) - x2Lo
    let stepLo := (b - a) / Num.ofInt lenn1
    let refLo := a - Num.ofInt (1 - imin) * stepLo
    hpFloat ⟨ref, refLo⟩ ⟨stepHi, stepLo⟩ 0 len imin

/-- Julia `range_start_stop_length(start::Float64, stop::Float64, len)`. -/
def startStopLength (start stop : Float) (len : Nat) : SRL :=
  if len < 2 then ⟨⟨start, 0⟩, ⟨start, -stop⟩, len, 1⟩
  else if start == stop then hpFloat ⟨start, 0⟩ ⟨0, 0⟩ 0 len 1
  else
    -- Julia overwrites the numerators; only the denominators are used
    let (_, startD) := rat start
    let (_, stopD) := rat stop
    let exact : Option SRL :=
      if startD != 0 && stopD != 0 then
        let den := lcmUnchecked startD stopD
        let m : Float := fTwoPow53
        let denF := Num.ofInt den
        if den != 0 && (denF * start).abs ≤ m && (denF * stop).abs ≤ m then
          let sn := roundInt (denF * start)
          let en := roundInt (denF * stop)
          if Num.ofInt sn / denF == start && Num.ofInt en / denF == stop then
            some (linspaceRat sn en len den)
          else none
        else none
      else none
    match exact with
    | some r => r
    | none => linspaceFloat start stop len

/-- Julia `floatrange(Float64, start_n, step_n, len, den)`. -/
def floatrange (startN stepN : Int) (len : Nat) (den : Int) : SRL :=
  if len < 2 || stepN == 0 then hpRatio startN den stepN den 0 len 1 else
  let imin := roundInt (Num.ofInt (-startN) / Num.ofInt stepN + 1)
  let imin : Int := max 1 (min imin len)
  let refN := startN + (imin - 1) * stepN
  hpRatio refN den stepN den (nbitslen len imin) len imin

/-- Julia `isbetween(a, x, b)`. -/
@[inline] def isBetween (a x b : Float) : Bool := (a ≤ x && x ≤ b) || (b ≤ x && x ≤ a)

/-- Julia `(:)(start::Float64, step::Float64, stop::Float64)`. Returns `none`
for a zero step (Julia throws). -/
def colonSRL (start step stop : Float) : Option SRL :=
  if step == 0 then none else
  let (stepN, stepD) := rat step
  let exact : Option SRL :=
    if stepD != 0 && Num.ofInt stepN / Num.ofInt stepD == step then
      let (startN, startD) := rat start
      let (stopN, stopD) := rat stop
      if startD != 0 && stopD != 0 &&
          Num.ofInt startN / Num.ofInt startD == start && Num.ofInt stopN / Num.ofInt stopD == stop then
        let den := lcmUnchecked startD stepD
        let m : Float := fTwoPow53
        let denF := Num.ofInt den
        if den != 0 && (start * denF).abs ≤ m && (step * denF).abs ≤ m &&
            Int.tmod den startD == 0 && Int.tmod den stepD == 0 then
          let startN := roundInt (start * denF)
          let stepN := roundInt (step * denF)
          let lenI : Int := max 0 (Int.tdiv (den * stopN - stopD * startN + stepN * stopD) (stepN * stopD))
          let len := lenI.toNat
          if isBetween start (start + Num.ofInt (lenI - 1) * step) (stop + step / 2) &&
              !isBetween start (start + Num.ofInt lenI * step) stop then
            some (floatrange startN stepN len den)
          else none
        else none
      else none
    else none
  match exact with
  | some r => some r
  | none =>
    let lf := (stop - start) / step
    let len : Nat :=
      if lf < 0 then 0
      else if lf == 0 then 1
      else
        let len := roundInt lf + 1
        let stop' := start + Num.ofInt (len - 1) * step
        let over : Int := (if start < stop && stop < stop' then 1 else 0) + (if start > stop && stop > stop' then 1 else 0)
        (len - over).toNat
    some (hpFloat ⟨start, 0⟩ ⟨step, 0⟩ 0 len 1)

/-- Julia `range_start_step_length(a::Float64, st::Float64, len)`. -/
def startStepLength (a st : Float) (len : Nat) : SRL :=
  let (startN, startD) := rat a
  let (stepN, stepD) := rat st
  let exact : Option SRL :=
    if startD != 0 && stepD != 0 &&
        Num.ofInt startN / Num.ofInt startD == a && Num.ofInt stepN / Num.ofInt stepD == st then
      let den := lcmUnchecked startD stepD
      let m : Float := fTwoPow53
      let denF := Num.ofInt den
      if (denF * a).abs ≤ m && (denF * st).abs ≤ m && Int.tmod den startD == 0 && Int.tmod den stepD == 0 then
        some (floatrange (roundInt (denF * a)) (roundInt (denF * st)) len den)
      else none
    else none
  match exact with
  | some r => r
  | none => hpFloat ⟨a, 0⟩ ⟨st, 0⟩ 0 len 1

end TwicePrecision

/-- Julia `collect(range(a, b; length = n))` for `Float64` endpoints (bit-exact).
Non-finite endpoints give NaNs (Julia throws). -/
def range (a b : Float) (n : Nat) : FloatArray :=
  if n ≥ 2 && !(a.isFinite && b.isFinite) && a != b then
    FloatArray.mk (Array.replicate n nan)
  else (TwicePrecision.startStopLength a b n).collect

/-- Julia `collect(a:s:b)` for `Float64` (bit-exact). A zero step gives `#[]`
(Julia throws). -/
def colon (a s b : Float) : FloatArray :=
  match TwicePrecision.colonSRL a s b with
  | some r => r.collect
  | none => .empty

/-- Julia `collect(range(a; step = s, length = n))` for `Float64` (bit-exact). -/
def rangeStep (a s : Float) (n : Nat) : FloatArray :=
  (TwicePrecision.startStepLength a s n).collect

/-- `n` samples `f(xᵢ)` at `xᵢ = range(a, b, n)`, as structure-of-arrays. -/
@[specialize] def sampleRange (f : Float → Float) (a b : Float) (n : Nat) : FloatArray × FloatArray :=
  let xs := range a b n
  let rec go (i : Nat) (ys : FloatArray) : FloatArray :=
    if h : i < xs.size then go (i + 1) (ys.push (f xs[i])) else ys
  termination_by xs.size - i
  (xs, go 0 (FloatArray.emptyWithCapacity xs.size))

end LeanPlot.Num
