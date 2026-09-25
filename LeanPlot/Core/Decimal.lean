import LeanPlot.Core.Num

/-
Exact decimal conversions between `Float`/`Float32` and decimal strings.

Everything here is computed with exact integer arithmetic, so results are
platform independent and match Julia's `Base.Ryu`:

* `shortest`/`shortest32`: shortest round-tripping digits (`Ryu.reduce_shortest`),
* `writeFixed`: `Ryu.writefixed(x, prec)` (exact, ties to even),
* `writeExp`: `Ryu.writeexp(x, prec)` split into mantissa and exponent,
* `floatOfDecimal`/`parseFloat?`: correctly rounded decimal → `Float`,
* `showFloat`: Julia-style shortest `repr` (for messages and goldens).

These are *not* on the SVG hot path; see `LeanPlot.Num.fmtFixed` for that.
-/

namespace LeanPlot.Num

/-- Decimal digit string of a natural number. -/
def natDigits (n : Nat) : String := toString n

/-- Number of decimal digits of `n` (`0 ↦ 1`). -/
def numDecDigits (n : Nat) : Nat := (toString n).length

/-- Round `num / den` to the nearest natural number, ties to even (`den > 0`). -/
def divRoundEven (num den : Nat) : Nat :=
  let q := num / den
  let r := num % den
  if 2 * r > den || (2 * r == den && q % 2 == 1) then q + 1 else q

/-- Exact value of a dyadic `m · 2^e` as a fraction `(numerator, denominator)`. -/
@[inline] private def dyadicFrac (m : Nat) (e : Int) : Nat × Nat :=
  if e ≥ 0 then (m <<< e.toNat, 1) else (m, 2 ^ (-e).toNat)

/-- Shortest decimal `(k, j)` with `k · 10^j` inside the rounding interval of the
positive binary number `m · 2^e` whose format has `p` significand bits and minimal
exponent `eMin` (the exponent of subnormals). Ties between equally short
candidates go to the one nearest the exact value, then to even `k`. Trailing
zeros are removed from `k`. This is the specification that Ryu implements. -/
def shortestDyadic (m : Nat) (e : Int) (p : Nat) (eMin : Int) : Nat × Int :=
  if m == 0 then (0, 0) else
  let lowerHalf := m == 2 ^ (p - 1) && e > eMin
  let inclusive := m % 2 == 0
  -- interval endpoints and value, all times 2^(e-2)
  let xv := 4 * m
  let xh := 4 * m + 2
  let xl := if lowerHalf then 4 * m - 1 else 4 * m - 2
  let ee : Int := e - 2
  let (nv, d) := dyadicFrac xv ee
  let (nh, _) := dyadicFrac xh ee
  let (nl, _) := dyadicFrac xl ee
  -- starting exponent: an overestimate of log10 of the upper endpoint
  -- (value < 2^l2, and 0.30103 > log10 2); the search walks downward from it
  let l2 : Int := (m.log2 : Int) + 1 + e
  let startJ : Int := Int.fdiv (l2 * 30103) 100000 + 2
  let rec search (fuel : Nat) (j : Int) : Nat × Int :=
    match fuel with
    | 0 => (divRoundEven nv d, 0)
    | fuel + 1 =>
      let (pl, ph, pv, q) :=
        if j ≥ 0 then (nl, nh, nv, d * 10 ^ j.toNat)
        else (nl * 10 ^ (-j).toNat, nh * 10 ^ (-j).toNat, nv * 10 ^ (-j).toNat, d)
      let kmin := if inclusive then (pl + q - 1) / q else pl / q + 1
      let kmax := if inclusive then ph / q else (ph - 1) / q
      if kmin ≤ kmax && kmax > 0 then
        let k0 := divRoundEven pv q
        let k := if k0 < kmin then kmin else if k0 > kmax then kmax else k0
        (k, j)
      else search fuel (j - 1)
  let (k, j) := search 1200 startJ
  let rec strip (fuel : Nat) (k : Nat) (j : Int) : Nat × Int :=
    match fuel with
    | 0 => (k, j)
    | fuel + 1 => if k != 0 && k % 10 == 0 then strip fuel (k / 10) (j + 1) else (k, j)
  strip 400 k j

/-- Shortest round-tripping decimal digits of `|x|` as `(digits, exponent)`,
i.e. `|x| = digits · 10^exponent` after reading back (Julia
`Base.Ryu.reduce_shortest(x::Float64)`). Zero gives `(0, 0)`. -/
def shortest (x : Float) : Nat × Int :=
  let d := decompose x
  -- Ryu reports zero as `(0, -325)`
  if d.man == 0 then (0, -325) else shortestDyadic d.man d.exp 53 (-1074)

/-- Julia `Base.Ryu.reduce_shortest(x::Float32)`. -/
def shortest32 (x : Float32) : Nat × Int :=
  let d := decompose32 x
  -- Ryu reports zero as `(0, -46)`
  if d.man == 0 then (0, -46) else shortestDyadic d.man d.exp 24 (-149)

/-- Insert a decimal point so that the last `prec` digits are fractional,
left-padding with zeros as needed. -/
private def placePoint (digits : String) (prec : Nat) : String :=
  if prec == 0 then digits else
  let padded := if digits.length ≤ prec then "".pushn '0' (prec + 1 - digits.length) ++ digits else digits
  let n := padded.length
  String.ofList (padded.toList.take (n - prec)) ++ "." ++ String.ofList (padded.toList.drop (n - prec))

/-- Julia `Base.Ryu.writefixed(x, prec)`: exactly `prec` fractional digits,
exact rounding with ties to even, sign kept for negative values that round to
zero (`writefixed(-1e-7, 3) = "-0.000"`). -/
def writeFixed (x : Float) (prec : Nat) : String :=
  if x.isNaN then "NaN"
  else if !x.isFinite then (if x > 0 then "Inf" else "-Inf")
  else
    let d := decompose x
    let num := d.man * 10 ^ prec
    let n : Nat := if d.exp ≥ 0 then num <<< d.exp.toNat else divRoundEven num (2 ^ (-d.exp).toNat)
    (if d.neg then "-" else "") ++ placePoint (natDigits n) prec

/-- Scientific decomposition used by `writeExp`: `|x| ≈ digits · 10^(exp - prec)`
with `digits` having exactly `prec + 1` decimal digits (unless `x = 0`). -/
structure SciDigits where
  neg : Bool
  digits : Nat
  exp : Int
  deriving Repr, Inhabited, BEq

/-- Exact scientific rounding of a finite `x` to `prec + 1` significant digits,
ties to even (Ryu `d2exp` semantics). -/
def sciDigits (x : Float) (prec : Nat) : SciDigits :=
  let d := decompose x
  if d.man == 0 then ⟨d.neg, 0, 0⟩ else
  let (fn, fd) := dyadicFrac d.man d.exp
  -- find e10 with 10^e10 ≤ |x| < 10^(e10+1)
  let est : Int := floorInt (Float.log10 x.abs)
  let ge (e10 : Int) : Bool :=   -- |x| ≥ 10^e10
    if e10 ≥ 0 then fn ≥ fd * 10 ^ e10.toNat else fn * 10 ^ (-e10).toNat ≥ fd
  let rec fix (fuel : Nat) (e10 : Int) : Int :=
    match fuel with
    | 0 => e10
    | fuel + 1 =>
      if !ge e10 then fix fuel (e10 - 1)
      else if ge (e10 + 1) then fix fuel (e10 + 1)
      else e10
  let e10 := fix 700 est
  let s : Int := e10 - prec
  let n := if s ≥ 0 then divRoundEven fn (fd * 10 ^ s.toNat) else divRoundEven (fn * 10 ^ (-s).toNat) fd
  if n ≥ 10 ^ (prec + 1) then ⟨d.neg, n / 10, e10 + 1⟩ else ⟨d.neg, n, e10⟩

/-- Julia `Base.Ryu.writeexp(x, prec)` split into its mantissa string (with sign,
`prec` fractional digits) and decimal exponent: `writeExp 1.5e-5 3 = ("1.500", -5)`. -/
def writeExp (x : Float) (prec : Nat) : String × Int :=
  if x.isNaN then ("NaN", 0)
  else if !x.isFinite then ((if x > 0 then "Inf" else "-Inf"), 0)
  else
    let s := sciDigits x prec
    let sign := if s.neg then "-" else ""
    let ds := natDigits s.digits
    let ds := if s.digits == 0 then "".pushn '0' (prec + 1) else ds
    let mant := if prec == 0 then ds else String.ofList (ds.toList.take 1) ++ "." ++ String.ofList (ds.toList.drop 1)
    (sign ++ mant, s.exp)

/-- Julia `Base.Ryu.writeexp(x, prec)` as a string, e.g. `"1.500e-05"`. -/
def writeExpString (x : Float) (prec : Nat) : String :=
  let (m, e) := writeExp x prec
  if !x.isFinite then m else
  let es := toString e.natAbs
  let es := if es.length < 2 then "0" ++ es else es
  m ++ "e" ++ (if e < 0 then "-" else "+") ++ es

/-- Correctly rounded `Float` nearest to `(-1)^neg · digits · 10^e10`
(ties to even), with overflow to `±∞` and underflow to `±0`. -/
def floatOfDecimal (neg : Bool) (digits : Nat) (e10 : Int) : Float :=
  if digits == 0 then (if neg then -0.0 else 0.0)
  else
    let nd : Int := numDecDigits digits
    if nd + e10 > 310 then (if neg then -inf else inf)
    else if nd + e10 < -330 then (if neg then -0.0 else 0.0)
    else if e10 ≥ 0 then roundToFloat neg (digits * 10 ^ e10.toNat) 0
    else
      let den := 10 ^ (-e10).toNat
      let s := 56 + den.log2 - min digits.log2 (56 + den.log2)
      let num := digits <<< s
      let q := num / den
      let r := num % den
      -- append a sticky bit so that ties are resolved correctly
      roundToFloat neg (2 * q + (if r == 0 then 0 else 1)) (-(s : Int) - 1)

/-- Parse a decimal floating-point literal: optional sign, digits with an
optional point, optional exponent (`e`/`E`), or `NaN`/`Inf`/`Infinity`.
Correctly rounded. -/
def parseFloat? (s : String) : Option Float :=
  let cs := s.trimAscii.toString.toList
  let (neg, cs) := match cs with
    | '-' :: r => (true, r)
    | '+' :: r => (false, r)
    | r => (false, r)
  let word := String.ofList cs
  if word == "NaN" || word == "nan" then some nan
  else if word == "Inf" || word == "inf" || word == "Infinity" then some (if neg then -inf else inf)
  else
    -- integer and fraction digits
    let rec digitsLoop (cs : List Char) (acc : Nat) (n : Nat) : Nat × Nat × List Char :=
      match cs with
      | c :: r => if c.isDigit then digitsLoop r (acc * 10 + (c.toNat - '0'.toNat)) (n + 1) else (acc, n, cs)
      | [] => (acc, n, [])
    let (ip, ni, rest) := digitsLoop cs 0 0
    let (m, nf, rest) := match rest with
      | '.' :: r =>
        let (fp, nf, rest') := digitsLoop r ip 0
        (fp, nf, rest')
      | _ => (ip, 0, rest)
    if ni + nf == 0 then none else
    let expPart : Option Int := match rest with
      | [] => some 0
      | c :: r =>
        if c == 'e' || c == 'E' then
          let (eneg, r) := match r with
            | '-' :: r' => (true, r')
            | '+' :: r' => (false, r')
            | r' => (false, r')
          let (ev, ne, r) := digitsLoop r 0 0
          if ne == 0 || !r.isEmpty then none else some (if eneg then -(ev : Int) else ev)
        else none
    match expPart with
    | none => none
    | some ex => some (floatOfDecimal neg m (ex - nf))

/-- Julia-style shortest representation (`repr(x)` for `Float64`): fixed
notation with at least one fractional digit when `1e-4 ≤ |x| < 1e6`,
otherwise `d.ddde±x` (e.g. `"0.1"`, `"1.0e-5"`, `"1.0e6"`, `"-0.0"`). -/
def showFloat (x : Float) : String :=
  if x.isNaN then "NaN"
  else if !x.isFinite then (if x > 0 then "Inf" else "-Inf")
  else
    let sign := if signBit x then "-" else ""
    if x == 0 then sign ++ "0.0" else
    let (k, j) := shortest x
    let ds := natDigits k
    let n := ds.length
    let lead : Int := j + n - 1
    if lead ≥ -4 && lead ≤ 5 then
      if j ≥ 0 then sign ++ ds ++ "".pushn '0' j.toNat ++ ".0"
      else
        let fr := (-j).toNat
        sign ++ placePoint ds fr
    else
      let mant := if n == 1 then ds ++ ".0" else String.ofList (ds.toList.take 1) ++ "." ++ String.ofList (ds.toList.drop 1)
      sign ++ mant ++ "e" ++ toString lead

end LeanPlot.Num
