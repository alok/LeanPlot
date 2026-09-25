import LeanPlot.Core.Ticks

/-
Axis and colour scales with Makie semantics.

`Scale` covers Makie's built-in axis scales: `identity`, `log10`, `log2`, `log`
(`ln`), `sqrt`, `Makie.pseudolog10`, `Makie.Symlog10(lower, upper; linscale)`
and `Makie.logit`. For each we provide the forward/inverse transforms, the
interval on which the scale is defined (`defined_interval`), Makie's default
limits (`defaultlimits`), and the automatic tick values and labels
(`get_ticks(automatic, scale, automatic, vmin, vmax)`).

Outside its domain a forward transform returns NaN (Julia would throw a
`DomainError`); `log(0) = -Inf` as in Julia.

`Scale.reversible` is Makie's `ReversibleScale(forward, inverse; limits, interval, name)`
(`src/types.jl:509-548`): any pair of mutually inverse functions, with default limits
`inverse.(limits)` and the defined interval `interval` (open, `(-Inf, Inf)` by default). Makie
ticks such an axis like a linear one (Wilkinson ticks of the data range,
`makielayout/lineaxis.jl:602`). The named scales of `src/layouting/transformation.jl:533-609`
(`AsinhScale`, `SinhScale`, `LogScale`, `LuptonAsinhScale`, `PowerScale`) are reversible scales.
-/

namespace LeanPlot

open LeanPlot.Num

/-- An axis or colour scale. -/
inductive Scale where
  /-- No transform. -/
  | identity
  /-- `log10`, defined on `(0, ∞)`. -/
  | log10
  /-- `log2`, defined on `(0, ∞)`. -/
  | log2
  /-- Natural logarithm, defined on `(0, ∞)`. -/
  | ln
  /-- `sqrt`, defined on `[0, ∞)`. -/
  | sqrt
  /-- `sign(x) * log10(|x| + 1)` (Makie `pseudolog10`). -/
  | pseudolog10
  /-- Makie `Symlog10(lower, upper; linscale)`: linear on `[lower, upper]`,
  logarithmic outside (`lower < 0 < upper`, `linscale > 0`). -/
  | symlog10 (lower upper linscale : Float)
  /-- `log(x / (1 - x))`, defined on `(0, 1)` (LogExpFunctions' formula). -/
  | logit
  /-- Makie `ReversibleScale(forward, inverse; limits, interval, name)`: default limits
  `inverse.(limits)`, defined on `interval` (`(lo, hi, loClosed, hiClosed)`). Two reversible
  scales are equal when their names and parameters are (the functions are not compared). -/
  | reversible (name : String) (fwd inv : Float → Float) (limits : Float × Float) (interval : Float × Float × Bool × Bool)
  deriving Inhabited

namespace Scale

/-- Structural equality; reversible scales compare by name, limits and interval. -/
protected def beq : Scale → Scale → Bool
  | identity, identity | log10, log10 | log2, log2 | ln, ln | sqrt, sqrt
  | pseudolog10, pseudolog10 | logit, logit => true
  | symlog10 a b c, symlog10 a' b' c' => a == a' && b == b' && c == c'
  | reversible n _ _ l i, reversible n' _ _ l' i' => n == n' && l == l' && i == i'
  | _, _ => false

instance : BEq Scale := ⟨Scale.beq⟩

instance : Repr Scale where
  reprPrec
    | symlog10 a b c, _ => s!"Scale.symlog10 {a} {b} {c}"
    | reversible n _ _ l _, _ => s!"Scale.reversible \"{n}\" {l}"
    | s, _ => "Scale." ++ (match s with
      | identity => "identity" | log10 => "log10" | log2 => "log2" | ln => "ln" | sqrt => "sqrt"
      | pseudolog10 => "pseudolog10" | logit => "logit" | _ => "?")

/-- Makie `ReversibleScale(forward, inverse; limits = (0, 10), interval = (-Inf, Inf), name)`.
Makie stores the limits as `Float32`; pass binary32 values to reproduce its default limits. -/
def custom (name : String) (fwd inv : Float → Float) (limits : Float × Float := (0, 10))
    (interval : Float × Float × Bool × Bool := (-Num.inf, Num.inf, false, false)) : Scale :=
  reversible name fwd inv limits interval

/-- Makie `AsinhScale(a = 0.1)`: `asinh(x/a)/asinh(1/a)` (`transformation.jl:541-546`). -/
def asinhScale (a : Float := 0.1) : Scale :=
  let k := Float.asinh (1 / a)
  custom "AsinhScale" (fun x => Float.asinh (x / a) / k) (fun y => a * Float.sinh (k * y))

/-- Makie `SinhScale(a = 1/3)`: `sinh(x/a)/sinh(1/a)` (`transformation.jl:555-560`). -/
def sinhScale (a : Float := 1 / 3) : Scale :=
  let k := Float.sinh (1 / a)
  custom "SinhScale" (fun x => Float.sinh (x / a) / k) (fun y => a * Float.asinh (k * y))

/-- Makie `LogScale(a = 1000, base = ℯ)`: `log_b(a·x + 1)/log_b(a + 1)`, default limits `(0, 3)`
(`transformation.jl:569-576`). -/
def logScale (a : Float := 1000) (base : Float := Float.exp 1) : Scale :=
  let lb := Float.log base
  let d := Float.log (a + 1) / lb
  custom "LogScale" (fun x => Float.log (a * x + 1) / lb / d) (fun y => (Float.pow (1 + a) y - 1) / a) (0, 3)

/-- Makie `LuptonAsinhScale(a = 0.1, Q = 0.01, frac = 0.1)` (`transformation.jl:592-597`). -/
def luptonAsinhScale (a : Float := 0.1) (q : Float := 0.01) (frac : Float := 0.1) : Scale :=
  let k := Float.asinh (frac * q)
  custom "LuptonAsinhScale" (fun x => Float.asinh (q * x / a) * frac / k) (fun y => a * Float.sinh (k * y / frac) / q)

/-- Makie `PowerScale(a = 1)`: `x^a` (`transformation.jl:604-609`). -/
def powerScale (a : Float := 1) : Scale :=
  custom "PowerScale" (fun x => Num.powF x a) (fun y => Num.powF y (1 / a))

end Scale

/-- LogExpFunctions' `logistic` lower cut-off for `Float64`. -/
def logisticLower : Float := -744.4400719213812
/-- LogExpFunctions' `logistic` upper cut-off for `Float64`. -/
def logisticUpper : Float := 36.7368005696771

/-- An interval with open/closed ends. -/
structure Interval where
  lo : Float
  hi : Float
  loClosed : Bool := false
  hiClosed : Bool := false
  deriving Repr, Inhabited, BEq

namespace Interval
/-- Membership test. -/
def contains (iv : Interval) (x : Float) : Bool :=
  (if iv.loClosed then iv.lo ≤ x else iv.lo < x) && (if iv.hiClosed then x ≤ iv.hi else x < iv.hi)
end Interval

namespace Scale

/-- Makie's name for the scale. -/
def name : Scale → String
  | identity => "identity" | log10 => "log10" | log2 => "log2" | ln => "log"
  | sqrt => "sqrt" | pseudolog10 => "pseudolog10" | symlog10 .. => "Symlog10" | logit => "logit"
  | reversible n .. => n

/-- Is this one of Makie's `LogFunctions` (`log10`, `log2`, `log`)? -/
def isLog : Scale → Bool
  | log10 | log2 | ln => true
  | _ => false

/-- The label base of a log scale (`"10"`, `"2"`, `"e"`). -/
def logBase : Scale → String
  | log2 => "2" | ln => "e" | _ => "10"

/-- The `Symlog10` shift so that 0 maps to 0. -/
@[inline] private def symShift (lower upper linscale : Float) : Float :=
  (-lower / (upper - lower) * fTwo - fOne) * linscale

/-- Forward transform (data → scaled). NaN outside the domain. -/
def forward : Scale → Float → Float
  | identity, x => x
  | log10, x => if x < fZero then nan else Float.log10 x
  | log2, x => if x < fZero then nan else Float.log2 x
  | ln, x => if x < fZero then nan else Float.log x
  | sqrt, x => if x < fZero then nan else Float.sqrt x
  | pseudolog10, x => sign x * Float.log10 (x.abs + fOne)
  | symlog10 lower upper linscale, x =>
    let y :=
      if lower < x && x < upper then ((x - lower) / (upper - lower) * fTwo - fOne) * linscale
      else sign x * (linscale + Float.log10 (x.abs / (if x > fZero then upper else lower.abs)))
    y - symShift lower upper linscale
  | logit, x =>
    -- LogExpFunctions.logit
    if x < fZero || x > fOne then nan
    else if fTwo * fTwo * x < fOne then -(Float.log (fOne / x - fOne)) else fTwo * Float.atanh (fTwo * x - fOne)
  | reversible _ f _ _ _, x => f x

/-- Inverse transform (scaled → data). -/
def inverse : Scale → Float → Float
  | identity, y => y
  | log10, y => exp10 y
  | log2, y => exp2J y
  | ln, y => expJ y
  | sqrt, y => if y < fZero then nan else y * y
  | pseudolog10, y => sign y * (exp10 y.abs - fOne)
  | symlog10 lower upper linscale, y =>
    let y := y + symShift lower upper linscale
    if y.abs < linscale then (y / linscale + fOne) / fTwo * (upper - lower) + lower
    else sign y * exp10 (y.abs - linscale) * (if y > fZero then upper else lower.abs)
  | logit, y =>
    -- LogExpFunctions.logistic (Float64 bounds)
    if y < logisticLower then fZero else if y > logisticUpper then fOne else
    let e := expJ y
    e / (fOne + e)
  | reversible _ _ g _ _, y => g y

/-- Makie `defined_interval(scale)`. -/
def definedInterval : Scale → Interval
  | log10 | log2 | ln => ⟨0, inf, false, false⟩
  | sqrt => ⟨0, inf, true, false⟩
  | logit => ⟨0, 1, false, false⟩
  | reversible _ _ _ _ (lo, hi, lc, hc) => ⟨lo, hi, lc, hc⟩
  | _ => ⟨-inf, inf, false, false⟩

/-- Makie `validate_limits_for_scale`: both limits inside the defined interval. -/
def validLimits (s : Scale) (lo hi : Float) : Bool :=
  s.definedInterval.contains lo && s.definedInterval.contains hi

/-- Makie `defaultlimits(scale)`: the limits used when an axis has no data. -/
def defaultLimits : Scale → Float × Float
  | identity => (0, 10)
  | sqrt => (0, 100)
  | logit => (0.01, 0.99)
  | pseudolog10 => (0, (inverse pseudolog10 3.0).toFloat32.toFloat)
  | symlog10 l u c => (inverse (symlog10 l u c) (-3), inverse (symlog10 l u c) 3)
  | reversible _ _ g (a, b) _ => (g a, g b)
  | s => (s.inverse 0, s.inverse 3)

/-- Makie's automatic ticks and labels for an axis with this scale over
`[vmin, vmax]` (`get_ticks(automatic, scale, automatic, vmin, vmax)`):
Wilkinson ticks for linear-like scales, `LogTicks` for log scales, decade ticks
for `pseudolog10`/`Symlog10` (falling back to Wilkinson). -/
def ticks (s : Scale) (vmin vmax : Float) : Array Float × Array TickLabel :=
  let linear : Unit → Array Float × Array TickLabel := fun _ =>
    let t := Ticks.wilkinson vmin vmax
    (t, formatTicksAuto t)
  match s with
  | log10 | log2 | ln =>
    let (t, scaled) := Ticks.logTicks s.forward s.inverse vmin vmax
    (t, logTickLabels s.logBase scaled)
  | pseudolog10 =>
    match Ticks.decadeTicks vmin vmax 5 with
    | some t => (t, t.map decadeLabel)
    | none => linear ()
  | symlog10 lower upper _ =>
    if lower ≤ vmin && vmax ≤ upper then linear () else
    let kminPos : Int := max 0 (ceilInt (Float.log10 upper))
    let kminNeg : Int := max 0 (ceilInt (Float.log10 (-lower)))
    match Ticks.decadeTicks vmin vmax 5 kminPos kminNeg with
    | some t => (t, t.map decadeLabel)
    | none => linear ()
  | _ => linear ()

/-- Makie minor ticks `IntervalsBetween(n, mirror)` for this scale. -/
def minorTicks (s : Scale) (n : Nat) (mirror : Bool) (majors : Array Float) (vmin vmax : Float) : Array Float :=
  if s.isLog then Ticks.minorIntervalsBetweenLog s.forward s.inverse n mirror majors vmin vmax
  else Ticks.minorIntervalsBetween n mirror majors vmin vmax

/-- Position of `v` as a fraction of `[lo, hi]` in scaled space (Makie's tick
placement: `(scale(v) - scale(lo)) / (scale(hi) - scale(lo))`). -/
def fraction (s : Scale) (lo hi v : Float) : Float :=
  let a := s.forward lo
  (s.forward v - a) / (s.forward hi - a)

end Scale

end LeanPlot
