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
  deriving Repr, Inhabited, BEq

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

/-- Is this one of Makie's `LogFunctions` (`log10`, `log2`, `log`)? -/
def isLog : Scale → Bool
  | log10 | log2 | ln => true
  | _ => false

/-- The label base of a log scale (`"10"`, `"2"`, `"e"`). -/
def logBase : Scale → String
  | log2 => "2" | ln => "e" | _ => "10"

/-- The `Symlog10` shift so that 0 maps to 0. -/
@[inline] private def symShift (lower upper linscale : Float) : Float :=
  (-lower / (upper - lower) * 2 - 1) * linscale

/-- Forward transform (data → scaled). NaN outside the domain. -/
def forward : Scale → Float → Float
  | identity, x => x
  | log10, x => if x < 0 then nan else Float.log10 x
  | log2, x => if x < 0 then nan else Float.log2 x
  | ln, x => if x < 0 then nan else Float.log x
  | sqrt, x => if x < 0 then nan else Float.sqrt x
  | pseudolog10, x => sign x * Float.log10 (x.abs + 1)
  | symlog10 lower upper linscale, x =>
    let y :=
      if lower < x && x < upper then ((x - lower) / (upper - lower) * 2 - 1) * linscale
      else sign x * (linscale + Float.log10 (x.abs / (if x > 0 then upper else lower.abs)))
    y - symShift lower upper linscale
  | logit, x =>
    -- LogExpFunctions.logit
    if x < 0 || x > 1 then nan
    else if 4 * x < 1 then -(Float.log (1 / x - 1)) else 2 * Float.atanh (2 * x - 1)

/-- Inverse transform (scaled → data). -/
def inverse : Scale → Float → Float
  | identity, y => y
  | log10, y => exp10 y
  | log2, y => exp2J y
  | ln, y => expJ y
  | sqrt, y => if y < 0 then nan else y * y
  | pseudolog10, y => sign y * (exp10 y.abs - 1)
  | symlog10 lower upper linscale, y =>
    let y := y + symShift lower upper linscale
    if y.abs < linscale then (y / linscale + 1) / 2 * (upper - lower) + lower
    else sign y * exp10 (y.abs - linscale) * (if y > 0 then upper else lower.abs)
  | logit, y =>
    -- LogExpFunctions.logistic (Float64 bounds)
    if y < -744.4400719213812 then 0 else if y > 36.7368005696771 then 1 else
    let e := expJ y
    e / (1 + e)

/-- Makie `defined_interval(scale)`. -/
def definedInterval : Scale → Interval
  | log10 | log2 | ln => ⟨0, inf, false, false⟩
  | sqrt => ⟨0, inf, true, false⟩
  | logit => ⟨0, 1, false, false⟩
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
