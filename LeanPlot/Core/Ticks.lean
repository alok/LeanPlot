import LeanPlot.Core.Range
import LeanPlot.Core.Format
import LeanPlot.Core.JuliaExp

/-
Tick locators, ported exactly from Makie 0.24 / PlotUtils 1.5.

* `optimizeTicks`: `PlotUtils.optimize_ticks` (Wilkinson's extended labelling
  search, the Gadfly port), including the non-strict retry and the
  `fallback_ticks` path for (near-)degenerate spans.
* `wilkinson vmin vmax`: Makie's default `WilkinsonTicks(5; k_min = 3)` wrapper
  (`extend_ticks = false, strict_span = true`).
* `logTicks`: Makie `LogTicks(WilkinsonTicks(5, k_min = 3))`: Wilkinson on the
  transformed limits, mapped back through the inverse transform.
* `minorIntervalsBetween`: Makie `IntervalsBetween(n, mirror)`, linear and log
  variants (with Julia `a:s:b` ranges for the mirrored ends).
* `decadeTicks`: Makie's decade picker for `pseudolog10` / `Symlog10` axes.
* `isWithinLimits`: Makie's filter of tick values against the axis limits.

All arithmetic reproduces Julia's evaluation order, `round(x; sigdigits)`,
and `10.0^z`, so tick values are bit-identical to the oracle.
-/

namespace LeanPlot.Ticks

open LeanPlot.Num

/-- Parameters of `PlotUtils.optimize_ticks`. The defaults are Makie's
`WilkinsonTicks(5; k_min = 3)` as called by `Makie.get_tickvalues`. -/
structure WilkinsonConfig where
  kIdeal : Nat := 5
  kMin : Nat := 3
  kMax : Nat := 10
  /-- Nice step multipliers with their niceness scores, in preference order. -/
  q : Array (Float × Float) := #[(1.0, 1.0), (5.0, 0.9), (2.0, 0.7), (2.5, 0.5), (3.0, 0.2)]
  granularityWeight : Float := 1.0 / 4.0
  simplicityWeight : Float := 1.0 / 6.0
  coverageWeight : Float := 1.0 / 3.0
  nicenessWeight : Float := 1.0 / 4.0
  extendTicks : Bool := false
  strictSpan : Bool := true
  spanBuffer : Option Float := none
  deriving Repr, Inhabited

/-- PlotUtils `bounding_order_of_magnitude(xspan, base)`: the smallest integer
`b` with `xspan ≤ base^b` (found by bracketing and bisection, as in PlotUtils). -/
def boundingOrderOfMagnitude (xspan base : Float) : Int :=
  let rec down (fuel : Nat) (a : Int) : Int :=
    match fuel with
    | 0 => a
    | fuel + 1 => if xspan < powInt base a then down fuel (a - 1) else a
  let rec up (fuel : Nat) (b : Int) : Int :=
    match fuel with
    | 0 => b
    | fuel + 1 => if xspan > powInt base b then up fuel (b + 1) else b
  let rec bisect (fuel : Nat) (a b : Int) : Int :=
    match fuel with
    | 0 => b
    | fuel + 1 =>
      if a + 1 < b then
        let c := Int.tdiv (a + b) 2
        if xspan < powInt base c then bisect fuel a c else bisect fuel c b
      else b
  bisect 128 (down 4000 1) (up 4000 1)

/-- Julia `round(x, RoundDown; digits = i)` (base 10). -/
def floorDigits (x : Float) (i : Int) : Float :=
  if !x.isFinite then x
  else if i ≥ 0 then
    let invstep := powInt 10.0 i
    if invstep.isFinite then
      let y := (x * invstep).floor / invstep
      if y.isFinite then y else x
    else
      let s := powF 10.0 (ofInt i / 2)
      let y := ((x * s) * s).floor / s / s
      if y.isFinite then y else x
  else
    let step := powInt 10.0 (-i)
    let y := (x / step).floor * step
    if y.isFinite then y
    else if x > 0 then 0.0 else if x < 0 then -inf else x

/-- PlotUtils `postdecimal_digits(x)`: the first `i ∈ [-308, 309]` with
`floor(x; digits = i) == x`, else 0. -/
def postdecimalDigits (x : Float) : Int :=
  let rec go (fuel : Nat) (i : Int) : Int :=
    match fuel with
    | 0 => 0
    | fuel + 1 => if i > 309 then 0 else if x == floorDigits x i then i else go fuel (i + 1)
  go 700 (-308)

/-- PlotUtils `fallback_ticks`: used when the span is (numerically) empty or no
labelling satisfies the constraints. -/
def fallbackTicks (xMin xMax : Float) (kMin : Nat) (strictSpan : Bool) : Array Float × Float × Float :=
  let (xMin, xMax) :=
    if !strictSpan && isApprox xMin xMax (Float.sqrt eps64) then (prevFloat xMin, nextFloat xMax)
    else (xMin, xMax)
  let ts := if kMin != 2 && xMin.isFinite && xMax.isFinite then (range xMin xMax kMin).toList.toArray
    else #[xMin, xMax]
  (ts, xMin, xMax)

/-- Result of one `optimize_ticks_typed` pass. -/
structure SearchResult where
  highScore : Float
  ticks : Array Float
  viewMin : Float
  viewMax : Float
  deriving Repr, Inhabited

/-- One pass of PlotUtils `optimize_ticks_typed` (base 10, linear scale). -/
def optimizeTicksTyped (xMin xMax : Float) (cfg : WilkinsonConfig) (strictSpan : Bool) : SearchResult := Id.run do
  let base : Float := 10.0
  let xspan := xMax - xMin
  let z0 := boundingOrderOfMagnitude xspan base
  let maxPost := cfg.q.foldl (init := (none : Option Int)) fun acc (qv, _) =>
    let d := postdecimalDigits qv
    some (match acc with | none => d | some m => max m d)
  let numDigits := boundingOrderOfMagnitude (jmax xMin.abs xMax.abs) base + maxPost.getD 0
  let kMin := cfg.kMin
  let kMax := cfg.kMax
  let kIdealF := ofInt cfg.kIdeal
  let mut best : SearchResult := ⟨-inf, #[], xMin, xMax⟩
  let mut z := z0
  -- the outer loop runs a handful of times (while 2k_max·10^(z+1) > xspan)
  for _ in [0:4000] do
    if !(ofInt (2 * kMax) * powInt base (z + 1) > xspan) then break
    let sigdigits : Int := max 1 (numDigits - z)
    let pz := powInt base z
    for k in [kMin:2 * kMax + 1] do
      for (qv, qs) in cfg.q do
        let tickspan := qv * pz
        if tickspan < eps64 then continue
        let span := ofInt ((k : Int) - 1) * tickspan
        if span < xspan then continue
        let rFloat := (xMax - span) / tickspan
        if !rFloat.isFinite then continue
        let mut r : Int := ceilInt rFloat
        -- linear scale: every step is "nice"
        let niceScale := true
        let qscore := qs
        for _ in [0:4 * k + 8] do
          if !(ofInt r * tickspan ≤ xMin) then break
          -- candidate labels
          let (s0, imin, imax) : Array Float × Nat × Nat :=
            if cfg.extendTicks then
              ((Array.range (3 * k)).map (fun (i : Nat) => ofInt (r + (i : Int) - (k : Int)) * tickspan), k, 2 * k - 1)
            else
              ((Array.range k).map (fun (i : Nat) => ofInt (r + (i : Int)) * tickspan), 0, k - 1)
          let vmin0 := roundSigDigits s0[imin]! sigdigits
          let vmax0 := roundSigDigits s0[imax]! sigdigits
          let s1 := (s0.set! imin vmin0).set! imax vmax0
          let (sel, viewmin, viewmax) : Array Float × Float × Float :=
            if strictSpan then
              let viewmin := jmax vmin0 xMin
              let viewmax := jmin vmax0 xMax
              let buf := cfg.spanBuffer.getD 0.0 * (viewmax - viewmin)
              let sel := (s1.extract 0 (imax + 1)).filter fun v => viewmin - buf ≤ v && v ≤ viewmax + buf
              (sel, viewmin, viewmax)
            else (s1.extract 0 (imax + 1), vmin0, vmax0)
          let len := sel.size
          let hasZero := r ≤ 0 && r.natAbs < k
          let s : Float := if hasZero && niceScale then 1.0 else 0.0
          let g : Float :=
            if 0 < len && len < 2 * cfg.kIdeal then
              1 - ofInt ((len : Int) - cfg.kIdeal).natAbs / kIdealF
            else 0.0
          let c : Float :=
            if len > 1 then
              let effectiveSpan := ofInt ((len : Int) - 1) * tickspan
              (1.5 * xspan) / effectiveSpan
            else 0.0
          let mut score :=
            cfg.granularityWeight * g + cfg.simplicityWeight * s +
              cfg.coverageWeight * c + cfg.nicenessWeight * qscore
          if strictSpan && span > xspan then score := score - 10000
          if span ≥ 2 * xspan then score := score - 1000
          if score > best.highScore && kMin ≤ len && len ≤ kMax then
            best := ⟨score, sel, viewmin, viewmax⟩
          r := r + 1
    z := z - 1
  return best

/-- PlotUtils `optimize_ticks(x_min, x_max; ...)` (linear scale): the tick
locations and the chosen view limits. -/
def optimizeTicks (xMin xMax : Float) (cfg : WilkinsonConfig := {}) : Array Float × Float × Float :=
  let rtol := 1000.0 * eps64
  if isApprox xMin xMax rtol then fallbackTicks xMin xMax cfg.kMin cfg.strictSpan else
  let first := optimizeTicksTyped xMin xMax cfg cfg.strictSpan
  if first.highScore != -inf then (first.ticks, first.viewMin, first.viewMax)
  else if !cfg.strictSpan then fallbackTicks xMin xMax cfg.kMin cfg.strictSpan
  else
    -- PlotUtils warns "No strict ticks found" and retries without strict span
    let second := optimizeTicksTyped xMin xMax cfg false
    if second.highScore != -inf then (second.ticks, second.viewMin, second.viewMax)
    else fallbackTicks xMin xMax cfg.kMin cfg.strictSpan

/-- Makie's default tick values for a linear axis:
`get_tickvalues(WilkinsonTicks(5; k_min = 3), vmin, vmax)`. -/
def wilkinson (vmin vmax : Float) (cfg : WilkinsonConfig := {}) : Array Float :=
  (optimizeTicks vmin vmax cfg).1

/-- Makie `get_tickvalues(LogTicks(linear), scale, vmin, vmax)`: Wilkinson ticks
of the transformed limits, mapped back with `inverse`. Returns
`(ticks, scaledTicks)` so labels can be formatted from the exponents. -/
def logTicks (forward inverse : Float → Float) (vmin vmax : Float) (cfg : WilkinsonConfig := {}) :
    Array Float × Array Float :=
  let scaled := wilkinson (forward vmin) (forward vmax) cfg
  (scaled.map inverse, scaled)

/-- Julia `eps(x)` for `Float64` (spacing to the next larger magnitude). -/
def ulp (x : Float) : Float :=
  if !x.isFinite then nan else
  let a := x.abs
  if a == floatMax then powInt 2.0 971 else nextFloat a - a

/-- Makie `is_within_limits`: keep ticks within the limits up to `100 eps`. -/
def isWithinLimits (tv lo hi : Float) : Bool :=
  lo - 100 * ulp lo < tv && tv < hi + 100 * ulp hi

/-- Filter tick values (and parallel labels) to those Makie displays. -/
def filterWithinLimits {α : Type} (vals : Array Float) (labels : Array α) (lo hi : Float) :
    Array Float × Array α :=
  let idx := (Array.range vals.size).filter fun i => isWithinLimits vals[i]! lo hi
  (idx.map (vals[·]!), idx.filterMap (labels[·]?))

/-- Makie `get_minor_tickvalues(IntervalsBetween(n, mirror), identity, ticks, vmin, vmax)`.
Note: the mirrored values before the first major tick come out in descending
order, exactly as Makie produces them. -/
def minorIntervalsBetween (n : Nat) (mirror : Bool) (ticks : Array Float) (vmin vmax : Float) : Array Float :=
  if ticks.size < 2 || n == 0 then #[] else
  let nF := ofInt n
  let pre : Array Float :=
    if mirror then
      let stepsize := (ticks[1]! - ticks[0]!) / nF
      (colon (ticks[0]! - stepsize) (-stepsize) vmin).toList.toArray
    else #[]
  let inner : Array Float := Id.run do
    let mut out := #[]
    for i in [0:ticks.size - 1] do
      let lo := ticks[i]!
      let hi := ticks[i + 1]!
      let stepsize := (hi - lo) / nF
      let mut v := lo
      for _ in [1:n] do
        v := v + stepsize
        out := out.push v
    return out
  let post : Array Float :=
    if mirror then
      let m := ticks.size
      let stepsize := (ticks[m - 1]! - ticks[m - 2]!) / nF
      (colon (ticks[m - 1]! + stepsize) stepsize vmax).toList.toArray
    else #[]
  pre ++ inner ++ post

/-- Makie `get_minor_tickvalues(IntervalsBetween(n, mirror), scale::LogFunctions, ...)`:
minor ticks are linearly spaced *in data space* between major ticks; mirrored
intervals use the neighbouring decade computed through the scale. -/
def minorIntervalsBetweenLog (forward inverse : Float → Float) (n : Nat) (mirror : Bool)
    (ticks : Array Float) (vmin vmax : Float) : Array Float :=
  if ticks.size < 2 || n == 0 then #[] else
  let nF := ofInt n
  let pre : Array Float :=
    if mirror then
      let firstScaled := forward ticks[1]! - forward ticks[0]!
      let prevtick := inverse (forward ticks[0]! - firstScaled)
      let stepsize := (ticks[0]! - prevtick) / nF
      (colon (ticks[0]! - stepsize) (-stepsize) vmin).toList.toArray
    else #[]
  let inner : Array Float := Id.run do
    let mut out := #[]
    for i in [0:ticks.size - 1] do
      let lo := ticks[i]!
      let hi := ticks[i + 1]!
      let stepsize := (hi - lo) / nF
      let mut v := lo
      for _ in [1:n] do
        v := v + stepsize
        out := out.push v
    return out
  let post : Array Float :=
    if mirror then
      let m := ticks.size
      let lastScaled := forward ticks[m - 1]! - forward ticks[m - 2]!
      let nexttick := inverse (forward ticks[m - 1]! + lastScaled)
      let stepsize := (nexttick - ticks[m - 1]!) / nF
      (colon (ticks[m - 1]! + stepsize) stepsize vmax).toList.toArray
    else #[]
  pre ++ inner ++ post

/-! ## Decade ticks (pseudolog10 / Symlog10 axes) -/

/-- Makie `_decade_select_anchored_zero`: `0, ±10^s, ±10^(2s), …`. -/
def decadeSelectAnchoredZero (nIdeal : Int) (kminPos kminNeg kmaxPos kmaxNeg : Int) : Option (Array Float) :=
  let hasPos := kmaxPos ≥ kminPos
  let hasNeg := kmaxNeg ≥ kminNeg
  if !(hasPos || hasNeg) then none else
  let smin := max (if hasPos then kminPos else 1) (if hasNeg then kminNeg else 1)
  let maxExtreme := max kmaxPos kmaxNeg
  let (bestS, _, _, _) := Id.run do
    let mut best : Int × Int × Int × Nat := (0, -1, -1, 1000000000)
    let mut s := smin
    for _ in [0:4000] do
      if s > maxExtreme then break
      let mut count : Int := 1
      let mut reach : Int := 0
      let mut k := s
      for _ in [0:4000] do
        if k > maxExtreme then break
        if k ≤ kmaxPos then
          count := count + 1
          reach := max reach k
        if k ≤ kmaxNeg then
          count := count + 1
          reach := max reach k
        k := k + s
      let d := (count - nIdeal).natAbs
      let (_, bc, br, bd) := best
      if d < bd || (d == bd && reach > br) || (d == bd && reach == br && count > bc) then
        best := (s, count, reach, d)
      s := s + 1
    return best
  if bestS == 0 then none else
  let ticks : Array Float := Id.run do
    let mut out : Array Float := #[0.0]
    let mut k := bestS
    for _ in [0:4000] do
      if k > maxExtreme then break
      if k ≤ kmaxPos then out := out.push (powInt 10.0 k)
      if k ≤ kmaxNeg then out := out.push (-(powInt 10.0 k))
      k := k + bestS
    return out
  some (ticks.qsort (· < ·))

/-- Makie `_decade_select_single_sided` (magnitudes `0 < vmin ≤ vmax`). -/
def decadeSelectSingleSided (vmin vmax : Float) (nIdeal : Int) (kminSide kmaxSide : Int) (signFactor : Float) :
    Option (Array Float) :=
  let kmin := max kminSide (ceilInt (Float.log10 vmin))
  let kmax := min kmaxSide (floorInt (Float.log10 vmax))
  let span := kmax - kmin
  if span < 1 then none else
  let (bestS, _, _, _) := Id.run do
    let mut best : Int × Int × Int × Nat := (1, span + 1, kmax, (span + 1 - nIdeal).natAbs)
    let mut s : Int := 2
    for _ in [0:4000] do
      if s > span then break
      let count := Int.tdiv span s + 1
      let reach := kmin + s * (count - 1)
      let d := (count - nIdeal).natAbs
      let (_, bc, br, bd) := best
      if d < bd || (d == bd && reach > br) || (d == bd && reach == br && count > bc) then
        best := (s, count, reach, d)
      s := s + 1
    return best
  let ks : Array Int := Id.run do
    let mut out := #[]
    let mut k := kmin
    for _ in [0:4000] do
      if k > kmax then break
      out := out.push k
      k := k + bestS
    return out
  some ((ks.map fun k => signFactor * exp10 (ofInt k)).qsort (· < ·))

/-- Makie `_decade_auto_tickvalues(vmin, vmax, n_ideal; kmin_pos, kmin_neg)`. -/
def decadeTicks (vmin vmax : Float) (nIdeal : Int) (kminPos kminNeg : Int := 0) : Option (Array Float) :=
  let kmaxPos : Int := if vmax ≥ powInt 10.0 kminPos then floorInt (Float.log10 vmax) else -1
  let kmaxNeg : Int := if vmin ≤ -(powInt 10.0 kminNeg) then floorInt (Float.log10 (-vmin)) else -1
  let ticks :=
    if vmin ≤ 0 && 0 ≤ vmax then
      decadeSelectAnchoredZero nIdeal (max kminPos 1) (max kminNeg 1) kmaxPos kmaxNeg
    else if vmin > 0 then decadeSelectSingleSided vmin vmax nIdeal kminPos kmaxPos 1.0
    else decadeSelectSingleSided (-vmax) (-vmin) nIdeal kminNeg kmaxNeg (-1.0)
  match ticks with
  | none => none
  | some ts => if (ts.filter (· != 0)).size < 2 then none else some ts

end LeanPlot.Ticks
