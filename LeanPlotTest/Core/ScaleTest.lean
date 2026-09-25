import LeanPlotTest.Core.Harness
import LeanPlot.Core.Scale

/-!
Scales against Makie (`scales.json`): forward and inverse transforms (NaN
outside the domain where Julia throws), default limits and defined intervals.
Forward transforms use the C `log10`/`log2`/`log`, which may differ from
Julia's by an ulp, so values are compared to 1e-15 relative; inverses use the
bit-exact Julia `exp` port.
-/

namespace LeanPlotTest.Core.ScaleTest

open LeanPlot LeanPlot.Num

/-- Close to 1e-15 relative (NaNs and infinities must match exactly). Reversible scales go
through the C `asinh`/`sinh`/`pow`, which may differ from Julia's by a few ulps, so they are
compared to 1e-13 relative. -/
def near (a b : Float) (tol : Float := 1e-15) : Bool :=
  (a.isNaN && b.isNaN) || a == b || (a.isFinite && b.isFinite && (a - b).abs ≤ tol * max a.abs b.abs)

/-- Cartan `plot.md` (contour of Himmelblau's function): `ReversibleScale(x -> x^(1/10), x -> x^10)`. -/
def plotmdPow10 : Scale := Scale.custom "#1" (fun x => Num.powF x 0.1) (fun y => Num.powF y 10)

/-- Cartan `plot.md` (heatmap with a log-like axis):
`ReversibleScale(x -> asinh(x/2)/log(10), x -> 2sinh(log(10)*x))`. -/
def plotmdAsinh : Scale :=
  Scale.custom "#3" (fun x => Float.asinh (x / 2) / Float.log 10) (fun y => 2 * Float.sinh (Float.log 10 * y))

/-- The scale suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "scales.json" | return
  for c in j.arrD do
    let s : Scale := match (c.get "name").string with
      | "log10" => .log10 | "log2" => .log2 | "ln" => .ln | "sqrt" => .sqrt
      | "pseudolog10" => .pseudolog10 | "logit" => .logit
      | "symlog10" => .symlog10 (c.get "lower").float (c.get "upper").float (c.get "linscale").float
      | "AsinhScale" => Scale.asinhScale (c.get "a").float
      | "SinhScale" => Scale.sinhScale (c.get "a").float
      | "LogScale" => Scale.logScale (c.get "a").float
      | "LuptonAsinhScale" => Scale.luptonAsinhScale (c.get "a").float (c.get "Q").float (c.get "frac").float
      | "PowerScale" => Scale.powerScale (c.get "a").float
      | "plotmd_pow10" => plotmdPow10
      | "plotmd_asinh" => plotmdAsinh
      | _ => .identity
    let tol : Float := match s with | .reversible .. => 1e-13 | _ => 1e-15
    let xs := (c.get "x").floats
    let fw := (c.get "forward").floats
    for i in [0:xs.size] do
      let got := s.forward xs[i]!
      check s!"{s.name} forward({showFloat xs[i]!})" (near got fw[i]! tol) fun _ => s!"got {showFloat got}, want {showFloat fw[i]!}"
    let ys := (c.get "y").floats
    let iv := (c.get "inverse").floats
    for i in [0:ys.size] do
      let got := s.inverse ys[i]!
      check s!"{s.name} inverse({showFloat ys[i]!})" (near got iv[i]! tol) fun _ => s!"got {showFloat got}, want {showFloat iv[i]!}"
    let dl := (c.get "defaultlimits").floats
    let (lo, hi) := s.defaultLimits
    check s!"{s.name} defaultlimits" (near lo dl[0]! tol && near hi dl[1]! tol) fun _ => s!"got ({showFloat lo}, {showFloat hi}), want {showFloats dl}"
    let ivj := (c.get "interval").arrD
    let di := s.definedInterval
    check s!"{s.name} defined_interval"
      (bitEq di.lo ivj[0]!.float && bitEq di.hi ivj[1]!.float && di.loClosed == ivj[2]!.boolean && di.hiClosed == ivj[3]!.boolean)
      fun _ => s!"got ({showFloat di.lo}, {showFloat di.hi}, {di.loClosed}, {di.hiClosed})"

end LeanPlotTest.Core.ScaleTest
