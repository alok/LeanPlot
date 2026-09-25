import LeanPlotTest.Core.Harness
import LeanPlot.Core.Ticks
import LeanPlot.Core.Scale

/-!
Tick locators and labels against Makie (`ticks.json`): Wilkinson ticks for
hundreds of ranges (bit-exact), `format_ticks_auto` labels, `optimize_ticks`
options, log axes, pseudolog10/Symlog10 decade ticks, minor ticks.
-/

namespace LeanPlotTest.Core.TicksTest

open LeanPlot LeanPlot.Num LeanPlot.Ticks

/-- Decode an oracle label. -/
def labelOf (j : J) : TickLabel :=
  match j.get? "plain" with
  | some p => .plain p.string
  | none => .sup (j.get "base").string (j.get "sup").string

/-- Show labels compactly. -/
def showLabels (ls : Array TickLabel) : String := ", ".intercalate (ls.toList.map TickLabel.toCaret)

/-- Resolve a scale name from the oracle. -/
def scaleOf (c : J) : Scale :=
  match (c.get "scale").string with
  | "log10" => .log10
  | "log2" => .log2
  | "ln" => .ln
  | "pseudolog10" => .pseudolog10
  | "symlog10" => .symlog10 (c.get "lower").float (c.get "upper").float 1.0
  | _ => .identity

/-- The ticks suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "ticks.json" | return
  for c in (j.get "wilkinson").arrD do
    let a := (c.get "vmin").float
    let b := (c.get "vmax").float
    let want := (c.get "ticks").floats
    let got := wilkinson a b
    check s!"wilkinson({showFloat a}, {showFloat b})" (floatsBitEq got want)
      fun _ => s!"got {showFloats got}, want {showFloats want}"
    let labs := c.get "labels"
    if !labs.isNull then
      let wantL := labs.arrD.map labelOf
      let gotL := formatTicksAuto want
      check s!"labels({showFloat a}, {showFloat b})" (gotL == wantL)
        fun _ => s!"got {showLabels gotL}, want {showLabels wantL}"
  for c in (j.get "optimize").arrD do
    let a := (c.get "vmin").float
    let b := (c.get "vmax").float
    let ext := (c.get "extend").boolean
    let strict := (c.get "strict").boolean
    let cfg : WilkinsonConfig := { kMin := (c.get "kmin").nat, extendTicks := ext, strictSpan := strict }
    let (got, lo, hi) := optimizeTicks a b cfg
    let want := (c.get "ticks").floats
    check s!"optimize_ticks({showFloat a}, {showFloat b}; extend={ext}, strict={strict})"
      (floatsBitEq got want && bitEq lo (c.get "lo").float && bitEq hi (c.get "hi").float)
      fun _ => s!"got {showFloats got} [{showFloat lo}, {showFloat hi}], want {showFloats want} [{showFloat (c.get "lo").float}, {showFloat (c.get "hi").float}]"
  for c in ((j.get "log").arrD ++ (j.get "decade").arrD) do
    let s := scaleOf c
    let a := (c.get "vmin").float
    let b := (c.get "vmax").float
    let want := (c.get "ticks").floats
    let wantL := (c.get "labels").arrD.map labelOf
    let (got, gotL) := s.ticks a b
    check s!"{s.name} ticks({showFloat a}, {showFloat b})" (floatsApprox 0 got want || floatsBitEq got want)
      fun _ => s!"got {showFloats got}, want {showFloats want}"
    check s!"{s.name} labels({showFloat a}, {showFloat b})" (gotL == wantL)
      fun _ => s!"got {showLabels gotL}, want {showLabels wantL}"
  for c in (j.get "minor").arrD do
    let s := scaleOf c
    let a := (c.get "vmin").float
    let b := (c.get "vmax").float
    let n := (c.get "n").nat
    let mirror := (c.get "mirror").boolean
    let ticks := (c.get "ticks").floats
    let want := (c.get "minor").floats
    let got := s.minorTicks n mirror ticks a b
    check s!"{s.name} minor({showFloat a}, {showFloat b}, n={n}, mirror={mirror})" (floatsBitEq got want)
      fun _ => s!"got {showFloats got}, want {showFloats want}"
  for c in (j.get "format").arrD do
    let t := (c.get "ticks").floats
    let wantL := (c.get "labels").arrD.map labelOf
    let gotL := formatTicksAuto t
    check s!"format_ticks_auto {showFloats t}" (gotL == wantL) fun _ => s!"got {showLabels gotL}, want {showLabels wantL}"
    let wantP := (c.get "plain").arrD.map J.string
    let gotP := formatTicksPlain t
    check s!"format_ticks_plain {showFloats t}" (gotP == wantP) fun _ => s!"got {gotP}, want {wantP}"

end LeanPlotTest.Core.TicksTest
