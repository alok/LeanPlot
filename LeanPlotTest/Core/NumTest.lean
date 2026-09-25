import LeanPlotTest.Core.Harness
import LeanPlot.Core.Range
import LeanPlot.Core.Format
import LeanPlot.Core.JuliaExp

/-!
Numerics against the Julia oracle (`num.json`): `10.0^z`, `range`,
`LinRange`, `a:s:b`, `range(a; step, length)`, Ryu shortest/fixed/exp,
`repr`, `round(x; sigdigits)`; plus unit tests of the SVG number formatter.
-/

namespace LeanPlotTest.Core.NumTest

open LeanPlot.Num

/-- Unit tests of `fmtFixed` (no oracle needed). -/
def fmtUnit : TestM Unit := do
  let cases : List (Nat × Float × String) :=
    [(3, 1.23456, "1.235"), (3, 2.5, "2.5"), (3, -0.0001, "0"), (3, 1e-7, "0"), (3, -0.0, "0"),
     (3, 100, "100"), (3, -12.3456, "-12.346"), (3, 0.0005, "0"), (3, 0.0015, "0.002"),
     (2, 3.14159, "3.14"), (0, 2.5, "2"), (0, 3.5, "4"), (3, 1e300, "1000000000000000"),
     (3, 0.0 / 0.0, "0"), (3, 1.0 / 0.0, "0"), (3, 400.1, "400.1"), (3, 0.1 + 0.2, "0.3")]
  for (d, x, s) in cases do
    let got := fmtFixed d x
    check s!"fmtFixed {d} {showFloat x}" (got == s) fun _ => s!"got {got}, want {s}"

/-- Oracle comparisons. -/
def oracle : TestM Unit := do
  let some j ← loadOracle "num.json" | return
  -- 10.0^z
  for c in (j.get "pow10").arrD do
    let z := (c.get "z").int
    let want := (c.get "v").float
    let got := pow10 z
    check s!"pow10 {z}" (bitEq got want) fun _ => s!"got {showFloat got}, want {showFloat want}"
  -- range(a, b; length = n)
  for c in (j.get "range").arrD do
    let a := (c.get "a").float
    let b := (c.get "b").float
    let n := (c.get "n").nat
    let want := (c.get "v").floats
    let got := (range a b n).toList.toArray
    check s!"range({showFloat a}, {showFloat b}, {n})" (floatsBitEq got want)
      fun _ => s!"got {showFloats (got.extract 0 6)}…, want {showFloats (want.extract 0 6)}…"
  for c in (j.get "linrange").arrD do
    let a := (c.get "a").float
    let b := (c.get "b").float
    let n := (c.get "n").nat
    let want := (c.get "v").floats
    let got := (linRange a b n).toList.toArray
    check s!"LinRange({showFloat a}, {showFloat b}, {n})" (floatsBitEq got want)
      fun _ => s!"got {showFloats (got.extract 0 6)}…, want {showFloats (want.extract 0 6)}…"
  for c in (j.get "colon").arrD do
    let a := (c.get "a").float
    let s := (c.get "s").float
    let b := (c.get "b").float
    let want := (c.get "v").floats
    let got := (colon a s b).toList.toArray
    check s!"{showFloat a}:{showFloat s}:{showFloat b}" (floatsBitEq got want)
      fun _ => s!"got {showFloats got}, want {showFloats want}"
  for c in (j.get "rangestep").arrD do
    let a := (c.get "a").float
    let s := (c.get "s").float
    let n := (c.get "n").nat
    let want := (c.get "v").floats
    let got := (rangeStep a s n).toList.toArray
    check s!"range({showFloat a}; step={showFloat s}, length={n})" (floatsBitEq got want)
      fun _ => s!"got {showFloats got}, want {showFloats want}"
  -- Julia exp / exp2 / exp10
  for c in (j.get "exp").arrD do
    let x := (c.get "x").float
    for (nm, f) in [("exp", expJ), ("exp2", exp2J), ("exp10", exp10)] do
      let want := (c.get nm).float
      let got := f x
      check s!"{nm}({showFloat x})" (bitEq got want) fun _ => s!"got {showFloat got}, want {showFloat want}"
  -- Ryu
  for c in (j.get "ryu").arrD do
    let x := (c.get "x").float
    let s64 := (c.get "s64").arrD
    let (k, e) := shortest x.abs
    check s!"shortest {showFloat x}" (s64.size == 2 && toString k == s64[0]!.string && toString e == s64[1]!.string)
      fun _ => s!"got ({k}, {e}), want {s64.map J.string}"
    let s32 := c.get "s32"
    if !s32.isNull then
      let (k, e) := shortest32 x.abs.toFloat32
      let w := s32.arrD
      check s!"shortest32 {showFloat x}" (w.size == 2 && toString k == w[0]!.string && toString e == w[1]!.string)
        fun _ => s!"got ({k}, {e}), want {w.map J.string}"
    let fixed := (c.get "fixed").arrD
    for p in [0:fixed.size] do
      let got := writeFixed x p
      check s!"writefixed({showFloat x}, {p})" (got == fixed[p]!.string) fun _ => s!"got {got}, want {fixed[p]!.string}"
    let ex := (c.get "exp").arrD
    for p in [0:ex.size] do
      let got := writeExpString x p
      check s!"writeexp({showFloat x}, {p})" (got == ex[p]!.string) fun _ => s!"got {got}, want {ex[p]!.string}"
    let r := (c.get "repr").string
    check s!"repr {r}" (showFloat x == r) fun _ => s!"got {showFloat x}"
    let sig := (c.get "sig").arrD
    for i in [0:sig.size] do
      let want := sig[i]!.float
      let got := roundSigDigits x (i + 1 : Nat)
      check s!"round({showFloat x}, sigdigits={i+1})" (bitEq got want) fun _ => s!"got {showFloat got}, want {showFloat want}"

/-- The numerics suite. -/
def suite : TestM Unit := do
  fmtUnit
  oracle

end LeanPlotTest.Core.NumTest
