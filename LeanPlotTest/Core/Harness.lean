import LeanPlotTest.Core.Json
import LeanPlot.Core.Decimal

/-!
Tiny test harness for the core suites: a state monad tallying passes/failures,
bit-exact and tolerance float comparisons, and oracle-file access.
-/

namespace LeanPlotTest.Core

/-- Running tally of a test suite. -/
structure Tally where
  passed : Nat := 0
  failed : Nat := 0
  msgs : Array String := #[]

/-- Test monad. -/
abbrev TestM := StateT Tally IO

/-- Record one check. `detail` is only evaluated on failure. -/
def check (name : String) (ok : Bool) (detail : Unit → String := fun _ => "") : TestM Unit :=
  modify fun t =>
    if ok then { t with passed := t.passed + 1 }
    else { t with failed := t.failed + 1, msgs := t.msgs.push s!"{name}: {detail ()}" }

/-- Bit-exact float equality (all NaNs are equal). -/
def bitEq (a b : Float) : Bool := a.toBits == b.toBits || (a.isNaN && b.isNaN)

/-- Float equality up to an absolute tolerance (NaNs equal, infinities equal). -/
def approxEq (tol : Float) (a b : Float) : Bool :=
  (a.isNaN && b.isNaN) || a == b || (a - b).abs ≤ tol

/-- Show a float array, Julia-style. -/
def showFloats (xs : Array Float) : String :=
  "[" ++ ", ".intercalate (xs.toList.map LeanPlot.Num.showFloat) ++ "]"

/-- Arrays equal bit-for-bit. -/
def floatsBitEq (a b : Array Float) : Bool :=
  a.size == b.size && (Array.range a.size).all fun i => bitEq a[i]! b[i]!

/-- Arrays equal within `tol`. -/
def floatsApprox (tol : Float) (a b : Array Float) : Bool :=
  a.size == b.size && (Array.range a.size).all fun i => approxEq tol a[i]! b[i]!

/-- Directory of the core oracle goldens (relative to the package root). -/
def oracleDir : System.FilePath := "LeanPlotTest" / "oracle" / "core"

/-- Load an oracle golden, recording a failure (and returning `none`) if it is
missing or malformed. -/
def loadOracle (file : String) : TestM (Option J) := do
  let p := oracleDir / file
  if !(← p.pathExists) then
    check s!"oracle {file}" false (fun _ => s!"missing {p} (run from the package root)")
    return none
  try
    let j ← readJson p
    return some j
  catch e =>
    check s!"oracle {file}" false (fun _ => toString e)
    return none

/-- Run a suite: print a summary line and up to `maxShow` failure messages. -/
def runSuite (name : String) (t : TestM Unit) (maxShow : Nat := 25) : IO (Nat × Nat) := do
  let t0 ← IO.monoMsNow
  let ((), tally) ← t.run {}
  let t1 ← IO.monoMsNow
  IO.println s!"[{name}] passed {tally.passed}, failed {tally.failed} ({t1 - t0} ms)"
  for m in tally.msgs.extract 0 maxShow do
    IO.println s!"  FAIL {m}"
  if tally.msgs.size > maxShow then
    IO.println s!"  ... {tally.msgs.size - maxShow} more failures"
  return (tally.passed, tally.failed)

end LeanPlotTest.Core
