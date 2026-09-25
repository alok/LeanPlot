import LeanPlotTest.Core.Harness

/-!
Shared helpers for the recipe-algorithm suites: the core tally harness
(`check`, `bitEq`, `runSuite`, …) plus access to the recipe oracle goldens in
`LeanPlotTest/oracle/recipes/` and small comparison utilities.
-/

namespace LeanPlotTest.Recipes

export LeanPlotTest.Core (J Tally TestM check bitEq approxEq floatsBitEq floatsApprox showFloats runSuite readJson)

/-- Directory of the recipe oracle goldens (relative to the package root). -/
def oracleDir : System.FilePath := "LeanPlotTest" / "oracle" / "recipes"

/-- Load a recipe oracle golden, recording a failure (and returning `none`) if it
is missing or malformed. -/
def loadOracle (file : String) : TestM (Option J) := do
  let p := oracleDir / file
  if !(← p.pathExists) then
    check s!"oracle {file}" false (fun _ => s!"missing {p} (run from the package root)")
    return none
  try
    return some (← readJson p)
  catch e =>
    check s!"oracle {file}" false (fun _ => toString e)
    return none

/-- A `FloatArray` from a JSON array of numbers. -/
def floatArr (j : J) : FloatArray := ⟨j.floats⟩

/-- Bit-exact equality of two float arrays (NaNs equal). -/
def faBitEq (a b : FloatArray) : Bool := floatsBitEq a.data b.data

/-- Equality within `tol` of two float arrays. -/
def faApprox (tol : Float) (a b : FloatArray) : Bool := floatsApprox tol a.data b.data

/-- Show a float array. -/
def showFA (a : FloatArray) : String := showFloats a.data

/-- Largest absolute difference between two equally long arrays (NaN-aware:
matching NaNs count as 0, a NaN against a number as `inf`). -/
def maxAbsDiff (a b : FloatArray) : Float :=
  if a.size != b.size then LeanPlot.Num.inf else
  (List.range a.size).foldl (init := 0.0) fun m i =>
    let x := a.get! i
    let y := b.get! i
    let d := if x.isNaN && y.isNaN then 0.0 else if x.isNaN || y.isNaN then LeanPlot.Num.inf else (x - y).abs
    if d > m then d else m

end LeanPlotTest.Recipes
