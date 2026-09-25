import LeanPlot
import LeanPlotTest

/-- A registered test suite: a name and an action returning `(passed, failed)`. -/
structure Suite where
  name : String
  run : IO (Nat × Nat)

/-- Every suite `lake test` runs (cwd must be the package root: suites read goldens by relative path). -/
def suites : List Suite := [
  ⟨"Font", LeanPlotTest.Font.run⟩,
  ⟨"Raster", LeanPlotTest.Raster.run⟩,
  ⟨"Core", LeanPlotTest.Core.run⟩,
  ⟨"Figure", LeanPlotTest.Figure.run⟩,
  ⟨"Recipes", LeanPlotTest.Recipes.run⟩
]

/-- Golden test driver: `lake test` runs everything; `lake exe leanplottest A B` runs named suites. -/
def main (args : List String) : IO UInt32 := do
  let chosen := if args.isEmpty then suites else suites.filter (args.contains ·.name)
  let mut totalPass := 0
  let mut totalFail := 0
  for s in chosen do
    let (p, f) ← s.run
    IO.println s!"[{if f == 0 then "PASS" else "FAIL"}] {s.name}: {p} passed, {f} failed"
    totalPass := totalPass + p
    totalFail := totalFail + f
  IO.println s!"TOTAL: {totalPass} passed, {totalFail} failed"
  return if totalFail == 0 then 0 else 1
