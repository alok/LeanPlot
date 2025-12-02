import Lean
import LeanPlot.API

/--
Entry point for the {lit}`leanplot` executable.
Verifies the project builds correctly.
-/
def main : IO Unit :=
  IO.println "LeanPlot: Interactive plotting for Lean 4"
