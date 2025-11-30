import Lean

/--
Tiny executable to prove the subpackage builds. The PNG export is visual and
verified via the PngExportDemo.lean file in the infoview.
-/

def main : IO Unit := do
  IO.println "LeanPlot PNG export demo workspace built successfully."

