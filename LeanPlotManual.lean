import VersoManual
import Verso.Doc

/-!
# LeanPlot Manual

A temporary, minimal placeholder so that `DocsMain` and the `lake exe docs` target
compile successfully.  This should be replaced with real documentation as the
library evolves.
-/

open Verso.Doc

/--
A stub `Manual` value containing just an (empty) top-level part titled `"LeanPlot"`.
-/
noncomputable def LeanPlotManual : Part Verso.Genre.Manual :=
  .mk
    #[@Inline.text Verso.Genre.Manual "LeanPlot"] -- Title content
    "LeanPlot"                                   -- Slug / file name
    none                                          -- No metadata yet
    #[]                                           -- No introductory blocks yet
    #[]                                           -- No sub-sections yet
