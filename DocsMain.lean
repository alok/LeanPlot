import VersoManual
import LeanPlotManual

open Verso.Genre Manual

/-- Configuration for the LeanPlot manual generation. -/
def docsConfig : Config := {
  emitTex := false,
  emitHtmlSingle := false,
  emitHtmlMulti := true,
  htmlDepth := 2,
  sourceLink := some "https://github.com/alok/LeanPlot",
  issueLink := some "https://github.com/alok/LeanPlot/issues"
}

/-- Entry point for `lake exe docs`.  Generates the multi-page HTML manual into
`docs/html-multi` (overridable via `--output` CLI flag). -/
def main : IO UInt32 :=
  manualMain (%doc LeanPlotManual) (config := docsConfig)
