/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import Manual
import Manual.Meta
import VersoManual

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

def config : Config where
  emitTeX := false
  emitHtmlSingle := true
  emitHtmlMulti := true
  htmlDepth := 2
  destination := "_out/docs"
  sourceLink := some "https://github.com/alok/LeanPlot"
  issueLink := some "https://github.com/alok/LeanPlot/issues"

def main := manualMain (%doc Manual) (config := config)
