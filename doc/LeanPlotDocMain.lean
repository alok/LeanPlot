/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/

import VersoManual
import LeanPlotDoc

open Verso Doc
open Verso.Genre Manual

def config : Config where
  emitTeX := false
  emitHtmlSingle := true
  emitHtmlMulti := true
  htmlDepth := 2
  destination := "_out/docs"

def main := manualMain (%doc LeanPlotDoc) (config := config)
