/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import VersoManual
import Manual.Meta

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

set_option pp.rawOnError true

#doc (Manual) "API Reference" =>
%%%
tag := "api-reference"
%%%

LeanPlot provides a layered API following {deftech}_progressive disclosure_:

1. *Tier 0 (Zero-Config)*: Functions like `plot`, `plotMany`, `scatter`, and `bar` that just work
2. *Tier 1 (Components)*: Mid-level functions like `sample` and `mkLineChart` for more control
3. *Tier 2 (Recharts)*: Direct access to Recharts JSX components for full customization

# Zero-Config Functions
%%%
tag := "zero-config"
%%%

These {tech}[progressive disclosure] Tier-0 functions require no configuration.

## plot
%%%
tag := "api-plot"
%%%

{docstring LeanPlot.API.plot}

## plotMany
%%%
tag := "api-plotmany"
%%%

{docstring LeanPlot.API.plotMany}

## scatter
%%%
tag := "api-scatter"
%%%

{docstring LeanPlot.API.scatter}

## bar
%%%
tag := "api-bar"
%%%

{docstring LeanPlot.API.bar}

# Components Layer
%%%
tag := "components-layer"
%%%

The components layer provides more control over chart construction.

## sample
%%%
tag := "api-sample"
%%%

{docstring LeanPlot.Components.sample}

## sampleMany
%%%
tag := "api-samplemany"
%%%

{docstring LeanPlot.Components.sampleMany}

# The ToFloat Typeclass
%%%
tag := "tofloat"
%%%

{docstring LeanPlot.ToFloat}

This typeclass allows plotting functions that return any numeric type, not just `Float`.
Built-in instances exist for `Float`, `Nat`, `Int`, and `Rat`.
