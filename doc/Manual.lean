/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import VersoManual

import Manual.Meta
import Manual.QuickStart
import Manual.Concepts
import Manual.API

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

set_option pp.rawOnError true

#doc (Manual) "LeanPlot: Interactive Plotting for Lean 4" =>
%%%
tag := "leanplot-manual"
authors := ["Alok Singh"]
%%%

LeanPlot turns Lean 4 code into _interactive, React-powered charts_ that render
right inside VS Code's infoview. Built on top of
[ProofWidgets4](https://github.com/leanprover-community/ProofWidgets4) and
[Recharts](https://recharts.org), it lets you inspect functions and data
visually while you prove.

# Features

* *Smart plotting* - `#plot (fun x => x^2)` just works with automatic axis labels and styling
* *One-liner helpers* - produce beautiful plots with `plot`, `plotMany`, `scatter`, `bar`
* *Composable graphics* - overlay or stack plots with the `+` operator
* *Log/linear scales* - visualize exponential growth with logarithmic axes
* *Data transformations* - apply scales, normalize, and smooth data

{include 0 Manual.QuickStart}

{include 0 Manual.Concepts}

{include 0 Manual.API}

# Color Palette
%%%
tag := "palette"
%%%

LeanPlot automatically assigns colors from a carefully chosen palette.

{docstring LeanPlot.Palette.colorFromNat}

{docstring LeanPlot.Palette.autoColors}

# Demo Gallery
%%%
tag := "demos"
%%%

LeanPlot includes many demos under `LeanPlot/Demos/`:

* `SmartPlottingDemo` - Zero-effort beautiful plots (start here!)
* `LinearDemo`, `QuadraticDemo`, `CubicDemo` - Basic function plots
* `OverlayDemo` - Overlaying multiple functions
* `TrigDemo` - Trigonometric functions
* `LogScaleDemo` - Logarithmic scales
* `GrammarDemo` - Grammar of Graphics DSL
* `TransformDemo` - Data transformations
* `FacetDemo` - Grid layouts

Open any demo file and hover over `#plot` or `#html` commands to see the charts.

# Contributing
%%%
tag := "contributing"
%%%

Contributions are welcome! Check the [GitHub repository](https://github.com/alok/LeanPlot)
for issues and pull requests.

# License
%%%
tag := "license"
%%%

LeanPlot is released under the Apache License 2.0.
