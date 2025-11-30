/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import LeanPlot.API
import LeanPlot.DSL
import LeanPlot.Components
import LeanPlot.ToFloat
import LeanPlot.PlotComposition
import LeanPlot.Palette
import VersoManual

-- Access the manual genre
open Verso.Genre Manual

-- Access Lean code in code blocks
open Verso.Genre.Manual.InlineLean

-- Open LeanPlot namespaces for examples
open LeanPlot.API
open LeanPlot.PlotComposition

set_option pp.rawOnError true
set_option linter.verso.markup.emph false

#doc (Manual) "LeanPlot: Interactive Plotting for Lean 4" =>
%%%
authors := ["Alok Singh"]
shortTitle := "LeanPlot"
%%%

LeanPlot turns Lean 4 code into _interactive, React-powered charts_ that render right inside VS Code's infoview.
Built on top of ProofWidgets4 and Recharts, it lets you inspect functions and data visually while you prove.

# Quick Start
%%%
tag := "quickstart"
%%%

## Installation
%%%
tag := "installation"
%%%

Add LeanPlot to your project's `lakefile.toml`:

```
[[require]]
name = "LeanPlot"
url = "https://github.com/alok/LeanPlot"
```

Then fetch and build the dependencies:

```
lake update
lake build
```

## Your First Plot
%%%
tag := "first-plot"
%%%

:::paragraph
Create a new `.lean` file, open the VS Code infoview, and try:

```lean
#plot (fun x => x^2)
```

This renders a parabola with automatic axis labels and styling!
:::

:::paragraph
You can customize the number of samples:

```lean
#plot (fun t => Float.sin t) using 400
```
:::

:::paragraph
For multiple functions, use {name}`plotMany`:

```lean
#html plotMany #[("sin", fun x => Float.sin x),
                 ("cos", fun x => Float.cos x)]
```
:::

# Core Concepts
%%%
tag := "concepts"
%%%

## Progressive Disclosure
%%%
tag := "progressive-disclosure"
%%%

LeanPlot follows a {deftech}_progressive disclosure_ philosophy with three API tiers:

1. *Tier 0 (Zero-Config)*: Functions like {name}`LeanPlot.API.plot`, {name}`LeanPlot.API.plotMany`, {name}`LeanPlot.API.scatter`, and {name}`LeanPlot.API.bar` that just work with sensible defaults

2. *Tier 1 (Components)*: Mid-level functions like {name}`LeanPlot.Components.sample` and {name}`LeanPlot.Components.mkLineChart` for more control

3. *Tier 2 (Recharts)*: Direct access to Recharts JSX components for full customization

## The ToFloat Typeclass
%%%
tag := "tofloat"
%%%

{docstring LeanPlot.ToFloat}

This typeclass allows plotting functions that return any numeric type, not just `Float`.

# API Reference
%%%
tag := "api"
%%%

## Zero-Config Functions
%%%
tag := "zero-config"
%%%

These are the {tech}[progressive disclosure] Tier-0 functions - they just work!

### plot
%%%
tag := "api-plot"
%%%

{docstring LeanPlot.API.plot}

### plotMany
%%%
tag := "api-plotMany"
%%%

{docstring LeanPlot.API.plotMany}

### scatter
%%%
tag := "api-scatter"
%%%

{docstring LeanPlot.API.scatter}

### bar
%%%
tag := "api-bar"
%%%

{docstring LeanPlot.API.bar}

## Components Layer
%%%
tag := "components"
%%%

The components layer provides more control over chart construction.

### sample
%%%
tag := "api-sample"
%%%

{docstring LeanPlot.Components.sample}

### sampleMany
%%%
tag := "api-sampleMany"
%%%

{docstring LeanPlot.Components.sampleMany}

# Plot Composition
%%%
tag := "composition"
%%%

LeanPlot supports composing multiple plots using the `+` operator via the Algebra module.
Import `LeanPlot.Algebra` and use the `line` function to create composable line plots.

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

- `SmartPlottingDemo` - Zero-effort beautiful plots (start here!)
- `LinearDemo`, `QuadraticDemo`, `CubicDemo` - Basic function plots
- `OverlayDemo` - Overlaying multiple functions
- `TrigDemo` - Trigonometric functions
- `LogScaleDemo` - Logarithmic scales
- `GrammarDemo` - Grammar of Graphics DSL
- `TransformDemo` - Data transformations
- `FacetDemo` - Grid layouts
- `SeriesKindDemo` - Type-safe series

# Architecture
%%%
tag := "architecture"
%%%

LeanPlot has a layered architecture following {tech}[progressive disclosure]:

:::paragraph
*User Code* → *Tier 0 API* → *Components* → *Recharts* → *VS Code Infoview*

Each layer adds more control at the cost of more verbosity. Most users only need Tier 0.
:::

# Contributing
%%%
tag := "contributing"
%%%

Contributions welcome! Check the GitHub repository for issues and PRs.

# License
%%%
tag := "license"
%%%

LeanPlot is released under the Apache License 2.0.
