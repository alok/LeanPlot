/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import LeanPlot.API
import LeanPlot.DSL
import LeanPlot.PlotComposition
import VersoManual

-- Access the manual genre
open Verso.Genre Manual

-- Access Lean code in code blocks
open Verso.Genre.Manual.InlineLean

set_option pp.rawOnError true
set_option linter.verso.markup.emph false

#doc (Manual) "LeanPlot: Interactive Plotting for Lean 4" =>
%%%
authors := ["Alok Singh"]
shortTitle := "LeanPlot"
%%%

LeanPlot turns Lean 4 code into _interactive, React-powered charts_ that render right inside VS Code's infoview.
Built on top of ProofWidgets4 and Recharts,
it lets you inspect functions and data visually while you prove.

# Quick Start

## Installation

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

Create a new `.lean` file, open the VS Code infoview, and paste:

```lean
-- Simple syntax - just pass a function directly:
#plot (fun x => x^2)

-- With custom samples:
#plot (fun t => Float.sin t) using 400

-- Works without parentheses too:
#plot fun x => Float.tanh x
```

Hover over `#plot` and you'll see beautiful interactive charts with automatic axis labels, colors, and styling!

# Core Concepts

## Progressive Disclosure

LeanPlot follows a _progressive disclosure_ philosophy with three API tiers:

1. *Tier 0 (Zero-Config)*: Functions like `plot`, `plotMany`, `scatter`, and `bar` that just work with sensible defaults
2. *Tier 1 (Components)*: Mid-level functions like `sample`, `mkLineChart` for more control
3. *Tier 2 (Recharts)*: Direct access to Recharts JSX components for full customization

## The #plot Command

The `#plot` command is the primary way to visualize functions:

```lean
#plot (fun x => x^2)              -- Basic function
#plot (fun x => x^2) using 400    -- Custom sample count
#plot fun t => Float.sin t        -- No parens needed
```

For expressions that already return `Html`, use `#html` instead:

```lean
#html plotMany #[("sin", fun x => Float.sin x),
                 ("cos", fun x => Float.cos x)]
```

# API Reference

## Zero-Config Functions

### plot

Renders a line chart for a single function with automatic everything:

```
def plot {β} [ToFloat β] (f : Float → β)
    (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : Html
```

### plotMany

Compares multiple functions with automatic legend and colors:

```
def plotMany {β} [ToFloat β]
    (fns : Array (String × (Float → β)))
    (steps : Nat := 200)
    (domain : Float × Float := (0.0, 1.0)) : Html
```

### scatter

Creates a scatter plot from a function:

```
def scatter {β} [ToFloat β] (f : Float → β)
    (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : Html
```

### bar

Creates a bar chart from discrete values:

```
def bar {β} [ToFloat β] (f : Float → β)
    (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : Html
```

# Advanced Features

## Grammar of Graphics DSL

For complex visualizations, use the Grammar of Graphics DSL inspired by ggplot2:

```
import LeanPlot.GrammarOfGraphics

#html (
  plot (fun x => x * x)
    |> fun p => PlotBuilder.withTitle p "Quadratic Function"
    |> fun p => PlotBuilder.withSize p 500 400
    |> PlotBuilder.build
    |> Render.render
)
```

## Log Scales

Visualize exponential growth with logarithmic axes:

```
import LeanPlot.Scale

-- Use log scale on Y axis
#html (
  PlotBuilder.plot (fun x => Float.exp x)
    |> fun p => PlotBuilder.logY p 10.0
    |> PlotBuilder.build
    |> Render.render
)
```

## Data Transformations

Apply transforms to your data using `LeanPlot.Transform`:

- Log transform
- Sqrt scale
- Normalization
- Moving average

## Faceting

Create grid layouts of multiple subplots:

```
import LeanPlot.Faceting

#html facetGrid specs 2 2  -- 2x2 grid of plots
```

## Plot Composition

Overlay or stack multiple plots:

```lean

-- Use + operator to overlay plots
#plot (line (fun x => x) "y" + line (fun x => x^2) "y²")
```

# Demo Gallery

LeanPlot includes many demos under `LeanPlot/Demos/`:

- `SmartPlottingDemo` - Zero-effort beautiful plots (recommended starting point)
- _LinearDemo_, _QuadraticDemo_, _CubicDemo_ - Basic function plots
- _OverlayDemo_ - Overlaying multiple functions
- _TrigDemo_ - Trigonometric functions
- _LogScaleDemo_ - Logarithmic scales
- _GrammarDemo_ - Grammar of Graphics DSL
- _TransformDemo_ - Data transformations
- _FacetDemo_ - Grid layouts
- _SeriesKindDemo_ - Type-safe series

# PNG Export

Add a Save PNG button around any plot:

```
import LeanPlot.Debug
open LeanPlot.API LeanPlot.Debug

#html withSavePNG (plot (fun x => x^2)) "my-plot" "quadratic.png"
```

# Architecture

LeanPlot has a layered architecture:

- _User Code_: `#plot (fun x => x^2)`
- _Tier 0 (LeanPlot.API)_: `plot`, `plotMany`, `scatter`, `bar` - Zero-config, automatic everything
- _Tier 1 (LeanPlot.Components)_: `sample`, `sampleMany`, `mkLineChart`, `mkScatterChart` - More control, explicit parameters
- _Tier 2 (ProofWidgets + Recharts)_: `LineChart`, `ScatterChart`, `BarChart` - Full JSX access, complete customization
- _VS Code Infoview_: Interactive React widgets

# Contributing

Contributions welcome! Check the GitHub repository for issues and PRs.

# License

LeanPlot is released under the Apache License 2.0.
