/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import VersoManual
import Manual.Meta
import LeanPlot.API
import LeanPlot.DSL

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean
open LeanPlot.API
set_option pp.rawOnError true

#doc (Manual) "API Reference" =>
%%%
tag := "api-reference"
%%%

LeanPlot provides a layered API following {deftech}_progressive disclosure_:

1. *Tier 0 (Zero-Config)*: Functions like {name}`plot`, {name}`plotMany`, {name}`scatter`, and {name}`bar` that just work
2. *Tier 1 (Components)*: Mid-level functions like {name}`sample` and `mkLineChart` for more control
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

Plot a single function with automatic styling and axis labels.

![plot example](img/plot_quadratic.svg)

```lean
#plot (fun x => x^2)
```

{docstring LeanPlot.API.plot}

## plotMany
%%%
tag := "api-plotmany"
%%%

Compare multiple functions on a single chart with automatic colors and legend.

![plotMany example](img/plot_sincos.svg)

```lean
#html plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]
```

{docstring LeanPlot.API.plotMany}

## scatter
%%%
tag := "api-scatter"
%%%

Create scatter plots for visualizing discrete data points.

![scatter example](img/scatter_demo.svg)

```lean
#html scatter (fun x => x^2 + noise) (steps := 50)
```

{docstring LeanPlot.API.scatter}

## bar
%%%
tag := "api-bar"
%%%

Create bar charts for discrete or categorical data.

```lean
#html bar (fun i => i^2) (steps := 10)
```

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

This typeclass allows plotting functions that return any numeric type, not just {name}`Float`.
Built-in instances exist for {name}`Float`, {name}`Nat`, {name}`Int`, and {name}`Rat`.
