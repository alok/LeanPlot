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

#doc (Manual) "Core Concepts" =>
%%%
tag := "concepts"
%%%

This chapter explains the key concepts behind LeanPlot's design.

# Progressive Disclosure
%%%
tag := "progressive-disclosure"
%%%

LeanPlot follows a {deftech}_progressive disclosure_ philosophy.
This means the API is layered so that simple tasks require simple code,
while advanced customization is still possible when needed.

The three tiers are:

1. *Tier 0 (Zero-Config)*: One-liner helpers like {name}`plot`, {name}`scatter`, {name}`bar`
2. *Tier 1 (Components)*: Building blocks like {name}`LeanPlot.Components.sample`, `mkLineChart`
3. *Tier 2 (Recharts)*: Direct JSX access for full control

Most users only need Tier 0. The other tiers exist for when you need
fine-grained control over rendering.

# Architecture
%%%
tag := "architecture"
%%%

LeanPlot has a layered architecture:

```
User Code → Tier 0 API → Components → Recharts → VS Code Infoview
```

Each layer adds more control at the cost of more verbosity.

## ProofWidgets4

LeanPlot is built on top of [ProofWidgets4](https://github.com/leanprover-community/ProofWidgets4),
which provides the infrastructure for rendering React components in the VS Code infoview.

## Recharts

The actual charts are rendered using [Recharts](https://recharts.org),
a composable charting library built on React components.

# The #plot Command
%%%
tag := "plot-command"
%%%

The `#plot` command is defined in `LeanPlot.DSL` and provides convenient syntax
for plotting functions:

```lean
-- Basic usage
#plot (fun x => x^2)
```

```lean
-- With custom sample count
#plot (fun x => Float.sin x) using 400
```

## Doc Comments as Captions

You can add a doc comment before `#plot` to display a caption above the chart:

```lean
/-- The classic parabola y = x² -/
#plot (fun x => x^2)
```

This acts as a "poor man's legend" – the doc string appears as a title above
the rendered chart when you hover.

Behind the scenes, `#plot f` expands to `#html LeanPlot.API.plot f`.

For expressions that already return `Html` (like {name}`plotMany`), use `#html` directly:

```lean
#html plotMany #[("sin", Float.sin), ("cos", Float.cos)]
```
