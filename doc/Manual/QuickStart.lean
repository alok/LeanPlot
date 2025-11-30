/-
Copyright (c) 2024-2025 LeanPlot Authors. All rights reserved.
Released under Apache 2.0 license.
-/
import VersoManual
import Manual.Meta

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean
open LeanPlot.API

set_option pp.rawOnError true

#doc (Manual) "Quick Start" =>
%%%
tag := "quickstart"
%%%

This chapter gets you plotting in minutes.

# Installation
%%%
tag := "installation"
%%%

Add LeanPlot to your project's `lakefile.toml`:

```
[[require]]
name = "LeanPlot"
git = "https://github.com/alok/LeanPlot"
```

Then fetch and build the dependencies:

```
lake update
lake build
```

You'll need `node`/`npm` on your PATH - ProofWidgets handles the bundling automatically.

# Your First Plot
%%%
tag := "first-plot"
%%%

Create a new `.lean` file, import LeanPlot, and use the `#plot` command:

```
import LeanPlot.API
import LeanPlot.DSL

-- Plot a simple function
#plot (fun x => x^2)
```

![Simple quadratic plot](img/plot_quadratic.svg)

This renders a parabola with automatic axis labels and styling in the VS Code infoview!

## Customizing Samples

You can customize the number of sample points:

```
#plot (fun t => Float.sin t) using 400
```

## Multiple Functions

For multiple functions on the same chart, use `plotMany`:

```
#html plotMany #[("sin", fun x => Float.sin x),
                 ("cos", fun x => Float.cos x)]
```

![Sin and cos comparison](img/plot_sincos.svg)

This automatically assigns colors and creates a legend.

## More Examples

*Damped oscillation:*

![Damped oscillation](img/plot_damped.svg)

*Tanh activation function:*

![Tanh function](img/plot_tanh.svg)

*Scatter plot:*

![Scatter plot](img/scatter_demo.svg)
