# LeanPlot

<p align="center">
  <img src="docs/img/line_y_equals_x.png" width="220" alt="Line y = x">
  <img src="docs/img/quadratic_y_equals_x2.png" width="220" alt="Quadratic y = x²">
  <img src="docs/img/overlay_yx_and_x2.png" width="220" alt="Overlay of y = x and y = x²">
</p>

LeanPlot turns Lean 4 code into **interactive, React-powered charts that render right inside VS Code's infoview**. Built on top of [ProofWidgets4](https://github.com/leanprover-community/ProofWidgets4) and [Recharts](https://recharts.org), it lets you inspect functions and data visually while you prove.

**[Documentation](https://alok.github.io/LeanPlot/)**

---

## Features

* **Simple plotting** – `#plot (fun x => x^2)` just works with automatic axis labels and styling
* **One-liner helpers** – `plot`, `plotMany`, `scatter`, `bar` for beautiful plots with zero config
* **Composable graphics** – overlay or stack plots with the `+` operator
* **Grammar of Graphics** – fluent builder pattern inspired by ggplot2
* **Faceting** – multiple sub-plots in a grid layout
* **Log/linear scales** – visualize exponential growth with logarithmic axes
* **Data transformations** – apply scales, normalize, and smooth data

---

## Installation

Add LeanPlot to your project's `lakefile.toml`:

```toml
[[require]]
name = "LeanPlot"
git = "https://github.com/alok/LeanPlot"
```

Then fetch and build:

```bash
lake update
lake build
```

You'll need `node`/`npm` on your PATH – ProofWidgets handles the bundling automatically.

---

## Quick Start

```lean
import LeanPlot.API
import LeanPlot.DSL

-- Simple function plot
#plot (fun x => x^2)

-- With custom sample count
#plot (fun t => Float.sin t) using 400

-- Doc comments become chart captions (a poor man's legend!)
/-- The classic parabola y = x² -/
#plot (fun x => x^2)

-- Multiple functions with automatic legend
#html plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- Scatter plot
#html scatter (fun x => x^2) (steps := 50)

-- Bar chart
#html bar (fun i => i^2) (steps := 10)
```

Hover over `#plot` or `#html` in VS Code to see the interactive charts!

### PNG Export

```lean
import LeanPlot.API
import LeanPlot.Debug
open LeanPlot.API LeanPlot.Debug

#html withSavePNG (plot (fun x => x^2)) "my-plot" "quadratic.png"
```

### Advanced Composition

```lean
import LeanPlot.Algebra
open LeanPlot.Algebra

#plot (
  line "y"  (fun x : Float => x) +
  line "y²" (fun x => x*x)
)
```

### Grammar of Graphics

```lean
import LeanPlot.GrammarOfGraphics
import LeanPlot.Core
open LeanPlot.GrammarOfGraphics

#html (
  plot (fun x => x * x)
    |> fun p => PlotBuilder.withTitle p "Quadratic Function"
    |> fun p => PlotBuilder.withSize p 500 400
    |> PlotBuilder.build
    |> Render.render
)
```

---

## Demo Gallery

* `LeanPlot.Demos.SmartPlottingDemo` – Zero-config beautiful plots (start here!)
* `LeanPlot.Demos.LinearDemo`, `QuadraticDemo`, `CubicDemo` – Basic function plots
* `LeanPlot.Demos.OverlayDemo` – Overlaying multiple functions
* `LeanPlot.Demos.TrigDemo` – Trigonometric functions
* `LeanPlot.Demos.LogScaleDemo` – Logarithmic scales
* `LeanPlot.Demos.GrammarDemo` – Grammar of Graphics DSL
* `LeanPlot.Demos.TransformDemo` – Data transformations
* `LeanPlot.Demos.FacetDemo` – Grid layouts

Open any demo and hover over `#plot` or `#html` to see the charts.

---

## Documentation

Full documentation is available at **https://alok.github.io/LeanPlot/**

To build docs locally:

```bash
lake build leanplot-docs
.lake/build/bin/leanplot-docs
python3 -m http.server 8000 --directory _out/docs/html-multi
```

---

## License

Apache License 2.0 – see `LICENSE` for details.
