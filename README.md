# LeanPlot

<p align="center">
  <img src="docs/img/line_y_equals_x.png" width="220" alt="Line y = x">
  <img src="docs/img/quadratic_y_equals_x2.png" width="220" alt="Quadratic y = x²">
  <img src="docs/img/overlay_yx_and_x2.png" width="220" alt="Overlay of y = x and y = x²">
</p>

LeanPlot turns Lean 4 code into **interactive, React-powered charts that render right inside VS Code's infoview**.  Built on top of [ProofWidgets4](https://github.com/leanprover-community/ProofWidgets4) and [Recharts](https://recharts.org), it lets you inspect functions and data visually while you prove.

---

## ✨ Key features

* **One-liner helpers `lineChart` / `scatterChart`** – produce a plot from a Lean function or an array of points with zero configuration.
* **Composable graphics algebra** – overlay or stack plots with the `+` operator or `PlotSpec.stack`.
* **Layered API** – start at the high level and drop down to `PlotSpec` or the raw Recharts props whenever you need fine-grained control.
* **Sampling utilities** – `sample` / `sampleMany` uniformly sample any codomain that implements `[ToFloat]`.
* **Demo gallery** – ready-to-run examples under `LeanPlot/Demos` (linear, quadratic, cubic, overlay, stack, bar, area…).

---

## 🏗 Installation

Add LeanPlot to your project's `lakefile.toml`:

```toml
[[require]]
name = "LeanPlot"
url = "https://github.com/alok/LeanPlot"
```

or to `lakefile.lean`:

```lean
require LeanPlot from git
  "https://github.com/alok/LeanPlot" @ "main"
```

Then fetch and build the deps:

```bash
lake update
lake build
```

(You'll need `node`/`npm` on your PATH – ProofWidgets handles the bundling automatically.)

---

## 🚀 Quick start

Create a new `.lean` file, open the infoview, and paste:

```lean
import LeanPlot.Algebra

open LeanPlot.Algebra

#plot (
  line "y"  (fun x : Float ↦ x) +
  line "y²" (fun x ↦ x*x)
)
```

Hover over `#plot` and you'll see an interactive chart with two series.

---

## 🏟 Demo gallery

* `LeanPlot.Demos.LinearDemo`     – `y = x`
* `LeanPlot.Demos.QuadraticDemo`  – `y = x²`
* `LeanPlot.Demos.CubicDemo`      – `y = x³`
* `LeanPlot.Demos.OverlayDemo`    – overlay of `y = x` and `y = x²`
* `LeanPlot.Demos.StackDemo`      – stacking via `+` and `PlotSpec.stack`

Open any demo and hover the `#html` command to run it.

---

## 🛠 Development

```bash
just build   # lake build
just linter  # run Std.Tactic.Lint (WIP)
just docs    # regenerate docs (TBD)
```

Contributions welcome – check `TODO.md` and open an issue or PR.

## 📄 License

LeanPlot is released under the Apache License 2.0; see `LICENSE` for details.
