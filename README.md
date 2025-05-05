# LeanPlot

LeanPlot is a **tiny plotting wrapper for Lean 4's [`ProofWidgets4`](https://github.com/leanprover-community/ProofWidgets4) ecosystem**.  It lets you write Lean code that produces interactive charts—rendered directly in VS Code's infoview—using the rich [Recharts](https://recharts.org) React library.

The goal is to grow a _grammar-of-graphics_ style API over time.  For now we provide convenience helpers around the most common use-cases: sampling Lean functions and visualising the result as line charts.

---

## ✨ Features (0.2.x)

* **Tier-0 zero-config helpers**  `LeanPlot.API.lineChart` and `scatterChart` – go from a Lean function _or_ an array of points to an interactive plot with **one line of Lean**.
* `sample` / `sampleMany` – lower-level helpers to uniformly sample functions on an interval (works for any codomain that has a `[ToFloat]` instance).
* `mkLineChart` / `mkScatterChart` – escape hatches that let you customise every Recharts prop once you outgrow Tier-0.
* Ready-to-run demos under `LeanPlot/Demos` (linear, quadratic, cubic, overlay).

---

## 📦 Installation

Add LeanPlot as a dependency in your project's `lakefile.toml`:

```toml
[[require]]
name = "LeanPlot"
url = "https://github.com/alok/LeanPlot"
```

or in `lakefile.lean`:

```lean
require LeanPlot from git
  "https://github.com/alok/LeanPlot" @ "main"
```

Then run:

```bash
lake update
lake build
```

Make sure you have node/npm installed—the ProofWidgets build will take care of JS bundling automatically.

---

## 🚀 Quick start

Open a `.lean` file in VS Code with the infoview visible and paste:

```lean
import LeanPlot.API

open LeanPlot.API

-- One-liner!  Put your cursor on the `#plot` line.
#plot (lineChart (fun x : Float => x))
```

You should see an interactive line chart pop up.

---

## 🏟 Demo gallery

See `Gallery.md` for the roadmap of examples we plan to support.  The following are already available:

* `LeanPlot.Demos.LinearDemo`   – `y = x`
* `LeanPlot.Demos.QuadraticDemo` – `y = x²`
* `LeanPlot.Demos.CubicDemo`    – `y = x³`
* `LeanPlot.Demos.OverlayDemo`  – overlay of `y = x` and `y = x²`

Run them by putting your cursor over the `#html` command in each file.

---

## 🛠 Development

```bash
just build        # = lake build
just linter       # run Std.Tactic.Lint (setup WIP)
just docs         # regenerate docs (TBD)
```

Contributions welcome!  Check the TODO list and open a PR or issue.

---

## 📄 Licence

LeanPlot is released under the MIT licence.  See `LICENSE` for details.
