# LeanPlot

LeanPlot is a **tiny plotting wrapper for Lean 4's [`ProofWidgets4`](https://github.com/leanprover-community/ProofWidgets4) ecosystem**.  It lets you write Lean code that produces interactive charts—rendered directly in VS Code's infoview—using the rich [Recharts](https://recharts.org) React library.

The goal is to grow a _grammar-of-graphics_ style API over time.  For now we provide convenience helpers around the most common use-cases: sampling Lean functions and visualising the result as line charts.

---

## ✨ Features (0.1.x)

* `sample` / `sampleMany` – uniformly sample functions on a numeric interval.
* `mkLineChart` – build a Recharts `<LineChart>` (with X/Y axes) from an `Array Json`.
* Ready-to-run demos under `LeanPlot/Demos`.

---

## 📦 Installation

Add LeanPlot as a dependency in your project's `lakefile.toml`:

```toml
package LeanPlot where
  srcDir := "LeanPlot"

require proofwidgets from git
  "https://github.com/leanprover-community/ProofWidgets4" @ "main"
require leanplot from git
  "https://github.com/YOUR_GITHUB/LeanPlot" @ "main"
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
import LeanPlot.Components
open Lean ProofWidgets Recharts LeanPlot.Components
open scoped ProofWidgets.Jsx

/-- Plot `y = x` on `[0,1]`.  Put your cursor on the `#html` line. -/
#html mkLineChart (sample (fun x => x) 200 0 1) #[("y", "#1f77b4")] 400 400
```

You should see an interactive line chart pop up.

---

## 🏟 Demo gallery

See `Gallery.md` for the roadmap of examples we plan to support.  The following are already available:

* `LeanPlot.Demos.overlay` – overlays `y = x` and `y = x²`.

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