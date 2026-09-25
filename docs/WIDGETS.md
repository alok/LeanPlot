# LeanPlot widgets: figures in the infoview

`widgets/` is a separate Lake package, `LeanPlotWidgets`. It shows LeanPlot figures in the Lean
infoview through [ProofWidgets](https://github.com/leanprover-community/ProofWidgets4). The root
`LeanPlot` package stays dependency-free. Only users who want infoview output pull in
ProofWidgets.

```lean
import LeanPlotWidgets
open LeanPlot

#plot Float.sin                                    -- Makie `lines` of sin on -5..5
#plot (fun x => x * Float.exp (-x)) on 0..8 using 400
#plot [Float.sin, Float.cos] on 0..2*Num.pi

/-- Two waves and a legend. -/
#figure
  let xs := Num.range 0 10 101
  Figure.new |>.axis 1 1 (Axis2.new (title := "waves") (xlabel := "t")
      |>.linesFn Float.sin xs (label := "sin") |>.linesFn Float.cos xs (label := "cos"))
    |>.legend 1 2 (1, 1)
```

Put the cursor on a command to see its figure in the infoview. A doc comment on the command
becomes the figure's caption.

## Using it

The package lives in the `widgets/` subdirectory of the LeanPlot repository:

```toml
[[require]]
name = "LeanPlotWidgets"
git = "https://github.com/alok/LeanPlot"
rev = "<commit>"
subDir = "widgets"
```

It requires `LeanPlot` with `path = ".."`, which resolves inside the same checkout. Downstream
projects therefore get a matching LeanPlot automatically and should not require LeanPlot a
second time. This was checked with a scratch consumer that required the branch through a local
git URL with `subDir = "widgets"`.

Requirements:

* Toolchain `leanprover/lean4:v4.35.0-rc3` (`widgets/lean-toolchain`), the same as LeanPlot.
* ProofWidgets `v0.0.114`, the ProofWidgets tag for this toolchain. It is fetched by `lake`.
* **No npm or node.** ProofWidgets commits its compiled JavaScript (`widget/js/*.js`) together
  with a `lake.trace`, so Lake reuses those files as long as the TypeScript sources are
  unchanged. The `[[require]]` sets ProofWidgets' `errorOnBuild` option, as Mathlib does. If
  Lake ever wants to run npm (because the checkout under `.lake/packages/proofwidgets` is
  modified or stale), the build fails with instructions to delete that directory and build
  again, instead of producing an npm error. A fresh `lake build` on a machine that has npm
  installed did not create `node_modules`.
* A C toolchain is **not** needed beyond what Lean ships. The library sets `precompileModules`
  (see [Performance](#performance)). Lake compiles LeanPlot, ProofWidgets and LeanPlotWidgets
  to shared libraries with the `clang` bundled in the Lean toolchain.

Building and testing inside the repository:

```bash
cd widgets
lake build        # the library and the tests (checked while they elaborate)
lake test         # the same tests (the test driver is the LeanPlotWidgetsTest library)
```

A cold build on an M4 Max took 16 s wall-clock with all caches disabled
(`LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR=`). That covers fetching ProofWidgets, compiling
LeanPlot and ProofWidgets, the three shared libraries and the tests.

## `#figure`

`#figure e` elaborates and evaluates `e`, renders the scene, and saves a ProofWidgets
`HtmlDisplayPanel` on the command. `e` may be:

| type of `e` | shown as |
|---|---|
| `Figure` | the figure |
| `Scene` | the scene (already laid out, device pixels) |
| `Axis2`, `Axis3` | the axis alone in `Figure.new` (600 × 450), like Makie's `lines(...)` |
| `IO α` for any of the above | the result of running the action (e.g. a figure read from data files) |

The accepted types are open. `#figure` works for any `α` with a `LeanPlot.Widgets.ToScene α`
instance, and for `IO α` of such an `α` (through the class `FigureEval`). Nothing is written to
disk. If evaluation throws, the error is reported on the command and no widget is saved.

## `#plot`

`#plot` is Makie's non-mutating `lines`: one `Axis2` with Makie's defaults in a default
`Figure`.

```
#plot e
#plot e on a..b
#plot e on a..b using n
#plot e using n
```

| type of `e` (class `Plottable`) | drawn as (Makie) |
|---|---|
| `Float → Float` | `lines(range(a, b, length = n), f)` |
| `Float → Float × Float` | a parametric curve at `range(a, b, length = n)` |
| `Array`/`List (Float → Float)` | one `lines!` per function, cycling Makie's palette |
| `FloatArray`, `Array`/`List` of `Float`, `Nat` or `Int` | `lines(ys)`: `x = 1, 2, …, n` |
| `Pts2`, `FloatArray × FloatArray`, `Array Float × Array Float`, `Array`/`List (Float × Float)` | `lines(xs, ys)` |
| `Figure`, `Scene`, `Axis2`, `Axis3`, `IO` of these | shown as by `#figure` |

* **Functions are elaborated at `Float → Float` first**, so `#plot fun x => x * x` and
  `#plot (fun x => x ^ 2)` need no type ascription. Only if that fails is `e` elaborated
  without an expected type, and then the `Plottable` instance picks the recipe. This is how
  parametric curves (`fun t => (Float.cos t, Float.sin t)`) and data are handled.
* **Default domain `-5..5`, 201 samples.** Makie has no domain-less function plot. It copies
  `tryrange` from Plots.jl, which starts at `-5..5`, and `range(-5, 5, length = 201)` has step
  0.05 and hits 0 exactly. `using n` sets the sample count, and `n ≥ 2` is required. The
  domain must be finite and non-empty.
* **Uniform sampling, not Makie's adaptive grid.** For a function on an interval,
  `lines(a..b, f)` samples with `PlotUtils.adapted_grid`, which jitters its points with a
  seeded `MersenneTwister`. `#plot` samples Julia's `range(a, b, length = n)` bit for bit
  (`Num.range`). This is deterministic and matches `lines(range(a, b, length = n), f)` point
  for point.
* `on`/`using` apply to functions only. For data they are ignored with a warning.
* `Plottable` is open. Add `instance : Plottable MyCurve` to make `#plot myCurve` work. The
  class method adds plots to a given `Axis2` and receives the `PlotSpec`.

Syntax notes:

* **Write the domain without spaces around `..`**: `-1..1`, `0..2*Num.pi`, `-Num.pi..Num.pi`.
  In `a .. b`, the `..` after a space parses as the "insert placeholders" argument of an
  application `a ..`, the same pitfall as Mathlib's `∫ x in a..b`.
* With `on`/`using`, the plotted term is parsed at maximal precedence. It must be an
  identifier, a literal, or parenthesized: `#plot (fun x => x * x) on 0..1`. Without clauses
  any term works, e.g. `#plot fun x => x * x` or `#plot Pts2.sample Float.exp 0 1 50`.
* `on` is a non-reserved keyword (`&"on"`). Importing LeanPlotWidgets does not make `on` a
  token, so identifiers named `on` keep working. The clause also parses where `on` is a
  reserved token, as with Mathlib's `f on g` notation. This was checked by declaring an
  `infixl:2 " on "` notation next to `#plot … on …`.

## Display options

`set_option leanplot.widgets.format "svg" | "png" | "inline"` selects how figures are embedded
(the default is `"svg"`):

| format | HTML | notes |
|---|---|---|
| `svg` | `<img src="data:image/svg+xml;base64,…">` | exactly `Scene.renderSVG`, the file `Figure.save "x.svg"` writes. The `<img>` isolates the document from the page (no shared ids or styles). |
| `png` | `<img src="data:image/png;base64,…">` | exactly `Scene.renderPNG`. The page holds one bitmap instead of many vector paths, which helps for very large meshes. |
| `inline` | `<div dangerouslySetInnerHTML={{__html: svg}}>` | a fallback for hosts that block `data:` images. Every `id="…"` and `url(#…)` is prefixed with `lp<hash of the SVG>-`, so two inline figures on one page cannot pick up each other's clip paths or gradients. |

All formats use `max-width: 100%; height: auto`, so a 600 px figure shrinks to a narrow
infoview and keeps its aspect ratio. The figure has its own (white) background, so it reads the
same in light and dark themes. The VS Code infoview sets no Content-Security-Policy
(vscode-lean4 `infoview.ts`), so `data:` images load.

## Using figures in other widgets

```lean
open ProofWidgets Jsx in
#html <div>{fig.toHtml}{(plotFigure Float.cos).toHtml { width := some 300 }}</div>
```

| API | |
|---|---|
| `Scene.toHtml`, `Figure.toHtml`, `Axis2.toHtml`, `Axis3.toHtml` `(opts : Widgets.Options := {})` | `ProofWidgets.Html` |
| `Widgets.Options` | `format`, `width` (CSS px; height follows), `fit` (max-width 100%), `alt`, `caption`, `svg` (SVG writer options) |
| `Widgets.html (a : α) [ToScene α]` | the same, for any displayable value |
| `Widgets.message a : CoreM MessageData` | a structured message (`logInfo` a figure from a tactic or command) |
| `HtmlEval Figure/Scene/Axis2/Axis3` | `#html fig` works |
| `Widgets.svgDataUri`, `Widgets.pngDataUri`, `Widgets.prefixIds` | the building blocks |
| `Widgets.plotFigure a (spec : PlotSpec)` | the figure `#plot a on … using …` shows |

The HTML trees contain only `element` and `text` nodes and no components, so they need no RPC
session and serialize to plain JSON (`Server.rpcEncode`).

## How it is tested

The infoview cannot be observed from a build, so the tests check everything up to the JSON the
infoview receives.

* `LeanPlotWidgetsTest/Html.lean` pins down the HTML trees: tag, attributes, style objects,
  display sizes and rounding, captions, and the exact wire JSON. It decodes the data URIs back
  to bytes and compares them with `Figure.toSVG` and `Figure.toPNG` (PNG signature included).
  For the inline format it checks that every id and every reference is prefixed and that the
  root element is rescaled. It also covers `#html fig` and `Widgets.message`.
* `LeanPlotWidgetsTest/Commands.lean` defines `#guard_widget expected => cmd`. The command
  elaborates `cmd`, reads the panel widgets it saved back from the info trees, and requires
  exactly one `HtmlDisplayPanel` whose `html` prop equals the wire JSON of `expected`. All
  forms of `#figure` and `#plot` (domains with negative and compound bounds, `using`,
  parametric curves, lists, data, captions, the three formats) are compared with
  `Figure.toHtml`/`plotFigure`. Error messages are pinned with `#guard_msgs`. Failures save no
  widget (`#guard_no_widget`). A mutation check (wrong figure, wrong format, a command that
  saves nothing) makes `#guard_widget` fail as intended.
* Manual check: the wire JSON of the svg, png and inline formats (two inline figures on one
  page), a caption and `width := 300` was rendered with React 18 using the element and text
  branches of ProofWidgets' `renderHtml` (`widget/src/htmlDisplay.tsx`). Everything displayed
  at the expected sizes, scaled to a 460 px pane, with distinct inline ids.
* Interpreted and native evaluation produce byte-identical SVG and PNG. This was checked by
  hash for a line/scatter figure, a surface and a heatmap.

## Performance

Measured on an Apple M4 Max. The figure is the README's `sin` figure at 600 × 450.

| step | interpreted | native (`precompileModules`) |
|---|---|---|
| `Figure.toScene` (layout, ticks, text), first call in a process | 278 ms | 1 ms |
| `Figure.toScene`, later calls | 48 ms | ≤ 1 ms |
| `Scene.renderSVG` | 7 ms | < 1 ms |
| base64 data URI (13 kB) + HTML + JSON | 15 ms | ~1 ms |
| `Scene.renderPNG` | 3080 ms | 16 ms |
| a whole `#plot` command (20 in a file, averaged) | ~400 ms | ~4 ms |
| the widget test library (`lake build`) | ~35 s | ~1.2 s |

Commands evaluate user code with `evalExpr`. Without native code, all of LeanPlot (layout, font
outlines, the SVG writer and especially the rasterizer) runs in the IR interpreter. The
`LeanPlotWidgets` library therefore sets `precompileModules = true`. Lake then builds shared
libraries for it and for everything it imports (LeanPlot, ProofWidgets) and loads them as
plugins when elaborating any importing file. This covers the language server (`lake
setup-file` lists the plugins) and downstream packages. `lake env lean` does not load plugins
and falls back to the interpreter, with the same output.

## Design notes

* **One renderer.** The widget shows the SVG writer's output, so the infoview is WYSIWYG with
  CI goldens and saved files. No Recharts and no JavaScript of our own (the v0 design on
  `archive/v0-recharts` had both). Interactivity (sliders, 3D rotation) would be a new
  ProofWidgets component. It is not part of this package yet.
* **`<img>` by default.** The SVG writer numbers clip paths `c0, c1, …` and gradients `g0, …`.
  Inlined as-is, two figures on one page would share ids, and the second figure would clip to
  the first one's rectangles. An `<img>` gives each figure its own document, and the inline
  format prefixes ids instead. The SVG draws text as glyph outlines, so it needs no fonts in
  either case.
* **Panel widgets, not messages**, as ProofWidgets' `#html` does. `#figure`/`#plot` log
  nothing on success, so they add no diagnostics to a file. `Widgets.message` is available
  when a message is wanted.
* **No file writes at elaboration time** (DESIGN.md rule). Use `Figure.save` from `#eval` or an
  executable.
