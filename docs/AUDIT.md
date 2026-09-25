# LeanPlot audit and restructuring spec (for the chakravala-ecosystem Lean port)

Audited repo: `/Users/alokbeniwal/leanplot` (GitHub `alok/LeanPlot`), read-only.
Checked-out branch: `algebra-tunable-core` (HEAD `217969c`, 2026-01-07). Compared with `main` (HEAD `93ebc25`, 2026-07-10).
Merge base: `4cbd9bb` ("chore: update toolchain to v4.27.0-rc1"). `main` is 3 commits ahead of the merge base and `algebra-tunable-core` is 6.
Unless a line says otherwise, file:line citations are to the `algebra-tunable-core` working tree. That tree is identical to the branch HEAD except for the untracked `.claude/` directory and a 730 MB untracked `Makie.jl/` clone at the repo root.

Scratch evidence produced for this audit (the repo itself was not modified):

- `…/scratchpad/leanplot-scratch/core/`: a zero-dependency Lake project with the 16 pure modules copied out and built on `leanprover/lean4:v4.35.0-rc3`. It needed two one-line patches. `CoreTest.lean` is a headless PNG/SVG driver plus benchmark. `tmp/Goldens.lean` holds golden values.
- `…/scratchpad/leanplot-scratch/oracle/`: `makie_oracle.jl` and `makie_oracle.json` (Makie/CairoMakie goldens: ticks, palette, viridis, contour levels, streamplot), plus the reference renders `ref_lines.png/.svg` and `ref_stream.png`.
- `…/scratchpad/leanplot-scratch/decls.md`: the auto-generated public declaration index, reproduced as Appendix A.

---

## 1. Purpose & scope

LeanPlot's stated purpose is "interactive, React-powered charts that render right inside VS Code's infoview" (README.md:9). It is built on ProofWidgets4 and Recharts. In practice the repo contains **four overlapping plotting stacks** and one static-export stack:

| Stack | Entry types | Output | Needs VS Code / ProofWidgets? | Status |
|---|---|---|---|---|
| A. Tier-0 helpers | `LeanPlot.API.{plot,plotMany,scatter,bar}` (API.lean:99-140), `LeanPlot.Components.*` (Components.lean) | `ProofWidgets.Html` (Recharts JSX) | yes | works in infoview only |
| B. `PlotSpec` spec layer | `LeanPlot.PlotSpec` (Specification.lean:59-76), `SeriesDSpecPacked` (Series.lean:124-128) | Html via Recharts | yes | works in infoview only; `+` concatenates rows (bug, see §4.9) |
| C. `Graphic` algebra | `LeanPlot.Graphic` (Graphic.lean:96-111) with `+`, `\|\|\|`, `/` | Html; also `toRenderPlan` (Graphic.lean:525-546) | the type imports ProofWidgets | the "first-class" API |
| D. Grammar-of-graphics / composition | `GrammarOfGraphics.PlotBuilder` (GrammarOfGraphics.lean:69-79), `PlotComposition`, `Faceting`, `Core.Plot` (Core.lean:50-97), root-level `LinePlot` (Algebra.lean:37-92) | Html | yes | partially stubbed (`logX`/`logY` ignored) |
| E. Static export | `Backend.RenderPlan` (Backend.lean:227-240) + CPU bitmap (Backend/Bitmap.lean); `Render.{Bitmap,Rasterize,PNG}`; `LeanPlot.SVG` (SVG.lean); `Render.Export.{savePNG,saveSVG}` (Render/Export.lean) | PNG bytes, SVG text | **The pure modules do not. `Render/Export.lean` does, because it imports `Graphic`** | minimal: no text in PNG, no ticks, no antialiasing, uncompressed PNG |

Interactive extras (all JS, all VS Code-only): `#plot +tunable` source-rewriting panel (TunablePlot.lean:47-384, only on `algebra-tunable-core`), `#iplot` slider panel (Interactive.lean:97-~497), and a "Save PNG" button via html2canvas loaded from a CDN (Debug.lean:28-38).

For the downstream project (a Lean port of Michael Reed's Grassmann/Cartan/Fatou/Adapode/FlowGeometry stack), the relevant scope is **headless, CI-renderable SVG/PNG of scientific plots**, matching what the Julia packages do through their Makie extensions (§7.3). The Recharts/React stacks A–D are not a viable basis for that. Stack E is the right seed, but it is too thin (≈1.2 kLOC, 2D lines/points/bars only).

Toolchains: `main` is on `leanprover/lean4:v4.32.0-rc1` with `proofwidgets` pinned to `v0.0.104` and `verso` pinned to `v4.32.0-rc1` (main:lakefile.toml, main:lake-manifest.json). `algebra-tunable-core` is on `v4.27.0-rc1` with **unpinned** `proofwidgets` and `verso` (lakefile.toml:54-60; manifest inputRev `main`). The downstream Grassmann repo's lakefile has LeanPlot commented out as a dependency because of "Verso dependency issues" (`/Users/alokbeniwal/Grassmann/lakefile.toml:17-20`). CI (`.github/workflows/lean_action_ci.yml`) was green on `main` at v4.32.0-rc1 (gh run 29128075623). The docs deploy job failed (run 29128075583).

---

## 2. Public API inventory

Appendix A has the complete machine-generated index of every non-private `def/abbrev/structure/inductive/class/instance/syntax/macro/notation` with file:line: 731 lines covering all non-Demo, non-Test modules. This section gives the **semantic** inventory, grouped by layer, with signatures and exact behaviour. ASCII/Unicode operator aliases are listed in §2.8.

### 2.1 Pure core (no dependencies; builds headless; verified on v4.35.0-rc3 after patches)

**`LeanPlot/Constants.lean`**
- `LeanPlot.Constants.defaultW : Nat := 400` (:10), `defaultH : Nat := 300` (:13).
- `_root_.Float.pi : Float := 3.14159265358979323846` (:20). This is a **root-namespace pollution hazard**.

**`LeanPlot/ToFloat.lean`**
- `class LeanPlot.ToFloat (α : Type u) where toFloat : α → Float` (:23-25), `export ToFloat (toFloat)` (:27), `toFloatFn` (:30).
- Instances: `Float` = id (:35), `Nat` = `Float.ofNat` (:38), `Int` = `Float.ofInt` (:41), generic `[Coe α Float] : ToFloat α` (:47-48), `Rat` = `num/den` as floats (:57-58).
- On `algebra-tunable-core`, :50 is `/-! ### \`Rat\` instance`. Lean ≥ 4.32 rejects this with "Incorrect header nesting" under `doc.verso = true`. `main` already has `##`, which is GitHub issue #7, closed.

**`LeanPlot/Palette.lean`**
- Ten viridis-sampled hex constants, `darkPurple #440154 … yellow #fde725` (:22-49).
- `defaultPalette : Array String`, in this order: `#b5de2b, #1f9e89, #3e4a89, #31688e, #26828e, #482878, #35b779, #6ece58, #440154, #fde725` (:52-63).
- `colorFromNat n = defaultPalette[n % 10]!` (:67-68). `autoColors names = names.zipIdx.map (n,i) ↦ (n, colorFromNat i)` (:75-76). A public def literally named `LeanPlot.Palette.f (n) : List Color` (:80-82).
- **Root-level** list-comprehension `syntax "[" term "|" term " in " term (", " term)? "]" : term` with `macro_rules` (:86-100). **Root-level** `def squares`, `def evensSq` (:107-109). Top-level `#eval`s run on every build (:111-112).

**`LeanPlot/Scale.lean`**
- `inductive ScaleType | Linear | Logarithmic (base : Float := 10.0)` (:13-18).
- `transform`: log gives `log v / log base` if `v > 0`, **else 0** (:21-26). `inverseTransform`: `base ^ v` (:29-32). `transformArray` (:35). `structure ScaleConfig {xScale yScale}` (:39-43).

**`LeanPlot/Transform.lean`**
- `_root_.Float.sign` (:12-13, root pollution).
- `structure Scale {forward inverse : Float → Float, name : String}` (:16-23).
- `linearScale` (:26). `logScale base` (:33-37): forward is `0` for `x ≤ 0`. `sqrtScale` (:40): forward `Float.sqrt`, NaN for negatives. `powerScale e` (:47-51): odd-symmetric. `symlogScale C` (:54-58): `sign x * log(1 + |x|/C)`.
- `applyScale` (:61). `transformFunction f xScale yScale = yScale.forward ∘ f ∘ xScale.inverse` (:65-69).
- `normalize` (:72-79): min–max, and a constant input maps to `0.5`. `standardize` (:82-90): population variance, and σ = 0 maps to 0. `clamp` (:93-94).
- `smoothMovingAverage w` (:97-105): the window is centered with half-width `w/2`, truncated at the edges; it returns the input unchanged if `w = 0 ∨ size < w`.

**`LeanPlot/AutoDomain.lean`**
- `autoDomain f (N := 100) : Float × Float` (:28-53). It samples `x_i = -1 + 2i/(N-1)` for `i ∈ [0,N)` (plus `f(-1)` as the seed), then widens by 5 %. A flat function gives `(lo-1, hi+1)`. `N = 0` gives `(0,1)`.

**`LeanPlot/Backend.lean`** (added on `algebra-tunable-core`; no imports)
- `structure Color {r g b : Float, a : Float := 1.0}` in [0,1] (:25-30). Constants `black white red green blue` (:34-38).
- `Color.fromHex : String → Option Color` accepts `"#rrggbb"` or `"rrggbb"`, case-insensitive (:56-62).
- `Color.toHex` computes `(f*255).toUInt8`, which **truncates**: `0.5 → 7f` (:65-70). It uses the deprecated `String.mk` (:68).
- `structure Point {x y : Float}` (:75-78). `structure Rect {xMin yMin xMax yMax}` (:81-86) with `width height center fromSize expand union` (:90-106).
- `LineStyle {color := black, width := 1.5, dashPattern := #[]}` (:113-120). `FillStyle {color := blue, opacity := 0.7}` (:126-131). `MarkerStyle {color := blue, size := 4.0, shape := "circle"}` (:137-144).
- `inductive Primitive` (:150-159): `line (pts) (LineStyle) (name)`, `area (pts) (FillStyle) (Option LineStyle) (name)`, `scatter (pts) (MarkerStyle) (name)`, `bars (pts) (FillStyle) (barWidth : Float) (name)`.
- `Primitive.name/points/bounds` (:163-183). The bounds of an empty primitive are `[0,1]²`.
- `AxisConfig {label, domain, tickCount := 5, tickFormat : Option (Float → String), showGrid := true}` (:190-201). **Unused by any backend.**
- `GlobalStyle {title, background := white, showLegend := true, margin := 40, fontFamily, fontSize := 12}` (:206-219).
- `structure RenderPlan {primitives, viewport : Rect, dataBounds : Rect, xAxis yAxis : AxisConfig, style : GlobalStyle}` (:227-240). `RenderPlan.empty w h` (:245-248). `addPrimitive` (:251-258), which unions bounds. `legendEntries` (:261-262).
- `inductive RenderOutput | html | bitmap (w h) (pixels : ByteArray) | svg | png` (:269-278).
- `class RenderBackend (B) {backendName, create : IO B, render : B → RenderPlan → IO RenderOutput, cleanup}` (:281-289).
- `Backend.CoordTransform {dataBounds pixelBounds : Rect}` (:294-299) with `fromPlan` (:304-312), `toPixelX` (:315-320), `toPixelY` (:323-329, y inverted), `toPixel` (:332-333). These use a Float pixel space and **are not used by the bitmap backend**, which uses the Nat-based `Render.CoordTransform`.

**`LeanPlot/Render/Bitmap.lean`**
- `structure RGB {r g b : UInt8}` (:12-19). Constants (:23-31).
- `RGB.fromHex` **requires** a leading `#` and exactly 7 chars. It uses `String.trim`, which **was removed in v4.35**: `s.trim` must become `s.trimAscii.toString` (:34-50).
- `RGB.blend c1 c2 α`: α is clamped to [0,1] and channels are truncated (:53-58).
- `structure Bitmap {width height : Nat, pixels : Array RGB}`, row-major, y down (:65-72).
- `fill`, `create` (white) (:77-81); `getPixel?`, `getPixel` (:84-91). `setPixel` (:94-98) silently ignores out-of-bounds writes. `blendPixel` (:101-107) is unused. `fillRect`, `hLine`, `vLine` (:110-129). `toScanlines` emits filter byte 0 plus RGB triplets per row (:132-143).

**`LeanPlot/Render/Rasterize.lean`**
- `Bitmap.drawLine x0 y0 x1 y1 color` is Bresenham (:25-47). `drawThickLine … thickness` draws parallel 1-px Bresenham copies (:50-66).
- `structure Render.CoordTransform {dataXMin dataXMax dataYMin dataYMax : Float, pixelLeft pixelRight pixelTop pixelBottom : Nat}` (:71-82).
- `create bmp xMin xMax yMin yMax (margin := 40)` (:87-95). `toPixelX` (:98-104) and `toPixelY` (:107-113) round to `Int`.
- `drawAxes` (:118-133). `drawPolyline` always uses thickness 2 (:136-148). `plotFunction` (:151-161).

**`LeanPlot/Render/PNG/{CRC32,Adler32,Encode}.lean`**
- `crc32Polynomial = 0xEDB88320` (CRC32:11). `makeTableEntry` (:14-23). `crc32Table : Array UInt32`, 256 entries (:26-27). `updateCrcByte` (:31-33), `updateCrc32` (:36-37), `crc32` (:40-42).
- `adler32Mod = 65521` (Adler32:10). `adler32` reduces mod 65521 every byte (:13-24).
- `pushU32BE` (Encode:19-23), `pushU16LE` (:26-28), `pngSignature` (:33-34). `mkChunk type data` returns **empty** if `type` is not 4 bytes (:37-44). `mkIHDR w h (bitDepth := 8) (colorType := 2)` (:47-56), `mkIEND` (:59), `maxBlockSize = 65535` (:64).
- `mkDeflateBlock` writes stored blocks (:67-72). `splitIntoBlocks` (:75-85). `wrapZlib` writes CMF/FLG `78 01` (:88-98). `encode : Bitmap → ByteArray` (:103-109). `writePNG` (:112-113).

**`LeanPlot/SVG.lean`** (no imports)
- `Dims {width := 400, height := 300, margin := 40 : Nat}` (:16-23). `Point` (:26-31) is unused. `floatMin`/`floatMax` (:10-13).
- `linePath pts dims : String` (:34-59). `scatterPoints pts dims color` (:62-83). `axisLabels xLabel yLabel dims` (:86-90). `grid dims` (:93-102).
- `lineChartSVG pts title (color := "#2563eb") (dims := {})` (:105-115). `scatterChartSVG` (:118-128). `multiLineChartSVG (series : Array (String × Array (Float×Float) × String)) title dims` (:131-152).
- `sampleFn f (steps := 200) (min := 0) (max := 1)` (:155-167).

**`LeanPlot/Backend/Bitmap.lean`**
- `BitmapBackend {lineThickness := 2}` (:31-34). The field is unused.
- `colorToRGB` truncates (:39-42). There is a private 6-color palette (:47-54) that is unused in the render path. `renderPrimitive` (:77-126). `renderPlanToBitmap` (:129-148). `instance : RenderBackend BitmapBackend` (:152-163).
- `renderToPNG : RenderPlan → ByteArray` (:168-170). `savePlanPNG` (:173-175).

**`LeanPlot/Backend/SIMD.lean`**: a stub, self-described as "falls back to the scalar implementation" (:12-19). It defines `PointsSoA` (:29-49), `SIMDBackend` (:60-65), `renderPlanSIMD` (:199-214), `renderSIMDToPNG` (:233-235) and `saveSIMDPNG` (:238-240). It measured the same speed as the scalar path (§8.4).

**`LeanPlot/Backend/GPU/Interface.lean`**: opaque types `GPUContext/GPUBuffer/GPUShader` (:36-54), `DeviceLocation`, `DeviceBuffer`, `ShaderFormat`, `CompiledShader`, `RenderTarget`, `Framebuffer`, `GPUCapabilities`, `class GPUBackend`, `Vertex`, `packVertices` and `planToVertices` (listing in Appendix A). **No implementation and no FFI.** It is pure speculative scaffolding.

### 2.2 Recharts/ProofWidgets layer (VS Code only)

- `Components.lean`:
  - `sample f steps domainOpt : Array Json` (:33-57). Rows are `{x, y}`; the default domain is **[0,1]**.
  - `sampleMany fns steps min max` (:64-83).
  - `mkLineChart`, `mkLineChartWithLabels`, `mkLineChartFull` (:90-166).
  - Component wrappers re-exporting Recharts JS: `ScatterChart/Scatter/AreaChart/Area/BarChart/Bar/ComposedChart` (:168-303).
  - `mkScatterChart`, `mkBarChart` (:205-325).
  - `plotSimple` (:338-349): a fixed color `#2563eb` and labels `"x"`, `"f(x)"`.
  - `plotManySimple` (:360-376): its own 6-color Tailwind palette. `scatterSimple` uses `#dc2626` and `barSimple` uses `#16a34a` (:379-388).
- `API.lean`: `xyArrayToJson` (:31-32), `mkLineChart` (:37-39), `lineChart` (:52-65), `scatterChart` (:73-76), `plot` (:99-102), `plotMany` (:113-116), `scatter f` (:125-128), `bar f` (:137-140). **Note:** `API.scatter/bar` take a *function*, while `LeanPlot.scatter/bar` (Graphic) take *points*.
- `Axis.lean`: `AxisProps` (:18-38), plus `XAxis` and `YAxis` components (:40-47). `Legend.lean`: `LegendProps`, `Legend`, `LegendComp` (:18-33). `WarningBanner.lean` (:9-28). `Recharts.lean` is empty apart from a namespace. `Axes.lean` duplicates `Axis.lean` and is excluded from the root import (LeanPlot.lean:42), but the `LeanPlot.*` glob still compiles it.
- `Series.lean`:
  - `SeriesKind` (:21-30) and four detail records (:42-73).
  - Indexed `inductive SeriesDetails : SeriesKind → Type` (:79-87). `SeriesDSpec (k)` (:93-100). `SeriesDSpecPacked {kind, spec : SeriesDSpec kind}` (:124-128).
  - Accessors, constructors `mkLine/mkScatter/mkBar/mkArea`, and `setColor` (:159-229).
  - Renderers (:245-278). Lines render with `LineType.monotone` (:246), i.e. **cubic smoothing between samples**, which misrepresents sampled data and parametric curves.
- `LegacyLayer.lean`: `LegacyLayerSpec` (:34-45). It carries `@[deprecated SeriesDSpecPacked …]` naming a constant that is defined *downstream* in Series.lean. On v4.35 this fails with `Unknown constant SeriesDSpecPacked` (verified by elaborating the file against the sibling v4.35 checkout's built deps).
- `Specification.lean`:
  - `AxisSpec` (:38-47). `PlotSpec {chartData : Array Json, series : Array LayerSpec, xAxis, yAxis, title, width, height, legend}` (:59-76). `class ToPlotSpec` (:81-83).
  - `PlotSpec.line/scatter/bar/area/lines` (:99-236). The default domain here is **(-1,1)**, inconsistent with `Components.sample` and `Graphic`, which use (0,1).
  - Combinators `withTitle/withWidth/withHeight/withSize/withXLabel/withYLabel/withLegend/withXDomain/withYDomain/withSeriesColor/withSeriesColorAt/addSeries` (:242-315).
  - `overlay` (:321-329), `stack` (:332), `HAdd PlotSpec` (:334-335), `addLine/addScatter/addBar` (:338-407).
  - `class RenderFragment` (:412-416). `render : PlotSpec → Html` (:430-511).
  - `instance HtmlEval PlotSpec` (:526-574) runs elaboration-time validation: duplicate names or keys and missing JSON keys cause `throwError`.
- `Graphic.lean` (C):
  - `PlotOpts {domain : Option (Float×Float), samples := 200, color, name}` (:45-54). `Style {title, width := 400, height := 300, showLegend := true, xLabel, yLabel}` (:57-70).
  - `GraphicTag` (:73-90) and `structure Graphic` (:96-111), the representation described in §3.
  - `mkFn/mkPoints/mkBars/mkArea/mkOverlay/mkFacetH/mkFacetV/mkStyled` (:116-145).
  - Operators: `Add` = overlay (:152-153), `infixr:60 " ||| "` = facetH (:157), `HDiv` = facetV (:160-161).
  - `plot [ToFloat β] (f : Float → β) (opts := {})` (:173-174), `scatter (pts)` (:183-184), `bar (pts)` (:193-194), `areaPlot f` (:202-203).
  - Fluent setters `domain lo hi`, `samples n`, `color c`, `named n` (:237-265) recurse through composites via `updateOpts` (:210-229). `title`, `size`, `xLabel`, `yLabel`, `legend` (:285-322) wrap in or modify `styled` via `updateStyle` (:268-277).
  - `Graphic.toPlotSpec` (:434-460). `Graphic.toRenderPlan` (:525-546). `render` (:605-609). `instance HtmlEval Graphic` (:615-616). `ToPlotSpec Graphic` (:618-619). `abbrev line := @plot` (:624).
- `Plot.lean`:
  - `withCaption` (:31-46).
  - `syntax (name := plotCmd) (docComment)? "#plot" ("+tunable")? term : command` (:52) with `elabPlotCmd` (:74-139).
  - `syntax (name := renderCmd) "#render" term ("as" str)? : command` (:160) with `elabRenderCmd` (:163-196). `#render` elaborates a `Graphic`, runs `toRenderPlan |> Backend.renderToPNG`, and writes `.leanplot/last.png` or the given path **at elaboration time**. It logs `"Rendered to {path} ({n} bytes)"`. This command exists only on `algebra-tunable-core`.
- `DSL.lean`:
  - Removes the original `#plot` elaborator via `attribute [-command_elab]` (:47).
  - Adds `#plot t using n` (:50) and `#plot t domain=(a,b) steps=n size=(w,h)` (:53-56).
  - `elabPlotNew` (:109-134) wraps the term in `LeanPlot.API.plot` and falls back to the original elaborator. `elabPlotUsing` (:136-156), `elabPlotNamed` (:158-208).
  - Ends with `export LeanPlot.API (plot plotMany scatter bar)` at **root** (:211).
- `TunablePlot.lean`: `TunableSeries` (:14-22), `TunablePlotProps` (:24-43), and the `TunablePlotPanel` widget, which is about 330 lines of inline JS (:47-384).
- `Interactive.lean`: `ParamState` (:50-60), `InteractivePlotProps` (:64-93), the `InteractivePlotPanel` widget (:97-~497), `sampleFunction` (:500-507), defaults (:509-513), `syntax #iplot …` (:529-533), `parseTermAsFloat` (:536-~584), `elabIplotCmd` (:586-661), and `Graphic.renderInteractive` (:671-674), which is a no-op wrapper.
- `GrammarOfGraphics.lean`:
  - `Aesthetic` (:25-35), `Geom` (:37-55), `Layer` (:57-67), `PlotBuilder {spec, layers, globalAes, scaleConfig}` (:69-79).
  - Builder ops (:83-160), `build` (:162-183), and top-level `plot/scatterPlot/barPlot/areaPlot/plotLine/plotScatter/plotBar/plotArea/plotLines` (:186-236).
  - `notation:50 x:50 " >> " f:51 => f x` (:238, **global**). `Facet.grid/gridNamed` (:248-262). `_FacetChain`, `infixr:65 " ⫽ "` (:282-306).
  - A live `#html` demo inside the library module (:309-313).
- `PlotComposition.lean`: `mergeAligned` (:33-67), which ignores the domains it computes. `gridLayout` (:73-94), `verticalStack` (:96-108), `normalizeYScale` (:110-135), `applyColorScheme` (:137-152).
- `Faceting.lean`: `facetGrid`, `facetGridNamed` (:50-60).
- `Core.lean`:
  - `Renderable`, `class Render`, `CoeTC α Html` (:18-31). `Layer {html}`, `ToLayer`, `Plot {layers}`, `ToPlot` (:36-62).
  - `Plot.overlay` (:67). **Global** `instance (priority := 2000) [ToPlot α] [ToPlot β] : HAdd α β Plot`, and the same for `HMul` and `HDiv` (:74-84).
  - `Render Plot` stacks layers vertically; it is **not** an overlay (:96-109).
- `Algebra.lean`: **root-namespace** `LineSeries`, `LinePlot`, `LinePlot.overlay`, `LinePlot.toHtml`, `line` (:37-84), plus `Render`/`ToLayer` instances (:86-92). README.md:89 says `open LeanPlot.Algebra`, but no such namespace exists.
- `JsonExt.lean`: **global** `instance : Coe String Json` (:24) and `Coe (Option String) (Option Json)` (:30). `Json.keys` (:42), `jsonHasKeys` (:52), `HasKeys` Prop with a `Decidable` instance (:60-63), `#assert_keys` (:82-86).
- `AssertKeys.lean`: a helper only. `Utils.lean`: `isInvalidFloat`, `jsonDataHasInvalidFloats` (:9-25).
- `Metaprogramming.lean`: `ParameterRole` (:29), `inferParameterRole` (:99), `smartNames` and `smartLabels` (:180-200), `fixDuplicates` (:202), and friends. Axis-label heuristics from binder names map `t` to "time".
- `Debug.lean`: `SavePNG` widget loading html2canvas from jsdelivr at runtime (:28-30). `withSavePNG` (:34-38).
- `CLI/Export.lean` + `CLI/ExportMain.lean`: `leanplot-export --fn {sin|cos|tan|linear|quad|cubic|exp|tanh} --out f.json [--steps --min --max]` dumps `{x,y}` JSON rows (Export:17-60, ExportMain:13-55). It uses its own hand-rolled float parser (ExportMain:15-34), which does not accept exponents.

### 2.3 Executables (lakefile.toml)

`leanplot` (Main.lean, prints a banner), `jsonKeyCheckTest`, `leanplot-export`, `leanplot-docs` (Verso), `gendocimages` (GenDocImages.lean, which writes 6 SVGs to `doc/img/` via `LeanPlot.SVG`), and `testexport` (TestExport.lean: `Graphic.saveSVG`/`savePNG`, which **transitively needs ProofWidgets**).

### 2.4 Commands / macros

`#plot`, `#plot +tunable`, `#plot … using n`, `#plot … domain=(a,b) steps=n size=(w,h)`, `#render t [as "path"]`, `#iplot f domain=(..) steps=n color="…" size=(w,h)`, `#assert_keys j keys`, and the list comprehension `[e | x in xs, p]`.

### 2.5–2.7 Constants and defaults

Default chart size 400×300 (Constants:10-13). Default samples 200 everywhere. Export PNG default is 800×600 with margin 60 (Render/Export.lean:19-29). RenderPlan margin is 40 (Graphic.lean:545). Bitmap backend minimum dimension is 100 (Backend/Bitmap.lean:130-131).

### 2.8 Operators

| Operator | Unicode/ASCII | Meaning | Where | Scope |
|---|---|---|---|---|
| `+` | ASCII | Graphic overlay | Graphic.lean:152 | instance |
| `+` | ASCII | PlotSpec overlay (row concat) | Specification.lean:334 | instance |
| `+ * /` | ASCII | generic `ToPlot` overlay (renders stacked) | Core.lean:74-84 | global, priority 2000 |
| `\|\|\|` | ASCII | Graphic horizontal facet; **overloads core `HOr` notation** | Graphic.lean:157 | global `infixr:60` |
| `/` | ASCII | Graphic vertical facet | Graphic.lean:160 | instance |
| `⫽` (U+2AFD) | Unicode only | facet chain of PlotBuilders | GrammarOfGraphics.lean:293 | global `infixr:65` |
| `>>` | ASCII | reverse application `x >> f = f x` | GrammarOfGraphics.lean:238 | global `notation:50` |
| `[e \| x in xs, p]` | ASCII | list comprehension | Palette.lean:86-100 | global syntax |

The `|||` overload is harmless for parsing: `(1:UInt64) ||| 2 &&& 0` still evaluates to 1. However, every downstream `a ||| b` becomes an overloaded elaboration, and type errors are reported as `overloaded, errors …` (verified in `leanplot-scratch/core/tmp/Notation*.lean`). Grassmann uses `|||` on basis bitmasks pervasively, so this **must** be `scoped`.

---

## 3. Data representations

### 3.1 `Graphic`: a tag struct, not an inductive (Graphic.lean:96-111)

```lean
structure Graphic where
  tag : GraphicTag                      -- fn | points | bars | area | overlay | facetH | facetV | styled
  func : Float → Float := fun _ => 0    -- used by fn/area only
  pts  : Array (Float × Float) := #[]   -- used by points/bars only
  opts : PlotOpts := {}                 -- leaves only
  child1 child2 : Option Graphic := none -- composites; styled uses child1
  style : Style := {}                   -- styled only
```

Invariants are **not** enforced: composite nodes carry dummy `func`/`pts`, and a leaf can carry children. Every traversal is `partial` and has `| _, _ => (#[], idx)` fallbacks for impossible shapes (e.g. :367-373, :499-507). Everything is a runtime value. Points are `Array (Float × Float)`, which boxes each tuple *and* its two floats.

**Port:** make it a real `inductive Mark`/`Layer` tree (§8.2).

### 3.2 `PlotSpec` / `SeriesDSpecPacked`

The data is JSON rows (`Array Json`, each row `{"x": …, "<dataKey>": …}`). The series list is an existential Σ `⟨kind, SeriesDSpec kind⟩` (Series.lean:124-128), whose `details : SeriesDetails kind` index guarantees that the detail record matches the kind. This is the only real dependent-type use in the repo. It has zero runtime value for rendering, since the kind is a runtime tag anyway. Axis domains are stored as `Array Json` `[lo, hi]` (Specification.lean:46).

### 3.3 `RenderPlan` (Backend.lean:227-240)

- `primitives` are in z-order, first drawn first.
- `viewport` is a `Rect` in pixels starting at (0,0). `dataBounds` is the union of primitive bounds.
- Coordinates: data y points up, pixel y points down. `pixel = margin + (d - dMin)/(dMax - dMin) * (extent - 2·margin)`, and for y `pixelBottom - …` (Backend.lean:315-329; Rasterize.lean:98-113).
- There is **no padding** in `toRenderPlan`, so data touches the frame. The `Render/Export` path pads 10 % (:190-195).

### 3.4 Pixel buffers

- `Render.Bitmap {width height : Nat, pixels : Array RGB}` (Bitmap.lean:65-72). The layout is **row-major, top row first**, index `y*width + x`. Each `RGB` is a boxed 3-byte struct. No alpha.
- `toScanlines` produces the PNG raw stream `[0, r,g,b, r,g,b, …]` per row (:132-143).
- `RenderOutput.bitmap` claims "Raw RGBA pixel data" (Backend.lean:272-273), but the bitmap backend puts **filtered RGB scanlines** there (Backend/Bitmap.lean:157-161). The docstring and the data disagree.

### 3.5 Color representations: three incompatible ones

- CSS strings (Palette, PlotOpts.color, Recharts props).
- `Backend.Color` with Float RGBA in [0,1].
- `Render.RGB` with UInt8 RGB.

There are also four different default palettes:
- viridis-derived 10-color (Palette.lean:52-63),
- Tailwind 6-color (Components.lean:371),
- the Export 6-color (Render/Export.lean:36-43),
- Backend/Bitmap private 6-color (:47-54).

None of them matches Makie's default, which is the Wong palette (§6.3).

### 3.6 Compile-time vs runtime

Everything is runtime. Width, height, sample counts and dimensions are `Nat` fields. There are no type indices on sizes, no proofs, and no `FloatArray`/`ByteArray` except in PNG encoding.

---

## 4. Algorithms (exact semantics as implemented)

### 4.1 Uniform function sampling

The following functions all use the same scheme: `Graphic.sampleFn` (Graphic.lean:329-341), `SVG.sampleFn` (SVG.lean:155-167), `Components.sample` (Components.lean:33-57), `PlotSpec.line` (Specification.lean:108-122), `Rasterize.plotFunction` (:151-161) and `Render/Export.getGraphicBounds` (:106-139).

```
if n == 0 then #[] else
  step := (hi - lo) / n
  x := lo; repeat n+1 times: push (x, f x); x := x + step     -- accumulating
```

The x values accumulate, so they drift by O(n·ulp): the last sample is not exactly `hi`. Julia's `range(a,b,length=n+1)`/`LinRange` computes each point directly from `a`, `b` and `i` (`LinRange`: `(1-t)*a + t*b` style with `t = i/n`; `range`: TwicePrecision). **The oracle must match the Julia formula**, so the port should use `lo + (hi - lo) * (i / n)` or the exact LinRange lerp, not accumulation.

`Render/Export.renderGraphicToSvg` uses `lo + i*step`, which is not accumulating but differs from both (:222-226). `Interactive.sampleFunction` uses `lo + (hi-lo)*i/n` (:500-507).

Default domains are inconsistent: (0,1) for Graphic, Components, API and Interactive; (-1,1) for PlotSpec.line/area/lines; [-1,1] for autoDomain.

### 4.2 Bounds

- `Primitive.bounds` is a fold of min/max over the points (Backend.lean:176-183). It uses `Float` `min`/`max`, so NaN handling depends on argument order: NaN is dropped when it appears as the second arg of `<`-based min.
- `getGraphicBounds`:
  - fn/area: sample y-min/max over `samples+1` points (:108-139).
  - points/bars: min/max of the coordinates. An empty point set gives `none`.
  - overlay: union. facets: `none`.

### 4.3 Coordinate transform

`toPixelX(x) = round(left + (x - xMin)/(xMax - xMin) * (right - left))`. If the range is 0 the result is `left`. `toPixelY` mirrors this with `bottom - …`, and a zero range gives `bottom` (Rasterize.lean:98-113).

`floatToInt` goes through a saturating `toUInt64`: NaN becomes 0 and ±∞ becomes ±(2⁶⁴-1) (:16-20). `CoordTransform.create` uses Nat subtraction `bmp.width - margin`, which saturates to 0 when width < margin (:93-95).

### 4.4 Line rasterization

Classic Bresenham (Rasterize.lean:25-47):

```
dx=|x1-x0|, dy=|y1-y0|, sx=sign, sy=sign, err=(dx>dy ? dx : -dy)/2
loop up to dx+dy+1 times: plot (x,y) if x,y ≥ 0; stop at (x1,y1);
  e2=err; if e2 > -dx: err -= dy, x += sx; if e2 < dy: err += dx, y += sy
```

- There is **no clipping**. Out-of-canvas segments iterate their full length, and huge but finite coordinates (1e12) would loop about 10¹² times.
- There is no antialiasing.
- `drawThickLine` draws `t` parallel 1-px lines offset along y for mostly horizontal segments and along x otherwise, with offsets `-t/2 … t-1-t/2` (:50-66). It has no caps or joins.
- Polylines always use t = 2 and ignore `LineStyle.width` and `dashPattern`.

### 4.5 Primitive rendering (bitmap backend, Backend/Bitmap.lean:77-126)

- `line`: thick polyline.
- `area`: the **same polyline twice (no fill)**. The comment says "full fill would need scanline conversion" (:84-93).
- `scatter`: a filled `size×size` square centred at the rounded pixel (:95-110). `MarkerStyle.shape` is ignored.
- `bars`: `fillRect(px - bw/2, min(py,py0), bw, max(1,|py-py0|))` with `bw = max 1 (floatToNat barWidth)` in **pixels** (:112-126). `Graphic.collectPrimitives` passes `barWidth = 0.8` (Graphic.lean:498), which gives **1-pixel bars** from `#render`.
- Axes are gray lines at y=0 / x=0 if they are in range, else on the frame border (Rasterize.lean:118-133).
- There are **no ticks, tick labels, title, legend or text of any kind** (no font).

`Render/Export.renderToBitmap` (Render/Export.lean:182-205):
- pads bounds by 10 %;
- ignores `opts.color`, using a hard-coded 6-colour list (:36-44);
- samples function layers over the **whole padded x-range**, not the layer domain (:48, `plotFunction g.func t …`);
- draws area as a line;
- uses 3×3 scatter dots;
- uses bar width `plotWidth/(2n)`;
- renders facets as nothing.

### 4.6 PNG encoding (verified correct, but uncompressed)

- CRC-32 is reflected, poly `0xEDB88320`, init `0xFFFFFFFF`, final xor, table-driven. `crc32("IEND") = 0xAE426082` ✓.
- Adler-32 uses mod 65521 per byte (slow but correct). `adler32("Wikipedia") = 0x11E60398` ✓.
- zlib header is `0x78 0x01` (check: `0x7801 % 31 = 0` ✓). DEFLATE uses only **stored** blocks (BTYPE=00). Each block is `[BFINAL] [LEN lo hi] [~LEN lo hi] data`, with block length at most 65535. Adler-32 is written big-endian at the end.
- IHDR: 8-bit, colour type 2 (RGB), no interlace. One IDAT, then IEND.
- File size is exactly `8 + 25 + (12 + 2 + 5·⌈R/65535⌉ + R + 4) + 12` with `R = H·(1+3W)`. For 800×600 that is **1,440,773 bytes**, while `zlib -9` on the same raw stream gives **6,430 bytes (224× smaller)**.
- Edge case: `H = 0` gives `R = 0`, so there are zero deflate blocks and **no final block**, which is an invalid zlib stream (`splitIntoBlocks` returns `#[]`, Encode.lean:75-85).

### 4.7 SVG writers

`SVG.linePath`:
- **Panics on an empty input** (`xs[0]!`, SVG.lean:42).
- Normalises to its own min/max, so `multiLineChartSVG` **scales every series independently** (:133-135), which is wrong for overlays.
- `Float.toString` prints six decimals (`40.000000`).
- Titles and labels are **not XML-escaped**. `"a<b"` produces invalid XML, verified with `rsvg-convert` ("Couldn't find end of Start Tag").
- `grid` draws 5×5 dashed lines. There are **no tick labels**.

`Render/Export.saveSVG` (:275-300):
- has no padding;
- divides by `xMax - xMin` with no zero guard, so a flat function gives NaN/Inf coordinates;
- always draws the axes through data (0,0), even when that is off-canvas;
- draws fn layers with a **hard-coded** stroke `#3e4a89`, ignoring `opts.color` (:227);
- uses `r=3` points and area `fill-opacity=0.3`;
- has no title, legend, ticks, or facets.

### 4.8 Other algorithms

- `autoDomain` is covered in §2.1.
- `smoothMovingAverage` allocates a `List.range` array per element, O(n·w) (Transform.lean:97-105).
- `PlotComposition.normalizeYScale` computes the global y-range over all series keys, then applies `withYDomain` (:110-135).
- `Metaprogramming.fixDuplicates` turns `#["x","y","x","z","x"]` into `#["x","y","x_2","z","x_3"]` (Metaprogramming.lean:258).

### 4.9 Semantic bugs (would bite anyone relying on them)

1. `PlotSpec.overlay` **concatenates rows** instead of merging on x. It also drops the title (Specification.lean:321-329).
2. `Graphic.mergeChartData` merges layers **by row index**, not by x value. Layers with different domains or sample counts are misaligned in Recharts (Graphic.lean:414-431).
3. `#iplot` always plots **x²** regardless of the function (Interactive.lean:626-629; the literal is at :628 `let y := x * x  -- Default to x²`).
4. `GrammarOfGraphics.logX/logY` store `scaleConfig`, but `build` ignores it, so they are no-ops (:148-183).
5. `Core.Render Plot` stacks layers vertically, so the README "Advanced Composition" `line … + line …` (README.md:88-95) is not an overlay. `LinePlot` is also in the root namespace (Algebra.lean).
6. `DSL.elabPlotNamed` builds `Syntax.mkNumLit (toString lo)` from a Float (DSL.lean:182-191). A string like `"-2.000000"` is not a valid numeric literal. Likely broken for negative or non-integer domains (inferred from the code; not executed).
7. `DSL` deletes the original `#plot` elaborator (:47), and `elabPlotNew` only matches the `#plot t` shapes, so `#plot +tunable t` likely stops elaborating whenever `LeanPlot.DSL` is imported. The root `LeanPlot.lean` imports both. Inferred from the code; not executed.
8. `Series.renderLine` uses `LineType.monotone` interpolation (Series.lean:246), which draws curvature that is not in the data.
9. Bars from `#render` are 1 px wide (§4.5). PNG export ignores per-layer colours, and SVG export ignores fn colour.
10. `mergeAligned` computes domains and discards them (PlotComposition.lean:63-67).

---

## 5. Display / printing formats (exact)

- **SVG header** (SVG.lean:108): `<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">`. The background is `<rect width="100%" height="100%" fill="white"/>`.
- **SVG title** is `<text x="{W/2}" y="20" text-anchor="middle" font-size="14" font-weight="600" fill="#374151" font-family="monospace">{title}</text>`. W/2 uses Nat division.
- **Grid** (:93-102): for i=0..4, `<line x1="{m}" y1="{m+(h-m)*i/4}" x2="{w}" y2=… stroke="#e5e7eb" stroke-dasharray="3,3"/>` plus the vertical counterpart. Here `w = W-m`, `h = H-m`, and all arithmetic is Nat.
- **Frame**: `<rect x="{m}" y="{m}" width="{W-2m}" height="{H-2m}" fill="none" stroke="#d1d5db"/>`.
- **Path**: `<path d="{M/L list}" fill="none" stroke="{color}" stroke-width="2"/>`. The path data format is `M{x},{y} L{x},{y} ` with a trailing space and `Float.toString` six-decimal numbers. Golden for `[(0,0),(1,1)]` with default dims: `"M40.000000,260.000000 L360.000000,40.000000 "`.
- **Scatter**: `<circle cx=".." cy=".." r="4" fill="{color}" opacity="0.7"/>`.
- **Axis labels**: `<text x="{w/2 float}" y="{h-5}" … font-size="12" fill="#666">x</text>` and the rotated y label `transform="rotate(-90,15,{h/2})"`.
- **Legend (multi)**: `<rect x="{W-m-60}" y="{m+10+15i}" width="12" height="12" fill=".."/>` and `<text x="+16" y="+10" font-size="10" fill="#374151">{name}</text>`.
- **Export SVG** (Render/Export.lean:287-300) starts with `<?xml version="1.0" encoding="UTF-8"?>`, then `<svg xmlns=… width=… height=…>` with no viewBox. Paths use `" M {x} {y} L …"` with spaces.
- **PNG**: as in §4.6. The 1×1 red golden is 72 bytes, listed in §6.2.
- **`#render`** logs `Rendered to {outputPath} ({n} bytes)` (Plot.lean:196).
- **`#plot` caption**: `div` with style `{fontSize:"14px", fontWeight:"500", color:"#374151", marginBottom:"8px", fontFamily: ui-monospace…}` (Plot.lean:31-46).
- **Recharts JSON props**: `LineChart{width,height,data}`, `Line{type:"monotone", dataKey, stroke, dot:false}`, and a y-axis label object `{value, angle:-90, position:"left"}` (Specification.lean:448-453).
- **Warning banner**: "Plot data contains invalid values (NaN/Infinity) and may not render correctly." (Specification.lean:507).

---

## 6. Examples with expected outputs (golden candidates)

### 6.1 From README/docs (verbatim usage, no asserted outputs)

README.md:50-72:
```lean
#plot (fun x => x^2)
#plot (fun t => Float.sin t) using 400
/-- The classic parabola y = x² -/
#plot (fun x => x^2)
#html plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]
#html scatter (fun x => x^2) (steps := 50)
#html bar (fun i => i^2) (steps := 10)
```

TestExport.lean:14-38 writes `test_plot.svg`, `test_plot.png` and `test_overlay.svg`. Checked-in copies sit at the repo root.

GenDocImages.lean writes `doc/img/{plot_quadratic,plot_sincos,scatter_demo,plot_damped,plot_tanh,plot_cubic}.svg`. These are deterministic, so they can serve as SVG text goldens for the old writer.

Metaprogramming.lean:251-264 (comment-asserted outputs):
- `smartNames myTimeFunction = #["time"]`
- `smartLabels myTimeFunction = ("time","f(time)")`
- `smartNames myDuplicateFunction = #["x","y","x_2"]`
- `fixDuplicates #["x","y","x","z","x"] = #["x","y","x_2","z","x_3"]`
- `fixDuplicates #["time","time","velocity"] = #["time","time_2","velocity"]`

Palette.lean:111-112: `squares = [0,1,4,9,16,25]`, `evensSq = [0,4,16]`. No test in the repo asserts anything with `#guard`: all tests are `#eval` prints or build-only.

### 6.2 Measured goldens (Lean, v4.35.0-rc3, `leanplot-scratch/core/tmp/Goldens.lean`)

```
crc32("IEND")          = 0xae426082
adler32("Wikipedia")   = 0x11e60398
mkIEND bytes           = [0,0,0,0,73,69,78,68,174,66,96,130]
mkIHDR 2 3             = [0,0,0,13,73,72,68,82,0,0,0,2,0,0,0,3,8,2,0,0,0,54,136,73,214]
encode (1×1 red), 72 bytes =
 [137,80,78,71,13,10,26,10, 0,0,0,13,73,72,68,82,0,0,0,1,0,0,0,1,8,2,0,0,0,144,119,83,222,
  0,0,0,15,73,68,65,84, 120,1, 1,4,0,251,255, 0,255,0,0, 3,1,1,0, 141,29,229,130,
  0,0,0,0,73,69,78,68,174,66,96,130]
PNG size(800×600) = 1,440,773 ; PNG size(512×512) = 787,072 (formula §4.6)
Color.fromHex "#3e4a89" |>.map toHex = some "#3e4a89"
Color.toHex {r:=0.5,g:=1,b:=0} = "#7fff00"          -- truncation
RGB.fromHex "#3E4A89" = some {62,74,137}; RGB.fromHex "3e4a89" = none
RGB.blend white black 0.5 = {127,127,127}
SVG.sampleFn id 3 = [(0,0),(0.333333,0.333333),(0.666667,0.666667),(1,1)]
autoDomain (x*x) 100 = (-0.049893, 1.049995); autoDomain (const 3) = (2, 4)
normalize #[1,2,3] = #[0,0.5,1]; smoothMovingAverage 3 #[1,2,3,4,5] = #[1.5,2,3,4,4.5]
Scale.transform (Logarithmic 10) 1000 = 3; … (-5) = 0
colorFromNat 0 = "#b5de2b"; colorFromNat 11 = "#1f9e89"
(1e30 : Float).toUInt64 = 18446744073709551615 ; (NaN).toUInt64 = 0 ; (-3.7).toUInt8 = 0 ; (300.2).toUInt8 = 255
SVG.linePath [(0,0),(1,1)] {} = "M40.000000,260.000000 L360.000000,40.000000 "
```

`CoreTest` produces `out/plan.png`: 800×600 with a blue sine polyline, a red "cos" scatter of 5×5 squares, three green bars 10 px wide, and grey axes through the origin. It has no text. A Python zlib/CRC check passes for every chunk.

### 6.3 Makie oracle goldens (Julia 1.13 env, `leanplot-scratch/oracle/makie_oracle.json`)

These are the target behaviours for the rewritten core.

- **Wilkinson ticks** use `WilkinsonTicks(5, k_min=3)`. This is Makie's default (`makielayout/lineaxis.jl:604`, `types.jl:1999-2003`), wrapping `PlotUtils.optimize_ticks` with `extend_ticks=false, strict_span=true`, `Q=[(1,1),(5,.9),(2,.7),(2.5,.5),(3,.2)]` and weights `1/4, 1/6, 1/3, 1/4` (`makielayout/ticklocators/wilkinson.jl:16-45`).
  - (0,1) → [0, 0.5, 1]
  - (-3.14159,3.14159) → [-2, 0, 2]
  - (0,7.3) → [0, 2.5, 5]
  - (13,97) → [25, 50, 75]
  - (-0.001,0.002) → [-0.001, 0, 0.001, 0.002]
  - (1,1000) → [200, 400, 600, 800, 1000]
  - (-1,1) → [-1, -0.5, 0, 0.5, 1]
  - (0,100) → [0, 50, 100]
- **Tick labels** use `Showoff.showoff` with a Unicode minus `−` (U+2212) (`lineaxis.jl:696,759-761`).
  - [0,.2,…,1] → `0.0 0.2 0.4 0.6 0.8 1.0`
  - [-3…3] → `−3 −2 −1 0 1 2 3`
  - [0,.0005,…,.002] → `0.0000 0.0005 0.0010 0.0015 0.0020`
  - [0,250,…,1000] → `0 250 500 750 1000`
- **Default categorical palette** is Wong (`theming.jl:5`): (0,0.447,0.698), (0.902,0.624,0), (0,0.620,0.451), (0.8,0.475,0.655), (0.337,0.706,0.914), (0.835,0.369,0), (0.941,0.894,0.259). Makie cycles colours per plot type: `lines!` then `scatter!` both got wong[1] in `ref_lines.png`.
- **viridis** has 256 entries.
  - First = (0.267004, 0.004874, 0.329415). Last = (0.993248, 0.906157, 0.143936).
  - `interpolated_getindex` at t = .25 → (0.230223, 0.321297, 0.545488).
  - t = .5 → (0.128148, 0.565107, 0.550892).
  - t = .75 → (0.362859, 0.786695, 0.386589).
- **Contour levels**: `to_levels(n, (zmin,zmax))` gives `n` interior equally spaced levels, `zmin + k(zmax-zmin)/(n+1)` (`basic_recipes/contours.jl:123-127`). For n=5 on (0,1): [1/6, 1/3, 1/2, 2/3, 5/6]. **contourf** band edges use `range(Float32(mi), nextfloat(Float32(ma)), length=levels+1)` (`contourf.jl:53`). For 10 on (0,1): [0, 0.1000000089, …, 1.0000001192], in Float32.
- **streamplot** (`basic_recipes/streamplot.jl:134-215`) is **deterministic**. It seeds with the R_N low-discrepancy sequence, φ = golden ratio for N=2, cell index `ceil(((0.5 + φ^{-i}·ind) % 1)·res_i)`.
  - It takes fixed-length Euler steps `x += d·stepsize·v/|v|` in both directions `d=±1`, uses per-cell visit masks, and inserts NaN separators between lines. All arithmetic is **Float32**.
  - Golden for `f(p)=(-y,x)` on [-1,1]², gridsize (8,8), stepsize .05, maxsteps 40: 13 arrows, 338 line points.
  - First arrows: (-0.125,-0.125), (-0.375,-0.875), (0.125,0.625), …
  - First direction: (0.70710677, -0.70710677).
  - First line points: NaN, (-0.125,-0.125), (-0.16035534, -0.08964466), (-0.18475352, -0.04600146), …
- Other Makie defaults: linewidth 1.5, markersize 9 (`theming.jl:49-55`), autolimit margin 5 % per side (`types.jl:529-531`), CairoMakie `px_per_unit = 2` (`CairoMakie/src/screen.jl:93`). Axis3 defaults are azimuth 1.275π and elevation π/8 (`types.jl:1775-1782`).

---

## 7. Dependencies

### 7.1 External packages used by LeanPlot

| Dep | Pin (main / algebra) | Used by | Symbols used |
|---|---|---|---|
| ProofWidgets4 | `v0.0.104` / unpinned (`627a97…` in manifest) | Series, Specification, Components, Graphic, Plot, DSL, Interactive, TunablePlot, Axis, Axes, Legend, Core, Debug, Faceting, PlotComposition, GrammarOfGraphics, LegacyLayer, WarningBanner, Recharts | `Html`, `Html.element/text`, `Component`, `HtmlEval`, `HtmlDisplayPanel`, `HtmlCommand.evalCommandMHtml`, `Widget.savePanelWidgetInfo`, `Server.rpcEncode`, `MessageData.ofComponent`, `Recharts.{LineChart,Line,LineType,Recharts.javascript}`, `Jsx` (`open scoped ProofWidgets.Jsx`), `@[widget_module]` |
| Verso | `v4.32.0-rc1` / `main` | only `doc/` (`Manual` lib, `leanplot-docs` exe) | `VersoManual`, `manualMain`, `Verso.Genre.Manual.*` |
| (transitive via Verso) | | | subverso, MD4Lean (C code), plausible, illuminate, Cli (the sibling checkout sizes are 32 MB verso, 9 MB illuminate, 21 MB proofwidgets) |

ProofWidgets ships prebuilt JS in `widget/js/`, so **npm is not required when consuming it**, even though README.md:44 says it is. Its lakefile only runs npm when the TypeScript sources change (proofwidgets `lakefile.lean:54-79`). ProofWidgets `v0.0.114` is the tag bumped for `v4.35.0-rc3` (commit `c643bbb`).

Because Lake fetches **all** `[[require]]`s of a package, any downstream `require LeanPlot` currently pulls ProofWidgets **and** Verso with its whole tree, even if it imports only `LeanPlot.Render.PNG.Encode`. That is the root cause of Grassmann disabling the dependency.

`leanOptions` also sets `doc.verso = true` (lakefile.toml:14). This is a **core** Lean option enabling Verso-syntax docstrings, not the Verso package. Under v4.3x it emits hundreds of "Code element could be more specific" warnings, and it turns header nesting and `{name}` role resolution into errors.

### 7.2 Chakravala packages

LeanPlot depends on none.

### 7.3 What the chakravala ecosystem needs from a plotting backend

This comes from the Julia Makie extensions and is the target feature set.

| Julia source | Makie calls needed |
|---|---|
| `Grassmann.jl/ext/MakieExt.jl:19-28` | `lines`/`lines!` of `Vector{Chain}` → Point2/3; `arrows(points, vectors)` |
| `Cartan.jl/ext/MakieExt.jl` (893 lines) | `lines` (2D/3D, `colormap`-coloured by scalar, `:grays`, dashed), `linesegments`, `scatter`, `arrows`/`arrows2d`/`arrows3d` (+ `lengthscale`, `argarrows`), `streamplot` (2D & 3D fields; :116-138, :412-436), `mesh` (triangle mesh with per-vertex scalar colour; :85-129), `wireframe`, `surface`, `contour`/`contourf`/`contour3d` (:139-158), `volume` (3D scalar), `poly`, `text`, `Legend`, `Colorbar`, `Axis`/`Axis3`/`LScene` |
| `Fatou.jl/ext/MakieExt.jl:6-48` | `heatmap`/`contour`/`contourf` of a Fatou/Julia set iteration matrix (transposed/reversed conventions :24, :36), `Colorbar`, named ColorSchemes colormaps (`K.meta.cmap`), `surface`, `arrows(x,y,u,v)` quiver; `orbit` cobweb plot with `lines!`, dashed/dotted styles, `scatter!(marker=:x)`, `Legend`, `xlims!/ylims!` (:50-78) |
| `Adapode.jl/ext/MakieExt.jl:19-80` | many `lines`/`lines!` of flow integrals (multi-curve overlay), `graylines` |
| `FlowGeometry.jl/ext/MakieExt.jl:19-42` | `lines` of airfoil profiles (complex → 2D) |

A sibling probe (`scratchpad/probe_makie.out`) shows the default outputs of the Cartan recipes. For example, `lines RealFunction` yields a `lines` plot with `color=Array(63)` and `cmap=viridis`, i.e. a **per-vertex colour mapped through viridis by default**.

---

## 8. Porting / restructuring notes

### 8.1 Capability vs gap matrix (headless SVG/PNG unless noted)

| Feature | Today | Gap to close |
|---|---|---|
| Line y=f(x) | ✓ PNG (aliased, 2 px, no labels), ✓ SVG (`SVG.lineChartSVG`; Export path via Graphic needs ProofWidgets); HTML Recharts with monotone smoothing | antialiased stroke with width/caps/joins/dash; per-vertex colour via colormap (Cartan default); NaN-break segments (Makie convention) |
| Parametric curves (x(t),y(t)), 3D curves | only at `RenderPlan`/`SVG` level (arbitrary point arrays); Graphic has no parametric constructor; Recharts cannot (sorts by x + monotone) | first-class `lines(xs, ys)` / `lines(pts3)`; adaptive sampling optional |
| Scatter | ✓ square dots (PNG), circles (SVG); no shapes/sizes/per-point colour | marker set (circle, rect, diamond, x, +, triangle), per-point size/colour, stroke |
| Arrows / quiver | ✗ | `arrows2d(x,y,u,v; lengthscale, align ∈ {tail,center,tip}, normalize)` per Makie `arrows.jl:57-119`; filled-triangle heads needs polygon fill |
| Streamlines | ✗ | port `streamplot_impl` exactly (Float32), plus arrowheads at seeds |
| Heatmap / raster / fractals | ✗ (only manual `Bitmap.setPixel`, as Grassmann's old `mandelbrotBitmap` did) | `image`/`heatmap` mark: `Grid2 nx ny` of Float + colormap + clims, cell-edge semantics like Makie; SVG embeds `<image href="data:image/png;base64,…">`; raster blits nearest-neighbour |
| Contours | ✗ | marching squares (+ saddle disambiguation) for isolines; isobands for contourf; levels per `to_levels`/`_get_isoband_levels` |
| Triangle meshes | ✗ (no polygon fill at all) | scanline triangle fill with per-vertex colour (Gouraud or flat), painter's sort for 3D, wireframe |
| 3D projection | ✗ | Axis3 camera (azimuth/elevation/perspectiveness), orthographic + perspective, depth sort, 3D axes box |
| Colormaps | ✗ (categorical palette only; four inconsistent palettes) | viridis/magma/inferno/plasma/cividis/grays/RdBu/… as 256-entry LUTs (dump from ColorSchemes via oracle), `interpolated_getindex`, Colorbar |
| Legends / axes / ticks | HTML ✓ (Recharts); SVG: grid + axis names, no tick labels; PNG: axis lines only, **no text** | Wilkinson ticks + Showoff labels; axis frame, spines, grid; legends; titles; text in PNG needs an embedded font |
| Subplots / facets | HTML only (CSS grid) | Figure grid layout with shared axes, colorbar columns, gaps (Makie `rowgap=colgap=18`, `figure_padding=16`) |
| Animation frames | ✗ | `Figure → frame n` API writing `frame_0001.png…` (optionally APNG); deterministic |

### 8.2 Recommended architecture: light core, heavy optionals

Target the `v4.35.0-rc3` toolchain, the same one Grassmann will use. Use **one git repo and three Lake packages**, so that downstream fetches zero transitive dependencies:

```
leanplot/                          (git root)
├─ lakefile.toml    package "LeanPlot"            -- NO [[require]] at all
│   [[lean_lib]] LeanPlot   (globs = LeanPlot.+ ; no Demos/Tests in the lib glob)
│   [[lean_lib]] LeanPlotTest ; [[lean_exe]] leanplot-golden (@[test_driver])
│   [[lean_exe]] leanplot  (CLI: render a .json scene or run examples)
├─ widgets/lakefile.toml   package "LeanPlotWidgets"
│   require LeanPlot from ".." ; require proofwidgets @ "v0.0.114"
│   -- #plot/#figure commands: show the SAME SVG string in the infoview via
│   -- Html.element "img" #[("src", "data:image/svg+xml;base64,…")] (WYSIWYG with CI)
│   -- optional: tunable sliders rewritten against Figure params (old TunablePlot JS)
└─ docs/lakefile.toml      package "LeanPlotDocs"
    require LeanPlotWidgets from "../widgets" ; require verso @ <tag for toolchain>
```

Downstream Grassmann uses `[[require]] name = "LeanPlot" git = "https://github.com/alok/LeanPlot" rev = "<sha>"`. That gives no ProofWidgets and no Verso. It can build and run `lake exe` examples in CI on Linux without node. Infoview users additionally require `LeanPlotWidgets` with `subDir = "widgets"`.

Rules for the core package:
- No global notations. Operators such as `+` (overlay) and `|||`-like layout are `scoped` inside `namespace LeanPlot`.
- No `_root_` definitions, no `Coe String Json`, no priority-2000 generic `HAdd`/`HMul`, no top-level `#eval`/`#html` in library modules.
- No `doc.verso` (or fix all roles).
- Demos and tests live outside the library glob.
- Drop Recharts entirely. There is one renderer, so infoview output equals file output.

**Pipeline (pure until the final write):**

```
user API (recipes) ──► Figure (layout tree of Axis2/Axis3/Colorbar/Legend/Text)
   ──► layout solve (sizes, tick computation, autolimits, text metrics) ──► Scene (device-space DrawOps)
   ──► backends: SVG writer (String) │ Raster (RGBA8 canvas → PNG bytes) │ (widget: SVG → img)
```

**IR sketch (to implement):**

```lean
-- data (SoA, unboxed)
structure Pts2 where xs ys : FloatArray; h : xs.size = ys.size
structure Grid2 (nx ny : Nat) where z : FloatArray; h : z.size = nx * ny   -- z[i + nx*j], i ↔ x (Julia column-major)
structure TriMesh where pos : FloatArray /- 3n -/; tri : ByteArray /- or Array UInt32, 3m -/; valid : ∀ …   -- indices < n (checked once)
inductive ColorSpec | solid (c : RGBA) | byValue (v : FloatArray) (cmap : Colormap) (clim : Option (Float×Float))
inductive Mark
  | lines (p : Pts2 ⊕ Pts3) (color : ColorSpec) (width : Float) (style : LineStyle)
  | segments …  | scatter (p) (marker : Marker) (size : Float) (color : ColorSpec)
  | arrows (origin dir : Pts2 ⊕ Pts3) (style : ArrowStyle)
  | heatmap {nx ny} (x : Edges nx) (y : Edges ny) (z : Grid2 nx ny) (cmap) (clim)
  | contour {nx ny} … (levels : Levels) | contourf …
  | mesh (m : TriMesh) (color : ColorSpec) | surface {nx ny} … | wireframe …
  | poly (outline : Pts2) (fill stroke) | text (pos) (s : String) (align) (size)
structure Axis2 where marks : Array Mark; xscale yscale : Scale; limits : Option Rect; title xlabel ylabel : String; …
structure Axis3 where marks : Array Mark; azimuth := 1.275*π; elevation := π/8; perspectiveness := 0; …
inductive Block | axis2 (a : Axis2) | axis3 (a : Axis3) | colorbar … | legend … | label …
structure Figure where size : Nat × Nat := (600, 450); cells : Array (GridPos × Block)
inductive DrawOp  -- device space, backend-agnostic
  | path (d : Path) (stroke : Option Stroke) (fill : Option Fill) (clip : Option Rect)
  | image (w h : Nat) (rgba : ByteArray) (dst : Rect) | text (pos) (s) (font) (anchor) (rot)
```

### 8.3 Dependent types: where they are free, and where to stop

The following cost no runtime:
- **Buffer-size invariants as proof fields.** `Canvas (w h : Nat)` with `data : ByteArray` and `h : data.size = 4*w*h`; `Grid2 nx ny`; `Pts2` with `xs.size = ys.size`. The proofs are erased. Inner loops can then use `data[i]'proof`/`uget` without bounds checks. The index arithmetic `i = 4*(y*w + x) + c < 4*w*h` from `x < w`, `y < h`, `c < 4` is one `omega`/`Nat.lt_of…` lemma, proven once and reused.
- **Sizes as structure parameters** (`nx ny`, `w h`). These are `Nat` arguments passed at runtime, a negligible cost. They prevent mixing grids of different shapes in contour/heatmap/surface code.
- **`Fin`-indexed tick/level arrays and layout cells** (`Fin rows × Fin cols`).
- **Mesh validity**: face indices `< nverts`, checked once in a smart constructor (`TriMesh.mk? : … → Option TriMesh`), then carried as a proof.
- **Colormap non-emptiness** (`lut.size ≥ 2`) makes `interpolate` total.
- **PNG layout theorems**: `scanlines.size = h*(1+bpp*w)`, stored-block `len ≤ 65535` (build blocks from `Fin 65536`), and CRC table entries equal to the spec (`native_decide` over 256 entries). These are cheap proofs that pin the format and have caught real bugs, e.g. the H = 0 invalid stream in §4.6.

**Do not** index types by Float values (domains, limits) or by colour. Keep marks as a runtime inductive: type-level heterogeneity, as in `SeriesDSpecPacked`, bought nothing and complicated every call site. Scales and ticks on `Float` are not provable. Test them with the oracle and Plausible-style properties instead (ticks sorted, inside `[vmin,vmax]` when `strict_span`, count within `[k_min,k_max]`).

### 8.4 Performance: measured and prescribed

Measurements come from `CoreTest` on Apple Silicon, v4.35.0-rc3, forced via `IO.lazyPure`. With plain `pure (expr)`, the compiler hoists closed terms and the timings read about 0.

| Workload | Time |
|---|---|
| `renderToPNG` 800×600 (line + scatter + bars) | 11.2 ms. 1600×1200: 43.9 ms |
| `renderSIMDToPNG` (the "SIMD" stub) | 10.7 ms, no gain |
| Mandelbrot 512², 64 iterations, `for … do if … then break` with `let mut` Floats in `Id.run` | **446 ms**. The same loop writing a flat `ByteArray` is also 448 ms, so the pixel store is *not* the bottleneck |
| Same Mandelbrot with a **tail-recursive `escape` (Float args, Nat fuel)** | **54 ms (8.2× faster)**. `for`/`break` with mutable Floats boxes every Float through the `ForInStep` closure |
| 262,144 `Bitmap.setPixel` (Array RGB, boxed) | 1.6 ms. Allocation is cheap when unique, but memory is about 32 B/pixel versus 4 B/pixel for RGBA8 |
| `PNG.encode` 512² | 6.1 ms. 1920×1080 `create + toScanlines`: 10.4 ms |

Rules for the port:
- Hot loops over Floats (fractals, streamplot integration, marching squares, rasterization) are written as tail-recursive functions or `Nat.fold` with explicit Float accumulators, never `for … break` with `let mut` Floats.
- Use `FloatArray` / `ByteArray` SoA throughout. Never use `Array (Float × Float)`, which costs 3 allocations per point, or `Array RGB`.
- Keep buffers unique: thread `Canvas` linearly and never keep an old alias, or `set!` copies the whole buffer.
- Use `@[specialize]` on higher-order samplers such as `sample (f : Float → Float)` so the closure call is inlined per call site.
- PNG needs **real DEFLATE**. At minimum: fixed-Huffman with LZ77 hash-chain matching, or dynamic Huffman. Add the Sub/Up/Paeth filter heuristic (choose the minimum sum of absolute differences per row) and Adler-32 with deferred modulo (every 5552 bytes). Expect about 200× smaller files, which is mandatory for committing goldens.
- Output-size determinism: use a fixed-precision formatter for SVG numbers (e.g. 2–3 decimals, trailing zeros trimmed, `-0` normalized). `Float.toString` prints six decimals, loses small values (`1e-7 → 0.000000`) and prints giant strings for 1e300.
- Julia is fast here because of `@generated` + StaticArrays point types (unboxed `Point2f`), in-place `push!` into typed vectors, and Cairo in C. Lean reaches comparable speed only with unboxed scalar structs (structures of `Float` fields are stored unboxed in ctor objects, but *arrays of them are arrays of pointers*), hence SoA.

### 8.5 Julia/JS-specific material to drop

- Recharts and all JSON-row data models (`PlotSpec.chartData`, `jsonHasKeys`, `#assert_keys`, `WarningBanner`).
- Inline React panels (TunablePlot, Interactive), which can be rebuilt later in `widgets/` against `Figure` parameters.
- html2canvas via CDN (Debug.lean).
- GPU/SIMD stubs.
- `Metaprogramming` label heuristics (nice to have; not core).
- `LegacyLayer`, and the four duplicate palettes.
- Makie's Observables/ComputePipeline reactivity, which is Julia-specific. Model "animation" as a pure function `Nat → Figure`.
- Makie's `convert_arguments` trait machinery: replace with typeclasses `ToPts2 α`, `ToGrid2 α`, and so on. Grassmann `Chain` types implement them in the downstream package, not in LeanPlot.

### 8.6 Suggested module decomposition (new core) with rough LOC

| Module | Content | LOC |
|---|---|---|
| `LeanPlot/Core/Num.lean` | lerp, clamp, `linRange` (exact Julia LinRange formula), NaN utilities, fixed-precision float formatter, Showoff-compatible labels with U+2212 | 250 |
| `LeanPlot/Core/Color.lean` | RGBA8/RGBAf, hex parse/print (rounding), named colours, Wong palette, src-over blend | 200 |
| `LeanPlot/Core/Colormap.lean` + `Colormaps/Data.lean` | LUT type (non-empty proof), `interpolatedGetIndex`, clim normalization; viridis/magma/inferno/plasma/cividis/grays/RdBu/turbo tables (generated from ColorSchemes via oracle) | 200 + ~2100 data lines |
| `LeanPlot/Core/Scale.lean` | identity/log10/log2/ln/sqrt/pseudolog/symlog with domain checks (NaN outside, not 0) | 150 |
| `LeanPlot/Core/Ticks.lean` | port of `PlotUtils.optimize_ticks` (Wilkinson extended) + log ticks + minor ticks | 350 |
| `LeanPlot/Core/Geometry.lean` | Vec2/Vec3/Mat4, Rect, Liang–Barsky clipping, Axis3 camera (azimuth/elevation/perspectiveness), projection | 350 |
| `LeanPlot/Core/Data.lean` | `Pts2/Pts3/Grid2/TriMesh` SoA with size proofs; conversions (`ToPts2` class) | 250 |
| `LeanPlot/Scene/Mark.lean`, `Scene/Style.lean` | Mark inductive, Stroke/Fill/Marker/ArrowStyle/TextStyle | 300 |
| `LeanPlot/Figure/{Figure,Layout,Axis2,Axis3,Legend,Colorbar}.lean` | blocks, grid layout, autolimits (5 % margins), ticks → DrawOps, text metrics (monospace approximation, or glyph advance table from the embedded font) | 900 |
| `LeanPlot/Recipes/{Lines,Scatter,Arrows,Streamplot,Heatmap,Contour,Contourf,Mesh,Surface,Wireframe,Poly,Text}.lean` | recipes → primitive marks; exact Makie algorithms where oracle-testable | 1700 |
| `LeanPlot/Backend/SVG.lean` | writer: XML escaping, clipPath per axis, `<image>` for heatmaps (PNG+base64), deterministic number formatting | 400 |
| `LeanPlot/Backend/Raster/{Canvas,Path,Stroke,Text,Image}.lean` | RGBA8 canvas (size proof), nonzero/even-odd scanline fill with analytic coverage AA (accumulation buffer), stroker (butt/round caps, miter/round joins, dashes), embedded bitmap or Hershey font, image blit | 1300 + font data |
| `LeanPlot/Backend/PNG.lean` | CRC32, Adler32, filters, DEFLATE (LZ77 + fixed/dynamic Huffman), RGBA/RGB/Gray, base64 | 650 |
| `LeanPlot/IO.lean` | `Figure.save : FilePath → IO Unit` dispatching on `.svg/.png`, frame sequences | 80 |
| `LeanPlotTest/*` | golden runner, image diff (max-abs/PSNR), oracle JSON loaders | 400 |
| `widgets/LeanPlotWidgets/*` | `#figure` command, SVG-in-infoview, optional sliders | 300 |
| **Total (excluding generated tables/font data)** | | **≈ 7,800** |

### 8.7 Migration from the existing branches

Harvest from the old code:
- `Render/PNG/CRC32`, `Adler32`, and the container parts of `Encode` (verified correct).
- The Bresenham routine as the fallback for 1-px hairlines.
- `Backend.RenderPlan`'s idea of a backend-agnostic IR. Replace its content.
- The `#render` idea as an **executable** (not an elaboration-time side effect, which writes files during `lake build` and is non-hermetic).
- The SVG header/grid styling only as a reference.

Do not carry over the Recharts stack, `Graphic` as a struct, `PlotSpec`, or any global notation.

For v4.35, the old core needs only:
- `String.trim` → `trimAscii.toString` (Render/Bitmap.lean:35);
- the ToFloat header level (algebra branch, :50);
- `String.mk` → `String.ofList` (Backend.lean:68, a warning);
- LegacyLayer's forward `@[deprecated]` reference.

A sibling agent is already merging `algebra-tunable-core` into `main` and bumping to v4.35.0-rc3 + ProofWidgets v0.0.114 (`scratchpad/leanplot-up`). Its build failed exactly on `LeanPlot.Render.Bitmap` and `LeanPlot.LegacyLayer`, which matches the above.

### 8.8 CI recipe (Linux, no node, no VS Code)

1. `lake build` of the core package (zero dependencies; roughly 1–2 minutes cold).
2. `lake exe leanplot-golden` renders every example figure to `out/*.svg` and `out/*.png`. It then:
   - byte-compares SVGs to committed goldens (deterministic writer);
   - decodes the PNGs with the Lean decoder, or `python -c zlib`, and compares pixels with tolerance (max-abs ≤ 2 per channel, ≤ 0.1 % differing pixels, to absorb AA changes);
   - checks JSON numeric goldens (ticks, levels, streamlines) against the oracle dumps with rel-tol 1e-6. Use 1e-5 for Float32-derived values.
3. Optional job: render our SVG with `resvg` and compute SSIM against the CairoMakie reference PNGs (§9). Treat this as a warning, not a failure.
4. Upload `out/` as an artifact. In PRs, post a contact sheet.

---

## 9. Oracle test plan (Julia 1.13 env with CairoMakie; dump JSON + reference PNG/SVG)

The harness is `julia --startup-file=no --project=<juliaenv> oracle/leanplot_oracle.jl`, extending `leanplot-scratch/oracle/makie_oracle.jl`. Emit NaN as the string `"NaN"`, because JSON.jl refuses NaN (observed). For point data, emit flat arrays in Julia's memory order.

| # | Function (Julia) | Lean target | Inputs | Compare |
|---|---|---|---|---|
| 1 | `Makie.get_tickvalues(WilkinsonTicks(5,k_min=3), a, b)` | `Ticks.wilkinson` | 2,000 ranges: a ~ U(-1e6,1e6) × 10^U(-8,8) spans, including tiny spans (1e-12), huge (1e300), negative, symmetric, integer endpoints, `a==b` (fallback path), inf/nan (expect error/fallback) | exact equality after `round(sigdigits=12)` |
| 2 | `Makie.get_ticklabels(automatic, ticks)` (Showoff + U+2212) | `Num.showoff` | the tick sets from #1 | exact string |
| 3 | `LogTicks(WilkinsonTicks(5,k_min=3))` on log10 axes | `Ticks.log` | ranges 10^U(-10,10) | exact |
| 4 | `to_colormap(name)` full LUTs for `:viridis,:magma,:inferno,:plasma,:cividis,:grays,:RdBu,:turbo,:twilight` + `interpolated_getindex(cm, t)` | `Colormap.*` | 256 entries each; t ∈ 1,001 uniform points + random + {0,1,NaN,-0.1,1.1} | abs ≤ 1e-6 (Float32 source) |
| 5 | `Makie.wong_colors()`; the palette cycle (`lines!`/`scatter!` per-type counters) | `Color.wong`, cycle rules | – | exact |
| 6 | `range(a,b,length=n)` and `LinRange(a,b,n)` elements | `Num.linRange` | n ∈ {2..1000}, random a,b | bitwise equality |
| 7 | autolimits: `Makie.reset_limits!` → `ax.finallimits[]` for lines/scatter data | `Figure.autolimits` | random point clouds, flat data, single point | rel 1e-6 |
| 8 | `Makie.to_levels(n, (lo,hi))`, `Makie._get_isoband_levels(n, lo, hi)` | `Contour.levels` | n ∈ 1..20, random ranges | exact (Float32 for contourf) |
| 9 | Isolines: `Contour.contours(x, y, z, levels)` → `Contour.lines` coordinates | `Contour.isolines` | analytic fields (x²+y², sin x cos y, saddle `xy`, a Fatou iteration matrix) on 7×9, 64×64 grids | as **sets of polylines** (orientation- and start-point-insensitive), Hausdorff ≤ 1e-9 |
| 10 | Isobands for contourf: `Isoband.isobands` polygons | `Contour.isobands` | same fields | per-band total area, rel 1e-9, + vertex sets |
| 11 | `Makie.streamplot_impl(Point2f, f, Rect, (nx,ny), stepsize, maxsteps, density)` full output (arrow_pos, arrow_dir, line_points incl. NaN breaks, colors = norm) | `Recipes.Streamplot` (Float32 arithmetic) | fields: rotation, saddle (x,-y), source (x,y), Cartan-style dipole; gridsize (8,8), (32,32); density 0.5/1/2; 3D variant (Point3f, gridsize (8,8,8)) | exact counts; points abs ≤ 1e-5 |
| 12 | `arrows2d`/`arrows3d` geometry: `Makie._process_arrow_arguments(pos, dir, align, lengthscale, normalize, argmode)` (arrows.jl:71-90) and the resulting shaft/tip polygons in data space | `Recipes.Arrows` | align ∈ {:tail,:center,:tip,0.3}, lengthscale ∈ {0.5,1,2}, normalize ∈ {true,false} | abs ≤ 1e-12 |
| 13 | Axis3 camera: `Makie.Axis3` → projection·view matrix for (azimuth, elevation, perspectiveness, aspect) | `Geometry.axis3Camera` | the defaults (1.275π, π/8, 0) and a grid of angles | matrix entries abs ≤ 1e-6; project 100 random points and compare |
| 14 | Heatmap cell edges: `Makie.edges(xs)` / the heatmap `x,y` conversion for centres vs edges, regular and irregular grids | `Recipes.Heatmap.edges` | length 1, 2, n; irregular spacing | exact |
| 15 | Colour mapping of matrices: `Makie.numbers_to_colors(z, cmap, identity, clim, lowclip, highclip, nan_color)` | `Colormap.mapGrid` | Fatou.jl `FilledSet.iter` matrices (from the Fatou oracle agent) + random with NaN | RGBA abs ≤ 1/255 |
| 16 | Mesh normals and per-vertex colour for `mesh(verts, faces, color=scalar)` | `Recipes.Mesh` | icosphere, a Cartan `SimplexTopology` mesh from the MeshTopology oracle | normals abs ≤ 1e-6 |
| 17 | Reference renders: CairoMakie `save(png, px_per_unit=1)` for ~40 canonical figures (lines, scatter, arrows, streamplot, heatmap(viridis), contour, contourf, mesh 2D/3D, surface, wireframe, facets, legend, colorbar, log axes) | our SVG → resvg PNG, and our raster PNG | same figure specs from a shared JSON "figure spec" file read by both Julia and Lean | SSIM ≥ 0.85 (layout/font differ; this catches gross errors), plus exact numeric checks from rows 1–16 |
| 18 | PNG codec round-trip | `PNG.encode` → `zlib.decompress`/`FileIO.load` | random RGBA canvases 1×1…997×613, H=0 and W=0 edges | exact pixels; CRCs valid |

Input distributions should always include:
- degenerate spans (`a == b`);
- ±0, NaN and ±Inf in data, which should be dropped from bounds, break lines Makie-style, and map to `nan_color` in heatmaps;
- single-point series;
- non-uniform grids;
- the exact figure specs used by the downstream Grassmann/Cartan/Fatou examples. Dump those from `Cartan/docs/src/plot.md` and the `probe_makie.jl` recipe probes.

---

## Appendix A: complete public declaration index (auto-generated; Demos/ and Test/ excluded; private decls omitted)

Namespace tracking is heuristic: it follows `namespace X … end X` blocks, and root-level declarations show an empty namespace. Line numbers refer to the `algebra-tunable-core` working tree.

#### `LeanPlot/API.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 31 | def | `LeanPlot.API` | `xyArrayToJson (pts : Array (Float × Float)) : Array Json :=` |
| 37 | def | `LeanPlot.API` | `mkLineChart (data : Array Json)` |
| 52 | def | `LeanPlot.API` | `lineChart {β} [ToFloat β]` |
| 73 | def | `LeanPlot.API` | `scatterChart (pts : Array (Float × Float))` |
| 99 | def | `LeanPlot.API` | `plot {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)` |
| 113 | def | `LeanPlot.API` | `plotMany {β} [ToFloat β] (fns : Array (String × (Float → β)))` |
| 125 | def | `LeanPlot.API` | `scatter {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)` |
| 137 | def | `LeanPlot.API` | `bar {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)` |

#### `LeanPlot/Algebra.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 37 | structure | `` | `LineSeries where` |
| 43 | structure | `` | `LinePlot where` |
| 56 | def | `LinePlot` | `overlay (p q : LinePlot) : LinePlot :=` |
| 64 | def | `LinePlot` | `toHtml (p : LinePlot)` |
| 80 | def | `` | `line (name : String) (f : Float → Float)` |
| 86 | instance | `` | `: Render LinePlot where` |
| 91 | instance | `` | `: ToLayer LinePlot where` |

#### `LeanPlot/AutoDomain.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 28 | def | `LeanPlot.AutoDomain` | `autoDomain {β} [ToFloat β] (f : Float → β) (N : Nat := 100) : Float × Float :=` |

#### `LeanPlot/Axes.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 18 | structure | `LeanPlot.Axis` | `AxisProps where` |
| 38 | def | `LeanPlot.Axis` | `XAxis : ProofWidgets.Component AxisProps where` |
| 43 | def | `LeanPlot.Axis` | `YAxis : ProofWidgets.Component AxisProps where` |

#### `LeanPlot/Axis.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 18 | structure | `LeanPlot.Axis` | `AxisProps where` |
| 40 | def | `LeanPlot.Axis` | `XAxis : ProofWidgets.Component AxisProps where` |
| 45 | def | `LeanPlot.Axis` | `YAxis : ProofWidgets.Component AxisProps where` |

#### `LeanPlot/Backend.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 25 | structure | `LeanPlot.Backend` | `Color where` |
| 34 | def | `LeanPlot.Backend.Color` | `black : Color := { r := 0, g := 0, b := 0 }` |
| 35 | def | `LeanPlot.Backend.Color` | `white : Color := { r := 1, g := 1, b := 1 }` |
| 36 | def | `LeanPlot.Backend.Color` | `red : Color := { r := 1, g := 0, b := 0 }` |
| 37 | def | `LeanPlot.Backend.Color` | `green : Color := { r := 0, g := 1, b := 0 }` |
| 38 | def | `LeanPlot.Backend.Color` | `blue : Color := { r := 0, g := 0, b := 1 }` |
| 56 | def | `LeanPlot.Backend.Color` | `fromHex (input : String) : Option Color := do` |
| 65 | def | `LeanPlot.Backend.Color` | `toHex (c : Color) : String :=` |
| 75 | structure | `LeanPlot.Backend` | `Point where` |
| 81 | structure | `LeanPlot.Backend` | `Rect where` |
| 90 | def | `LeanPlot.Backend.Rect` | `width (r : Rect) : Float := r.xMax - r.xMin` |
| 91 | def | `LeanPlot.Backend.Rect` | `height (r : Rect) : Float := r.yMax - r.yMin` |
| 92 | def | `LeanPlot.Backend.Rect` | `center (r : Rect) : Point := { x := (r.xMin + r.xMax) / 2, y := (r.yMin + r.yMax) / 2 }` |
| 95 | def | `LeanPlot.Backend.Rect` | `fromSize (w h : Float) : Rect :=` |
| 99 | def | `LeanPlot.Backend.Rect` | `expand (r : Rect) (p : Point) : Rect :=` |
| 104 | def | `LeanPlot.Backend.Rect` | `union (r1 r2 : Rect) : Rect :=` |
| 113 | structure | `LeanPlot.Backend` | `LineStyle where` |
| 123 | def | `LeanPlot.Backend` | `LineStyle.default : LineStyle := {}` |
| 126 | structure | `LeanPlot.Backend` | `FillStyle where` |
| 134 | def | `LeanPlot.Backend` | `FillStyle.default : FillStyle := {}` |
| 137 | structure | `LeanPlot.Backend` | `MarkerStyle where` |
| 147 | def | `LeanPlot.Backend` | `MarkerStyle.default : MarkerStyle := {}` |
| 150 | inductive | `LeanPlot.Backend` | `Primitive where` |
| 163 | def | `LeanPlot.Backend.Primitive` | `name : Primitive → String` |
| 169 | def | `LeanPlot.Backend.Primitive` | `points : Primitive → Array Point` |
| 176 | def | `LeanPlot.Backend.Primitive` | `bounds (p : Primitive) : Rect :=` |
| 190 | structure | `LeanPlot.Backend` | `AxisConfig where` |
| 206 | structure | `LeanPlot.Backend` | `GlobalStyle where` |
| 227 | structure | `LeanPlot.Backend` | `RenderPlan where` |
| 245 | def | `LeanPlot.Backend.RenderPlan` | `empty (width height : Nat) : RenderPlan :=` |
| 251 | def | `LeanPlot.Backend.RenderPlan` | `addPrimitive (plan : RenderPlan) (prim : Primitive) : RenderPlan :=` |
| 261 | def | `LeanPlot.Backend.RenderPlan` | `legendEntries (plan : RenderPlan) : Array String :=` |
| 269 | inductive | `LeanPlot.Backend` | `RenderOutput where` |
| 281 | class | `LeanPlot.Backend` | `RenderBackend (B : Type) where` |
| 294 | structure | `LeanPlot.Backend` | `CoordTransform where` |
| 304 | def | `LeanPlot.Backend.CoordTransform` | `fromPlan (plan : RenderPlan) : CoordTransform :=` |
| 315 | def | `LeanPlot.Backend.CoordTransform` | `toPixelX (t : CoordTransform) (x : Float) : Float :=` |
| 323 | def | `LeanPlot.Backend.CoordTransform` | `toPixelY (t : CoordTransform) (y : Float) : Float :=` |
| 332 | def | `LeanPlot.Backend.CoordTransform` | `toPixel (t : CoordTransform) (p : Point) : Point :=` |

#### `LeanPlot/Backend/Bitmap.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 31 | structure | `LeanPlot.Backend` | `BitmapBackend where` |
| 39 | def | `LeanPlot.Backend` | `colorToRGB (c : Color) : RGB :=` |
| 129 | def | `LeanPlot.Backend` | `renderPlanToBitmap (plan : RenderPlan) : Render.Bitmap := Id.run do` |
| 152 | instance | `LeanPlot.Backend` | `: RenderBackend BitmapBackend where` |
| 168 | def | `LeanPlot.Backend` | `renderToPNG (plan : RenderPlan) : ByteArray :=` |
| 173 | def | `LeanPlot.Backend` | `savePlanPNG (path : System.FilePath) (plan : RenderPlan) : IO Unit := do` |

#### `LeanPlot/Backend/GPU/Interface.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 37 | opaque | `LeanPlot.Backend.GPU` | `GPUContextImpl : NonemptyType` |
| 39 | def | `LeanPlot.Backend.GPU` | `GPUContext : Type := GPUContextImpl.type` |
| 40 | instance | `LeanPlot.Backend.GPU` | `: Nonempty GPUContext := GPUContextImpl.property` |
| 43 | opaque | `LeanPlot.Backend.GPU` | `GPUBufferImpl : NonemptyType` |
| 45 | def | `LeanPlot.Backend.GPU` | `GPUBuffer : Type := GPUBufferImpl.type` |
| 46 | instance | `LeanPlot.Backend.GPU` | `: Nonempty GPUBuffer := GPUBufferImpl.property` |
| 49 | opaque | `LeanPlot.Backend.GPU` | `GPUShaderImpl : NonemptyType` |
| 51 | def | `LeanPlot.Backend.GPU` | `GPUShader : Type := GPUShaderImpl.type` |
| 52 | instance | `LeanPlot.Backend.GPU` | `: Nonempty GPUShader := GPUShaderImpl.property` |
| 57 | inductive | `LeanPlot.Backend.GPU` | `DeviceLocation where` |
| 71 | structure | `LeanPlot.Backend.GPU` | `DeviceBuffer where` |
| 85 | def | `LeanPlot.Backend.GPU.DeviceBuffer` | `fromCPU (data : ByteArray) : DeviceBuffer :=` |
| 89 | def | `LeanPlot.Backend.GPU.DeviceBuffer` | `onCPU (b : DeviceBuffer) : Bool :=` |
| 93 | def | `LeanPlot.Backend.GPU.DeviceBuffer` | `onGPU (b : DeviceBuffer) : Bool :=` |
| 101 | inductive | `LeanPlot.Backend.GPU` | `ShaderFormat where` |
| 111 | structure | `LeanPlot.Backend.GPU` | `CompiledShader where` |
| 122 | inductive | `LeanPlot.Backend.GPU` | `RenderTarget where` |
| 130 | structure | `LeanPlot.Backend.GPU` | `Framebuffer where` |
| 146 | structure | `LeanPlot.Backend.GPU` | `GPUCapabilities where` |
| 165 | class | `LeanPlot.Backend.GPU` | `GPUBackend (G : Type) where` |
| 212 | structure | `LeanPlot.Backend.GPU` | `Vertex where` |
| 224 | def | `LeanPlot.Backend.GPU.Vertex` | `packVertices (vertices : Array Vertex) : ByteArray := Id.run do` |
| 242 | def | `LeanPlot.Backend.GPU` | `planToVertices (plan : RenderPlan) : Array Vertex := Id.run do` |

#### `LeanPlot/Backend/SIMD.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 29 | structure | `LeanPlot.Backend` | `PointsSoA where` |
| 39 | def | `LeanPlot.Backend.PointsSoA` | `size (p : PointsSoA) : Nat := p.xs.size` |
| 42 | def | `LeanPlot.Backend.PointsSoA` | `fromPoints (pts : Array Point) : PointsSoA :=` |
| 46 | def | `LeanPlot.Backend.PointsSoA` | `get (p : PointsSoA) (i : Nat) : Point :=` |
| 60 | structure | `LeanPlot.Backend` | `SIMDBackend where` |
| 199 | def | `LeanPlot.Backend` | `renderPlanSIMD (plan : RenderPlan) : Bitmap := Id.run do` |
| 218 | instance | `LeanPlot.Backend` | `: RenderBackend SIMDBackend where` |
| 233 | def | `LeanPlot.Backend` | `renderSIMDToPNG (plan : RenderPlan) : ByteArray :=` |
| 238 | def | `LeanPlot.Backend` | `saveSIMDPNG (path : System.FilePath) (plan : RenderPlan) : IO Unit := do` |

#### `LeanPlot/CLI/Export.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 11 | inductive | `LeanPlot.CLI` | `FnName` |
| 17 | def | `LeanPlot.CLI.FnName` | `fromString (s : String) : Option FnName :=` |
| 30 | def | `LeanPlot.CLI.FnName` | `toString : FnName → String` |
| 43 | def | `LeanPlot.CLI` | `fnOf : FnName → (Float → Float)` |
| 54 | def | `LeanPlot.CLI` | `sampleNamed (name : FnName) (steps : Nat) (min max : Float) : Array Json :=` |
| 58 | def | `LeanPlot.CLI` | `encodeJson (rows : Array Json) : String :=` |

#### `LeanPlot/CLI/ExportMain.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 13 | def | `` | `main (args : List String) : IO Unit := do` |

#### `LeanPlot/Components.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 33 | def | `LeanPlot.Components` | `sample {β} [ToFloat β]` |
| 64 | def | `LeanPlot.Components` | `sampleMany {β} [ToFloat β]` |
| 90 | def | `LeanPlot.Components` | `mkLineChart (data : Array Json) (seriesStrokes : Array (String × String)) (w h : Nat := 400) : Html :=` |
| 108 | def | `LeanPlot.Components` | `mkLineChartWithLabels (data : Array Json)` |
| 138 | def | `LeanPlot.Components` | `mkLineChartFull (data : Array Json)` |
| 170 | structure | `LeanPlot.Components` | `ScatterChartProps where` |
| 182 | def | `LeanPlot.Components` | `ScatterChart : ProofWidgets.Component ScatterChartProps where` |
| 187 | structure | `LeanPlot.Components` | `ScatterProps where` |
| 196 | def | `LeanPlot.Components` | `Scatter : ProofWidgets.Component ScatterProps where` |
| 205 | def | `LeanPlot.Components` | `mkScatterChart (data : Array Json) (fillColor : String)` |
| 230 | structure | `LeanPlot.Components` | `AreaChartProps where` |
| 240 | def | `LeanPlot.Components` | `AreaChart : ProofWidgets.Component AreaChartProps where` |
| 246 | structure | `LeanPlot.Components` | `AreaProps where` |
| 257 | def | `LeanPlot.Components` | `Area : ProofWidgets.Component AreaProps where` |
| 262 | structure | `LeanPlot.Components` | `BarChartProps where` |
| 272 | def | `LeanPlot.Components` | `BarChart : ProofWidgets.Component BarChartProps where` |
| 277 | structure | `LeanPlot.Components` | `BarProps where` |
| 286 | def | `LeanPlot.Components` | `Bar : ProofWidgets.Component BarProps where` |
| 291 | structure | `LeanPlot.Components` | `ComposedChartProps where` |
| 301 | def | `LeanPlot.Components` | `ComposedChart : ProofWidgets.Component ComposedChartProps where` |
| 310 | def | `LeanPlot.Components` | `mkBarChart (data : Array Json) (fillColor : String)` |
| 338 | def | `LeanPlot.Components` | `plotSimple {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)` |
| 360 | def | `LeanPlot.Components` | `plotManySimple {β} [ToFloat β] (fns : Array (String × (Float → β)))` |
| 379 | def | `LeanPlot.Components` | `scatterSimple {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)` |
| 385 | def | `LeanPlot.Components` | `barSimple {β} [ToFloat β] (f : Float → β) (steps : Nat := 200)` |
| 393 | def | `LeanPlot.Components` | `mkLineChartWithAutoLabels (data : Array Json)` |

#### `LeanPlot/Constants.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 10 | def | `LeanPlot.Constants` | `defaultW : Nat := 400` |
| 13 | def | `LeanPlot.Constants` | `defaultH : Nat := 300` |
| 20 | def | `` | `_root_.Float.pi : Float := 3.14159265358979323846` |

#### `LeanPlot/Core.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 18 | structure | `` | `Renderable where` |
| 23 | class | `` | `Render (α : Type u) where` |
| 31 | instance | `` | `(α) [Render α] : CoeTC α Html where coe := render` |
| 36 | structure | `` | `Layer where` |
| 42 | class | `` | `ToLayer (α : Type u) where` |
| 47 | instance | `` | `: ToLayer Layer where toLayer := id` |
| 50 | structure | `` | `Plot where` |
| 56 | class | `` | `ToPlot (α : Type u) where` |
| 61 | instance | `` | `: ToPlot Plot where toPlot := id` |
| 62 | instance | `` | `[ToLayer α] : ToPlot α where` |
| 67 | def | `` | `Plot.overlay (p q : Plot) : Plot := ⟨p.layers ++ q.layers⟩` |
| 70 | instance | `` | `: HAdd Plot Plot Plot where hAdd := Plot.overlay` |
| 74 | instance | `` | `(priority := 2000) [ToPlot α] [ToPlot β] : HAdd α β Plot where` |
| 78 | instance | `` | `(priority := 2000) [ToPlot α] [ToPlot β] : HMul α β Plot where` |
| 82 | instance | `` | `(priority := 2000) [ToPlot α] [ToPlot β] : HDiv α β Plot where` |
| 87 | instance | `` | `: Render Renderable where` |
| 90 | instance | `` | `: Render Layer where` |
| 96 | instance | `` | `: Render Plot where` |

#### `LeanPlot/DSL.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 50 | syntax | `LeanPlot.DSL` | `(name := plotCmdUsing) (docComment)? "#plot " term " using " num : command` |
| 53 | syntax | `LeanPlot.DSL` | `(name := plotCmdNamed) (docComment)? "#plot " term` |
| 109 | def | `LeanPlot.DSL` | `elabPlotNew : CommandElab := fun stx => do` |
| 136 | def | `LeanPlot.DSL` | `elabPlotUsing : CommandElab := fun stx => do` |
| 158 | def | `LeanPlot.DSL` | `elabPlotNamed : CommandElab := fun stx => do` |

#### `LeanPlot/Debug.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 17 | structure | `LeanPlot.Debug` | `SavePNGProps where` |
| 28 | def | `LeanPlot.Debug` | `SavePNG : ProofWidgets.Component SavePNGProps where` |
| 34 | def | `LeanPlot.Debug` | `withSavePNG (plot : Html) (id : String := "leanplot-debug-target") (fileName := "chart.png") : Html :=` |

#### `LeanPlot/Faceting.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 50 | def | `LeanPlot.Faceting` | `facetGrid (plots : Array PlotSpec) (cols : Nat := 2) : Html :=` |
| 56 | def | `LeanPlot.Faceting` | `facetGridNamed (plots : Array (String × PlotSpec)) (cols : Nat := 2) : Html :=` |

#### `LeanPlot/GrammarOfGraphics.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 25 | structure | `LeanPlot.GrammarOfGraphics` | `Aesthetic where` |
| 37 | inductive | `LeanPlot.GrammarOfGraphics` | `Geom where` |
| 57 | structure | `LeanPlot.GrammarOfGraphics` | `Layer where` |
| 69 | structure | `LeanPlot.GrammarOfGraphics` | `PlotBuilder where` |
| 83 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `new : PlotBuilder := {}` |
| 86 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `fromSpec (spec : PlotSpec) : PlotBuilder := { spec := spec }` |
| 92 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `title (title : String) (pb : PlotBuilder) : PlotBuilder :=` |
| 96 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `size (width height : Nat) (pb : PlotBuilder) : PlotBuilder :=` |
| 100 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `legend (showLegend : Bool) (pb : PlotBuilder) : PlotBuilder :=` |
| 104 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `xLabel (label : String) (pb : PlotBuilder) : PlotBuilder :=` |
| 108 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `yLabel (label : String) (pb : PlotBuilder) : PlotBuilder :=` |
| 112 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `xDomain (min max : Float) (pb : PlotBuilder) : PlotBuilder :=` |
| 116 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `yDomain (min max : Float) (pb : PlotBuilder) : PlotBuilder :=` |
| 120 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `aes (x : String) (y : String) (pb : PlotBuilder) : PlotBuilder :=` |
| 124 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `layer (layer : Layer) (pb : PlotBuilder) : PlotBuilder :=` |
| 128 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `points (data : Array Json) (name : String := "points") (pb : PlotBuilder) : PlotBuilder :=` |
| 132 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `line (data : Array Json) (name : String := "line") (pb : PlotBuilder) : PlotBuilder :=` |
| 136 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `bars (data : Array Json) (name : String := "bars") (pb : PlotBuilder) : PlotBuilder :=` |
| 140 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `area (data : Array Json) (name : String := "area") (pb : PlotBuilder) : PlotBuilder :=` |
| 144 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `overlay (spec : PlotSpec) (pb : PlotBuilder) : PlotBuilder :=` |
| 148 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `logX (base : Float := 10.0) (pb : PlotBuilder) : PlotBuilder :=` |
| 155 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `logY (base : Float := 10.0) (pb : PlotBuilder) : PlotBuilder :=` |
| 162 | def | `LeanPlot.GrammarOfGraphics.PlotBuilder` | `build (pb : PlotBuilder) : PlotSpec :=` |
| 186 | def | `LeanPlot.GrammarOfGraphics` | `plot {β} [ToFloat β] (f : Float → β) : PlotBuilder :=` |
| 191 | def | `LeanPlot.GrammarOfGraphics` | `scatterPlot (points : Array (Float × Float)) : PlotBuilder :=` |
| 196 | def | `LeanPlot.GrammarOfGraphics` | `barPlot (points : Array (Float × Float)) : PlotBuilder :=` |
| 201 | def | `LeanPlot.GrammarOfGraphics` | `areaPlot {β} [ToFloat β] (f : Float → β) : PlotBuilder :=` |
| 206 | def | `LeanPlot.GrammarOfGraphics` | `plotLine {β} [ToFloat β] (f : Float → β)` |
| 212 | def | `LeanPlot.GrammarOfGraphics` | `plotScatter (points : Array (Float × Float))` |
| 216 | instance | `LeanPlot.GrammarOfGraphics` | `: ToPlotSpec PlotBuilder where` |
| 220 | def | `LeanPlot.GrammarOfGraphics` | `plotBar (points : Array (Float × Float))` |
| 225 | def | `LeanPlot.GrammarOfGraphics` | `plotArea {β} [ToFloat β] (f : Float → β)` |
| 231 | def | `LeanPlot.GrammarOfGraphics` | `plotLines {β} [Inhabited β] [ToFloat β]` |
| 238 | notation | `LeanPlot.GrammarOfGraphics` | `:50 x:50 " >> " f:51 => f x` |
| 248 | def | `LeanPlot.GrammarOfGraphics.Facet` | `grid (pbs : Array PlotBuilder) (cols : Nat := 2) : Html :=` |
| 253 | def | `LeanPlot.GrammarOfGraphics.Facet` | `gridNamed (pbs : Array (String × PlotBuilder)) (cols : Nat := 2) : Html :=` |
| 267 | def | `LeanPlot.GrammarOfGraphics` | `p1 := plot (fun x : Float => x)          -- y = x` |
| 268 | def | `LeanPlot.GrammarOfGraphics` | `p2 := plot (fun x : Float => x * x)      -- y = x²` |
| 269 | def | `LeanPlot.GrammarOfGraphics` | `p3 := plot (fun x : Float => x * x * x)  -- y = x³` |
| 282 | structure | `LeanPlot.GrammarOfGraphics` | `_FacetChain where` |
| 289 | def | `LeanPlot.GrammarOfGraphics` | `_FacetChain.concat (c₁ c₂ : _FacetChain) : _FacetChain :=` |
| 293 | infixr | `LeanPlot.GrammarOfGraphics` | `:65 " ⫽ " => _FacetChain.concat` |
| 296 | instance | `LeanPlot.GrammarOfGraphics` | `: Coe _FacetChain Html where` |
| 300 | instance | `LeanPlot.GrammarOfGraphics` | `: Coe PlotBuilder _FacetChain where` |
| 304 | instance | `LeanPlot.GrammarOfGraphics` | `: ProofWidgets.HtmlEval _FacetChain where` |

#### `LeanPlot/Graphic.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 22 | def | `` | `p := plot (fun x => x^2)       -- Graphic value` |
| 23 | def | `` | `q := plot Float.sin            -- Graphic value` |
| 24 | def | `` | `r := p + q                     -- Overlay composition` |
| 45 | structure | `LeanPlot` | `PlotOpts where` |
| 57 | structure | `LeanPlot` | `Style where` |
| 73 | inductive | `LeanPlot` | `GraphicTag where` |
| 96 | structure | `LeanPlot` | `Graphic where` |
| 116 | def | `LeanPlot.Graphic` | `mkFn (f : Float → Float) (opts : PlotOpts := {}) : Graphic :=` |
| 120 | def | `LeanPlot.Graphic` | `mkPoints (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=` |
| 124 | def | `LeanPlot.Graphic` | `mkBars (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=` |
| 128 | def | `LeanPlot.Graphic` | `mkArea (f : Float → Float) (opts : PlotOpts := {}) : Graphic :=` |
| 132 | def | `LeanPlot.Graphic` | `mkOverlay (g1 g2 : Graphic) : Graphic :=` |
| 136 | def | `LeanPlot.Graphic` | `mkFacetH (g1 g2 : Graphic) : Graphic :=` |
| 140 | def | `LeanPlot.Graphic` | `mkFacetV (g1 g2 : Graphic) : Graphic :=` |
| 144 | def | `LeanPlot.Graphic` | `mkStyled (g : Graphic) (s : Style) : Graphic :=` |
| 152 | instance | `LeanPlot` | `: Add Graphic where` |
| 157 | infixr | `LeanPlot` | `:60 " \|\|\| " => Graphic.mkFacetH` |
| 160 | instance | `LeanPlot` | `: HDiv Graphic Graphic Graphic where` |
| 168 | def | `LeanPlot` | `p := plot (fun x => x^2)` |
| 169 | def | `LeanPlot` | `q := plot Float.sin` |
| 173 | def | `LeanPlot` | `plot {β : Type} [ToFloat β] (f : Float → β) (opts : PlotOpts := {}) : Graphic :=` |
| 179 | def | `LeanPlot` | `data := #[(0, 0), (1, 1), (2, 4), (3, 9)]` |
| 183 | def | `LeanPlot` | `scatter (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=` |
| 189 | def | `LeanPlot` | `sales := #[(1, 100), (2, 150), (3, 120)]` |
| 193 | def | `LeanPlot` | `bar (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=` |
| 202 | def | `LeanPlot` | `areaPlot {β : Type} [ToFloat β] (f : Float → β) (opts : PlotOpts := {}) : Graphic :=` |
| 210 | def | `LeanPlot.Graphic` | `updateOpts (f : PlotOpts → PlotOpts) (g : Graphic) : Graphic :=` |
| 237 | def | `LeanPlot.Graphic` | `domain (lo hi : Float) : Graphic → Graphic :=` |
| 246 | def | `LeanPlot.Graphic` | `samples (n : Nat) : Graphic → Graphic :=` |
| 255 | def | `LeanPlot.Graphic` | `color (c : String) : Graphic → Graphic :=` |
| 264 | def | `LeanPlot.Graphic` | `named (n : String) : Graphic → Graphic :=` |
| 268 | def | `LeanPlot.Graphic` | `updateStyle (f : Style → Style) (g : Graphic) : Graphic :=` |
| 285 | def | `LeanPlot.Graphic` | `title (t : String) : Graphic → Graphic :=` |
| 294 | def | `LeanPlot.Graphic` | `size (w h : Nat) : Graphic → Graphic :=` |
| 303 | def | `LeanPlot.Graphic` | `xLabel (label : String) : Graphic → Graphic :=` |
| 312 | def | `LeanPlot.Graphic` | `yLabel (label : String) : Graphic → Graphic :=` |
| 321 | def | `LeanPlot.Graphic` | `legend (visible : Bool) : Graphic → Graphic :=` |
| 434 | def | `LeanPlot` | `Graphic.toPlotSpec (g : Graphic) : PlotSpec :=` |
| 525 | def | `LeanPlot` | `Graphic.toRenderPlan (g : Graphic) : Backend.RenderPlan :=` |
| 554 | def | `LeanPlot` | `renderFaceted (g : Graphic) (_direction : String) : Html :=` |
| 605 | def | `LeanPlot` | `render (g : Graphic) : Html :=` |
| 615 | instance | `LeanPlot` | `: HtmlEval Graphic where` |
| 618 | instance | `LeanPlot` | `: ToPlotSpec Graphic where` |
| 624 | abbrev | `LeanPlot` | `line := @plot` |

#### `LeanPlot/Interactive.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 50 | structure | `LeanPlot.Interactive` | `ParamState where` |
| 64 | structure | `LeanPlot.Interactive` | `InteractivePlotProps where` |
| 97 | def | `LeanPlot.Interactive` | `InteractivePlotPanel : Component InteractivePlotProps where` |
| 500 | def | `LeanPlot.Interactive` | `sampleFunction (f : Float → Float) (lo hi : Float) (n : Nat) : Json :=` |
| 509 | def | `LeanPlot.Interactive` | `defaultDomainLo : Float := 0.0` |
| 510 | def | `LeanPlot.Interactive` | `defaultDomainHi : Float := 1.0` |
| 511 | def | `LeanPlot.Interactive` | `defaultSteps : Nat := 200` |
| 512 | def | `LeanPlot.Interactive` | `defaultWidth : Nat := defaultW` |
| 513 | def | `LeanPlot.Interactive` | `defaultHeight : Nat := defaultH` |
| 529 | syntax | `` | `(name := iplotCmd) "#iplot " term` |
| 586 | def | `` | `elabIplotCmd : CommandElab` |
| 671 | def | `LeanPlot` | `Graphic.renderInteractive (g : Graphic) (_lineNum : Nat) (_uri : String) : Html :=` |

#### `LeanPlot/JsonExt.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 24 | instance | `LeanPlot` | `: Coe String Json where` |
| 30 | instance | `LeanPlot` | `: Coe (Option String) (Option Json) where` |
| 42 | def | `Lean` | `Json.keys : Json → Array String` |
| 52 | def | `LeanPlot` | `jsonHasKeys (j : Lean.Json) (req : Array String) : Bool :=` |
| 60 | def | `LeanPlot` | `HasKeys (j : Lean.Json) (req : Array String) : Prop :=` |
| 63 | instance | `LeanPlot` | `(j : Lean.Json) (req : Array String) : Decidable (HasKeys j req) := by` |
| 74 | def | `` | `myJson : Json := Json.mkObj [("foo", 1), ("bar", "baz")]` |
| 82 | syntax | `` | `(name := assertKeys) "#assert_keys " term:max ppSpace term:max : command` |
| 86 | def | `` | `elabAssertKeys : Elab.Command.CommandElab := fun stx => do` |

#### `LeanPlot/LegacyLayer.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 34 | structure | `LeanPlot` | `LegacyLayerSpec where` |

#### `LeanPlot/Legend.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 18 | structure | `LeanPlot.Legend` | `LegendProps where` |
| 28 | def | `LeanPlot.Legend` | `Legend : ProofWidgets.Component LegendProps where` |
| 33 | def | `LeanPlot.Legend` | `LegendComp : ProofWidgets.Component LegendProps := Legend` |

#### `LeanPlot/Metaprogramming.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 29 | inductive | `LeanPlot.Metaprogramming` | `ParameterRole` |
| 48 | instance | `LeanPlot.Metaprogramming` | `: ToString ParameterRole where` |
| 60 | structure | `LeanPlot.Metaprogramming` | `ParameterInfo where` |
| 69 | instance | `LeanPlot.Metaprogramming` | `: Inhabited ParameterInfo where` |
| 77 | structure | `LeanPlot.Metaprogramming` | `AxisLabels where` |
| 85 | structure | `LeanPlot.Metaprogramming` | `FunctionMetadata where` |
| 95 | def | `LeanPlot.Metaprogramming` | `stringContains (s : String) (sub : String) : Bool :=` |
| 99 | def | `LeanPlot.Metaprogramming` | `inferParameterRole (name : Name) : ParameterRole :=` |
| 114 | def | `LeanPlot.Metaprogramming` | `enhanceNameWithRole (baseName : String) (role : ParameterRole) : String :=` |
| 121 | def | `LeanPlot.Metaprogramming` | `extractParameterMetadata (expr : Expr) : Array ParameterInfo :=` |
| 138 | def | `LeanPlot.Metaprogramming` | `disambiguateParameterNames (params : Array ParameterInfo) : Array ParameterInfo :=` |
| 149 | def | `LeanPlot.Metaprogramming` | `generateAxisLabels (params : Array ParameterInfo) : AxisLabels :=` |
| 161 | def | `LeanPlot.Metaprogramming` | `analyzeFunction (expr : Expr) : FunctionMetadata :=` |
| 180 | def | `LeanPlot.Metaprogramming` | `smartLabels (expr : Expr) : String × String :=` |
| 191 | def | `LeanPlot.Metaprogramming` | `smartNames (expr : Expr) : Array String :=` |
| 202 | def | `LeanPlot.Metaprogramming` | `fixDuplicates (names : Array String) : Array String :=` |
| 212 | def | `LeanPlot.Metaprogramming` | `getParameterNames (expr : Expr) : Array String := smartNames expr` |
| 215 | def | `LeanPlot.Metaprogramming` | `getAxisLabels (expr : Expr) : String × String := smartLabels expr` |
| 218 | def | `LeanPlot.Metaprogramming` | `extractParameterNames (expr : Expr) : Array Name :=` |
| 224 | def | `LeanPlot.Metaprogramming` | `nameToString (n : Name) : String :=` |
| 231 | def | `LeanPlot.Metaprogramming` | `disambiguateNames (names : Array String) : Array String := fixDuplicates names` |
| 239 | def | `LeanPlot.Metaprogramming` | `myTimeFunction : Expr :=` |
| 243 | def | `LeanPlot.Metaprogramming` | `myDuplicateFunction : Expr :=` |

#### `LeanPlot/Palette.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 22 | def | `LeanPlot.Palette` | `darkPurple : Color := "#440154"` |
| 25 | def | `LeanPlot.Palette` | `indigo : Color := "#482878"` |
| 28 | def | `LeanPlot.Palette` | `bluePurple : Color := "#3e4a89"` |
| 31 | def | `LeanPlot.Palette` | `blue : Color := "#31688e"` |
| 34 | def | `LeanPlot.Palette` | `turquoise : Color := "#26828e"` |
| 37 | def | `LeanPlot.Palette` | `greenTurquoise : Color := "#1f9e89"` |
| 40 | def | `LeanPlot.Palette` | `green : Color := "#35b779"` |
| 43 | def | `LeanPlot.Palette` | `lime : Color := "#6ece58"` |
| 46 | def | `LeanPlot.Palette` | `yellowGreen : Color := "#b5de2b"` |
| 49 | def | `LeanPlot.Palette` | `yellow : Color := "#fde725"` |
| 52 | def | `LeanPlot.Palette` | `defaultPalette : Array Color := #[` |
| 67 | def | `LeanPlot.Palette` | `colorFromNat (n : Nat) : Color :=` |
| 75 | def | `LeanPlot.Palette` | `autoColors (names : Array String) : Array (String × String) :=` |
| 80 | def | `LeanPlot.Palette` | `f (n : Nat) : List Color :=` |
| 86 | syntax | `` | `"[" term "\|" term " in " term (", " term)? "]" : term` |
| 96 | macro_rules | `` | `` |
| 107 | def | `` | `squares  : List Nat := [ x ^ 2 \| x in List.range 6 ]` |
| 109 | def | `` | `evensSq  : List Nat := [ x ^ 2 \| x in List.range 6, x % 2 == 0 ]` |

#### `LeanPlot/Plot.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 31 | def | `LeanPlot.PlotCommand` | `withCaption (caption : String) (inner : Html) : Html :=` |
| 52 | syntax | `LeanPlot.PlotCommand` | `(name := plotCmd) (docComment)? "#plot" ("+tunable")? term : command` |
| 74 | def | `LeanPlot.PlotCommand` | `elabPlotCmd : CommandElab := fun stx => do` |
| 160 | syntax | `LeanPlot.RenderCommand` | `(name := renderCmd) "#render" term ("as" str)? : command` |
| 163 | def | `LeanPlot.RenderCommand` | `elabRenderCmd : CommandElab := fun stx => do` |

#### `LeanPlot/PlotComposition.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 33 | def | `LeanPlot.PlotComposition` | `mergeAligned (p q : PlotSpec) (_steps : Nat := 200) : PlotSpec :=` |
| 73 | def | `LeanPlot.PlotComposition` | `gridLayout (plots : Array PlotSpec) (cols : Nat := 2) : Html :=` |
| 96 | def | `LeanPlot.PlotComposition` | `verticalStack (plots : Array PlotSpec) : Html :=` |
| 110 | def | `LeanPlot.PlotComposition` | `normalizeYScale (plots : Array PlotSpec) : Array PlotSpec :=` |
| 137 | def | `LeanPlot.PlotComposition` | `applyColorScheme (plots : Array PlotSpec) (palette : Array String) : Array PlotSpec :=` |

#### `LeanPlot/Render/Bitmap.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 12 | structure | `LeanPlot.Render` | `RGB where` |
| 23 | def | `LeanPlot.Render.RGB` | `black   : RGB := ⟨0x00, 0x00, 0x00⟩` |
| 24 | def | `LeanPlot.Render.RGB` | `white   : RGB := ⟨0xFF, 0xFF, 0xFF⟩` |
| 25 | def | `LeanPlot.Render.RGB` | `red     : RGB := ⟨0xFF, 0x00, 0x00⟩` |
| 26 | def | `LeanPlot.Render.RGB` | `green   : RGB := ⟨0x00, 0xFF, 0x00⟩` |
| 27 | def | `LeanPlot.Render.RGB` | `blue    : RGB := ⟨0x00, 0x00, 0xFF⟩` |
| 28 | def | `LeanPlot.Render.RGB` | `yellow  : RGB := ⟨0xFF, 0xFF, 0x00⟩` |
| 29 | def | `LeanPlot.Render.RGB` | `cyan    : RGB := ⟨0x00, 0xFF, 0xFF⟩` |
| 30 | def | `LeanPlot.Render.RGB` | `magenta : RGB := ⟨0xFF, 0x00, 0xFF⟩` |
| 31 | def | `LeanPlot.Render.RGB` | `gray    : RGB := ⟨0x80, 0x80, 0x80⟩` |
| 34 | def | `LeanPlot.Render.RGB` | `fromHex (s : String) : Option RGB := do` |
| 53 | def | `LeanPlot.Render.RGB` | `blend (c1 c2 : RGB) (alpha : Float) : RGB :=` |
| 65 | structure | `LeanPlot.Render` | `Bitmap where` |
| 77 | def | `LeanPlot.Render.Bitmap` | `fill (width height : Nat) (color : RGB := RGB.white) : Bitmap :=` |
| 81 | def | `LeanPlot.Render.Bitmap` | `create (width height : Nat) : Bitmap := fill width height RGB.white` |
| 84 | def | `LeanPlot.Render.Bitmap` | `getPixel? (bmp : Bitmap) (x y : Nat) : Option RGB :=` |
| 90 | def | `LeanPlot.Render.Bitmap` | `getPixel (bmp : Bitmap) (x y : Nat) (default : RGB := RGB.white) : RGB :=` |
| 94 | def | `LeanPlot.Render.Bitmap` | `setPixel (bmp : Bitmap) (x y : Nat) (color : RGB) : Bitmap :=` |
| 101 | def | `LeanPlot.Render.Bitmap` | `blendPixel (bmp : Bitmap) (x y : Nat) (color : RGB) (alpha : Float) : Bitmap :=` |
| 110 | def | `LeanPlot.Render.Bitmap` | `fillRect (bmp : Bitmap) (x y w h : Nat) (color : RGB) : Bitmap := Id.run do` |
| 118 | def | `LeanPlot.Render.Bitmap` | `hLine (bmp : Bitmap) (x y length : Nat) (color : RGB) : Bitmap := Id.run do` |
| 125 | def | `LeanPlot.Render.Bitmap` | `vLine (bmp : Bitmap) (x y length : Nat) (color : RGB) : Bitmap := Id.run do` |
| 132 | def | `LeanPlot.Render.Bitmap` | `toScanlines (bmp : Bitmap) : ByteArray := Id.run do` |

#### `LeanPlot/Render/Export.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 19 | structure | `LeanPlot.Render` | `PngConfig where` |
| 34 | def | `LeanPlot.Render` | `renderGraphicLayer (bmp : Bitmap) (g : Graphic) (t : CoordTransform)` |
| 106 | def | `LeanPlot.Render` | `getGraphicBounds (g : Graphic) : Option (Float × Float × Float × Float) :=` |
| 182 | def | `LeanPlot.Render` | `renderToBitmap (g : Graphic) (config : PngConfig := {}) : Bitmap := Id.run do` |
| 208 | def | `LeanPlot.Render` | `savePNG (path : System.FilePath) (g : Graphic) (config : PngConfig := {}) : IO Unit := do` |
| 213 | def | `LeanPlot.Render` | `renderGraphicToSvg (g : Graphic) (toSvgX toSvgY : Float → Float)` |
| 275 | def | `LeanPlot.Render` | `saveSVG (path : System.FilePath) (g : Graphic) (width : Nat := 800) (height : Nat := 600) : IO Unit := do` |
| 309 | def | `LeanPlot` | `Graphic.savePNG (g : Graphic) (path : String) (width : Nat := 800) (height : Nat := 600) : IO Unit :=` |
| 313 | def | `LeanPlot` | `Graphic.saveSVG (g : Graphic) (path : String) (width : Nat := 800) (height : Nat := 600) : IO Unit :=` |

#### `LeanPlot/Render/PNG/Adler32.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 10 | def | `LeanPlot.Render.PNG` | `adler32Mod : UInt32 := 65521` |
| 13 | def | `LeanPlot.Render.PNG` | `adler32 (data : ByteArray) : UInt32 :=` |

#### `LeanPlot/Render/PNG/CRC32.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 11 | def | `LeanPlot.Render.PNG` | `crc32Polynomial : UInt32 := 0xEDB88320` |
| 14 | def | `LeanPlot.Render.PNG` | `makeTableEntry (n : UInt8) : UInt32 :=` |
| 26 | def | `LeanPlot.Render.PNG` | `crc32Table : Array UInt32 :=` |
| 31 | def | `LeanPlot.Render.PNG` | `updateCrcByte (crc : UInt32) (byte : UInt8) : UInt32 :=` |
| 36 | def | `LeanPlot.Render.PNG` | `updateCrc32 (crc : UInt32) (data : ByteArray) : UInt32 :=` |
| 40 | def | `LeanPlot.Render.PNG` | `crc32 (data : ByteArray) : UInt32 :=` |

#### `LeanPlot/Render/PNG/Encode.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 19 | def | `LeanPlot.Render.PNG` | `pushU32BE (ba : ByteArray) (val : UInt32) : ByteArray :=` |
| 26 | def | `LeanPlot.Render.PNG` | `pushU16LE (ba : ByteArray) (val : UInt16) : ByteArray :=` |
| 33 | def | `LeanPlot.Render.PNG` | `pngSignature : ByteArray :=` |
| 37 | def | `LeanPlot.Render.PNG` | `mkChunk (chunkType : String) (data : ByteArray) : ByteArray :=` |
| 47 | def | `LeanPlot.Render.PNG` | `mkIHDR (width height : UInt32) (bitDepth : UInt8 := 8) (colorType : UInt8 := 2) : ByteArray :=` |
| 59 | def | `LeanPlot.Render.PNG` | `mkIEND : ByteArray := mkChunk "IEND" ByteArray.empty` |
| 64 | def | `LeanPlot.Render.PNG` | `maxBlockSize : Nat := 65535` |
| 67 | def | `LeanPlot.Render.PNG` | `mkDeflateBlock (data : ByteArray) (isFinal : Bool) : ByteArray :=` |
| 75 | def | `LeanPlot.Render.PNG` | `splitIntoBlocks (data : ByteArray) : Array (ByteArray × Bool) := Id.run do` |
| 88 | def | `LeanPlot.Render.PNG` | `wrapZlib (data : ByteArray) : ByteArray :=` |
| 103 | def | `LeanPlot.Render.PNG` | `encode (bmp : Bitmap) : ByteArray :=` |
| 112 | def | `LeanPlot.Render.PNG` | `writePNG (path : System.FilePath) (bmp : Bitmap) : IO Unit := do` |

#### `LeanPlot/Render/Rasterize.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 25 | def | `LeanPlot.Render` | `Bitmap.drawLine (bmp : Bitmap) (x0 y0 x1 y1 : Int) (color : RGB) : Bitmap := Id.run do` |
| 50 | def | `LeanPlot.Render` | `Bitmap.drawThickLine (bmp : Bitmap) (x0 y0 x1 y1 : Int) (thickness : Nat) (color : RGB) : Bitmap := Id.run do` |
| 71 | structure | `LeanPlot.Render` | `CoordTransform where` |
| 87 | def | `LeanPlot.Render.CoordTransform` | `create (bmp : Bitmap) (xMin xMax yMin yMax : Float) (margin : Nat := 40) : CoordTransform :=` |
| 98 | def | `LeanPlot.Render.CoordTransform` | `toPixelX (t : CoordTransform) (x : Float) : Int :=` |
| 107 | def | `LeanPlot.Render.CoordTransform` | `toPixelY (t : CoordTransform) (y : Float) : Int :=` |
| 118 | def | `LeanPlot.Render` | `Bitmap.drawAxes (bmp : Bitmap) (t : CoordTransform) (axisColor : RGB := RGB.gray) : Bitmap := Id.run do` |
| 136 | def | `LeanPlot.Render` | `Bitmap.drawPolyline (bmp : Bitmap) (points : Array (Float × Float)) (t : CoordTransform) (color : RGB) : Bitmap := Id.run do` |
| 151 | def | `LeanPlot.Render` | `Bitmap.plotFunction (bmp : Bitmap) (f : Float → Float) (t : CoordTransform) (samples : Nat) (color : RGB) : Bitmap :=` |

#### `LeanPlot/SVG.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 10 | def | `LeanPlot.SVG` | `floatMin (a b : Float) : Float := if a < b then a else b` |
| 13 | def | `LeanPlot.SVG` | `floatMax (a b : Float) : Float := if a > b then a else b` |
| 16 | structure | `LeanPlot.SVG` | `Dims where` |
| 26 | structure | `LeanPlot.SVG` | `Point where` |
| 34 | def | `LeanPlot.SVG` | `linePath (points : Array (Float × Float)) (dims : Dims) : String :=` |
| 62 | def | `LeanPlot.SVG` | `scatterPoints (points : Array (Float × Float)) (dims : Dims) (color : String) : String :=` |
| 86 | def | `LeanPlot.SVG` | `axisLabels (xLabel yLabel : String) (dims : Dims) : String :=` |
| 93 | def | `LeanPlot.SVG` | `grid (dims : Dims) : String :=` |
| 105 | def | `LeanPlot.SVG` | `lineChartSVG (points : Array (Float × Float)) (title : String)` |
| 118 | def | `LeanPlot.SVG` | `scatterChartSVG (points : Array (Float × Float)) (title : String)` |
| 131 | def | `LeanPlot.SVG` | `multiLineChartSVG (series : Array (String × Array (Float × Float) × String)) (title : String)` |
| 155 | def | `LeanPlot.SVG` | `sampleFn (f : Float → Float) (steps : Nat := 200) (min : Float := 0) (max : Float := 1) : Array (Float × Float) :=` |

#### `LeanPlot/Scale.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 13 | inductive | `LeanPlot.Scale` | `ScaleType where` |
| 21 | def | `LeanPlot.Scale` | `transform (scale : ScaleType) (value : Float) : Float :=` |
| 29 | def | `LeanPlot.Scale` | `inverseTransform (scale : ScaleType) (value : Float) : Float :=` |
| 35 | def | `LeanPlot.Scale` | `transformArray (scale : ScaleType) (values : Array Float) : Array Float :=` |
| 39 | structure | `LeanPlot.Scale` | `ScaleConfig where` |

#### `LeanPlot/Series.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 21 | inductive | `LeanPlot` | `SeriesKind where` |
| 33 | instance | `LeanPlot` | `: ToString SeriesKind where` |
| 42 | structure | `LeanPlot` | `LineSeriesDetails where` |
| 51 | structure | `LeanPlot` | `ScatterSeriesDetails where` |
| 61 | structure | `LeanPlot` | `BarSeriesDetails where` |
| 67 | structure | `LeanPlot` | `AreaSeriesDetails where` |
| 79 | inductive | `LeanPlot` | `SeriesDetails : SeriesKind → Type where` |
| 93 | structure | `LeanPlot` | `SeriesDSpec (k : SeriesKind) where` |
| 105 | def | `LeanPlot.SeriesKind` | `toDetailType : SeriesKind → Type` |
| 112 | def | `LeanPlot.SeriesKind` | `fromString? (s : String) : Option SeriesKind :=` |
| 124 | structure | `LeanPlot` | `SeriesDSpecPacked where` |
| 132 | def | `LeanPlot` | `LegacyLayerSpec.toSeriesDSpec? (layer : LegacyLayerSpec) : Option SeriesDSpecPacked :=` |
| 145 | def | `LeanPlot` | `SeriesDSpecPacked.toLegacyLayerSpec (packed : SeriesDSpecPacked) : LegacyLayerSpec :=` |
| 163 | def | `LeanPlot.SeriesDSpecPacked` | `name (p : SeriesDSpecPacked) : String := p.spec.name` |
| 165 | def | `LeanPlot.SeriesDSpecPacked` | `dataKey (p : SeriesDSpecPacked) : String := p.spec.dataKey` |
| 167 | def | `LeanPlot.SeriesDSpecPacked` | `kind' (p : SeriesDSpecPacked) : SeriesKind := p.kind` |
| 169 | def | `LeanPlot.SeriesDSpecPacked` | `color (p : SeriesDSpecPacked) : String :=` |
| 176 | def | `LeanPlot.SeriesDSpecPacked` | `dot? (p : SeriesDSpecPacked) : Option Bool :=` |
| 181 | def | `LeanPlot.SeriesDSpecPacked` | `typeString (p : SeriesDSpecPacked) : String := toString p.kind` |
| 185 | def | `LeanPlot.SeriesDSpecPacked` | `mkLine (name dataKey color : String) (dot : Bool := false) : SeriesDSpecPacked :=` |
| 190 | def | `LeanPlot.SeriesDSpecPacked` | `mkScatter (name dataKey color : String) (shape : String := "") : SeriesDSpecPacked :=` |
| 195 | def | `LeanPlot.SeriesDSpecPacked` | `mkBar (name dataKey color : String) : SeriesDSpecPacked :=` |
| 200 | def | `LeanPlot.SeriesDSpecPacked` | `mkArea (name dataKey fill : String) (stroke : String := "") : SeriesDSpecPacked :=` |
| 206 | def | `LeanPlot.SeriesDSpecPacked` | `setColor (p : SeriesDSpecPacked) (color : String) : SeriesDSpecPacked :=` |
| 231 | instance | `LeanPlot` | `: Inhabited SeriesDSpecPacked :=` |
| 245 | def | `LeanPlot` | `renderLine (_name dataKey : String) (details : LineSeriesDetails) : Html :=` |
| 249 | def | `LeanPlot` | `renderScatter (_name dataKey : String) (details : ScatterSeriesDetails) : Html :=` |
| 254 | def | `LeanPlot` | `renderBar (_name dataKey : String) (details : BarSeriesDetails) : Html :=` |
| 259 | def | `LeanPlot` | `renderArea (_name dataKey : String) (details : AreaSeriesDetails) : Html :=` |
| 268 | def | `LeanPlot` | `renderSeriesByKind (kind : SeriesKind) (name dataKey : String)` |
| 277 | def | `LeanPlot` | `SeriesDSpecPacked.render (packed : SeriesDSpecPacked) : Html :=` |

#### `LeanPlot/Specification.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 38 | structure | `LeanPlot` | `AxisSpec where` |
| 52 | abbrev | `LeanPlot` | `LayerSpec := SeriesDSpecPacked` |
| 56 | abbrev | `LeanPlot` | `SeriesSpec := LayerSpec` |
| 59 | structure | `LeanPlot` | `PlotSpec where` |
| 81 | class | `LeanPlot` | `ToPlotSpec (α : Type u) where` |
| 86 | def | `LeanPlot` | `toPlotSpec {α} [ToPlotSpec α] (value : α) : PlotSpec :=` |
| 89 | instance | `LeanPlot` | `: ToPlotSpec PlotSpec where` |
| 99 | def | `LeanPlot.PlotSpec` | `line {β} [ToFloat β]` |
| 135 | def | `LeanPlot.PlotSpec` | `scatter (points : Array (Float × Float)) (name : String := "y")` |
| 151 | def | `LeanPlot.PlotSpec` | `bar (points : Array (Float × Float)) (name : String := "y")` |
| 165 | def | `LeanPlot.PlotSpec` | `area {β} [ToFloat β]` |
| 206 | def | `LeanPlot.PlotSpec` | `lines {β} [Inhabited β] [ToFloat β]` |
| 242 | def | `LeanPlot.PlotSpec` | `withTitle (spec : PlotSpec) (t : String) : PlotSpec :=` |
| 247 | def | `LeanPlot.PlotSpec` | `withWidth (spec : PlotSpec) (w : Nat) : PlotSpec :=` |
| 252 | def | `LeanPlot.PlotSpec` | `withHeight (spec : PlotSpec) (h : Nat) : PlotSpec :=` |
| 257 | def | `LeanPlot.PlotSpec` | `withSize (spec : PlotSpec) (w h : Nat) : PlotSpec :=` |
| 262 | def | `LeanPlot.PlotSpec` | `withXLabel (spec : PlotSpec) (label : String) : PlotSpec :=` |
| 269 | def | `LeanPlot.PlotSpec` | `withYLabel (spec : PlotSpec) (label : String) : PlotSpec :=` |
| 277 | def | `LeanPlot.PlotSpec` | `withLegend (spec : PlotSpec) (shouldShow : Bool) : PlotSpec :=` |
| 282 | def | `LeanPlot.PlotSpec` | `withXDomain (spec : PlotSpec) (min max : Float) : PlotSpec :=` |
| 290 | def | `LeanPlot.PlotSpec` | `withYDomain (spec : PlotSpec) (min max : Float) : PlotSpec :=` |
| 298 | def | `LeanPlot.PlotSpec` | `withSeriesColor (spec : PlotSpec) (name : String) (color : String) : PlotSpec :=` |
| 305 | def | `LeanPlot.PlotSpec` | `withSeriesColorAt (spec : PlotSpec) (idx : Nat) (color : String) : PlotSpec :=` |
| 314 | def | `LeanPlot.PlotSpec` | `addSeries (spec : PlotSpec) (series : LayerSpec) : PlotSpec :=` |
| 321 | def | `LeanPlot.PlotSpec` | `overlay (p q : PlotSpec) : PlotSpec :=` |
| 332 | def | `LeanPlot.PlotSpec` | `stack := overlay` |
| 334 | instance | `LeanPlot.PlotSpec` | `: HAdd PlotSpec PlotSpec PlotSpec where` |
| 338 | def | `LeanPlot.PlotSpec` | `addLine {β} [ToFloat β]` |
| 398 | def | `LeanPlot.PlotSpec` | `addScatter` |
| 404 | def | `LeanPlot.PlotSpec` | `addBar` |
| 412 | class | `LeanPlot.PlotSpec` | `RenderFragment (α : Type) where` |
| 419 | abbrev | `LeanPlot.PlotSpec` | `RenderSeries (α : Type) := RenderFragment α` |
| 422 | instance | `LeanPlot.PlotSpec` | `: RenderFragment _root_.LeanPlot.LayerSpec where` |
| 426 | instance | `LeanPlot.PlotSpec` | `: RenderFragment AxisSpec where` |
| 430 | def | `LeanPlot.PlotSpec` | `render (spec : PlotSpec) : Html :=` |
| 526 | instance | `LeanPlot.PlotSpec` | `: HtmlEval PlotSpec where` |
| 578 | instance | `LeanPlot.PlotSpec` | `{β : Type u} [Inhabited β] : Inhabited (String × (Float → β)) where` |
| 581 | instance | `LeanPlot.PlotSpec` | `: Render PlotSpec where` |

#### `LeanPlot/ToFloat.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 23 | class | `LeanPlot` | `ToFloat (α : Type u) : Type u where` |
| 30 | def | `LeanPlot` | `toFloatFn {α} [ToFloat α] (a : α) : Float :=` |
| 35 | instance | `LeanPlot` | `instToFloatFloat : ToFloat Float where` |
| 38 | instance | `LeanPlot` | `instToFloatNat : ToFloat Nat where` |
| 41 | instance | `LeanPlot` | `instToFloatInt : ToFloat Int where` |
| 47 | instance | `LeanPlot` | `[Coe α Float] : ToFloat α where` |
| 57 | instance | `LeanPlot` | `instToFloatRat : ToFloat Rat where` |

#### `LeanPlot/Transform.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 12 | def | `LeanPlot.Transform` | `_root_.Float.sign (x : Float) : Float :=` |
| 16 | structure | `LeanPlot.Transform` | `Scale where` |
| 26 | def | `LeanPlot.Transform` | `linearScale : Scale := {` |
| 33 | def | `LeanPlot.Transform` | `logScale (base : Float := 10.0) : Scale := {` |
| 40 | def | `LeanPlot.Transform` | `sqrtScale : Scale := {` |
| 47 | def | `LeanPlot.Transform` | `powerScale (exponent : Float) : Scale := {` |
| 54 | def | `LeanPlot.Transform` | `symlogScale (C : Float := 1.0) : Scale := {` |
| 61 | def | `LeanPlot.Transform` | `applyScale (scale : Scale) (values : Array Float) : Array Float :=` |
| 65 | def | `LeanPlot.Transform` | `transformFunction {β} [ToFloat β]` |
| 72 | def | `LeanPlot.Transform` | `normalize (values : Array Float) : Array Float :=` |
| 82 | def | `LeanPlot.Transform` | `standardize (values : Array Float) : Array Float :=` |
| 93 | def | `LeanPlot.Transform` | `clamp (min max : Float) (values : Array Float) : Array Float :=` |
| 97 | def | `LeanPlot.Transform` | `smoothMovingAverage (windowSize : Nat) (values : Array Float) : Array Float :=` |

#### `LeanPlot/TunablePlot.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 14 | structure | `LeanPlot.TunablePlot` | `TunableSeries where` |
| 24 | structure | `LeanPlot.TunablePlot` | `TunablePlotProps where` |
| 48 | def | `LeanPlot.TunablePlot` | `TunablePlotPanel : Component TunablePlotProps where` |

#### `LeanPlot/Utils.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 9 | def | `LeanPlot.Utils` | `isInvalidFloat (f : Float) : Bool :=` |
| 19 | def | `LeanPlot.Utils` | `jsonDataHasInvalidFloats (jsonData : Array Json) (keysForFloatCheck : Array String) : Bool :=` |

#### `LeanPlot/WarningBanner.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 9 | structure | `LeanPlot` | `WarningBannerProps where` |
| 18 | def | `LeanPlot` | `WarningBanner (props : WarningBannerProps) : Html :=` |

#### `Main.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 8 | def | `` | `main : IO Unit :=` |

#### `TestExport.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 14 | def | `` | `main : IO Unit := do` |

#### `GenDocImages.lean`

| line | kind | namespace | declaration |
|---|---|---|---|
| 13 | def | `` | `Float.pi : Float := 3.14159265358979323846` |
| 16 | def | `` | `main : IO Unit := do` |
