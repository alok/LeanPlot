import LeanPlot.Constants
import LeanPlot.Palette
import LeanPlot.ToFloat
import LeanPlot.Specification
import ProofWidgets.Component.HtmlDisplay
import Lean.Data.Json

/-!
# LeanPlot.Graphic - First-Class Algebraic Graphics

This module defines `Graphic`, the core type for LeanPlot's algebra of graphics.

A `Graphic` is a first-class value representing a plot that can be:
- Composed with operators: `+` (overlay), `|` (horizontal facet), `/` (vertical facet)
- Configured with fluent combinators: `.domain()`, `.samples()`, `.color()`, `.title()`
- Rendered via `#eval` (through `HtmlEval` instance) or exported to PNG/SVG

## Example Usage

```
def p := plot (fun x => x^2)       -- Graphic value
def q := plot Float.sin            -- Graphic value
def r := p + q                     -- Overlay composition
#eval r                            -- Renders in infoview

-- With configuration
#eval (plot sin).domain(-π, π).samples(500).title("Sine Wave")

-- Faceting
#eval plot sin ||| plot cos ||| plot tan   -- Side by side
#eval plot sin / plot cos                  -- Stacked vertically
```
-/

namespace LeanPlot

open Lean ProofWidgets
open LeanPlot.Constants LeanPlot.Palette
open scoped ProofWidgets.Jsx

/-! ## Core Types -/

/-- Configuration options for a single plot layer. -/
structure PlotOpts where
  /-- Domain for x-axis sampling. `none` means auto-detect or use (0, 1). -/
  domain : Option (Float × Float) := none
  /-- Number of sample points for function plots. -/
  samples : Nat := 200
  /-- CSS color string (e.g., "#ff0000" or "red"). `none` means auto-assign. -/
  color : Option String := none
  /-- Name for legend/tooltip. `none` means auto-generate. -/
  name : Option String := none
  deriving Inhabited, Repr

/-- Global styling options that apply to an entire graphic. -/
structure Style where
  /-- Title displayed above the chart. -/
  title : Option String := none
  /-- Chart width in pixels. -/
  width : Nat := defaultW
  /-- Chart height in pixels. -/
  height : Nat := defaultH
  /-- Whether to show the legend. -/
  showLegend : Bool := true
  /-- X-axis label. -/
  xLabel : Option String := none
  /-- Y-axis label. -/
  yLabel : Option String := none
  deriving Inhabited, Repr

/-- Tag for graphic variants -/
inductive GraphicTag where
  /-- Function plot tag -/
  | fn
  /-- Scatter plot tag -/
  | points
  /-- Bar chart tag -/
  | bars
  /-- Area chart tag -/
  | area
  /-- Overlay composition tag -/
  | overlay
  /-- Horizontal facet tag -/
  | facetH
  /-- Vertical facet tag -/
  | facetV
  /-- Styled graphic tag -/
  | styled
  deriving DecidableEq, Inhabited

/-- The core algebraic graphic type.

A `Graphic` represents a composable plot that can be built up from primitives
and combined using algebraic operators. -/
structure Graphic where
  /-- Tag identifying the variant -/
  tag : GraphicTag
  /-- Function for fn/area variants -/
  func : Float → Float := fun _ => 0
  /-- Points for points/bars variants -/
  pts : Array (Float × Float) := #[]
  /-- Options for leaf variants -/
  opts : PlotOpts := {}
  /-- First child graphic for composite variants -/
  child1 : Option Graphic := none
  /-- Second child graphic for composite variants -/
  child2 : Option Graphic := none
  /-- Style for styled variant -/
  style : Style := {}
  deriving Inhabited

namespace Graphic

/-- Construct a function plot -/
def mkFn (f : Float → Float) (opts : PlotOpts := {}) : Graphic :=
  { tag := .fn, func := f, opts := opts }

/-- Construct a scatter plot -/
def mkPoints (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=
  { tag := .points, pts := pts, opts := opts }

/-- Construct a bar chart -/
def mkBars (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=
  { tag := .bars, pts := pts, opts := opts }

/-- Construct an area chart -/
def mkArea (f : Float → Float) (opts : PlotOpts := {}) : Graphic :=
  { tag := .area, func := f, opts := opts }

/-- Construct an overlay -/
def mkOverlay (g1 g2 : Graphic) : Graphic :=
  { tag := .overlay, child1 := some g1, child2 := some g2 }

/-- Construct a horizontal facet -/
def mkFacetH (g1 g2 : Graphic) : Graphic :=
  { tag := .facetH, child1 := some g1, child2 := some g2 }

/-- Construct a vertical facet -/
def mkFacetV (g1 g2 : Graphic) : Graphic :=
  { tag := .facetV, child1 := some g1, child2 := some g2 }

/-- Construct a styled graphic -/
def mkStyled (g : Graphic) (s : Style) : Graphic :=
  { tag := .styled, child1 := some g, style := s }

end Graphic

/-! ## Algebraic Operators -/

/-- Overlay operator: `p + q` places both plots on the same axes. -/
instance : Add Graphic where
  add := Graphic.mkOverlay

/-- Horizontal facet operator: `p ||| q` places plots side by side.
Note: We use `|||` instead of `|` to avoid conflicts with pattern matching. -/
infixr:60 " ||| " => Graphic.mkFacetH

/-- Vertical facet operator: `p / q` stacks plots vertically. -/
instance : HDiv Graphic Graphic Graphic where
  hDiv := Graphic.mkFacetV

/-! ## Smart Constructors -/

/-- Create a line plot from a function.

```
def p := plot (fun x => x^2)
def q := plot Float.sin
#eval p + q
```
-/
def plot {β : Type} [ToFloat β] (f : Float → β) (opts : PlotOpts := {}) : Graphic :=
  Graphic.mkFn (fun x => toFloat (f x)) opts

/-- Create a scatter plot from an array of points.

```
def data := #[(0, 0), (1, 1), (2, 4), (3, 9)]
#eval scatter data
```
-/
def scatter (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=
  Graphic.mkPoints pts opts

/-- Create a bar chart from an array of (x, height) pairs.

```
def sales := #[(1, 100), (2, 150), (3, 120)]
#eval bar sales
```
-/
def bar (pts : Array (Float × Float)) (opts : PlotOpts := {}) : Graphic :=
  Graphic.mkBars pts opts

/-- Create an area chart (filled region under curve).

```
#eval area (fun x => Float.exp (-x^2))
```
-/
def areaPlot {β : Type} [ToFloat β] (f : Float → β) (opts : PlotOpts := {}) : Graphic :=
  Graphic.mkArea (fun x => toFloat (f x)) opts

/-! ## Fluent Combinators -/

namespace Graphic

/-- Helper to update opts in a leaf graphic. -/
partial def updateOpts (f : PlotOpts → PlotOpts) (g : Graphic) : Graphic :=
  match g.tag with
  | GraphicTag.fn => { g with opts := f g.opts }
  | GraphicTag.points => { g with opts := f g.opts }
  | GraphicTag.bars => { g with opts := f g.opts }
  | GraphicTag.area => { g with opts := f g.opts }
  | GraphicTag.overlay =>
    { g with
      child1 := g.child1.map (updateOpts f)
      child2 := g.child2.map (updateOpts f) }
  | GraphicTag.facetH =>
    { g with
      child1 := g.child1.map (updateOpts f)
      child2 := g.child2.map (updateOpts f) }
  | GraphicTag.facetV =>
    { g with
      child1 := g.child1.map (updateOpts f)
      child2 := g.child2.map (updateOpts f) }
  | GraphicTag.styled =>
    { g with child1 := g.child1.map (updateOpts f) }

/-- Set the domain for function sampling.

```
#eval plot sin |> domain (-Float.pi) Float.pi
```
-/
def domain (lo hi : Float) : Graphic → Graphic :=
  updateOpts fun opts => { opts with domain := some (lo, hi) }

/-- Set the number of sample points for function plots.

```
#eval plot sin |> samples 1000
```
-/
def samples (n : Nat) : Graphic → Graphic :=
  updateOpts fun opts => { opts with samples := n }

/-- Set the color for a graphic.

```
#eval plot sin |> color "#ff0000"
```
-/
def color (c : String) : Graphic → Graphic :=
  updateOpts fun opts => { opts with color := some c }

/-- Set the name for legend/tooltip.

```
#eval plot sin |> named "Sine Wave"
```
-/
def named (n : String) : Graphic → Graphic :=
  updateOpts fun opts => { opts with name := some n }

/-- Helper to update or add style to a graphic. -/
def updateStyle (f : Style → Style) (g : Graphic) : Graphic :=
  match g.tag with
  | GraphicTag.styled => { g with style := f g.style }
  | GraphicTag.fn => Graphic.mkStyled g (f {})
  | GraphicTag.points => Graphic.mkStyled g (f {})
  | GraphicTag.bars => Graphic.mkStyled g (f {})
  | GraphicTag.area => Graphic.mkStyled g (f {})
  | GraphicTag.overlay => Graphic.mkStyled g (f {})
  | GraphicTag.facetH => Graphic.mkStyled g (f {})
  | GraphicTag.facetV => Graphic.mkStyled g (f {})

/-- Set the title for the entire graphic.

```
#eval plot sin |> title "My Sine Plot"
```
-/
def title (t : String) : Graphic → Graphic :=
  updateStyle fun s => { s with title := some t }

/-- Set the chart dimensions.

```
#eval plot sin |> size 600 400
```
-/
def size (w h : Nat) : Graphic → Graphic :=
  updateStyle fun s => { s with width := w, height := h }

/-- Set the x-axis label.

```
#eval plot sin |> xLabel "Time (s)"
```
-/
def xLabel (label : String) : Graphic → Graphic :=
  updateStyle fun s => { s with xLabel := some label }

/-- Set the y-axis label.

```
#eval plot sin |> yLabel "Amplitude"
```
-/
def yLabel (label : String) : Graphic → Graphic :=
  updateStyle fun s => { s with yLabel := some label }

/-- Show or hide the legend.

```
#eval (plot sin + plot cos) |> legend false
```
-/
def legend (visible : Bool) : Graphic → Graphic :=
  updateStyle fun s => { s with showLegend := visible }

end Graphic

/-! ## Rendering Infrastructure -/

/-- Sample a function uniformly over a domain. -/
private def sampleFn (f : Float → Float) (opts : PlotOpts) : Array Json :=
  let (lo, hi) := opts.domain.getD (0.0, 1.0)
  let n := opts.samples
  if n == 0 then #[] else
    (List.range (n + 1)).toArray.map fun i =>
      let x := lo + (hi - lo) * i.toFloat / n.toFloat
      let y := f x
      Json.mkObj [("x", toJson x), ("y", toJson y)]

/-- Internal: Collect all leaf layers from a graphic for rendering. -/
private partial def collectLayers (g : Graphic) (colorIdx : Nat := 0) :
    Array (SeriesDSpecPacked × Array Json) × Nat :=
  match g.tag with
  | GraphicTag.fn =>
    let data := sampleFn g.func g.opts
    let name := g.opts.name.getD s!"series{colorIdx}"
    let color := g.opts.color.getD (colorFromNat colorIdx)
    (#[(SeriesDSpecPacked.mkLine name name color, data)], colorIdx + 1)
  | GraphicTag.area =>
    let data := sampleFn g.func g.opts
    let name := g.opts.name.getD s!"area{colorIdx}"
    let color := g.opts.color.getD (colorFromNat colorIdx)
    (#[(SeriesDSpecPacked.mkArea name name color, data)], colorIdx + 1)
  | GraphicTag.points =>
    let data := g.pts.map fun (x, y) => Json.mkObj [("x", toJson x), ("y", toJson y)]
    let name := g.opts.name.getD s!"scatter{colorIdx}"
    let color := g.opts.color.getD (colorFromNat colorIdx)
    (#[(SeriesDSpecPacked.mkScatter name name color, data)], colorIdx + 1)
  | GraphicTag.bars =>
    let data := g.pts.map fun (x, y) => Json.mkObj [("x", toJson x), ("y", toJson y)]
    let name := g.opts.name.getD s!"bar{colorIdx}"
    let color := g.opts.color.getD (colorFromNat colorIdx)
    (#[(SeriesDSpecPacked.mkBar name name color, data)], colorIdx + 1)
  | GraphicTag.overlay =>
    match g.child1, g.child2 with
    | some g1, some g2 =>
      let (layers1, idx1) := collectLayers g1 colorIdx
      let (layers2, idx2) := collectLayers g2 idx1
      (layers1 ++ layers2, idx2)
    | _, _ => (#[], colorIdx)
  | GraphicTag.styled =>
    match g.child1 with
    | some inner => collectLayers inner colorIdx
    | none => (#[], colorIdx)
  | GraphicTag.facetH => (#[], colorIdx)  -- Handled separately
  | GraphicTag.facetV => (#[], colorIdx)  -- Handled separately

/-- Internal: Extract global style from a graphic. -/
private def getStyle (g : Graphic) : Style :=
  match g.tag with
  | GraphicTag.styled => g.style
  | GraphicTag.fn => {}
  | GraphicTag.points => {}
  | GraphicTag.bars => {}
  | GraphicTag.area => {}
  | GraphicTag.overlay => {}
  | GraphicTag.facetH => {}
  | GraphicTag.facetV => {}

/-- Internal: Check if graphic contains facets. -/
private partial def hasFacets (g : Graphic) : Bool :=
  match g.tag with
  | GraphicTag.facetH => true
  | GraphicTag.facetV => true
  | GraphicTag.styled =>
    match g.child1 with
    | some inner => hasFacets inner
    | none => false
  | GraphicTag.overlay =>
    match g.child1, g.child2 with
    | some g1, some g2 => hasFacets g1 || hasFacets g2
    | some g1, none => hasFacets g1
    | none, some g2 => hasFacets g2
    | none, none => false
  | GraphicTag.fn => false
  | GraphicTag.points => false
  | GraphicTag.bars => false
  | GraphicTag.area => false

/-- Internal: Merge all layer data into unified chart data with named columns. -/
private def mergeChartData (layers : Array (SeriesDSpecPacked × Array Json)) : Array Json :=
  if layers.isEmpty then #[] else
    -- Find the layer with most points to use as the x-axis reference
    let maxLen := layers.foldl (fun acc (_, data) => max acc data.size) 0
    (List.range maxLen).toArray.map fun i =>
      -- Build object from all layers for row i
      let obj := layers.foldl (init := ([] : List (String × Json))) fun acc (series, data) =>
        if h : i < data.size then
          let row := data[i]
          -- Extract x value from first layer only
          let acc' := if acc.isEmpty then
            match row.getObjVal? "x" with
            | .ok x => [("x", x)]
            | .error _ => []
          else acc
          -- Extract y value and store under series name
          match row.getObjVal? "y" with
          | .ok y => acc' ++ [(SeriesDSpecPacked.name series, y)]
          | .error _ => acc'
        else acc
      Json.mkObj obj

/-- Convert a `Graphic` into a `PlotSpec` for HTML rendering. -/
def Graphic.toPlotSpec (g : Graphic) : PlotSpec :=
  let style := getStyle g
  let (layers, _) := collectLayers g
  let chartData := mergeChartData layers
  let series := layers.map (·.1)
  let yKey := if h : series.size > 0 then
    SeriesDSpecPacked.dataKey series[0]!
  else
    "y"
  let yLabel := style.yLabel.orElse (if series.size == 1 then some (SeriesDSpecPacked.name series[0]!) else none)
  {
    chartData := chartData,
    series := series,
    xAxis := some { dataKey := "x", label := style.xLabel },
    yAxis := some { dataKey := yKey, label := yLabel },
    title := style.title,
    width := style.width,
    height := style.height,
    legend := style.showLegend
  }

/-- Render a single (non-faceted) graphic to HTML. -/
private def renderSingle (g : Graphic) : Html :=
  PlotSpec.render (Graphic.toPlotSpec g)

mutual
/-- Render faceted graphics. -/
partial def renderFaceted (g : Graphic) (_direction : String) : Html :=
  let _style := getStyle g
  match g.tag with
  | GraphicTag.facetH =>
    match g.child1, g.child2 with
    | some g1, some g2 =>
      let h1 := render g1
      let h2 := render g2
      let gridStyle := Json.mkObj [
        ("display", Json.str "grid"),
        ("gridTemplateColumns", Json.str "1fr 1fr"),
        ("gap", Json.str "10px")
      ]
      Html.element "div" #[("style", gridStyle)] #[h1, h2]
    | _, _ => Html.text ""
  | GraphicTag.facetV =>
    match g.child1, g.child2 with
    | some g1, some g2 =>
      let h1 := render g1
      let h2 := render g2
      let stackStyle := Json.mkObj [
        ("display", Json.str "flex"),
        ("flexDirection", Json.str "column"),
        ("gap", Json.str "10px")
      ]
      Html.element "div" #[("style", stackStyle)] #[h1, h2]
    | _, _ => Html.text ""
  | GraphicTag.styled =>
    match g.child1 with
    | some inner =>
      let innerHtml := render inner
      match g.style.title with
      | some t =>
        let titleStyle := Json.mkObj [
          ("fontSize", Json.str "16px"),
          ("fontWeight", Json.str "700"),
          ("marginBottom", Json.str "12px")
        ]
        (<div>
          <div style={titleStyle}>{Html.text t}</div>
          {innerHtml}
        </div> : Html)
      | none => innerHtml
    | none => Html.text ""
  | GraphicTag.fn => renderSingle g
  | GraphicTag.points => renderSingle g
  | GraphicTag.bars => renderSingle g
  | GraphicTag.area => renderSingle g
  | GraphicTag.overlay => renderSingle g

/-- Main render function: convert a `Graphic` to `Html`. -/
partial def render (g : Graphic) : Html :=
  if hasFacets g then
    renderFaceted g "horizontal"
  else
    renderSingle g
end

/-! ## HtmlEval Instance -/

/-- This instance allows `#eval g` to render a `Graphic` in the infoview. -/
instance : HtmlEval Graphic where
  eval g := pure (render g)

instance : ToPlotSpec Graphic where
  toPlotSpec := Graphic.toPlotSpec

/-! ## Convenience Aliases -/

/-- Alias for `plot` that's more explicit about being a line chart. -/
abbrev line := @plot

end LeanPlot
