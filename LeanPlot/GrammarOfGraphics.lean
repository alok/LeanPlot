import LeanPlot.Specification
import LeanPlot.ToFloat
import LeanPlot.Palette
import LeanPlot.Scale
import Lean.Data.Json
import LeanPlot.Faceting
import ProofWidgets.Component.HtmlDisplay

/-! # LeanPlot.GrammarOfGraphics

This module provides a functional DSL for building plots using a grammar-of-graphics
approach, inspired by ggplot2 and similar libraries.

The design philosophy emphasizes functional composition over builder patterns,
leveraging Lean's partial application and function composition operators.
-/

namespace LeanPlot.GrammarOfGraphics

open LeanPlot
open Lean
open ProofWidgets

/-- Aesthetic mapping for a plot layer -/
structure Aesthetic where
  /-- X-axis mapping -/
  x : Option String := none
  /-- Y-axis mapping -/
  y : Option String := none
  /-- Color mapping -/
  color : Option String := none
  /-- Size mapping -/
  size : Option String := none
  deriving Inhabited

/-- Geometric primitive ("geom") describing how data points should be drawn. -/
inductive Geom where
  /-- Render each data point as an individual marker (typically a
  filled circle) without connecting lines.  Use this for classical
  scatter plots. -/
  | Point
  /-- Connect successive data points with straight line segments.
  Suitable for time series, function plots, and any data where an
  implicit ordering along the *x* axis communicates continuity. -/
  | Line
  /-- Display each observation as a vertical or horizontal bar whose
  length encodes the data's value.  Ideal for histograms and discrete
  comparisons. -/
  | Bar
  /-- Fill the area under a curve (or between successive points) to
  emphasise cumulative magnitude.  Useful for density plots and
  confidence bands. -/
  | Area
  deriving Repr, Inhabited

/-- A layer combines data, aesthetics, and geometry -/
structure Layer where
  /-- The data for this layer -/
  data : Array Json := #[]
  /-- Aesthetic mappings -/
  aes : Aesthetic := {}
  /-- Geometry type -/
  geom : Geom := Geom.Line
  /-- Optional name for the layer -/
  name : Option String := none
  deriving Inhabited

/-- Builder pattern for plot construction -/
structure PlotBuilder where
  /-- Base plot specification -/
  spec : PlotSpec := {}
  /-- Layers to compose -/
  layers : Array Layer := #[]
  /-- Global aesthetic mappings -/
  globalAes : Aesthetic := {}
  /-- Scale configurations -/
  scaleConfig : Option Scale.ScaleConfig := none
  deriving Inhabited

namespace PlotBuilder

/-- Create a new plot builder -/
@[inline] def new : PlotBuilder := {}

/-- Create a plot builder from an existing PlotSpec -/
@[inline] def fromSpec (spec : PlotSpec) : PlotBuilder := { spec := spec }

-- Extended projection notation functions
-- These allow syntax like: myPlot.title "My Title"

/-- Set title using projection notation -/
@[inline] def title (title : String) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := { pb.spec with title := title } }

/-- Set dimensions using projection notation -/
@[inline] def size (width height : Nat) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := { pb.spec with width := width, height := height } }

/-- Set legend visibility using projection notation -/
@[inline] def legend (showLegend : Bool) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := { pb.spec with legend := showLegend } }

/-- Set x-axis label using projection notation -/
@[inline] def xLabel (label : String) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := pb.spec.withXLabel label }

/-- Set y-axis label using projection notation -/
@[inline] def yLabel (label : String) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := pb.spec.withYLabel label }

/-- Set x-axis domain using projection notation -/
@[inline] def xDomain (min max : Float) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := pb.spec.withXDomain min max }

/-- Set y-axis domain using projection notation -/
@[inline] def yDomain (min max : Float) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := pb.spec.withYDomain min max }

/-- Set global aesthetic mappings using projection notation -/
@[inline] def aes (x : String) (y : String) (pb : PlotBuilder) : PlotBuilder :=
  { pb with globalAes := { x := some x, y := some y } }

/-- Add a layer using projection notation -/
@[inline] def layer (layer : Layer) (pb : PlotBuilder) : PlotBuilder :=
  { pb with layers := pb.layers.push layer }

/-- Add points using projection notation -/
@[inline] def points (data : Array Json) (name : String := "points") (pb : PlotBuilder) : PlotBuilder :=
  pb.layer { data := data, geom := Geom.Point, name := some name }

/-- Add a line using projection notation -/
@[inline] def line (data : Array Json) (name : String := "line") (pb : PlotBuilder) : PlotBuilder :=
  pb.layer { data := data, geom := Geom.Line, name := some name }

/-- Add bars using projection notation -/
@[inline] def bars (data : Array Json) (name : String := "bars") (pb : PlotBuilder) : PlotBuilder :=
  pb.layer { data := data, geom := Geom.Bar, name := some name }

/-- Add an area using projection notation -/
@[inline] def area (data : Array Json) (name : String := "area") (pb : PlotBuilder) : PlotBuilder :=
  pb.layer { data := data, geom := Geom.Area, name := some name }

/-- Overlay an existing PlotSpec using projection notation -/
@[inline] def overlay (spec : PlotSpec) (pb : PlotBuilder) : PlotBuilder :=
  { pb with spec := pb.spec.overlay spec }

/-- Set logarithmic scale on x-axis using projection notation -/
@[inline] def logX (base : Float := 10.0) (pb : PlotBuilder) : PlotBuilder :=
  let newConfig := match pb.scaleConfig with
    | some cfg => { cfg with xScale := Scale.ScaleType.Logarithmic base }
    | none => { xScale := Scale.ScaleType.Logarithmic base, yScale := Scale.ScaleType.Linear }
  { pb with scaleConfig := some newConfig }

/-- Set logarithmic scale on y-axis using projection notation -/
@[inline] def logY (base : Float := 10.0) (pb : PlotBuilder) : PlotBuilder :=
  let newConfig := match pb.scaleConfig with
    | some cfg => { cfg with yScale := Scale.ScaleType.Logarithmic base }
    | none => { xScale := Scale.ScaleType.Linear, yScale := Scale.ScaleType.Logarithmic base }
  { pb with scaleConfig := some newConfig }

/-- Build the final PlotSpec -/
@[inline] def build (pb : PlotBuilder) : PlotSpec :=
  -- Merge all layer data if needed
  let allData := pb.layers.foldl (init := pb.spec.chartData) fun acc layer =>
    if layer.data.isEmpty then acc else acc ++ layer.data

  -- Convert layers to series
  let series := pb.layers.filterMap fun layer =>
    layer.name.map fun name =>
      let layerType := match layer.geom with
        | Geom.Point => "scatter"
        | Geom.Line => "line"
        | Geom.Bar => "bar"
        | Geom.Area => "area"
      {
        name := name,
        dataKey := name,
        color := Palette.colorFromNat pb.spec.series.size,
        type := layerType
      }

  { pb.spec with
    chartData := allData,
    series := pb.spec.series ++ series }

end PlotBuilder

-- Standalone functions for direct PlotSpec manipulation

/-- Create a plot from a function using the DSL -/
@[inline] def plot {β} [ToFloat β] (f : Float → β) : PlotBuilder :=
  let spec := LeanPlot.line f
  { spec := spec }

/-- Create a scatter plot using the DSL -/
@[inline] def scatterPlot (points : Array (Float × Float)) : PlotBuilder :=
  let spec := LeanPlot.scatter points
  { spec := spec }

/-- Create a bar plot using the DSL -/
@[inline] def barPlot (points : Array (Float × Float)) : PlotBuilder :=
  let spec := LeanPlot.bar points
  { spec := spec }

/-- Create an area plot using the DSL -/
@[inline] def areaPlot {β} [ToFloat β] (f : Float → β) : PlotBuilder :=
  let spec := LeanPlot.area f
  { spec := spec }

/-- Transform a function into a line plot -/
@[inline] def plotLine {β} [ToFloat β] (f : Float → β)
    (name : String := "y") (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : PlotSpec :=
  LeanPlot.line f name steps domain

/-- Transform points into a scatter plot -/
@[inline] def plotScatter (points : Array (Float × Float))
    (name : String := "y") : PlotSpec :=
  LeanPlot.scatter points name

/-- Transform points into a bar plot -/
@[inline] def plotBar (points : Array (Float × Float))
    (name : String := "y") : PlotSpec :=
  LeanPlot.bar points name

/-- Transform a function into an area plot -/
@[inline] def plotArea {β} [ToFloat β] (f : Float → β)
    (name : String := "y") (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : PlotSpec :=
  LeanPlot.area f name steps domain

/-- Compose multiple functions into a multi-line plot -/
@[inline] def plotLines {β} [Inhabited β] [ToFloat β]
    (fns : Array (String × (Float → β)))
    (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : PlotSpec :=
  LeanPlot.lines fns steps domain

/-- Alternative syntax using sections for cleaner composition -/
notation:50 x:50 " >> " f:51 => f x

/-! ## Faceting helpers --------------------------------------------------- -/

namespace Facet

/-- Build a faceted grid from an `Array` of `PlotBuilder`s.  Each builder is
first turned into a concrete `PlotSpec` via `PlotBuilder.build` before
delegating to `LeanPlot.Faceting.facetGrid`.  The optional `cols` argument
controls the number of columns in the resulting grid (default 2). -/
@[inline] def grid (pbs : Array PlotBuilder) (cols : Nat := 2) : Html :=
  LeanPlot.Faceting.facetGrid (pbs.map (·.build)) cols

/-- Like `Facet.grid` but additionally associates a caption/title with each
subplot.  Supply an `Array` of `(title, PlotBuilder)` tuples. -/
@[inline] def gridNamed (pbs : Array (String × PlotBuilder)) (cols : Nat := 2) : Html :=
  let arr : Array (String × PlotSpec) := pbs.map (fun (t, pb) => (t, pb.build))
  LeanPlot.Faceting.facetGridNamed arr cols

end Facet

/-!
### Infix operator `⫽` (U+2AFD) for quick faceting

We define a *right‐associative* operator that chains `PlotBuilder`s into a
faceted grid.  Example:

```
open LeanPlot.GrammarOfGraphics

def p1 := plot (fun x : Float => x)          -- y = x
def p2 := plot (fun x : Float => x * x)      -- y = x²
def p3 := plot (fun x : Float => x * x * x)  -- y = x³

#html p1 ⫽ p2 ⫽ p3  -- three small plots in one row
```

The operator collects the chained plots into an `Array` and finally produces
an `Html` grid (defaults to two columns; override with `Facet.grid`).
-/

/-- Internal accumulator used by the faceting operator `⫽`.  A
`_FacetChain` simply gathers the `PlotBuilder`s that should end up in
the same faceted grid.  Users never construct this type manually; it
is created implicitly via coercions from `PlotBuilder`. -/
structure _FacetChain where
  /-- The growing list of plots that will populate the resulting grid. -/
  acc : Array PlotBuilder := #[]

/-- Concatenate two faceting chains by appending their accumulated
`PlotBuilder`s.
This is the underlying implementation of the `⫽` infix operator. -/
@[inline] def _FacetChain.concat (c₁ c₂ : _FacetChain) : _FacetChain :=
  { acc := c₁.acc ++ c₂.acc }

/-- Faceting operator `⫽` (U+2AFD).  Right‐associative (precedence 65). -/
infixr:65 " ⫽ " => _FacetChain.concat

/-- Coerce a faceting chain into an `Html` grid (default 2 columns). -/
instance : Coe _FacetChain Html where
  coe c := Facet.grid c.acc 2

/-- Coerce a single `PlotBuilder` into a `_FacetChain`. -/
@[inline] instance : Coe PlotBuilder _FacetChain where
  coe pb := { acc := #[pb] }

/-- Evaluate a faceting chain directly inside `#html` blocks. -/
instance : ProofWidgets.HtmlEval _FacetChain where
  eval c := pure (Facet.grid c.acc 2)

/-! ### Live demo --------------------------------------------------------- -/

#html (
  plot (fun x : Float => x)
  ⫽ plot (fun x : Float => x * x)
  ⫽ plot (fun x : Float => x * x * x)
)

end LeanPlot.GrammarOfGraphics
