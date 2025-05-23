import LeanPlot.Specification
import LeanPlot.ToFloat
import LeanPlot.Palette
import LeanPlot.Scale
import Lean.Data.Json

/-! # LeanPlot.GrammarOfGraphics

This module provides a functional DSL for building plots using a grammar-of-graphics
approach, inspired by ggplot2 and similar libraries.

The design philosophy emphasizes functional composition over builder patterns,
leveraging Lean's partial application and function composition operators.
-/

namespace LeanPlot.GrammarOfGraphics

open LeanPlot
open Lean

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

/-- A geometry (geom) represents how data is rendered -/
inductive Geom where
  | Point
  | Line
  | Bar
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

/-- Add a title to the plot -/
@[inline] def withTitle (pb : PlotBuilder) (title : String) : PlotBuilder :=
  { pb with spec := pb.spec.withTitle title }

/-- Set the plot dimensions -/
@[inline] def withSize (pb : PlotBuilder) (width height : Nat) : PlotBuilder :=
  { pb with spec := pb.spec.withSize width height }

/-- Toggle legend visibility -/
@[inline] def withLegend (pb : PlotBuilder) (showLegend : Bool := true) : PlotBuilder :=
  { pb with spec := pb.spec.withLegend showLegend }

/-- Set x-axis label -/
@[inline] def withXLabel (pb : PlotBuilder) (label : String) : PlotBuilder :=
  { pb with spec := pb.spec.withXLabel label }

/-- Set y-axis label -/
@[inline] def withYLabel (pb : PlotBuilder) (label : String) : PlotBuilder :=
  { pb with spec := pb.spec.withYLabel label }

/-- Set x-axis domain -/
@[inline] def withXDomain (pb : PlotBuilder) (min max : Float) : PlotBuilder :=
  { pb with spec := pb.spec.withXDomain min max }

/-- Set y-axis domain -/
@[inline] def withYDomain (pb : PlotBuilder) (min max : Float) : PlotBuilder :=
  { pb with spec := pb.spec.withYDomain min max }

/-- Set global aesthetic mappings -/
@[inline] def aes (pb : PlotBuilder) (x : String) (y : String) : PlotBuilder :=
  { pb with globalAes := { x := some x, y := some y } }

/-- Add a layer with custom data and geometry -/
@[inline] def addLayer (pb : PlotBuilder) (layer : Layer) : PlotBuilder :=
  { pb with layers := pb.layers.push layer }

/-- Add a point layer -/
@[inline] def addPoints (pb : PlotBuilder) (data : Array Json) (name : String := "points") : PlotBuilder :=
  pb.addLayer { data := data, geom := Geom.Point, name := some name }

/-- Add a line layer -/
@[inline] def addLine (pb : PlotBuilder) (data : Array Json) (name : String := "line") : PlotBuilder :=
  pb.addLayer { data := data, geom := Geom.Line, name := some name }

/-- Add a bar layer -/
@[inline] def addBars (pb : PlotBuilder) (data : Array Json) (name : String := "bars") : PlotBuilder :=
  pb.addLayer { data := data, geom := Geom.Bar, name := some name }

/-- Add an area layer -/
@[inline] def addArea (pb : PlotBuilder) (data : Array Json) (name : String := "area") : PlotBuilder :=
  pb.addLayer { data := data, geom := Geom.Area, name := some name }

/-- Add an existing PlotSpec as a layer -/
@[inline] def addSpec (pb : PlotBuilder) (spec : PlotSpec) : PlotBuilder :=
  { pb with spec := pb.spec.overlay spec }

/-- Set logarithmic scale on x-axis -/
@[inline] def logX (pb : PlotBuilder) (base : Float := 10.0) : PlotBuilder :=
  let newConfig := match pb.scaleConfig with
    | some cfg => { cfg with xScale := Scale.ScaleType.Logarithmic base }
    | none => { xScale := Scale.ScaleType.Logarithmic base, yScale := Scale.ScaleType.Linear }
  { pb with scaleConfig := some newConfig }

/-- Set logarithmic scale on y-axis -/
@[inline] def logY (pb : PlotBuilder) (base : Float := 10.0) : PlotBuilder :=
  let newConfig := match pb.scaleConfig with
    | some cfg => { cfg with yScale := Scale.ScaleType.Logarithmic base }
    | none => { xScale := Scale.ScaleType.Linear, yScale := Scale.ScaleType.Logarithmic base }
  { pb with scaleConfig := some newConfig }

/-- Build the final PlotSpec from the builder -/
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

/-- Add a line layer to an existing plot -/
@[inline] def addLine {β} [ToFloat β] (spec : PlotSpec)
    (f : Float → β) (name : String) (color : Option String := none) : PlotSpec :=
  let newSpec := LeanPlot.line f name 200 none color
  spec.overlay newSpec

/-- Add a scatter layer to an existing plot -/
@[inline] def addScatter (spec : PlotSpec)
    (points : Array (Float × Float)) (name : String) (color : Option String := none) : PlotSpec :=
  let newSpec := LeanPlot.scatter points name color
  spec.overlay newSpec

/-- Add a bar layer to an existing plot -/
@[inline] def addBar (spec : PlotSpec)
    (points : Array (Float × Float)) (name : String) (color : Option String := none) : PlotSpec :=
  let newSpec := LeanPlot.bar points name color
  spec.overlay newSpec

/-- Set logarithmic scale on x-axis -/
@[inline] def logX (spec : PlotSpec) (base : Float := 10.0) : PlotSpec :=
  -- For now, we just return the spec unchanged since scale transformations
  -- need to be implemented in the renderer
  spec

/-- Set logarithmic scale on y-axis -/
@[inline] def logY (spec : PlotSpec) (base : Float := 10.0) : PlotSpec :=
  -- For now, we just return the spec unchanged since scale transformations
  -- need to be implemented in the renderer
  spec

/-- Compose multiple functions into a multi-line plot -/
@[inline] def plotLines {β} [Inhabited β] [ToFloat β]
    (fns : Array (String × (Float → β)))
    (steps : Nat := 200)
    (domain : Option (Float × Float) := none) : PlotSpec :=
  LeanPlot.lines fns steps domain

/-- Alternative syntax using sections for cleaner composition -/
notation:50 x:50 " >> " f:51 => f x

end LeanPlot.GrammarOfGraphics
