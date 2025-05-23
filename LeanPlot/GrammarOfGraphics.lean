import LeanPlot.Specification
import LeanPlot.ToFloat
import LeanPlot.Palette
import LeanPlot.Scale
import Lean.Data.Json

/-! # LeanPlot.GrammarOfGraphics

This module provides a fluent DSL for building plots using a grammar-of-graphics
approach, inspired by ggplot2 and similar libraries. The core idea is to build
plots through composition of layers, scales, and aesthetic mappings.
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

/-- Add a title to the plot -/
@[inline] def withTitle (pb : PlotBuilder) (title : String) : PlotBuilder :=
  { pb with spec := { pb.spec with title := some title } }

/-- Set the plot dimensions -/
@[inline] def withSize (pb : PlotBuilder) (width height : Nat) : PlotBuilder :=
  { pb with spec := { pb.spec with width := width, height := height } }

/-- Toggle legend visibility -/
@[inline] def withLegend (pb : PlotBuilder) (showLegend : Bool := true) : PlotBuilder :=
  { pb with spec := { pb.spec with legend := showLegend } }

/-- Set global aesthetic mappings -/
@[inline] def aes (pb : PlotBuilder) (x : String) (y : String) : PlotBuilder :=
  { pb with globalAes := { x := some x, y := some y } }

/-- Add a point layer -/
@[inline] def addPoints (pb : PlotBuilder) (data : Array Json) (name : String := "points") : PlotBuilder :=
  { pb with layers := pb.layers.push { data := data, geom := Geom.Point, name := some name } }

/-- Add a line layer -/
@[inline] def addLine (pb : PlotBuilder) (data : Array Json) (name : String := "line") : PlotBuilder :=
  { pb with layers := pb.layers.push { data := data, geom := Geom.Line, name := some name } }

/-- Add a bar layer -/
@[inline] def addBars (pb : PlotBuilder) (data : Array Json) (name : String := "bars") : PlotBuilder :=
  { pb with layers := pb.layers.push { data := data, geom := Geom.Bar, name := some name } }

/-- Add an area layer -/
@[inline] def addArea (pb : PlotBuilder) (data : Array Json) (name : String := "area") : PlotBuilder :=
  { pb with layers := pb.layers.push { data := data, geom := Geom.Area, name := some name } }

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

-- Infix operators for the DSL

/-- Forward pipe operator for fluent API -/
infixl:50 " |> " => Function.comp

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

end LeanPlot.GrammarOfGraphics
