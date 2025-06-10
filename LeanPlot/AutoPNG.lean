import ProofWidgets.Component.HtmlDisplay
import LeanPlot.API
import LeanPlot.Debug

open Lean ProofWidgets
open scoped ProofWidgets.Jsx
open LeanPlot.API
open LeanPlot.Debug

/-! # Automated PNG Export for Smart Plots

This module provides enhanced PNG export functionality that automatically
captures smart plots with meaningful filenames based on the function type.
-/

namespace LeanPlot.AutoPNG

/-- Automatically generate a PNG-saveable plot with smart naming -/
def smartPlotWithPNG (f : Float → Float) (name : String := "plot") 
    (steps : Nat := 200) (domain : Option (Float × Float) := none) : Html :=
  let plotHtml := plot f steps domain
  let fileName := s!"{name}_smart_plot.png"
  let targetId := s!"smart-plot-{name}"
  withSavePNG plotHtml targetId fileName

/-- Automatically generate a PNG-saveable multi-plot with smart naming -/
def smartMultiPlotWithPNG (fns : Array (String × (Float → Float))) (name : String := "multiplot")
    (steps : Nat := 200) (domain : Float × Float := (0.0, 1.0)) : Html :=
  let plotHtml := plotMany fns steps domain
  let fileName := s!"{name}_smart_multiplot.png"
  let targetId := s!"smart-multiplot-{name}"
  withSavePNG plotHtml targetId fileName

/-- Batch create multiple plots with PNG export -/
def createPlotBatch (plots : Array (String × (Float → Float))) : Array Html :=
  plots.mapIdx fun i (name, f) =>
    let targetId := s!"batch-plot-{i}"
    let fileName := s!"{name}_batch_plot.png"
    withSavePNG (plot f) targetId fileName

/-! ## Test Examples -/

-- Example 1: Single plot with automatic PNG export
def testQuadratic := smartPlotWithPNG (fun x => x^2) "quadratic"

-- Example 2: Multi-function plot with automatic PNG export  
def testTrigFunctions := smartMultiPlotWithPNG 
  #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)] 
  "trigonometric"

-- Example 3: Batch of plots
def testBatch := createPlotBatch #[
  ("linear", fun x => x),
  ("quadratic", fun x => x^2),
  ("cubic", fun x => x^3),
  ("exponential", fun x => Float.exp x)
]

#check testQuadratic
#check testTrigFunctions  
#check testBatch

end LeanPlot.AutoPNG
