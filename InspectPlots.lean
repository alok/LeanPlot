import LeanPlot.API
import Lean.Data.Json

open LeanPlot.API

/-! Inspect the structure of smart plots -/

-- Create some plots and inspect their structure
def quadPlot := plot (fun x => x^2)
def sinePlot := plot (fun t => Float.sin t)
def multiPlot := plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

-- Check that they are HTML elements
#check quadPlot  -- Should be ProofWidgets.Html
#check sinePlot 
#check multiPlot

-- Test the data generation
def sampleData := LeanPlot.Components.sample (fun x : Float => x^2) 10 none
#eval sampleData

def multiSampleData := LeanPlot.Components.sampleMany #[("sin", fun x : Float => Float.sin x), ("cos", fun x : Float => Float.cos x)] 10 0.0 1.0
#eval multiSampleData

#eval s!"Smart plotting structure tests complete! ✅"
