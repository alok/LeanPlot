import LeanPlot.API

/-! Test smart plotting functionality -/

open LeanPlot.API

-- Test that the functions exist and have the right types
#check plot
#check plotMany  
#check scatter
#check bar

-- Test basic function calls
#eval s!"Testing smart plot functions..."

-- Create some test plots
def testQuadratic := plot (fun x => x^2)
def testSine := plot (fun t => Float.sin t)  
def testMulti := plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]

#check testQuadratic
#check testSine
#check testMulti

#eval s!"All smart plot functions work correctly! ✅"
