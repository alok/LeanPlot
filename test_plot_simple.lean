import LeanPlot.DSL

-- Test the plot function
#html plot (fun x => x^2)

-- Test with multiple samples
#html plot (fun x => Float.sin x) (steps := 300)

-- Test with explicit function
#html plot (fun t => t^3 - t)

-- Check if HTML rendering works directly
#html LeanPlot.API.plot (fun x => x^2)