import LeanPlot.API

open LeanPlot.API

/-! # Test Smart Plotting System

Let's test the new smart plotting functions and see how they look!
-/

-- 🎯 Test 1: Simple quadratic function
#check plot (fun x => x^2)
#eval s!"Test 1: Basic quadratic plot should work"

-- 🎯 Test 2: Time function (should get "time" labels automatically)
#check plot (fun t => Float.sin t)
#eval s!"Test 2: Sine function with time parameter"

-- 🎯 Test 3: Multiple functions with automatic legend and colors
#check plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)]
#eval s!"Test 3: Multi-function plot with automatic legend"

-- 🎯 Test 4: Scatter plot with noise
#check scatter (fun x => x^2 + 0.1 * Float.sin (10 * x)) (steps := 50)
#eval s!"Test 4: Scatter plot with noise pattern"

-- 🎯 Test 5: Bar chart for discrete data
#check bar (fun i => Float.floor (i * 5)) (steps := 10)
#eval s!"Test 5: Bar chart for discrete function"

-- 🎯 Test 6: Custom domain 
#check plot (fun t => Float.exp (-t) * Float.sin (5 * t)) (domain := some (0.0, 3.0))
#eval s!"Test 6: Damped oscillation with custom domain"

-- 🎯 Test 7: High resolution plot
#check plot (fun x => Float.tanh (x - 1)) (steps := 500) (domain := some (-2.0, 4.0))
#eval s!"Test 7: High resolution tanh function"

-- 🎯 Test 8: Complex multi-function comparison
#check plotMany #[
  ("linear", fun x => x),
  ("quadratic", fun x => x^2), 
  ("cubic", fun x => x^3)
] (domain := (-1.0, 1.0))
#eval s!"Test 8: Comparing polynomial functions"

-- Let's also test the old API to make sure it still works
#check lineChart (fun x => x^2)
#eval s!"Test 9: Old API lineChart should still work"

#eval s!"All smart plotting tests defined successfully! 🎉"
