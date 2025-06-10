import LeanPlot.Metaprogramming

/-! # Test for Metaprogramming utilities -/

namespace LeanPlot.Test.Metaprogramming
open LeanPlot.Metaprogramming

-- Test simple lambda
def testFunction1 : ℝ → ℝ := fun x => x + 1

-- Test lambda with two parameters
def testFunction2 : ℝ → ℝ → ℝ := fun x y => x + y

-- Test lambda with meaningful parameter names
def testFunction3 : ℝ → ℝ := fun time => time * 2

-- Test the parameter extraction
#eval extractParameterNames (Expr.lam `x (Expr.const ``Nat []) (Expr.bvar 0) BinderInfo.default)

-- Test with actual function expressions  
#check testFunction1
#check testFunction2
#check testFunction3

-- Test the syntax macros
example : String := #extract_param_names (fun x => x + 1)
example : String := #extract_param_names (fun time velocity => time * velocity)

-- Test auto axis labels
example : String × String := #auto_axis_labels (fun x => x + 1)
example : String × String := #auto_axis_labels (fun time velocity => time * velocity)

end LeanPlot.Test.Metaprogramming
