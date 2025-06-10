import LeanPlot.Metaprogramming
import Lean

/-! # Test cases for duplicate parameter name disambiguation -/

namespace LeanPlot.Test.DuplicateNames
open LeanPlot.Metaprogramming Lean

-- Test the disambiguation function directly
#eval disambiguateNames #["x", "y", "x", "z", "x", "y"]
-- Expected: ["x", "y", "x_2", "z", "x_3", "y_2"]

#eval disambiguateNames #["time", "time", "velocity"]
-- Expected: ["time", "time_2", "velocity"]

#eval disambiguateNames #["a", "b", "a", "c", "a"]
-- Expected: ["a", "b", "a_2", "c", "a_3"]

#eval disambiguateNames #["x", "x", "x", "x"]
-- Expected: ["x", "x_2", "x_3", "x_4"]

-- Test with no duplicates
#eval disambiguateNames #["x", "y", "z"]
-- Expected: ["x", "y", "z"]

-- Test with empty array
#eval disambiguateNames #[]
-- Expected: []

-- Test with single element
#eval disambiguateNames #["x"]
-- Expected: ["x"]

-- Test parameter extraction with duplicates using direct functions
def testExpr1 : Expr := Expr.lam `x (Expr.const ``Float []) 
  (Expr.lam `y (Expr.const ``Float []) 
    (Expr.lam `x (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)
    BinderInfo.default) 
  BinderInfo.default

#eval getParameterNames testExpr1
-- Should handle shadowing and return disambiguated names

def testExpr2 : Expr := Expr.lam `time (Expr.const ``Float [])
  (Expr.lam `time (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)
  BinderInfo.default

#eval getParameterNames testExpr2
#eval getAxisLabels testExpr2

-- Test with more complex cases
def testExpr3 : Expr := Expr.lam `a (Expr.const ``Float [])
  (Expr.lam `b (Expr.const ``Float [])
    (Expr.lam `a (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)
    BinderInfo.default)
  BinderInfo.default

#eval getParameterNames testExpr3

/-- Test function to verify disambiguation works correctly -/
def testDisambiguation : IO Unit := do
  let test1 := disambiguateNames #["x", "y", "x"]
  let expected1 := #["x", "y", "x_2"]
  if test1 = expected1 then
    IO.println "✓ Test 1 passed: Basic disambiguation"
  else
    IO.println s!"✗ Test 1 failed: Expected {expected1}, got {test1}"
  
  let test2 := disambiguateNames #["a", "a", "a"]
  let expected2 := #["a", "a_2", "a_3"]
  if test2 = expected2 then
    IO.println "✓ Test 2 passed: Multiple duplicates"
  else
    IO.println s!"✗ Test 2 failed: Expected {expected2}, got {test2}"
  
  let test3 := disambiguateNames #["unique"]
  let expected3 := #["unique"]
  if test3 = expected3 then
    IO.println "✓ Test 3 passed: No duplicates"
  else
    IO.println s!"✗ Test 3 failed: Expected {expected3}, got {test3}"

-- Run the test
#eval testDisambiguation

end LeanPlot.Test.DuplicateNames
