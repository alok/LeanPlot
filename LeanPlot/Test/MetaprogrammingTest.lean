import LeanPlot.Metaprogramming
import Lean

/-! # Test for Metaprogramming utilities -/

namespace LeanPlot.Test.Metaprogramming
open LeanPlot.Metaprogramming
open Lean

-- Test simple lambda with Float
def testFunction1 : Float → Float := fun x => x + 1

-- Test lambda with two parameters
def testFunction2 : Float → Float → Float := fun x y => x + y

-- Test lambda with meaningful parameter names
def testFunction3 : Float → Float := fun time => time * 2

-- Test with actual function expressions
#check testFunction1
#check testFunction2
#check testFunction3

-- Test name to string
#eval nameToString `time
#eval nameToString `x
#eval nameToString `velocity

-- Test duplicate handling
#eval disambiguateNames #["x", "y", "x"]
#eval disambiguateNames #["time", "time", "velocity"]

-- Test axis labels generation
#eval smartLabels myTimeFunction
#eval smartNames myDuplicateFunction

end LeanPlot.Test.Metaprogramming
