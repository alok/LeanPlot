import LeanPlot.JsonExt
import Lean.Data.Json

open Lean Json

/-- A sample Json object for testing `assert_keys`. -/
def sampleJson : Json :=
  mkObj [("name", "test"), ("value", Json.num 10), ("active", Json.bool true)] -- Ensure Json.num for numbers

/-- Another sample Json object for testing. -/
def anotherJson : Json :=
  mkObj [("x", Json.num 1.0), ("y", Json.num 2.5)] -- Explicitly use Json.num

-- Test case that should pass
#assert_keys sampleJson #["name", "value"]

-- Test case that should also pass (all keys present)
#assert_keys sampleJson #["name", "value", "active"]

-- Test case for a different Json object
#assert_keys anotherJson #["x", "y"]

-- This test case *should* ideally fail at compile time if uncommented,
-- because "extraKey" is not in sampleJson.
-- To verify failure, one would uncomment this, run `lake build`,
-- and expect a compile-time error from the #assert_keys macro.

-- #assert_keys sampleJson #["name", "extraKey"]

/-- An empty Json object for testing edge cases. -/
def emptyJson : Json := mkObj []
#assert_keys emptyJson #[]

-- Test with an empty Json and non-empty keys array (should fail if uncommented)
-- #assert_keys emptyJson #["a"]

/-- A non-object Json value for testing edge cases. -/
def nonObjectJson : Json := Json.num 5
#assert_keys nonObjectJson #[] -- Should pass

-- #assert_keys nonObjectJson #["key"] -- Should fail if uncommented

/-- Main entry point for the `jsonKeyCheckTest` executable.
    Runs compile-time assertions and prints a success message. -/
def main : IO Unit :=
  IO.println "JsonKeyCheck tests passed (those not commented out)."

-- The #eval is fine for interactive testing but not for an exe root.
-- #eval IO.println "JsonKeyCheck tests passed (those not commented out)."
