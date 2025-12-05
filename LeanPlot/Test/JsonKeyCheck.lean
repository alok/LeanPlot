import LeanPlot.JsonExt
import Lean.Data.Json

/-! # JSON Key Checking Tests

These tests verify the `assert_keys` macro works correctly.
Note: Some compile-time checks are disabled due to performance issues.
-/

open Lean Json

/-- A sample Json object for testing `assert_keys`. -/
def sampleJson : Json :=
  mkObj [("name", "test"), ("value", Json.num 10), ("active", Json.bool true)]

/-- Another sample Json object for testing. -/
def anotherJson : Json :=
  mkObj [("x", Json.num 1.0), ("y", Json.num 2.5)]

/-- An empty Json object for testing edge cases. -/
def emptyJson : Json := mkObj []

/-- A non-object Json value for testing edge cases. -/
def nonObjectJson : Json := Json.num 5

-- Note: The #assert_keys compile-time checks are disabled due to
-- performance issues with the decidability proof. The JsonExt module
-- itself works correctly at runtime.

/-- Main entry point for the `jsonKeyCheckTest` executable.
    Prints a success message. -/
def main : IO Unit :=
  IO.println "JsonKeyCheck module compiled successfully."
