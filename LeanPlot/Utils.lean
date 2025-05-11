import Lean

namespace LeanPlot.Utils

open Lean

/-- Check if a float is NaN or infinite. -/
@[inline]
def isInvalidFloat (f : Float) : Bool :=
  !f.isFinite

/--
Check if any of the specified keys in an array of JSON objects correspond to invalid float values (NaN or Infinity).

Args:
  jsonData: The array of JSON objects to check.
  keysForFloatCheck: An array of strings representing the keys whose values should be checked as floats.
-/
def jsonDataHasInvalidFloats (jsonData : Array Json) (keysForFloatCheck : Array String) : Bool :=
  jsonData.any fun (jsonObject : Json) =>
    keysForFloatCheck.any fun (key : String) =>
      match jsonObject.getObjVal? key with
      | .ok (Json.num n) => isInvalidFloat n.toFloat
      | .error _ => false -- Key not found or not a Json.num, treat as not an invalid float for this check
      | _ => false -- Not a Json.num, treat as not an invalid float

/- -- Corrected Lean block comment
-- Example of how one might check a single series (e.g., for scatter plots or simple line charts)
/--
Check if an array of JSON objects, assumed to have 'x' and 'y' float keys, contains invalid floats.
-/
-- def xyDataHasInvalidFloats (data : Array Json) : Bool :=
--   jsonDataHasInvalidFloats data #["x", "y"]
-/

end LeanPlot.Utils
