import Lean
import Lean.Data.Json
import LeanPlot.Components

open Lean
open LeanPlot.Components

namespace LeanPlot.CLI

/-- Supported demo functions for CLI export. -/
inductive FnName
| sin | cos | tan | linear | quadratic | cubic | exp | tanh
deriving Repr, DecidableEq

namespace FnName

def fromString (s : String) : Option FnName :=
  match s.toLower with
  | "sin"       => some .sin
  | "cos"       => some .cos
  | "tan"       => some .tan
  | "linear"    => some .linear
  | "quad"      => some .quadratic
  | "quadratic" => some .quadratic
  | "cubic"     => some .cubic
  | "exp"       => some .exp
  | "tanh"      => some .tanh
  | _            => none

def toString : FnName → String
| .sin       => "sin"
| .cos       => "cos"
| .tan       => "tan"
| .linear    => "linear"
| .quadratic => "quadratic"
| .cubic     => "cubic"
| .exp       => "exp"
| .tanh      => "tanh"

end FnName

/-- Return the actual function corresponding to a `FnName`. -/
def fnOf : FnName → (Float → Float)
| .sin       => fun x => Float.sin x
| .cos       => fun x => Float.cos x
| .tan       => fun x => Float.tan x
| .linear    => fun x => x
| .quadratic => fun x => x * x
| .cubic     => fun x => x * x * x
| .exp       => fun x => Float.exp x
| .tanh      => fun x => Float.tanh x

/-- Sample a named function and return an array of JSON rows with fields `x` and `y`. -/
def sampleNamed (name : FnName) (steps : Nat) (min max : Float) : Array Json :=
  sample (fnOf name) steps (domainOpt := some (min, max))

/-- Pretty JSON for an array of rows. -/
def encodeJson (rows : Array Json) : String :=
  let j := Json.arr rows
  j.pretty 2

end LeanPlot.CLI
