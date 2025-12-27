import Lean
import LeanPlot.CLI.Export

open Lean
open LeanPlot.CLI

/--
`leanplot-export` — tiny CLI to dump sampled function data as JSON.

Usage:
  leanplot-export --fn sin --out out.json (--steps 200) (--min 0.0) (--max 1.0)
-/
def main (args : List String) : IO Unit := do
  -- Minimal float parser supporting `-? [0-9]+ (.[0-9]+)?`.
  let parseFloat? (s : String) : Option Float :=
    let (neg, body) := if s.startsWith "-" then (true, (s.drop 1).toString) else (false, s)
    let parts := body.splitOn "."
    match parts with
    | [a] =>
      match a.toNat? with
      | some n => some (if neg then - (n.toFloat) else n.toFloat)
      | none   => none
    | [a,b] =>
      match a.toNat?, b.toNat? with
      | some i, some f =>
        let rec pow10 (k : Nat) : Float :=
          match k with
          | 0 => 1.0
          | Nat.succ k' => 10.0 * pow10 k'
        let denom := pow10 b.length
        let v := Nat.toFloat i + Nat.toFloat f / denom
        some (if neg then -v else v)
      | _, _ => none
    | _ => none
  let rec get (k : String) (xs : List String) : Option String :=
    match xs with
    | [] => none
    | a :: b :: rest => if a == k then some b else get k (b :: rest)
    | _ => none

  let some fnStr := get "--fn" args
    | throw <| IO.userError "--fn <sin|cos|tan|linear|quadratic|cubic|exp|tanh> is required"
  let some outPath := get "--out" args
    | throw <| IO.userError "--out <file.json> is required"
  let steps : Nat := ((get "--steps" args).bind String.toNat?).getD 200
  let min   : Float := ((get "--min" args).bind parseFloat?).getD 0.0
  let max   : Float := ((get "--max" args).bind parseFloat?).getD 1.0

  let some fnName := FnName.fromString fnStr
    | throw <| IO.userError s!"Unknown function name: {fnStr}"

  let rows := sampleNamed fnName steps min max
  let json := encodeJson rows
  IO.FS.writeFile outPath json
  IO.println s!"wrote {outPath} ({rows.size} samples)"
