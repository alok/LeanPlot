import LeanPlot.Font

/-!
Test helpers for the font suite: a pass/fail counter, a minimal JSON reader (the package has
no dependencies, and the reference data is produced by `scripts/font/gen.py` and
`scripts/font/oracle.jl`), and a deterministic SVG path writer for goldens.
-/

namespace LeanPlotTest.Font

open LeanPlot

/-- Pass/fail tally with the first few failure messages. -/
structure Tally where
  /-- Checks that passed. -/
  passed : Nat := 0
  /-- Checks that failed. -/
  failed : Nat := 0
  /-- Failure messages (the first 40 are printed). -/
  msgs : Array String := #[]

namespace Tally

/-- Record one check. -/
def check (t : Tally) (ok : Bool) (msg : String) : Tally :=
  if ok then { t with passed := t.passed + 1 }
  else { t with failed := t.failed + 1, msgs := if t.msgs.size < 40 then t.msgs.push msg else t.msgs }

/-- Record one check whose message is only built on failure. -/
@[inline] def check' (t : Tally) (ok : Bool) (msg : Unit → String) : Tally :=
  if ok then { t with passed := t.passed + 1 } else t.check false (msg ())

/-- Combine two tallies. -/
def merge (a b : Tally) : Tally :=
  { passed := a.passed + b.passed, failed := a.failed + b.failed, msgs := a.msgs ++ b.msgs }

/-- Print a one-line summary (and failures) under `name`; return `(passed, failed)`. -/
def report (t : Tally) (name : String) : IO (Nat × Nat) := do
  IO.println s!"  font/{name}: {t.passed} passed, {t.failed} failed"
  for m in t.msgs do IO.println s!"    FAIL {m}"
  return (t.passed, t.failed)

end Tally

/-- π. -/
def pi : Float := 3.141592653589793

/-- `|a - b| ≤ tol`. -/
@[inline] def near (a b tol : Float) : Bool := Float.abs (a - b) ≤ tol

/-! ## Minimal JSON -/

/-- JSON values. -/
inductive Json where
  | null
  | bool (b : Bool)
  | num (x : Float)
  | str (s : String)
  | arr (xs : Array Json)
  | obj (kvs : Array (String × Json))
  deriving Inhabited

namespace Json

/-- Field of an object (`null` when absent or not an object). -/
def get (j : Json) (k : String) : Json :=
  match j with
  | .obj kvs => (kvs.find? (·.1 == k)).map (·.2) |>.getD .null
  | _ => .null

/-- Element of an array (`null` when out of range). -/
def idx (j : Json) (i : Nat) : Json :=
  match j with
  | .arr xs => xs[i]?.getD .null
  | _ => .null

/-- Number value (`NaN` otherwise). -/
def num! (j : Json) : Float :=
  match j with
  | .num x => x
  | _ => 0.0 / 0.0

/-- String value (`""` otherwise). -/
def str! (j : Json) : String :=
  match j with
  | .str s => s
  | _ => ""

/-- Array elements (empty otherwise). -/
def arr! (j : Json) : Array Json :=
  match j with
  | .arr xs => xs
  | _ => #[]

/-- Object entries (empty otherwise). -/
def obj! (j : Json) : Array (String × Json) :=
  match j with
  | .obj kvs => kvs
  | _ => #[]

/-- Whether the value is `null`. -/
def isNull (j : Json) : Bool :=
  match j with
  | .null => true
  | _ => false

private def isWs (b : UInt8) : Bool := b == 32 || b == 9 || b == 10 || b == 13

private partial def skipWs (s : ByteArray) (i : Nat) : Nat :=
  if i < s.size && isWs s[i]! then skipWs s (i + 1) else i

private partial def digits (s : ByteArray) (i : Nat) (acc : Nat) (n : Nat) : Nat × Nat × Nat :=
  if i < s.size && s[i]! ≥ 48 && s[i]! ≤ 57 then digits s (i + 1) (acc * 10 + (s[i]! - 48).toNat) (n + 1)
  else (acc, n, i)

private def parseNum (s : ByteArray) (i : Nat) : Except String (Json × Nat) := do
  let neg := i < s.size && s[i]! == 45
  let i := if neg then i + 1 else i
  let (ip, n, i) := digits s i 0 0
  if n == 0 then throw s!"bad number at {i}"
  let (m, fd, i) :=
    if i < s.size && s[i]! == 46 then
      let (f, k, j) := digits s (i + 1) ip 0
      (f, k, j)
    else (ip, 0, i)
  let (e, i) :=
    if i < s.size && (s[i]! == 101 || s[i]! == 69) then
      let j := i + 1
      let eneg := j < s.size && s[j]! == 45
      let j := if j < s.size && (s[j]! == 45 || s[j]! == 43) then j + 1 else j
      let (ev, _, j) := digits s j 0 0
      ((if eneg then -(ev : Int) else ev), j)
    else ((0 : Int), i)
  let ex : Int := e - fd
  let x := if ex ≥ 0 then Float.ofScientific m false ex.toNat else Float.ofScientific m true (-ex).toNat
  return (.num (if neg then -x else x), i)

private partial def parseStr (s : ByteArray) (i : Nat) (acc : ByteArray) : Except String (String × Nat) :=
  if i ≥ s.size then throw "unterminated string"
  else
    let c := s[i]!
    if c == 34 then
      match String.fromUTF8? acc with
      | some t => pure (t, i + 1)
      | none => throw "invalid UTF-8 in string"
    else if c == 92 then
      if i + 1 ≥ s.size then throw "bad escape" else
      let e := s[i + 1]!
      if e == 117 then
        -- \uXXXX (BMP only; surrogate pairs are not produced by our generators)
        let hex := (s.extract (i + 2) (i + 6)).toList.map (fun b => Char.ofNat b.toNat)
        let v := hex.foldl (fun a ch => a * 16 + (if ch.isDigit then ch.toNat - 48
          else if ch ≥ 'a' then ch.toNat - 87 else ch.toNat - 55)) 0
        parseStr s (i + 6) (acc ++ (String.singleton (Char.ofNat v)).toUTF8)
      else
        let r : UInt8 := match e with
          | 110 => 10 | 116 => 9 | 114 => 13 | 98 => 8 | 102 => 12 | x => x
        parseStr s (i + 2) (acc.push r)
    else parseStr s (i + 1) (acc.push c)

private partial def parseVal (s : ByteArray) (i : Nat) : Except String (Json × Nat) := do
  let i := skipWs s i
  if i ≥ s.size then throw "unexpected end"
  let c := s[i]!
  if c == 123 then parseObj s (skipWs s (i + 1)) #[]
  else if c == 91 then parseArr s (skipWs s (i + 1)) #[]
  else if c == 34 then
    let (t, j) ← parseStr s (i + 1) .empty
    return (.str t, j)
  else if c == 116 then return (.bool true, i + 4)
  else if c == 102 then return (.bool false, i + 5)
  else if c == 110 then return (.null, i + 4)
  else parseNum s i
where
  parseArr (s : ByteArray) (i : Nat) (acc : Array Json) : Except String (Json × Nat) := do
    if i < s.size && s[i]! == 93 then return (.arr acc, i + 1)
    let (v, j) ← parseVal s i
    let j := skipWs s j
    if j < s.size && s[j]! == 44 then parseArr s (skipWs s (j + 1)) (acc.push v)
    else if j < s.size && s[j]! == 93 then return (.arr (acc.push v), j + 1)
    else throw s!"expected , or ] at {j}"
  parseObj (s : ByteArray) (i : Nat) (acc : Array (String × Json)) : Except String (Json × Nat) := do
    if i < s.size && s[i]! == 125 then return (.obj acc, i + 1)
    unless i < s.size && s[i]! == 34 do throw s!"expected key at {i}"
    let (k, j) ← parseStr s (i + 1) .empty
    let j := skipWs s j
    unless j < s.size && s[j]! == 58 do throw s!"expected : at {j}"
    let (v, j) ← parseVal s (j + 1)
    let j := skipWs s j
    if j < s.size && s[j]! == 44 then parseObj s (skipWs s (j + 1)) (acc.push (k, v))
    else if j < s.size && s[j]! == 125 then return (.obj (acc.push (k, v)), j + 1)
    else throw s!"expected , or }} at {j}"

/-- Parse a JSON document. -/
def parse (text : String) : Except String Json := do
  let (v, _) ← parseVal text.toUTF8 0
  return v

/-- Read and parse a JSON file. -/
def readFile (path : System.FilePath) : IO Json := do
  match parse (← IO.FS.readFile path) with
  | .ok j => return j
  | .error e => throw <| IO.userError s!"{path}: {e}"

end Json

/-! ## Deterministic SVG path data -/

/-- Fixed-point decimal with `digits` fractional digits, trailing zeros trimmed, `-0` → `0`. -/
def fmt (x : Float) (digits : Nat := 2) : String :=
  let scale := Float.ofNat (10 ^ digits)
  let r := Float.round (x * scale)
  let neg := r < 0
  let n := (Float.abs r).toUInt64.toNat
  let ip := n / 10 ^ digits
  let fp := n % 10 ^ digits
  let fs := (toString fp)
  let fs := String.ofList (List.replicate (digits - fs.length) '0') ++ fs
  let fs := (fs.toList.reverse.dropWhile (· == '0')).reverse
  let body := if fs.isEmpty then toString ip else s!"{ip}.{String.ofList fs}"
  if neg && n != 0 then "-" ++ body else body

/-- SVG `d` attribute for a path (absolute commands, 2 decimals). -/
def pathD (p : Path) : String :=
  go 0 0 ""
where
  go (vi ci : Nat) (acc : String) : String :=
    if h : vi < p.verbs.size then
      let v := Verb.ofUInt8 p.verbs[vi]
      let n := v.arity
      let pts := (List.range n).map fun k => fmt p.coords[ci + k]!
      let cmd := match v with
        | .moveTo => "M" | .lineTo => "L" | .quadTo => "Q" | .cubicTo => "C" | .close => "Z"
      let acc := acc ++ cmd ++ " ".intercalate pts
      go (vi + 1) (ci + n) acc
    else acc
  termination_by p.verbs.size - vi

/-- Path to test data, relative to the package root (tests run from there). -/
def dataPath (name : String) : System.FilePath := "LeanPlotTest" / "Font" / "data" / name

/-- Path to golden files, relative to the package root. -/
def goldenPath (name : String) : System.FilePath := "LeanPlotTest" / "Font" / "golden" / name

end LeanPlotTest.Font
