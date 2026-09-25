import LeanPlot.Core.Decimal

/-!
A minimal JSON reader for the oracle goldens (no dependency on `Lean.Json`).
Numbers keep their source text and are converted with the correctly rounded
`LeanPlot.Num.parseFloat?`, so goldens round-trip bit-exactly. Non-finite
floats are encoded by the oracle as the strings `"NaN"`, `"Inf"`, `"-Inf"`.
-/

namespace LeanPlotTest.Core

/-- JSON values. -/
inductive J where
  | null
  | bool (b : Bool)
  | num (raw : String)
  | str (s : String)
  | arr (xs : Array J)
  | obj (kvs : Array (String × J))
  deriving Inhabited, Repr

namespace J

/-- Field lookup (objects). -/
def get? (j : J) (k : String) : Option J :=
  match j with
  | obj kvs => (kvs.find? (·.1 == k)).map (·.2)
  | _ => none

/-- Field lookup with `null` default. -/
def get (j : J) (k : String) : J := (j.get? k).getD .null

/-- Array elements (empty for non-arrays). -/
def arrD (j : J) : Array J := match j with | arr xs => xs | _ => #[]

/-- A float from a number or one of the strings `NaN`, `Inf`, `-Inf`. -/
def toFloat? (j : J) : Option Float :=
  match j with
  | num r => LeanPlot.Num.parseFloat? r
  | str s => LeanPlot.Num.parseFloat? s
  | _ => none

/-- Float with NaN default. -/
def float (j : J) : Float := j.toFloat?.getD LeanPlot.Num.nan

/-- Natural number (from a JSON number). -/
def nat (j : J) : Nat := match j with
  | num r => r.toNat?.getD 0
  | str s => s.toNat?.getD 0
  | _ => 0

/-- Integer (from a JSON number or string). -/
def int (j : J) : Int := match j with
  | num r => r.toInt?.getD 0
  | str s => s.toInt?.getD 0
  | _ => 0

/-- String contents. -/
def string (j : J) : String := match j with | str s => s | num r => r | _ => ""

/-- Bool contents. -/
def boolean (j : J) : Bool := match j with | bool b => b | _ => false

/-- Array of floats. -/
def floats (j : J) : Array Float := j.arrD.map float

/-- `true` for `null`. -/
def isNull (j : J) : Bool := match j with | null => true | _ => false

end J

private def isWs (b : UInt8) : Bool := b == 32 || b == 9 || b == 10 || b == 13

private partial def skipWs (s : ByteArray) (i : Nat) : Nat :=
  if h : i < s.size then (if isWs s[i] then skipWs s (i + 1) else i) else i

private def hexVal (b : UInt8) : Nat :=
  if b ≥ 48 && b ≤ 57 then (b - 48).toNat
  else if b ≥ 97 && b ≤ 102 then (b - 87).toNat
  else if b ≥ 65 && b ≤ 70 then (b - 55).toNat
  else 0

/-- Parse a string literal starting after the opening quote. Returns the
decoded string and the index after the closing quote. -/
private partial def parseStr (s : ByteArray) (i : Nat) (acc : ByteArray) : Except String (String × Nat) :=
  if h : i < s.size then
    let b := s[i]
    if b == 34 then
      match String.fromUTF8? acc with
      | some str => .ok (str, i + 1)
      | none => .error "invalid utf8 in string"
    else if b == 92 then
      if h2 : i + 1 < s.size then
        let e := s[i + 1]
        if e == 117 then
          -- \uXXXX (BMP only; the oracle emits UTF-8 directly for non-ASCII)
          let cp := hexVal s[i+2]! * 4096 + hexVal s[i+3]! * 256 + hexVal s[i+4]! * 16 + hexVal s[i+5]!
          let enc := (Char.ofNat cp).toString.toUTF8
          parseStr s (i + 6) (acc ++ enc)
        else
          let c : UInt8 := match e with
            | 110 => 10 | 116 => 9 | 114 => 13 | 98 => 8 | 102 => 12 | x => x
          parseStr s (i + 2) (acc.push c)
      else .error "bad escape"
    else parseStr s (i + 1) (acc.push b)
  else .error "unterminated string"

private def isNumByte (b : UInt8) : Bool :=
  (b ≥ 48 && b ≤ 57) || b == 45 || b == 43 || b == 46 || b == 101 || b == 69

private partial def numEnd (s : ByteArray) (i : Nat) : Nat :=
  if h : i < s.size then (if isNumByte s[i] then numEnd s (i + 1) else i) else i

mutual
private partial def parseVal (s : ByteArray) (i : Nat) : Except String (J × Nat) := do
  let i := skipWs s i
  if h : i < s.size then
    let b := s[i]
    if b == 123 then parseObj s (i + 1) #[]
    else if b == 91 then parseArr s (i + 1) #[]
    else if b == 34 then
      let (str, j) ← parseStr s (i + 1) .empty
      return (.str str, j)
    else if b == 116 then return (.bool true, i + 4)
    else if b == 102 then return (.bool false, i + 5)
    else if b == 110 then return (.null, i + 4)
    else
      let j := numEnd s i
      if j == i then throw s!"unexpected byte {b} at {i}"
      let raw := String.fromUTF8! (s.extract i j)
      return (.num raw, j)
  else throw "unexpected end"

private partial def parseArr (s : ByteArray) (i : Nat) (acc : Array J) : Except String (J × Nat) := do
  let i := skipWs s i
  if h : i < s.size then
    if s[i] == 93 then return (.arr acc, i + 1)
    let (v, j) ← parseVal s i
    let j := skipWs s j
    if h2 : j < s.size then
      if s[j] == 44 then parseArr s (j + 1) (acc.push v)
      else if s[j] == 93 then return (.arr (acc.push v), j + 1)
      else throw s!"expected , or ] at {j}"
    else throw "unexpected end in array"
  else throw "unexpected end in array"

private partial def parseObj (s : ByteArray) (i : Nat) (acc : Array (String × J)) : Except String (J × Nat) := do
  let i := skipWs s i
  if h : i < s.size then
    if s[i] == 125 then return (.obj acc, i + 1)
    if s[i] != 34 then throw s!"expected key at {i}"
    let (k, j) ← parseStr s (i + 1) .empty
    let j := skipWs s j
    if s[j]! != 58 then throw s!"expected : at {j}"
    let (v, j) ← parseVal s (j + 1)
    let j := skipWs s j
    if h2 : j < s.size then
      if s[j] == 44 then parseObj s (j + 1) (acc.push (k, v))
      else if s[j] == 125 then return (.obj (acc.push (k, v)), j + 1)
      else throw s!"expected , or }} at {j}"
    else throw "unexpected end in object"
  else throw "unexpected end in object"
end

/-- Parse a JSON document. -/
def parseJson (s : String) : Except String J := do
  let (v, _) ← parseVal s.toUTF8 0
  return v

/-- Read and parse a JSON file. -/
def readJson (path : System.FilePath) : IO J := do
  let s ← IO.FS.readFile path
  match parseJson s with
  | .ok j => return j
  | .error e => throw (IO.userError s!"{path}: {e}")

end LeanPlotTest.Core
