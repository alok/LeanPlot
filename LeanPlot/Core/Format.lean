import LeanPlot.Core.Decimal

/-
Number formatting for output.

* `fmtFixed d x`: the deterministic SVG number formatter: at most `d` decimals,
  trailing zeros trimmed, `-0` normalised to `0`, non-finite values printed as
  `0`. Fast (no big integers); byte-stable across platforms because it only uses
  IEEE arithmetic.
* Tick labels exactly as Makie 0.24 prints them (`Makie/src/tick_format.jl`, a
  vendored subset of Showoff.jl): uniform precision from shortest `Float32`
  digits, `Ryu.writefixed`, U+2212 `−` for minus signs, scientific notation
  (`1.5×10⁻⁵`) when the tick span exceeds four orders of magnitude.
-/

namespace LeanPlot.Num

/-- Unicode minus sign `−` (U+2212), used by Makie for negative tick labels. -/
def minusSign : String := "−"

/-- Powers of ten used by `fmtFixed`. -/
private def pow10Nat : Nat → Nat
  | 0 => 1 | 1 => 10 | 2 => 100 | 3 => 1000 | 4 => 10000 | 5 => 100000 | 6 => 1000000
  | n + 7 => 10000000 * pow10Nat n

/-- Deterministic fixed-point formatter: round `x` to `digits` decimals (ties to
even on `x · 10^digits`), trim trailing zeros and the point, print `-0` as `0`,
and print non-finite values as `0`. Magnitudes are clamped to `10^15`.

`fmtFixed 3 1.23456 = "1.235"`, `fmtFixed 3 2.5 = "2.5"`,
`fmtFixed 3 (-0.0001) = "0"`, `fmtFixed 3 1e-7 = "0"`. -/
def fmtFixed (digits : Nat) (x : Float) : String :=
  if !x.isFinite then "0" else
  let scale := ofInt (pow10Nat digits)
  let y := roundEven (x * scale)
  let lim : Float := 1.0e15 * scale
  let y := if y > lim then lim else if y < -lim then -lim else y
  let n := toIntExact y
  if n == 0 then "0" else
  let a := n.natAbs
  let p := pow10Nat digits
  let ip := a / p
  let fp := a % p
  let sign := if n < 0 then "-" else ""
  if fp == 0 then sign ++ toString ip else
  let fs := toString fp
  let fs := "".pushn '0' (digits - fs.length) ++ fs
  let fs := (fs.dropEndWhile (· == '0')).toString
  sign ++ toString ip ++ "." ++ fs

/-- The SVG coordinate formatter: `fmtFixed 3`. -/
@[inline] def fmtSvg (x : Float) : String := fmtFixed 3 x

/-! ## Tick labels (Makie / Showoff) -/

/-- A tick label. `sup base exponent` renders `base` followed by `exponent` as a
superscript, e.g. `sup "1.5×10" "−5"` for `1.5×10⁻⁵` and `sup "10" "3"` for
log-axis labels; this mirrors Makie's `rich(base, superscript(exponent))`. -/
inductive TickLabel where
  | plain (s : String)
  | sup (base : String) (exponent : String)
  deriving Repr, Inhabited, BEq, DecidableEq

/-- Map a character to its Unicode superscript (digits, `−`, `-`, `+`, `.`). -/
def superscriptChar (c : Char) : Char :=
  match c with
  | '0' => '⁰' | '1' => '¹' | '2' => '²' | '3' => '³' | '4' => '⁴'
  | '5' => '⁵' | '6' => '⁶' | '7' => '⁷' | '8' => '⁸' | '9' => '⁹'
  | '-' => '⁻' | '−' => '⁻' | '+' => '⁺' | '.' => '·'
  | c => c

namespace TickLabel
/-- Flatten to a single string using Unicode superscript glyphs. -/
def toUnicode : TickLabel → String
  | plain s => s
  | sup b e => b ++ String.map superscriptChar e
/-- Flatten to ASCII-ish text (`b^e`), for debugging. -/
def toCaret : TickLabel → String
  | plain s => s
  | sup b e => b ++ "^" ++ e
end TickLabel

/-- Replace a leading `-` by U+2212 (Makie `_replace_leading_hyphen`). -/
def replaceLeadingHyphen (s : String) : String :=
  if s.startsWith "-" then minusSign ++ (s.drop 1).toString else s

/-- Makie `_plain_label_precision` / Showoff `plain_precision_heuristic`:
the smallest number of decimals that shows every value's shortest `Float32`
digits. -/
def plainLabelPrecision (xs : Array Float) : Nat :=
  -- `none` plays the role of Julia's `typemax(Int)` start values
  let step (acc : Option (Int × Int)) (y : Float) : Option (Int × Int) :=
    if !y.isFinite then acc else
    let e10 : Int :=
      if y.abs ≤ 1.0e-16 then (match acc with | some (lo, _) => min lo 0 | none => 0)
      else (shortest32 y.toFloat32).2
    match acc with
    | some (lo, hi) => some (min lo e10, max hi e10)
    | none => some (e10, e10)
  match xs.foldl step none with
  | none => 0
  | some (lo, hi) => (max (min (-lo) (-hi + 16)) 0).toNat

/-- Julia `round(x; sigdigits = n)` (base 10, `RoundNearest`), bit-exact. -/
def roundSigDigits (x : Float) (n : Int) : Float :=
  if !x.isFinite then x else
  let h : Int := if x == fZero then 0 else 1 + floorInt (Float.log10 x.abs)
  let digits := n - h
  if digits ≥ 0 then
    let invstep := powInt fTen digits
    if invstep.isFinite then
      let y := roundEven (x * invstep) / invstep
      if y.isFinite then y else x
    else
      let invstepsqrt := powF fTen (ofInt digits / fTwo)
      let y := roundEven ((x * invstepsqrt) * invstepsqrt) / invstepsqrt / invstepsqrt
      if y.isFinite then y else x
  else
    let step := powInt fTen (-digits)
    let y := roundEven (x / step) * step
    if y.isFinite then y
    else if x > fZero then fZero else if x < fZero then -fZero else x

/-- Makie `_scientific_label_precision`. -/
def scientificLabelPrecision (xs : Array Float) : Nat :=
  let ys := xs.filterMap fun x =>
    if !x.isFinite then none
    else if x == 0 then some 0.0
    else
      let z := Float.log10 x.abs
      some (roundSigDigits (powF 10.0 (z - z.floor)) 15)
  plainLabelPrecision ys

/-- Makie `format_ticks_plain`: uniform-precision fixed notation with `−`. -/
def formatTicksPlain (xs : Array Float) (minus : Bool := true) : Array String :=
  let p := plainLabelPrecision xs
  xs.map fun x => let s := writeFixed x p; if minus then replaceLeadingHyphen s else s

/-- Makie `_pick_label_style`: scientific when the span exceeds four decades. -/
def useScientificLabels (xs : Array Float) : Bool :=
  if xs.isEmpty then false else
  let (lo, hi) := xs.foldl (init := (xs[0]!, xs[0]!)) fun (lo, hi) x => (jmin lo x, jmax hi x)
  hi != lo && (Float.log10 (hi - lo)).abs > 4

/-- Strip trailing zeros after a decimal point (Makie `_strip_trailing_zeros`). -/
def stripTrailingZeros (s : String) : String :=
  if s.contains '.' then
    ((s.dropEndWhile (· == '0')).toString.dropEndWhile (· == '.')).toString
  else s

/-- True when the fractional part of `base` is empty or all zeros. -/
def hasOnlyZeroFraction (base : String) : Bool :=
  match base.splitOn "." with
  | [_] => true
  | [_, fr] => fr.all (· == '0')
  | _ => false

/-- Makie `format_ticks_auto` (the default `Axis` tick formatter): plain labels
unless the values span more than four decades, then `m×10ᵉ` labels with a
uniform mantissa precision (padding zeros stripped only when every mantissa is
a whole number). -/
def formatTicksAuto (xs : Array Float) : Array TickLabel :=
  if !useScientificLabels xs then (formatTicksPlain xs).map .plain else
  let p := scientificLabelPrecision xs
  let pairs : Array (Option (String × Int)) := xs.map fun x =>
    if x == 0 then none else
    let (m, e) := writeExp x p
    some (replaceLeadingHyphen m, e)
  let canStrip := pairs.all fun
    | none => true
    | some (b, _) => hasOnlyZeroFraction b
  pairs.map fun
    | none => .plain "0"
    | some (b, e) =>
      let b := if canStrip then stripTrailingZeros b else b
      let es := if e < 0 then minusSign ++ toString e.natAbs else toString e
      .sup (b ++ "×10") es

/-- Labels for log-axis ticks (Makie `get_ticks(::LogTicks, ...)`): the base
(`"10"`, `"2"`, `"e"`) with the plain-formatted exponent as superscript. -/
def logTickLabels (base : String) (scaledTicks : Array Float) : Array TickLabel :=
  (formatTicksPlain scaledTicks).map fun s => .sup base s

/-- Makie `_decade_label` (pseudolog10 / symlog10 axes): `0`, `10ᵏ` or `−10ᵏ`. -/
def decadeLabel (t : Float) : TickLabel :=
  if t == 0 then .plain "0" else
  let k := roundInt (Float.log10 t.abs)
  .sup ((if t < 0 then minusSign else "") ++ "10") (toString k)

end LeanPlot.Num
