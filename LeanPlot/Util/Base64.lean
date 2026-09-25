/-
Base64 (RFC 4648, standard alphabet, `=` padding).

Used to embed PNG rasters in SVG as `data:image/png;base64,…` URIs. Both
directions are total. `decode` returns `none` on malformed input and is used
by the round-trip tests.
-/

namespace LeanPlot.Base64

/-- The 64-character standard alphabet `A–Z a–z 0–9 + /`. -/
def alphabet : String := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/-- Alphabet as bytes, for O(1) lookup. -/
private def alphabetBytes : ByteArray := alphabet.toUTF8

@[inline] private def sym (v : UInt32) : Char :=
  Char.ofNat (alphabetBytes.get! (v &&& 63).toNat).toNat

/-- Length of the Base64 encoding of `n` bytes (with padding). -/
def encodedLength (n : Nat) : Nat := 4 * ((n + 2) / 3)

/-- Encode bytes as padded Base64. -/
def encode (data : ByteArray) : String :=
  let n := data.size
  let rec go (i : Nat) (acc : String) : String :=
    if i + 3 ≤ n then
      let v := (data.get! i).toUInt32 <<< 16 ||| (data.get! (i+1)).toUInt32 <<< 8 ||| (data.get! (i+2)).toUInt32
      go (i + 3) ((((acc.push (sym (v >>> 18))).push (sym (v >>> 12))).push (sym (v >>> 6))).push (sym v))
    else if i + 2 = n then
      let v := (data.get! i).toUInt32 <<< 16 ||| (data.get! (i+1)).toUInt32 <<< 8
      (((acc.push (sym (v >>> 18))).push (sym (v >>> 12))).push (sym (v >>> 6))).push '='
    else if i + 1 = n then
      let v := (data.get! i).toUInt32 <<< 16
      (((acc.push (sym (v >>> 18))).push (sym (v >>> 12))).push '=').push '='
    else acc
  termination_by n - i
  go 0 ""

/-- Value of a Base64 symbol, `64` for `=`, `255` for anything else. -/
@[inline] private def value (c : UInt8) : UInt8 :=
  if c ≥ 65 && c ≤ 90 then c - 65
  else if c ≥ 97 && c ≤ 122 then c - 71
  else if c ≥ 48 && c ≤ 57 then c + 4
  else if c == 43 then 62
  else if c == 47 then 63
  else if c == 61 then 64
  else 255

/-- Decode padded Base64. ASCII whitespace is ignored; any other
non-alphabet byte, bad padding, or a length that is not a multiple of 4
yields `none`. -/
def decode (s : String) : Option ByteArray :=
  let raw := s.toUTF8
  -- strip whitespace
  let clean := raw.foldl (init := ByteArray.emptyWithCapacity raw.size) fun acc c =>
    if c == 32 || c == 10 || c == 13 || c == 9 then acc else acc.push c
  let n := clean.size
  if n % 4 != 0 then none else
  let rec go (i : Nat) (out : ByteArray) : Option ByteArray :=
    if i + 4 ≤ n then
      let a := value (clean.get! i); let b := value (clean.get! (i+1))
      let c := value (clean.get! (i+2)); let d := value (clean.get! (i+3))
      let last := i + 4 = n
      if a ≥ 64 || b ≥ 64 || c == 255 || d == 255 then none
      else if c == 64 then
        -- `xx==` : one byte, only at the end
        if d != 64 || !last then none
        else go (i + 4) (out.push ((a <<< 2) ||| (b >>> 4)))
      else if d == 64 then
        if !last then none
        else go (i + 4) ((out.push ((a <<< 2) ||| (b >>> 4))).push ((b <<< 4) ||| (c >>> 2)))
      else
        go (i + 4) (((out.push ((a <<< 2) ||| (b >>> 4))).push ((b <<< 4) ||| (c >>> 2))).push ((c <<< 6) ||| d))
    else some out
  termination_by n - i
  go 0 (ByteArray.emptyWithCapacity (n / 4 * 3))

/-- `data:<mime>;base64,<payload>` URI for embedding (e.g. PNG in SVG `<image>`). -/
def dataURI (mime : String) (data : ByteArray) : String :=
  "data:" ++ mime ++ ";base64," ++ encode data

end LeanPlot.Base64
