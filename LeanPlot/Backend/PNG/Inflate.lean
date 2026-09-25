import LeanPlot.Backend.PNG.Checksum
import LeanPlot.Backend.PNG.Deflate

/-
Minimal DEFLATE / zlib decoder (stored, fixed and dynamic blocks), after
Mark Adler's `puff.c`. It exists so that the test-suite can round-trip our own
encoder without external tools; it favours clarity over speed (canonical
Huffman decoding one bit at a time).
-/

namespace LeanPlot.PNG.Inflate

open LeanPlot.PNG.Deflate (lenBase lenExtra distBase distExtra clOrder fixedLitLens fixedDistLens)

/-- Bit reader state. The bit buffer is a `UInt64` (at most 23 bits are
pending): `Nat.shiftLeft` goes through GMP on every call. -/
structure Reader where
  data : ByteArray
  pos : Nat := 0
  bitbuf : UInt64 := 0
  bitcnt : UInt64 := 0

/-- Canonical Huffman decoding table in puff's `count`/`symbol` form. -/
structure Table where
  count : Array Nat   -- codes per length, index 0‥15
  symbol : Array Nat  -- symbols ordered by code

/-- Decoder monad: state + error. -/
abbrev M := StateT Reader (Except String)

/-- Read `k ≤ 16` bits LSB-first. -/
def bits (k : Nat) : M Nat := do
  let mut r ← get
  let k64 := k.toUInt64
  while r.bitcnt < k64 do
    if r.pos ≥ r.data.size then throw "inflate: unexpected end of input"
    r := { r with bitbuf := r.bitbuf ||| ((r.data.get! r.pos).toUInt64 <<< r.bitcnt),
                  pos := r.pos + 1, bitcnt := r.bitcnt + 8 }
  let v := r.bitbuf &&& (((1 : UInt64) <<< k64) - 1)
  set { r with bitbuf := r.bitbuf >>> k64, bitcnt := r.bitcnt - k64 }
  return v.toNat

/-- Build a decoding table from code lengths. -/
def mkTable (lens : Array Nat) : Table := Id.run do
  let mut count : Array Nat := Array.replicate 16 0
  for l in lens do count := count.modify l (· + 1)
  let mut offs : Array Nat := Array.replicate 16 0
  for l in [1:15] do offs := offs.set! (l + 1) (offs[l]! + count[l]!)
  let mut symbol : Array Nat := Array.replicate lens.size 0
  for s in [0:lens.size] do
    let l := lens[s]!
    if l != 0 then
      symbol := symbol.set! offs[l]! s
      offs := offs.modify l (· + 1)
  return { count := count.set! 0 0, symbol }

/-- Decode one symbol. -/
def decode (t : Table) : M Nat := do
  let mut code := 0
  let mut first := 0
  let mut index := 0
  for len in [1:16] do
    code := code ||| (← bits 1)
    let c := t.count[len]!
    if code < first + c then return t.symbol[index + (code - first)]!
    index := index + c
    first := (first + c) * 2
    code := code * 2
  throw "inflate: invalid Huffman code"

/-- Decode the compressed data of one block into `out`. -/
def codes (out : ByteArray) (lit dist : Table) : M ByteArray := do
  let mut out := out
  let mut fuel := 1099511627776
  while fuel > 0 do
    fuel := fuel - 1
    let sym ← decode lit
    if sym < 256 then out := out.push sym.toUInt8
    else if sym == 256 then return out
    else
      let k := sym - 257
      if k ≥ 29 then throw "inflate: bad length symbol"
      let len := lenBase[k]! + (← bits lenExtra[k]!)
      let ds ← decode dist
      if ds ≥ 30 then throw "inflate: bad distance symbol"
      let d := distBase[ds]! + (← bits distExtra[ds]!)
      if d > out.size then throw "inflate: distance too far back"
      let start := out.size - d
      for j in [0:len] do
        out := out.push (out.get! (start + j))
  return out

/-- Decode a raw DEFLATE stream. Returns the output and the reader (positioned
after the final block, byte-aligned). -/
def inflateRaw (data : ByteArray) (pos : Nat := 0) : Except String (ByteArray × Nat) := do
  let prog : M ByteArray := do
    let mut out := ByteArray.empty
    let mut last := false
    while !last do
      last := (← bits 1) == 1
      let ty ← bits 2
      if ty == 0 then
        -- stored: drop to byte boundary
        let r ← get
        set { r with bitbuf := 0, bitcnt := 0 }
        let len ← bits 16
        let nlen ← bits 16
        if len != (nlen ^^^ 0xFFFF) then throw "inflate: stored length mismatch"
        let r ← get
        if r.pos + len > r.data.size then throw "inflate: stored block overruns input"
        out := r.data.copySlice r.pos out out.size len false
        set { r with pos := r.pos + len }
      else if ty == 1 then
        out ← codes out (mkTable fixedLitLens) (mkTable fixedDistLens)
      else if ty == 2 then
        let nlen := (← bits 5) + 257
        let ndist := (← bits 5) + 1
        let ncode := (← bits 4) + 4
        if nlen > 286 || ndist > 30 then throw "inflate: bad counts"
        let mut cl : Array Nat := Array.replicate 19 0
        for k in [0:ncode] do cl := cl.set! clOrder[k]! (← bits 3)
        let clt := mkTable cl
        let mut lens : Array Nat := #[]
        while lens.size < nlen + ndist do
          let sym ← decode clt
          if sym < 16 then lens := lens.push sym
          else
            let (v, rep) ←
              if sym == 16 then do
                if lens.isEmpty then throw "inflate: repeat with no previous length"
                pure (lens[lens.size - 1]!, 3 + (← bits 2))
              else if sym == 17 then pure (0, 3 + (← bits 3))
              else pure (0, 11 + (← bits 7))
            if lens.size + rep > nlen + ndist then throw "inflate: too many lengths"
            for _ in [0:rep] do lens := lens.push v
        out ← codes out (mkTable (lens.extract 0 nlen)) (mkTable (lens.extract nlen (nlen + ndist)))
      else throw "inflate: invalid block type"
    return out
  let (out, r) ← prog.run { data, pos }
  return (out, r.pos)

/-- Decode a zlib stream and verify its header and Adler-32. -/
def zlibDecompress (data : ByteArray) : Except String ByteArray := do
  if data.size < 6 then throw "zlib: too short"
  let cmf := data.get! 0
  let flg := data.get! 1
  if cmf &&& 0x0F != 8 then throw "zlib: not deflate"
  if (cmf.toNat * 256 + flg.toNat) % 31 != 0 then throw "zlib: bad header check"
  if flg &&& 0x20 != 0 then throw "zlib: preset dictionary unsupported"
  let (out, pos) ← inflateRaw data 2
  if pos + 4 > data.size then throw "zlib: missing Adler-32"
  let a := ((data.get! pos).toUInt32 <<< 24) ||| ((data.get! (pos+1)).toUInt32 <<< 16) |||
           ((data.get! (pos+2)).toUInt32 <<< 8) ||| (data.get! (pos+3)).toUInt32
  if a != LeanPlot.PNG.adler32 out then throw "zlib: Adler-32 mismatch"
  return out

end LeanPlot.PNG.Inflate
