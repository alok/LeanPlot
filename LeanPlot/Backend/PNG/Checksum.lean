/-
CRC-32 (PNG chunks) and Adler-32 (zlib trailer).

Salvaged from the v0 encoder (`LeanPlot/Render/PNG/{CRC32,Adler32}.lean` on
`archive/v0-recharts`, verified there against `crc32("IEND") = 0xAE426082` and
`adler32("Wikipedia") = 0x11E60398`), rewritten as tail-recursive loops over
ranges. Adler-32 now defers the modulo to every 5552 bytes, which is zlib's
NMAX (the largest block for which the 32-bit sums cannot overflow).
-/

namespace LeanPlot.PNG

/-- Reflected CRC-32 polynomial used by PNG and zlib. -/
def crc32Polynomial : UInt32 := 0xEDB88320

/-- One CRC-32 table entry: eight shift/xor rounds of `n`. -/
def crcTableEntry (n : Nat) : UInt32 :=
  let rec loop (c : UInt32) : Nat → UInt32
    | 0 => c
    | k + 1 => loop (if c &&& 1 == 1 then (c >>> 1) ^^^ crc32Polynomial else c >>> 1) k
  loop n.toUInt32 8

/-- The 256-entry CRC-32 lookup table. -/
def crc32Table : Array UInt32 := Array.ofFn fun (i : Fin 256) => crcTableEntry i.val

/-- Feed `data[start, stop)` into a running (pre-inverted) CRC register. -/
def crc32Update (crc : UInt32) (data : ByteArray) (start : Nat := 0) (stop : Nat := data.size) : UInt32 :=
  let stop := min stop data.size
  let tbl := crc32Table
  let rec go (i : Nat) (c : UInt32) : UInt32 :=
    if i < stop then
      let idx := ((c ^^^ (data.get! i).toUInt32) &&& 0xFF).toNat
      go (i + 1) ((c >>> 8) ^^^ tbl[idx]!)
    else c
  termination_by stop - i
  go start crc

/-- CRC-32 of `data[start, stop)` (init `0xFFFFFFFF`, final xor `0xFFFFFFFF`). -/
def crc32 (data : ByteArray) (start : Nat := 0) (stop : Nat := data.size) : UInt32 :=
  crc32Update 0xFFFFFFFF data start stop ^^^ 0xFFFFFFFF

/-- The Adler-32 modulus, the largest prime below 2¹⁶. -/
def adlerMod : UInt32 := 65521

/-- Adler-32 of `data`. The modulo is deferred to every 5552 bytes (zlib's NMAX). -/
def adler32 (data : ByteArray) : UInt32 :=
  let n := data.size
  -- inner: sum a block without reducing
  let rec block (i stop : Nat) (a b : UInt32) : UInt32 × UInt32 :=
    if i < stop then
      let a := a + (data.get! i).toUInt32
      block (i + 1) stop a (b + a)
    else (a, b)
  termination_by stop - i
  let rec outer (i : Nat) (a b : UInt32) : UInt32 :=
    if i < n then
      let stop := min n (i + 5552)
      let (a, b) := block i stop a b
      outer stop (a % adlerMod) (b % adlerMod)
    else (b <<< 16) ||| a
  termination_by n - i
  decreasing_by omega
  outer 0 1 0

end LeanPlot.PNG
