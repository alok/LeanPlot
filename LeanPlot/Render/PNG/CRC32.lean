/-!
# CRC32 for PNG Chunks

Pure Lean implementation of CRC32 checksum used in PNG files.
Uses the standard CRC32 polynomial 0xEDB88320 (reflected).
-/

namespace LeanPlot.Render.PNG

/-- CRC32 polynomial (reflected form) -/
def crc32Polynomial : UInt32 := 0xEDB88320

/-- Generate CRC32 lookup table entry -/
def makeTableEntry (n : UInt8) : UInt32 :=
  let rec loop (crc : UInt32) (k : Nat) : UInt32 :=
    match k with
    | 0 => crc
    | k' + 1 =>
      let newCrc := if crc &&& 1 == 1
                    then (crc >>> 1) ^^^ crc32Polynomial
                    else crc >>> 1
      loop newCrc k'
  loop n.toUInt32 8

/-- Pre-computed CRC32 lookup table (256 entries) -/
def crc32Table : Array UInt32 :=
  Array.ofFn (fun i : Fin 256 => makeTableEntry i.val.toUInt8)

/-- Update CRC32 with a single byte -/
@[inline]
def updateCrcByte (crc : UInt32) (byte : UInt8) : UInt32 :=
  let index := ((crc ^^^ byte.toUInt32) &&& 0xFF).toUInt8.toNat
  (crc >>> 8) ^^^ crc32Table[index]!

/-- Update CRC32 with a ByteArray -/
def updateCrc32 (crc : UInt32) (data : ByteArray) : UInt32 :=
  data.foldl updateCrcByte crc

/-- Compute CRC32 of a ByteArray -/
def crc32 (data : ByteArray) : UInt32 :=
  let finalCrc := updateCrc32 0xFFFFFFFF data
  finalCrc ^^^ 0xFFFFFFFF

end LeanPlot.Render.PNG
