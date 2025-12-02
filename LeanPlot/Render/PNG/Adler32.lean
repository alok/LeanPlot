/-!
# Adler-32 Checksum

Adler-32 checksum used in zlib compression for PNG files.
-/

namespace LeanPlot.Render.PNG

/-- The Adler-32 modulus: largest prime less than 2^16 -/
def adler32Mod : UInt32 := 65521

/-- Compute Adler-32 checksum -/
def adler32 (data : ByteArray) : UInt32 :=
  let rec loop (i : Nat) (s1 s2 : UInt32) : UInt32 × UInt32 :=
    if h : i < data.size then
      let byte := data[i]
      let s1' := (s1 + byte.toUInt32) % adler32Mod
      let s2' := (s2 + s1') % adler32Mod
      loop (i + 1) s1' s2'
    else (s1, s2)
  termination_by data.size - i

  let (s1, s2) := loop 0 1 0
  (s2 <<< 16) ||| s1

end LeanPlot.Render.PNG
