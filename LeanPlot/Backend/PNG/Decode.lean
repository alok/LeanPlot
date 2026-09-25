import LeanPlot.Backend.PNG.Encode
import LeanPlot.Backend.PNG.Inflate

/-
Minimal PNG decoder for the formats `Encode.lean` writes: 8-bit greyscale,
RGB and RGBA, non-interlaced. Every chunk CRC is checked. This is a test aid
(round trips without external tools), not a general-purpose PNG reader.
-/

namespace LeanPlot.PNG

/-- A decoded image. -/
structure Image where
  width : Nat
  height : Nat
  colorType : ColorType
  /-- Row-major pixels, top row first, `width * height * colorType.bpp` bytes. -/
  pixels : ByteArray

/-- Read a big-endian `UInt32` at `i`. -/
@[inline] def readU32BE (b : ByteArray) (i : Nat) : Nat :=
  ((b.get! i).toNat <<< 24) ||| ((b.get! (i+1)).toNat <<< 16) ||| ((b.get! (i+2)).toNat <<< 8) ||| (b.get! (i+3)).toNat

/-- A parsed chunk: type and payload range. -/
structure Chunk where
  type : String
  start : Nat
  len : Nat

/-- Split a PNG file into chunks, verifying the signature and every CRC. -/
def chunks (file : ByteArray) : Except String (Array Chunk) := do
  if file.size < 8 || file.extract 0 8 != signature then throw "png: bad signature"
  let mut pos := 8
  let mut out : Array Chunk := #[]
  let mut fuel := file.size
  while pos < file.size && fuel > 0 do
    fuel := fuel - 1
    if pos + 12 > file.size then throw "png: truncated chunk header"
    let len := readU32BE file pos
    if pos + 12 + len > file.size then throw "png: truncated chunk"
    let type := String.fromUTF8? (file.extract (pos + 4) (pos + 8)) |>.getD "????"
    let crc := readU32BE file (pos + 8 + len)
    if crc != (crc32 file (pos + 4) (pos + 8 + len)).toNat then throw s!"png: CRC mismatch in {type}"
    out := out.push { type, start := pos + 8, len }
    pos := pos + 12 + len
  return out

/-- Decode a PNG written by `encode`. -/
def decode (file : ByteArray) : Except String Image := do
  let cs ← chunks file
  let some hdr := cs[0]? | throw "png: no chunks"
  if hdr.type != "IHDR" || hdr.len != 13 then throw "png: first chunk is not IHDR"
  let w := readU32BE file hdr.start
  let h := readU32BE file (hdr.start + 4)
  let depth := file.get! (hdr.start + 8)
  let some ct := ColorType.ofCode? (file.get! (hdr.start + 9)) | throw "png: unsupported colour type"
  if depth != 8 then throw "png: unsupported bit depth"
  if file.get! (hdr.start + 12) != 0 then throw "png: interlacing unsupported"
  if (cs.back?.map (·.type)) != some "IEND" then throw "png: missing IEND"
  let z := cs.foldl (init := ByteArray.empty) fun acc c =>
    if c.type == "IDAT" then file.copySlice c.start acc acc.size c.len false else acc
  let scan ← Inflate.zlibDecompress z
  let rowLen := w * ct.bpp
  if scan.size != h * (rowLen + 1) then throw s!"png: expected {h * (rowLen + 1)} scanline bytes, got {scan.size}"
  let pixels ← Filter.unfilterRows scan rowLen ct.bpp h
  return { width := w, height := h, colorType := ct, pixels }

/-- Decode and expand to RGBA8. -/
def decodeRGBA (file : ByteArray) : Except String (Nat × Nat × ByteArray) := do
  let img ← decode file
  let n := img.width * img.height
  let px := img.pixels
  let rgba := match img.colorType with
    | .rgba8 => px
    | .rgb8 => Id.run do
      let mut o := ByteArray.emptyWithCapacity (4 * n)
      for i in [0:n] do
        o := (((o.push (px.get! (3*i))).push (px.get! (3*i+1))).push (px.get! (3*i+2))).push 255
      return o
    | .gray8 => Id.run do
      let mut o := ByteArray.emptyWithCapacity (4 * n)
      for i in [0:n] do
        let g := px.get! i
        o := (((o.push g).push g).push g).push 255
      return o
  return (img.width, img.height, rgba)

end LeanPlot.PNG
