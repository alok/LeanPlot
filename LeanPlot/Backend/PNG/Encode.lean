import LeanPlot.Backend.PNG.Checksum
import LeanPlot.Backend.PNG.Deflate
import LeanPlot.Backend.PNG.Filter
import LeanPlot.Util.Base64

/-
PNG container: signature, IHDR, IDAT, IEND (PNG spec §5, §11).

The chunk layout is salvaged from the v0 encoder. Differences: the scanlines
are filtered adaptively and compressed with real DEFLATE (`Deflate.lean`)
instead of stored blocks, and the IDAT payload is split into 1 MiB chunks.
-/

namespace LeanPlot.PNG

/-- Pixel formats we write (8 bits per channel, no palette, no interlace). -/
inductive ColorType where
  /-- Greyscale, 1 byte per pixel (colour type 0). -/
  | gray8
  /-- Truecolour, 3 bytes per pixel (colour type 2). -/
  | rgb8
  /-- Truecolour with straight alpha, 4 bytes per pixel (colour type 6). -/
  | rgba8
  deriving Repr, Inhabited, BEq, DecidableEq

namespace ColorType
/-- Bytes per pixel. -/
def bpp : ColorType → Nat
  | gray8 => 1 | rgb8 => 3 | rgba8 => 4
/-- The IHDR colour-type code. -/
def code : ColorType → UInt8
  | gray8 => 0 | rgb8 => 2 | rgba8 => 6
/-- Inverse of `code` for the formats we support. -/
def ofCode? : UInt8 → Option ColorType
  | 0 => some gray8 | 2 => some rgb8 | 6 => some rgba8 | _ => none
end ColorType

/-- Encoder options. -/
structure Options where
  /-- DEFLATE effort. -/
  level : Deflate.Level := .default
  /-- Scanline filter policy. -/
  filter : Filter.Strategy := .adaptive
  /-- Use stored (uncompressed) DEFLATE blocks, as the v0 encoder did. -/
  stored : Bool := false
  deriving Inhabited

/-- Append a big-endian `UInt32`. -/
@[inline] def pushU32BE (b : ByteArray) (v : UInt32) : ByteArray :=
  (((b.push (v >>> 24).toUInt8).push (v >>> 16).toUInt8).push (v >>> 8).toUInt8).push v.toUInt8

/-- The 8-byte PNG signature. -/
def signature : ByteArray := ⟨#[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]⟩

/-- Append a chunk `length ‖ type ‖ data[start, start+len) ‖ CRC(type ‖ data)`. -/
def pushChunk (out : ByteArray) (type : String) (data : ByteArray) (start : Nat := 0)
    (len : Nat := data.size) : ByteArray :=
  let tb := type.toUTF8
  let out := pushU32BE out len.toUInt32
  let out := out ++ tb
  let out := data.copySlice start out out.size len false
  let crc := crc32Update (crc32Update 0xFFFFFFFF tb) data start (start + len) ^^^ 0xFFFFFFFF
  pushU32BE out crc

/-- IHDR payload. -/
def ihdr (w h : Nat) (ct : ColorType) : ByteArray :=
  let b := pushU32BE (pushU32BE .empty w.toUInt32) h.toUInt32
  ((((b.push 8).push ct.code).push 0).push 0).push 0

/-- Maximum IDAT chunk payload we emit. -/
def idatChunk : Nat := 1 <<< 20

/-- Encode raw row-major pixels (`w * h * bpp` bytes, top row first) as PNG.

The PNG spec forbids a zero width or height in IHDR, so for `w = 0` or
`h = 0` the result is well formed (valid chunks, CRCs and zlib stream) but
strict decoders such as libpng reject it; `decode` here accepts it. -/
def encode (w h : Nat) (ct : ColorType) (pixels : ByteArray) (opts : Options := {}) : ByteArray :=
  let rowLen := w * ct.bpp
  let scan := Filter.filterRows pixels rowLen ct.bpp h (if opts.stored then .fixed .none else opts.filter)
  let z := if opts.stored then Deflate.zlibStored scan else Deflate.zlibCompress scan opts.level
  let out := pushChunk (signature ++ .empty) "IHDR" (ihdr w h ct)
  let nIdat := max 1 ((z.size + idatChunk - 1) / idatChunk)
  let rec idats (k : Nat) (out : ByteArray) : ByteArray :=
    if k < nIdat then
      let s := k * idatChunk
      idats (k + 1) (pushChunk out "IDAT" z s (min idatChunk (z.size - s)))
    else out
  termination_by nIdat - k
  pushChunk (idats 0 out) "IEND" .empty

/-- Encode straight-alpha RGBA8 pixels. -/
def encodeRGBA (w h : Nat) (rgba : ByteArray) (opts : Options := {}) : ByteArray :=
  encode w h .rgba8 rgba opts

/-- Encode RGB8 pixels. -/
def encodeRGB (w h : Nat) (rgb : ByteArray) (opts : Options := {}) : ByteArray :=
  encode w h .rgb8 rgb opts

/-- Drop the alpha channel of RGBA8 pixels. -/
def rgbaToRGB (rgba : ByteArray) : ByteArray :=
  let n := rgba.size / 4
  let rec go (i : Nat) (out : ByteArray) : ByteArray :=
    if i < n then
      go (i + 1) (((out.push (rgba.get! (4*i))).push (rgba.get! (4*i+1))).push (rgba.get! (4*i+2)))
    else out
  termination_by n - i
  go 0 (ByteArray.emptyWithCapacity (3 * n))

/-- Is every alpha byte of RGBA8 pixels `255`? -/
def isOpaque (rgba : ByteArray) : Bool :=
  let n := rgba.size / 4
  let rec go (i : Nat) : Bool :=
    if i < n then rgba.get! (4*i + 3) == 255 && go (i + 1) else true
  termination_by n - i
  go 0

/-- Encode RGBA8 pixels, writing RGB8 when every pixel is opaque. -/
def encodeAuto (w h : Nat) (rgba : ByteArray) (opts : Options := {}) : ByteArray :=
  if isOpaque rgba then encodeRGB w h (rgbaToRGB rgba) opts else encodeRGBA w h rgba opts

/-- `data:image/png;base64,…` URI of an RGBA8 image (for SVG `<image href>`). -/
def dataURI (w h : Nat) (rgba : ByteArray) (opts : Options := {}) : String :=
  LeanPlot.Base64.dataURI "image/png" (encodeAuto w h rgba opts)

end LeanPlot.PNG
