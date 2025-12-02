import LeanPlot.Render.PNG.CRC32
import LeanPlot.Render.PNG.Adler32
import LeanPlot.Render.Bitmap

/-!
# PNG Encoding

Complete PNG file generation from bitmap data.
Uses uncompressed DEFLATE blocks for simplicity.
-/

namespace LeanPlot.Render.PNG

open LeanPlot.Render

/-! ## Byte Array Utilities -/

/-- Write UInt32 as big-endian bytes -/
def pushU32BE (ba : ByteArray) (val : UInt32) : ByteArray :=
  (ba.push ((val >>> 24).toUInt8))
    |>.push ((val >>> 16).toUInt8)
    |>.push ((val >>> 8).toUInt8)
    |>.push (val.toUInt8)

/-- Write UInt16 as little-endian bytes -/
def pushU16LE (ba : ByteArray) (val : UInt16) : ByteArray :=
  (ba.push (val.toUInt8))
    |>.push ((val >>> 8).toUInt8)

/-! ## PNG Structure -/

/-- PNG magic signature -/
def pngSignature : ByteArray :=
  ByteArray.mk #[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

/-- Create a PNG chunk with type and data -/
def mkChunk (chunkType : String) (data : ByteArray) : ByteArray :=
  let typeBytes := chunkType.toUTF8
  if typeBytes.size != 4 then ByteArray.empty
  else
    let lengthBytes := pushU32BE ByteArray.empty data.size.toUInt32
    let crc := crc32 (typeBytes ++ data)
    let crcBytes := pushU32BE ByteArray.empty crc
    lengthBytes ++ typeBytes ++ data ++ crcBytes

/-- Create IHDR chunk (image header) -/
def mkIHDR (width height : UInt32) (bitDepth : UInt8 := 8) (colorType : UInt8 := 2) : ByteArray :=
  let data := ByteArray.empty
    |> (pushU32BE · width)
    |> (pushU32BE · height)
    |>.push bitDepth
    |>.push colorType
    |>.push 0  -- compression
    |>.push 0  -- filter
    |>.push 0  -- interlace
  mkChunk "IHDR" data

/-- Create IEND chunk (image end) -/
def mkIEND : ByteArray := mkChunk "IEND" ByteArray.empty

/-! ## DEFLATE Encoding -/

/-- Maximum uncompressed DEFLATE block size -/
def maxBlockSize : Nat := 65535

/-- Create uncompressed DEFLATE block -/
def mkDeflateBlock (data : ByteArray) (isFinal : Bool) : ByteArray :=
  let len := data.size.toUInt16
  let nlen := ~~~len
  let bfinal : UInt8 := if isFinal then 0x01 else 0x00
  let header := ByteArray.empty.push bfinal |> (pushU16LE · len) |> (pushU16LE · nlen)
  header ++ data

/-- Split data into DEFLATE blocks -/
def splitIntoBlocks (data : ByteArray) : Array (ByteArray × Bool) := Id.run do
  let mut result : Array (ByteArray × Bool) := #[]
  let mut pos := 0
  while pos < data.size do
    let remaining := data.size - pos
    let chunkSize := min maxBlockSize remaining
    let chunk := data.extract pos (pos + chunkSize)
    let isFinal := pos + chunkSize >= data.size
    result := result.push (chunk, isFinal)
    pos := pos + chunkSize
  result

/-- Wrap data in zlib stream -/
def wrapZlib (data : ByteArray) : ByteArray :=
  -- Zlib header: CMF=0x78, FLG=0x01
  let result := ByteArray.empty.push 0x78 |>.push 0x01

  -- Add DEFLATE blocks
  let blocks := splitIntoBlocks data
  let result := blocks.foldl (init := result) fun acc (chunk, isFinal) =>
    acc ++ mkDeflateBlock chunk isFinal

  -- Add Adler-32 checksum
  pushU32BE result (adler32 data)

/-! ## PNG Generation -/

/-- Generate PNG from bitmap -/
def encode (bmp : Bitmap) : ByteArray :=
  let scanlines := bmp.toScanlines
  let zlibData := wrapZlib scanlines
  let ihdr := mkIHDR bmp.width.toUInt32 bmp.height.toUInt32
  let idat := mkChunk "IDAT" zlibData
  let iend := mkIEND
  pngSignature ++ ihdr ++ idat ++ iend

/-- Write PNG to file -/
def writePNG (path : System.FilePath) (bmp : Bitmap) : IO Unit := do
  IO.FS.writeBinFile path (encode bmp)

end LeanPlot.Render.PNG
