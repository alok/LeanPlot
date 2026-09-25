import LeanPlotTest.Raster.Util

/-!
PNG / DEFLATE / Base64 tests: checksum vectors, Huffman invariants, DEFLATE
round trips (our inflate), PNG round trips (our decoder), compression ratio
against stored blocks, an external round trip through Python's `zlib`
(stdlib only; skipped when `python3` is missing), and RFC 4648 vectors.
-/

namespace LeanPlotTest.Raster.PNGTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Stdlib-only Python checker: parses the PNG, verifies every CRC, inflates
with `zlib`, undoes the filters and compares with the raw pixels. -/
def pyChecker : String := "
import sys, zlib, struct
png = open(sys.argv[1], 'rb').read(); raw = open(sys.argv[2], 'rb').read()
assert png[:8] == b'\\x89PNG\\r\\n\\x1a\\n', 'signature'
pos = 8; idat = b''; ihdr = None; kinds = []
while pos < len(png):
    n, = struct.unpack('>I', png[pos:pos+4]); t = png[pos+4:pos+8]; d = png[pos+8:pos+8+n]
    crc, = struct.unpack('>I', png[pos+8+n:pos+12+n])
    assert zlib.crc32(t + d) & 0xffffffff == crc, 'crc ' + str(t)
    kinds.append(t)
    if t == b'IHDR': ihdr = d
    if t == b'IDAT': idat += d
    pos += 12 + n
assert kinds[0] == b'IHDR' and kinds[-1] == b'IEND', 'chunk order'
w, h, bd, ct, cm, fm, il = struct.unpack('>IIBBBBB', ihdr)
bpp = {0: 1, 2: 3, 6: 4}[ct]
scan = zlib.decompress(idat)
stride = w * bpp
out = bytearray(); prev = bytearray(stride)
for y in range(h):
    f = scan[y*(stride+1)]; row = bytearray(scan[y*(stride+1)+1:(y+1)*(stride+1)])
    assert f <= 4, 'filter type'
    for i in (range(stride) if f else ()):
        a = row[i-bpp] if i >= bpp else 0; b = prev[i]; c = prev[i-bpp] if i >= bpp else 0
        if f == 1: row[i] = (row[i] + a) & 255
        elif f == 2: row[i] = (row[i] + b) & 255
        elif f == 3: row[i] = (row[i] + ((a + b) >> 1)) & 255
        elif f == 4:
            p = a + b - c; pa = abs(p - a); pb = abs(p - b); pc = abs(p - c)
            row[i] = (row[i] + (a if pa <= pb and pa <= pc else (b if pb <= pc else c))) & 255
    out += row; prev = row
print('OK' if bytes(out) == raw else 'MISMATCH')
"

/-- Is `python3` available? -/
def hasPython : IO Bool := do
  try
    let o ← IO.Process.output { cmd := "python3", args := #["--version"] }
    return o.exitCode == 0
  catch _ => return false

/-- Round-trip `png` through Python; `none` when Python is unavailable. -/
def pythonRoundTrip (png raw : ByteArray) : IO (Option String) := do
  if !(← hasPython) then return none
  IO.FS.withTempDir fun dir => do
    let p := dir / "t.png"; let r := dir / "t.raw"
    IO.FS.writeBinFile p png
    IO.FS.writeBinFile r raw
    let o ← IO.Process.output { cmd := "python3", args := #["-c", pyChecker, p.toString, r.toString] }
    return some (o.stdout.trimAscii.toString ++ o.stderr.trimAscii.toString)

/-- A small plot-like scene: frame, grid, three curves, filled markers. -/
def plotScene (w h : Nat) : Scene := Id.run do
  let W := w.toFloat; let H := h.toFloat
  let mut s : Scene := { width := w, height := h }
  let frame : Rect := ⟨0.1 * W, 0.1 * H, 0.8 * W, 0.8 * H⟩
  for k in [0:6] do
    let x := frame.x + frame.w * k.toFloat / 5.0
    let y := frame.y + frame.h * k.toFloat / 5.0
    s := s.push (.path (polyline [(x, frame.y), (x, frame.y + frame.h)]) none (some { color := ⟨0.9, 0.9, 0.9, 1⟩ }) none)
    s := s.push (.path (polyline [(frame.x, y), (frame.x + frame.w, y)]) none (some { color := ⟨0.9, 0.9, 0.9, 1⟩ }) none)
  s := s.push (.path (Path.rect frame) none (some { color := .black }) none)
  for c in [0:3] do
    let mut xs : FloatArray := {}; let mut ys : FloatArray := {}
    for i in [0:400] do
      let t := i.toFloat / 399.0
      xs := xs.push (frame.x + frame.w * t)
      ys := ys.push (frame.y + frame.h * (0.5 + 0.35 * Float.sin (6.0 * t + c.toFloat)))
    s := s.push (.path (Path.polyline xs ys) none
      (some { color := ⟨0.2 * c.toFloat, 0.45, 0.7, 1⟩, width := 2.0, join := .round }) (some frame))
  for i in [0:20] do
    let t := i.toFloat / 19.0
    s := s.push (.path (circlePath (frame.x + frame.w * t) (frame.y + frame.h * (0.5 + 0.3 * Float.cos (5.0 * t))) 4.0)
      (some { color := ⟨0.9, 0.4, 0.1, 1⟩ }) (some { color := .black, width := 1 }) (some frame))
  return s

/-- All PNG-area checks. -/
def tests : T Unit := do
  -- checksums
  check "crc32 IEND" (PNG.crc32 "IEND".toUTF8 == 0xAE426082)
  check "crc32 check value" (PNG.crc32 "123456789".toUTF8 == 0xCBF43926)
  check "adler32 Wikipedia" (PNG.adler32 "Wikipedia".toUTF8 == 0x11E60398)
  check "adler32 empty" (PNG.adler32 .empty == 1)
  -- adler with deferred modulo across NMAX boundaries (all 0xFF bytes)
  let big := ByteArray.mk (Array.replicate 100000 255)
  let naive := Id.run do
    let mut a : Nat := 1; let mut b : Nat := 0
    for _ in [0:100000] do
      a := (a + 255) % 65521; b := (b + a) % 65521
    return (b * 65536 + a).toUInt32
  check "adler32 NMAX blocks" (PNG.adler32 big == naive)
  -- Huffman: complete, length-limited codes
  let freqs : Array Nat := (Array.range 286).map fun i => if i % 7 == 0 then 0 else (i * i * i) % 1000 + 1
  let lens := PNG.Huffman.codeLengths freqs 15
  check "huffman complete" (PNG.Huffman.isComplete lens)
  check "huffman ≤ 15 bits" (lens.all (· ≤ 15))
  -- Fibonacci frequencies force depth > 15 before limiting
  let fib : Array Nat := Id.run do
    let mut a := 1; let mut b := 1; let mut out := #[]
    for _ in [0:30] do
      out := out.push a; let c := a + b; a := b; b := c
    return out
  let flens := PNG.Huffman.codeLengths fib 15
  check "huffman fib limited" (flens.all (· ≤ 15) && PNG.Huffman.isComplete flens) s!"{flens}"
  let cl := PNG.Huffman.codeLengths #[0, 5, 0, 0] 7
  check "huffman single symbol completed" (PNG.Huffman.isComplete cl) s!"{cl}"
  -- DEFLATE round trips through our inflate
  let cases : List (String × ByteArray) :=
    [("empty", .empty), ("one", ⟨#[42]⟩), ("text", "the quick brown fox jumps over the lazy dog; the quick brown fox".toUTF8),
     ("run", ByteArray.mk (Array.replicate 100000 7)), ("random", randBytes 70000 1),
     ("lowentropy", randBytes 200000 2 3), ("periodic", ByteArray.mk ((Array.range 90000).map fun i => (i % 251).toUInt8))]
  for (name, d) in cases do
    for (lvName, lv) in [("fast", PNG.Deflate.Level.fast), ("default", .default), ("best", .best)] do
      let z := PNG.Deflate.zlibCompress d lv
      let ok := match PNG.Inflate.zlibDecompress z with | .ok o => o == d | .error _ => false
      check s!"deflate round trip {name}/{lvName}" ok
    let zs := PNG.Deflate.zlibStored d
    let ok := match PNG.Inflate.zlibDecompress zs with | .ok o => o == d | .error _ => false
    check s!"stored round trip {name}" ok
  -- random data falls back to stored blocks: at most a few block headers of overhead
  let r := randBytes 70000 9
  let zr := (PNG.Deflate.zlibCompress r).size
  let sr := (PNG.Deflate.zlibStored r).size
  check "random data ≈ stored size" (zr ≤ sr + 64) s!"{zr} vs stored {sr}"
  -- PNG round trips (our decoder) for several formats and odd sizes
  for (w, h) in [(1, 1), (7, 3), (97, 61), (256, 1), (1, 300)] do
    let px := randBytes (4 * w * h) (w * 1000 + h).toUInt64
    let png := PNG.encodeRGBA w h px
    match PNG.decode png with
    | .ok img => check s!"png rgba {w}x{h}" (img.width == w && img.height == h && img.pixels == px)
    | .error e => check s!"png rgba {w}x{h}" false e
    let rgb := randBytes (3 * w * h) (w * 7 + h).toUInt64
    match PNG.decode (PNG.encodeRGB w h rgb) with
    | .ok img => check s!"png rgb {w}x{h}" (img.pixels == rgb && img.colorType == .rgb8)
    | .error e => check s!"png rgb {w}x{h}" false e
    let g := randBytes (w * h) (w + 3 * h).toUInt64
    match PNG.decode (PNG.encode w h .gray8 g) with
    | .ok img => check s!"png gray {w}x{h}" (img.pixels == g)
    | .error e => check s!"png gray {w}x{h}" false e
  -- a large odd size: several 1 MiB IDAT chunks (random pixels barely compress)
  let bigPx := randBytes (4 * 997 * 613) 997
  let bigPng := PNG.encodeRGBA 997 613 bigPx
  check "png 997x613 spans several IDATs" (bigPng.size > 2 * PNG.idatChunk) s!"{bigPng.size} B"
  check "png rgba 997x613" (match PNG.decode bigPng with
    | .ok img => img.width == 997 && img.height == 613 && img.pixels == bigPx | _ => false)
  -- zero width or height: PNG forbids it (IHDR), but the stream stays well formed
  for (w, h) in [(0, 5), (5, 0), (0, 0)] do
    check s!"png {w}x{h} decodes empty" (match PNG.decode (PNG.encodeRGBA w h .empty) with
      | .ok img => img.width == w && img.height == h && img.pixels.size == 0 | _ => false)
  -- zero-height image still yields a valid zlib stream
  let z0 := PNG.Deflate.zlibCompress .empty
  check "empty zlib stream valid" (match PNG.Inflate.zlibDecompress z0 with | .ok o => o.size == 0 | _ => false)
  -- every filter strategy decodes
  let px := randBytes (4 * 33 * 17) 77 7
  for k in [PNG.Filter.Kind.none, .sub, .up, .average, .paeth] do
    let png := PNG.encodeRGBA 33 17 px { filter := .fixed k }
    check s!"png filter {repr k}" (match PNG.decode png with | .ok img => img.pixels == px | _ => false)
  -- compression ratio on a rendered plot
  let cv := (plotScene 800 600).toCanvas
  let png := cv.toPNG
  let stored := cv.toPNG { stored := true }
  let ratio := stored.size.toFloat / png.size.toFloat
  IO.println s!"  png 800x600 plot: {png.size} B compressed vs {stored.size} B stored (×{ratio})"
  check "plot compresses ≥ 20×" (ratio ≥ 20.0) s!"ratio {ratio}"
  check "plot png decodes" (match PNG.decodeRGBA png with
    | .ok (w, h, p) => w == 800 && h == 600 && p == cv.data | _ => false)
  -- external round trip through Python's zlib
  let small := (plotScene 160 120).toCanvas
  let cases : List (String × ByteArray × ByteArray) :=
    [("plot rgb", small.toPNG, PNG.rgbaToRGB small.data),
     ("random rgba", PNG.encodeRGBA 45 31 (randBytes (4*45*31) 5), randBytes (4*45*31) 5),
     ("stored rgba", PNG.encodeRGBA 45 31 (randBytes (4*45*31) 6) { stored := true }, randBytes (4*45*31) 6),
     -- unfiltered, so the stdlib checker stays fast on 2.4 MB of pixels
     ("997x613 multi-IDAT", PNG.encodeRGBA 997 613 bigPx { filter := .fixed .none }, bigPx),
     ("0x5", PNG.encodeRGBA 0 5 .empty, .empty), ("5x0", PNG.encodeRGBA 5 0 .empty, .empty)]
  for (name, png, raw) in cases do
    match ← pythonRoundTrip png raw with
    | none => IO.println s!"  (skipped python round trip {name}: no python3)"
    | some out => check s!"python zlib round trip {name}" (out == "OK") out
  -- Base64 (RFC 4648 §10)
  let vecs := [("", ""), ("f", "Zg=="), ("fo", "Zm8="), ("foo", "Zm9v"), ("foob", "Zm9vYg=="),
               ("fooba", "Zm9vYmE="), ("foobar", "Zm9vYmFy")]
  for (plain, enc) in vecs do
    check s!"base64 encode {plain}" (Base64.encode plain.toUTF8 == enc)
    check s!"base64 decode {enc}" (Base64.decode enc == some plain.toUTF8)
  let rb := randBytes 1001 11
  check "base64 round trip" (Base64.decode (Base64.encode rb) == some rb)
  check "base64 rejects junk" (Base64.decode "Zm9v!" == none && Base64.decode "Zg=a" == none)
  check "data uri prefix" ((PNG.dataURI 1 1 ⟨#[1, 2, 3, 255]⟩).startsWith "data:image/png;base64,iVBORw0KGgo")

/-- Run the PNG suite. -/
def run : IO (Nat × Nat) := runSuite "png" tests

end LeanPlotTest.Raster.PNGTests
