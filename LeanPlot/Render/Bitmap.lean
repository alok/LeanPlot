/-!
# LeanPlot.Render.Bitmap - RGBA Bitmap Type

This module provides a simple RGBA bitmap type for rendering plots to images.
-/

namespace LeanPlot.Render

/-! ## Color Types -/

/-- RGB color with 8-bit channels -/
structure RGB where
  /-- Red channel (0-255) -/
  r : UInt8
  /-- Green channel (0-255) -/
  g : UInt8
  /-- Blue channel (0-255) -/
  b : UInt8
  deriving Repr, BEq, Inhabited

namespace RGB

def black   : RGB := ⟨0x00, 0x00, 0x00⟩
def white   : RGB := ⟨0xFF, 0xFF, 0xFF⟩
def red     : RGB := ⟨0xFF, 0x00, 0x00⟩
def green   : RGB := ⟨0x00, 0xFF, 0x00⟩
def blue    : RGB := ⟨0x00, 0x00, 0xFF⟩
def yellow  : RGB := ⟨0xFF, 0xFF, 0x00⟩
def cyan    : RGB := ⟨0x00, 0xFF, 0xFF⟩
def magenta : RGB := ⟨0xFF, 0x00, 0xFF⟩
def gray    : RGB := ⟨0x80, 0x80, 0x80⟩

/-- Create RGB from hex string like "#FF0000" -/
def fromHex (s : String) : Option RGB := do
  let s := s.trim
  guard (s.length == 7 && s.front == '#')
  let chars := s.toList
  let parseHex (c : Char) : Option UInt8 :=
    if '0' ≤ c && c ≤ '9' then some (c.val.toUInt8 - '0'.val.toUInt8)
    else if 'A' ≤ c && c ≤ 'F' then some (c.val.toUInt8 - 'A'.val.toUInt8 + 10)
    else if 'a' ≤ c && c ≤ 'f' then some (c.val.toUInt8 - 'a'.val.toUInt8 + 10)
    else none
  let parseByte (hi lo : Char) : Option UInt8 := do
    let h ← parseHex hi
    let l ← parseHex lo
    return h * 16 + l
  let r ← parseByte (chars[1]!) (chars[2]!)
  let g ← parseByte (chars[3]!) (chars[4]!)
  let b ← parseByte (chars[5]!) (chars[6]!)
  return ⟨r, g, b⟩

/-- Blend two colors with alpha (0.0 = first color, 1.0 = second color) -/
def blend (c1 c2 : RGB) (alpha : Float) : RGB :=
  let a := if alpha < 0.0 then 0.0 else if alpha > 1.0 then 1.0 else alpha
  let r := (c1.r.toFloat * (1.0 - a) + c2.r.toFloat * a).toUInt8
  let g := (c1.g.toFloat * (1.0 - a) + c2.g.toFloat * a).toUInt8
  let b := (c1.b.toFloat * (1.0 - a) + c2.b.toFloat * a).toUInt8
  ⟨r, g, b⟩

end RGB

/-! ## Bitmap Type -/

/-- A simple 2D bitmap image with RGB pixels -/
structure Bitmap where
  /-- Width in pixels -/
  width : Nat
  /-- Height in pixels -/
  height : Nat
  /-- Pixel data (row-major order) -/
  pixels : Array RGB
  deriving Repr, Inhabited

namespace Bitmap

/-- Create a bitmap filled with a single color -/
def fill (width height : Nat) (color : RGB := RGB.white) : Bitmap :=
  { width, height, pixels := Array.replicate (width * height) color }

/-- Create a white bitmap -/
def create (width height : Nat) : Bitmap := fill width height RGB.white

/-- Get pixel at (x, y), returning none if out of bounds -/
def getPixel? (bmp : Bitmap) (x y : Nat) : Option RGB :=
  if x < bmp.width && y < bmp.height then
    bmp.pixels[y * bmp.width + x]?
  else none

/-- Get pixel at (x, y), returning default if out of bounds -/
def getPixel (bmp : Bitmap) (x y : Nat) (default : RGB := RGB.white) : RGB :=
  bmp.getPixel? x y |>.getD default

/-- Set pixel at (x, y), returning unchanged bitmap if out of bounds -/
def setPixel (bmp : Bitmap) (x y : Nat) (color : RGB) : Bitmap :=
  if x < bmp.width && y < bmp.height then
    let idx := y * bmp.width + x
    { bmp with pixels := bmp.pixels.set! idx color }
  else bmp

/-- Set pixel with alpha blending -/
def blendPixel (bmp : Bitmap) (x y : Nat) (color : RGB) (alpha : Float) : Bitmap :=
  if x < bmp.width && y < bmp.height then
    let idx := y * bmp.width + x
    let existing := bmp.pixels[idx]!
    let blended := RGB.blend existing color alpha
    { bmp with pixels := bmp.pixels.set! idx blended }
  else bmp

/-- Fill a rectangle with a color -/
def fillRect (bmp : Bitmap) (x y w h : Nat) (color : RGB) : Bitmap := Id.run do
  let mut result := bmp
  for dy in [:h] do
    for dx in [:w] do
      result := result.setPixel (x + dx) (y + dy) color
  result

/-- Draw a horizontal line -/
def hLine (bmp : Bitmap) (x y length : Nat) (color : RGB) : Bitmap := Id.run do
  let mut result := bmp
  for i in [:length] do
    result := result.setPixel (x + i) y color
  result

/-- Draw a vertical line -/
def vLine (bmp : Bitmap) (x y length : Nat) (color : RGB) : Bitmap := Id.run do
  let mut result := bmp
  for i in [:length] do
    result := result.setPixel x (y + i) color
  result

/-- Convert bitmap to PNG scanline format (filter byte + RGB for each row) -/
def toScanlines (bmp : Bitmap) : ByteArray := Id.run do
  let mut result := ByteArray.empty
  for row in [:bmp.height] do
    -- Add filter byte (0x00 = no filter)
    result := result.push 0x00
    for col in [:bmp.width] do
      let idx := row * bmp.width + col
      let pixel := bmp.pixels[idx]!
      result := result.push pixel.r
      result := result.push pixel.g
      result := result.push pixel.b
  result

end Bitmap

end LeanPlot.Render
