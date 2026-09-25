import LeanPlot.Scene.DrawOp
import LeanPlot.Core.Num
import LeanPlot.Core.ColorNames

/-
Colour helpers on top of `LeanPlot.RGBA` (straight sRGB + alpha in `[0, 1]`).

Makie stores colours as `RGBAf` (four `Float32`s) and converts 8-bit colours
through `N0f8`; this module reproduces both conversions exactly:

* `to8`: `Float → UInt8` like Colors.jl/FixedPointNumbers (`clamp01`, then
  `round(x·255)` with ties to even),
* `ofRGB8`: `UInt8 → Float` as `N0f8 → Float32` (`u/255` rounded to `Float32`),
* `ofHex?`/`toHex`: CSS hex (`#rgb`, `#rgba`, `#rrggbb`, `#rrggbbaa`),
* `named?`: the 666 Colors.jl names (CSS/X11 plus numbered variants) and
  `transparent`, case- and space-insensitive, as Makie's `to_color(:name)`,
* `wong`: Makie's default categorical palette `Makie.wong_colors()`,
* `blend` (source-over), `lerp`, RGBA8 packing into `ByteArray`s.
-/

namespace LeanPlot

open LeanPlot.Num

namespace RGBA

/-- Round every channel to `Float32`, as Makie's `RGBAf` does. -/
def toF32 (c : RGBA) : RGBA :=
  ⟨c.r.toFloat32.toFloat, c.g.toFloat32.toFloat, c.b.toFloat32.toFloat, c.a.toFloat32.toFloat⟩

/-- Opaque colour from `[0, 1]` channels. -/
@[inline] def rgb (r g b : Float) : RGBA := ⟨r, g, b, 1.0⟩

/-- Grey level `v` (opaque). -/
@[inline] def gray (v : Float) : RGBA := ⟨v, v, v, 1.0⟩

/-- `N0f8`-to-`Float32` conversion of an 8-bit channel (`u / 255`). -/
@[inline] def unit8 (u : UInt8) : Float := (u.toNat.toFloat / 255.0).toFloat32.toFloat

/-- Colour from 8-bit channels, with Makie's `RGBAf` rounding. -/
def ofRGB8 (r g b : UInt8) (a : UInt8 := 255) : RGBA :=
  ⟨unit8 r, unit8 g, unit8 b, unit8 a⟩

/-- Quantise a channel to 8 bits like `N0f8(clamp01(x))`: round `x·255`, ties
to even. NaN maps to 0. -/
@[inline] def to8 (x : Float) : UInt8 :=
  if x.isNaN then 0 else
  let y := roundEven (clamp01 x * 255.0)
  y.toUInt8

/-- Pack into `0xRRGGBBAA`. -/
def toRGBA8 (c : RGBA) : UInt32 :=
  (c.r |> to8).toUInt32 <<< 24 ||| (c.g |> to8).toUInt32 <<< 16 ||| (c.b |> to8).toUInt32 <<< 8 ||| (c.a |> to8).toUInt32

/-- Unpack `0xRRGGBBAA`. -/
def ofRGBA8 (u : UInt32) : RGBA :=
  ofRGB8 (u >>> 24).toUInt8 (u >>> 16).toUInt8 (u >>> 8).toUInt8 u.toUInt8

/-- Append the four 8-bit channels (R, G, B, A) to a byte buffer. -/
@[inline] def pushRGBA8 (buf : ByteArray) (c : RGBA) : ByteArray :=
  (((buf.push (to8 c.r)).push (to8 c.g)).push (to8 c.b)).push (to8 c.a)

/-- Read the colour stored at byte offset `4 i` of an RGBA8 buffer (missing
bytes read as 0). -/
def ofRGBA8At (buf : ByteArray) (i : Nat) : RGBA :=
  let k := 4 * i
  ofRGB8 (buf.get! k) (buf.get! (k + 1)) (buf.get! (k + 2)) (buf.get! (k + 3))

private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

private def hexByte (u : UInt8) : String :=
  String.ofList [hexDigit (u.toNat / 16), hexDigit (u.toNat % 16)]

/-- `#rrggbb` (lowercase), or `#rrggbbaa` when the 8-bit alpha is below 255. -/
def toHex (c : RGBA) : String :=
  let a := to8 c.a
  "#" ++ hexByte (to8 c.r) ++ hexByte (to8 c.g) ++ hexByte (to8 c.b) ++ (if a == 255 then "" else hexByte a)

/-- `#rrggbb` ignoring alpha (e.g. for SVG `fill`, with alpha in `fill-opacity`). -/
def toHexRGB (c : RGBA) : String :=
  "#" ++ hexByte (to8 c.r) ++ hexByte (to8 c.g) ++ hexByte (to8 c.b)

/-- Colors.jl `hex(c::RGB)`: six uppercase hex digits, no `#`. -/
def hexColors (c : RGBA) : String :=
  ((hexByte (to8 c.r) ++ hexByte (to8 c.g) ++ hexByte (to8 c.b)).map Char.toUpper)

private def hexVal? (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - 48)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 87)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 55)
  else none

/-- Parse CSS hex notation: `#rgb`, `#rgba`, `#rrggbb`, `#rrggbbaa` (the `#` is
optional, digits are case-insensitive). -/
def ofHex? (s : String) : Option RGBA := do
  let cs := s.trimAscii.toString.toList
  let cs := match cs with | '#' :: r => r | r => r
  let ds ← cs.mapM hexVal?
  match ds with
  | [r, g, b] => some (ofRGB8 (r * 17).toUInt8 (g * 17).toUInt8 (b * 17).toUInt8)
  | [r, g, b, a] => some (ofRGB8 (r * 17).toUInt8 (g * 17).toUInt8 (b * 17).toUInt8 (a * 17).toUInt8)
  | [r1, r2, g1, g2, b1, b2] => some (ofRGB8 (r1 * 16 + r2).toUInt8 (g1 * 16 + g2).toUInt8 (b1 * 16 + b2).toUInt8)
  | [r1, r2, g1, g2, b1, b2, a1, a2] =>
    some (ofRGB8 (r1 * 16 + r2).toUInt8 (g1 * 16 + g2).toUInt8 (b1 * 16 + b2).toUInt8 (a1 * 16 + a2).toUInt8)
  | _ => none

/-- The named-colour table (parsed once). -/
private def nameTable : Array (String × UInt32) :=
  (ColorNames.table.splitOn "\n").toArray.filterMap fun line =>
    match line.splitOn " " with
    | [n, h] =>
      let v := h.toList.foldl (fun acc c => acc * 16 + (hexVal? c).getD 0) 0
      some (n, v.toUInt32)
    | _ => none

/-- Binary search in the sorted name table. -/
private def lookupName (key : String) : Option UInt32 :=
  let t := nameTable
  let rec go (fuel lo hi : Nat) : Option UInt32 :=
    match fuel with
    | 0 => none
    | fuel + 1 =>
      if lo < hi then
        let mid := (lo + hi) / 2
        let (n, v) := t[mid]!
        if n == key then some v
        else if n < key then go fuel (mid + 1) hi
        else go fuel lo mid
      else none
  go 64 0 t.size

/-- Colors.jl / Makie named colour (`to_color(:royalblue)`), matched
case-insensitively with spaces removed; `transparent` is `(0, 0, 0, 0)`. -/
def named? (name : String) : Option RGBA :=
  let key := String.ofList ((name.toList.filter (· != ' ')).map Char.toLower)
  if key == "transparent" then some ⟨0, 0, 0, 0⟩ else
  (lookupName key).map fun (v : UInt32) =>
    ofRGB8 (v >>> 16).toUInt8 (v >>> 8).toUInt8 v.toUInt8

/-- Parse a colour given as hex (`#…`) or as a name. -/
def parse? (s : String) : Option RGBA :=
  if s.trimAscii.toString.startsWith "#" then ofHex? s
  else (named? s).orElse fun _ => ofHex? s

/-- Linear interpolation of all four channels. -/
def lerp (a b : RGBA) (t : Float) : RGBA :=
  ⟨Num.lerp a.r b.r t, Num.lerp a.g b.g t, Num.lerp a.b b.b t, Num.lerp a.a b.a t⟩

/-- Source-over compositing of straight-alpha colours (`src` over `dst`). -/
def blend (src dst : RGBA) : RGBA :=
  let ao := src.a + dst.a * (1 - src.a)
  if ao ≤ 0 then ⟨0, 0, 0, 0⟩ else
  let f (cs cd : Float) := (cs * src.a + cd * dst.a * (1 - src.a)) / ao
  ⟨f src.r dst.r, f src.g dst.g, f src.b dst.b, ao⟩

/-- Multiply alpha by `k`. -/
@[inline] def fade (c : RGBA) (k : Float) : RGBA := { c with a := c.a * k }

/-- Relative luminance (sRGB, Rec. 709 weights on linearised channels). -/
def luminance (c : RGBA) : Float :=
  let lin (u : Float) : Float := if u ≤ 0.04045 then u / 12.92 else Float.pow ((u + 0.055) / 1.055) 2.4
  0.2126 * lin c.r + 0.7152 * lin c.g + 0.0722 * lin c.b

end RGBA

/-- Makie's default categorical palette (`Makie.wong_colors()`, Bang Wong,
Nature Methods 2011) as `RGBAf`: blue, orange, green, reddish purple, sky
blue, vermilion, yellow. -/
def wongColors : Array RGBA :=
  #[RGBA.toF32 ⟨0 / 255, 114 / 255, 178 / 255, 1⟩, RGBA.toF32 ⟨230 / 255, 159 / 255, 0 / 255, 1⟩,
    RGBA.toF32 ⟨0 / 255, 158 / 255, 115 / 255, 1⟩, RGBA.toF32 ⟨204 / 255, 121 / 255, 167 / 255, 1⟩,
    RGBA.toF32 ⟨86 / 255, 180 / 255, 233 / 255, 1⟩, RGBA.toF32 ⟨213 / 255, 94 / 255, 0 / 255, 1⟩,
    RGBA.toF32 ⟨240 / 255, 228 / 255, 66 / 255, 1⟩]

/-- The `i`-th palette colour, cycling (Makie cycles the palette per plot type). -/
def wongColor (i : Nat) : RGBA := wongColors[i % wongColors.size]!

namespace MakieTheme
/-- Axis grid lines: `RGBAf(0, 0, 0, 0.12)`. -/
def gridColor : RGBA := RGBA.toF32 ⟨0, 0, 0, 0.12⟩
/-- Minor grid lines: `RGBAf(0, 0, 0, 0.05)`. -/
def minorGridColor : RGBA := RGBA.toF32 ⟨0, 0, 0, 0.05⟩
/-- Text, spines and ticks: black. -/
def textColor : RGBA := RGBA.black
/-- Figure and axis background: white. -/
def backgroundColor : RGBA := RGBA.white
/-- Default `nan_color` for colour mapping: fully transparent. -/
def nanColor : RGBA := RGBA.transparent
end MakieTheme

end LeanPlot
