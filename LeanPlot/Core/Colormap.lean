import LeanPlot.Core.Color
import LeanPlot.Core.Scale
import LeanPlot.Core.Range
import LeanPlot.Core.ColormapData

/-
Colormaps with Makie semantics.

A `Colormap` is a non-empty lookup table of RGBA entries (stored interleaved in
one `FloatArray`, the non-emptiness and whole-entry size are erased proof
fields). Lookup follows Makie exactly:

* `interpolatedGetIndex cm t` = `Makie.interpolated_getindex(cmap, t)`:
  `i = t·(n-1) + 1`, linear blend of the two neighbouring entries computed in
  `Float64` and rounded to `Float32` (`RGBAf`);
* `nearestGetIndex` = `Makie.nearest_getindex`;
* `lookup cm v (lo, hi)` normalises with `clamp((v - lo)/(hi - lo), 0, 1)`;
* `mapValue` = `Makie.numbers_to_colors` (colour scale, `lowclip`/`highclip`,
  `nan_color`);
* `resample` = `Makie.resample_cmap`.

Named maps (`Colormap.named? "viridis"`) are the exact `Makie.to_colormap`
tables (Float32 values) generated into `LeanPlot.ColormapData`.
-/

namespace LeanPlot

open LeanPlot.Num

/-- A non-empty RGBA lookup table, 4 floats per entry. -/
structure Colormap where
  /-- Makie name (informational). -/
  name : String := ""
  /-- Interleaved `r g b a` entries, channels in `[0, 1]`. -/
  lut : FloatArray
  /-- At least one entry and a whole number of entries. -/
  valid : 4 ≤ lut.size ∧ lut.size % 4 = 0

namespace Colormap

/-- Number of entries. -/
@[inline] def size (cm : Colormap) : Nat := cm.lut.size / 4

/-- A colormap has at least one entry. -/
theorem size_pos (cm : Colormap) : 0 < cm.size := by
  have := cm.valid; unfold size; omega

private theorem idx_lt (cm : Colormap) (i c : Nat) (hi : i < cm.size) (hc : c < 4) :
    4 * i + c < cm.lut.size := by
  have := cm.valid; unfold size at hi; omega

/-- Entry `i` (0-based) with a proof of range. -/
@[inline] def entry (cm : Colormap) (i : Nat) (h : i < cm.size) : RGBA :=
  ⟨cm.lut[4 * i]'(idx_lt cm i 0 h (by decide)), cm.lut[4 * i + 1]'(idx_lt cm i 1 h (by decide)),
   cm.lut[4 * i + 2]'(idx_lt cm i 2 h (by decide)), cm.lut[4 * i + 3]'(idx_lt cm i 3 h (by decide))⟩

/-- Entry `i`, clamped into range (total). -/
def get (cm : Colormap) (i : Nat) : RGBA :=
  if h : i < cm.size then cm.entry i h
  else cm.entry (cm.size - 1) (by have := cm.size_pos; omega)

/-- First entry. -/
def first (cm : Colormap) : RGBA := cm.get 0

/-- Last entry. -/
def last (cm : Colormap) : RGBA := cm.get (cm.size - 1)

/-- All entries. -/
def colors (cm : Colormap) : Array RGBA := (Array.range cm.size).map cm.get

/-- Build a colormap from colours; an empty array gives the one-entry map
`[black]` so that the result is always valid. -/
def ofColors (cs : Array RGBA) (name : String := "") : Colormap :=
  let cs := if cs.isEmpty then #[RGBA.black] else cs
  let lut := cs.foldl (init := FloatArray.emptyWithCapacity (4 * cs.size)) fun acc c =>
    (((acc.push c.r).push c.g).push c.b).push c.a
  if h : 4 ≤ lut.size ∧ lut.size % 4 = 0 then ⟨name, lut, h⟩
  else ⟨name, ⟨#[0, 0, 0, 1]⟩, by decide⟩

/-- Decode a generated table: `r g b` per entry, each channel the 8 hex digits
of a `Float32` bit pattern (alpha = 1). -/
def ofF32Hex (name : String) (hex : String) : Colormap :=
  let digit (c : Char) : UInt32 :=
    if c.isDigit then (c.toNat - 48).toUInt32 else ((c.toNat - 87) % 16).toUInt32
  let chars := hex.toList.toArray
  let nEntries := chars.size / 24
  let word (k : Nat) : Float :=
    let v := (List.range 8).foldl (fun acc i => acc * 16 + digit chars[8 * k + i]!) (0 : UInt32)
    (Float32.ofBits v).toFloat
  let cs := (Array.range nEntries).map fun e => (⟨word (3 * e), word (3 * e + 1), word (3 * e + 2), 1.0⟩ : RGBA)
  ofColors cs name

/-- Names of the built-in colormaps (`Makie.to_colormap` tables). -/
def builtinNames : Array String := ColormapData.all.map (·.1)

/-- A built-in colormap by Makie name (`"viridis"`, `"grays"`, `"RdBu"`, …). -/
def named? (name : String) : Option Colormap :=
  (ColormapData.all.find? (·.1 == name)).map fun (n, h) => ofF32Hex n h

/-- A built-in colormap by name, defaulting to viridis. -/
def named (name : String) : Colormap := (named? name).getD (ofF32Hex "viridis" ColormapData.cm_viridis)

/-- `Makie.to_colormap(:viridis)` (Makie's default colormap). -/
def viridis : Colormap := ofF32Hex "viridis" ColormapData.cm_viridis

/-- Reversed colormap (Makie `Reverse(cmap)`). -/
def reverse (cm : Colormap) : Colormap := ofColors cm.colors.reverse cm.name

/-- Multiply every alpha by `a` (Makie `(cmap, alpha)`), with `RGBAf` rounding. -/
def withAlpha (cm : Colormap) (a : Float) : Colormap :=
  ofColors (cm.colors.map fun c => RGBA.toF32 { c with a := c.a * a }) cm.name

/-- `Float64` blend `d·(1-t) + u·t` rounded to `Float32`, per channel (Makie's
`convert(RGBAf, downc * (1 - t) + upc * t)`). -/
@[inline] private def mix (d u : Float) (t : Float) : Float :=
  (d * (1 - t) + u * t).toFloat32.toFloat

/-- Makie `interpolated_getindex(cmap, i01)` for `i01 ∈ [0, 1]`. Non-finite
inputs (an error in Makie) give transparent. -/
def interpolatedGetIndex (cm : Colormap) (i01 : Float) : RGBA :=
  if !i01.isFinite then RGBA.transparent else
  let n := cm.size
  let i1len := i01 * ofInt ((n : Int) - 1) + 1
  let down := floorInt i1len
  let up := ceilInt i1len
  let dn := (down - 1).toNat
  let upn := (up - 1).toNat
  if down == up then cm.get dn else
  let t := i1len - ofInt down
  let d := cm.get dn
  let u := cm.get upn
  ⟨mix d.r u.r t, mix d.g u.g t, mix d.b u.b t, mix d.a u.a t⟩

/-- Makie `nearest_getindex(cmap, i01)`: entry `round(i01·(n-1)) + 1`. -/
def nearestGetIndex (cm : Colormap) (i01 : Float) : RGBA :=
  if !i01.isFinite then RGBA.transparent else
  cm.get (roundInt (i01 * ofInt ((cm.size : Int) - 1))).toNat

/-- Makie's clim normalisation `clamp((v - lo)/(hi - lo), 0, 1)`. -/
@[inline] def normalize (v lo hi : Float) : Float := clamp ((v - lo) / (hi - lo)) 0 1

/-- Makie `interpolated_getindex(cmap, value, (lo, hi))`. -/
def lookup (cm : Colormap) (v lo hi : Float) : RGBA :=
  cm.interpolatedGetIndex (normalize v lo hi)

/-- Makie `resample_cmap(cmap, k)`: `k` colours sampled at `range(0, 1, k)`. -/
def resample (cm : Colormap) (k : Nat) : Colormap :=
  ofColors ((range 0 1 k).toList.toArray.map cm.interpolatedGetIndex) cm.name

/-- Options of `numbers_to_colors`. `none` clips mean Makie's `automatic`
(clamp to the end colours). -/
structure MapOptions where
  scale : Scale := .identity
  lowclip : Option RGBA := none
  highclip : Option RGBA := none
  nanColor : RGBA := RGBA.transparent
  /-- `false`: nearest-entry lookup (`interpolate = false`). -/
  interpolate : Bool := true
  deriving Inhabited

/-- Makie `numbers_to_colors(number, colormap, colorscale, (lo, hi), lowclip,
highclip, nan_color, interpolate)` for a single value. -/
def mapValue (cm : Colormap) (lo hi : Float) (opts : MapOptions) (v : Float) : RGBA :=
  let sLo := opts.scale.forward lo
  let sHi := opts.scale.forward hi
  let cmin := jmin sLo sHi
  let cmax := jmax sLo sHi
  let s := opts.scale.forward v
  let below : Bool := match opts.lowclip with | some _ => decide (s < cmin) | none => false
  let above : Bool := match opts.highclip with | some _ => decide (s > cmax) | none => false
  if s.isNaN then opts.nanColor
  else if below then opts.lowclip.getD opts.nanColor
  else if above then opts.highclip.getD opts.nanColor
  else if opts.interpolate then cm.lookup s cmin cmax
  else cm.nearestGetIndex (normalize s cmin cmax)

/-- Map every value through `mapValue`, appending RGBA8 bytes (4 per value) to a
fresh buffer. Tail-recursive; one allocation for the output. -/
def mapToRGBA8 (cm : Colormap) (lo hi : Float) (opts : MapOptions) (vs : FloatArray) : ByteArray :=
  let rec go (i : Nat) (acc : ByteArray) : ByteArray :=
    if h : i < vs.size then go (i + 1) (RGBA.pushRGBA8 acc (cm.mapValue lo hi opts vs[i]))
    else acc
  termination_by vs.size - i
  go 0 (ByteArray.emptyWithCapacity (4 * vs.size))

/-- Automatic colour range of some data (Makie `distinct_extrema_nan`: finite
extrema, widened by `±0.5` when constant, `(0, 1)` if there is no finite value). -/
def autoRange (vs : FloatArray) : Float × Float :=
  let (lo, hi) := distinctExtrema vs
  if lo.isNaN then (0, 1) else (lo, hi)

end Colormap

end LeanPlot
