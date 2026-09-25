import LeanPlot.Scene.DrawOp
import LeanPlot.Backend.Raster.Num

/-
RGBA8 canvas with an erased size invariant.

Pixels are row-major, top row first, 4 bytes per pixel, **straight**
(non-premultiplied) alpha, which is what PNG stores. Painting is src-over.
When the destination is opaque (the common case: plots have an opaque
background) the blend reduces to one lerp per channel.

`Canvas w h` carries `data.size = 4 * (w * h)` as a proof field, so the size
cannot drift. Every mutation goes through `setByte`/`blend`, which preserve it,
and the canvas is threaded linearly so each write is in place.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- An 8-bit RGBA raster of `w × h` pixels (straight alpha, row-major, top row first). -/
structure Canvas (w h : Nat) where
  /-- `4 * (w * h)` bytes, pixel `(x, y)` at offset `4 * (y * w + x)`. -/
  data : ByteArray
  /-- Size invariant (erased at runtime). -/
  size_eq : data.size = 4 * (w * h)

/-- One pixel, for inspection and tests. -/
structure Px where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8
  deriving Repr, BEq, Inhabited

/-- Channel `[0,1]` → byte, rounding, clamped (NaN ↦ 0). -/
@[inline] def toByte (v : Float) : UInt8 := (v * K.c255 + K.half).toUInt8

/-- Colour → pixel bytes. -/
def pxOf (c : RGBA) : Px := ⟨toByte c.r, toByte c.g, toByte c.b, toByte c.a⟩

/-- `n` zero bytes, built by doubling (O(log n) `memcpy`s instead of `n` pushes). -/
def zeroBytes (n : Nat) : ByteArray :=
  let rec grow (b : ByteArray) (fuel : Nat) : ByteArray :=
    match fuel with
    | 0 => b
    | f + 1 =>
      if 2 * b.size ≤ n then grow (b ++ b) f
      else b.copySlice 0 b b.size (n - b.size) false
  if n == 0 then .empty else grow ⟨#[0]⟩ 64

/-- Clamp to `[0, 1]` (NaN ↦ 0). -/
@[inline] def clamp01 (v : Float) : Float := if v > K.zero then (if v < K.one then v else K.one) else K.zero

/-- Source-over blend of straight colour `(sr, sg, sb)` (in byte units,
`0‥255`) with effective opacity `α ∈ [0, 1]` (colour alpha × coverage) into
the pixel at byte offset `i` of `d`. -/
@[inline] def blendPx (d : ByteArray) (i : Nat) (sr sg sb α : Float) : ByteArray :=
  let da := d.get! (i + 3)
  if α ≥ K.opaqueCut then
    ((((d.set! i (sr + K.half).toUInt8).set! (i+1) (sg + K.half).toUInt8).set! (i+2) (sb + K.half).toUInt8).set! (i+3) 255)
  else if da == 255 then
    let dr := (d.get! i).toFloat; let dg := (d.get! (i+1)).toFloat; let db := (d.get! (i+2)).toFloat
    (((d.set! i (dr + (sr - dr) * α + K.half).toUInt8).set! (i+1) (dg + (sg - dg) * α + K.half).toUInt8).set! (i+2)
      (db + (sb - db) * α + K.half).toUInt8).set! (i+3) 255
  else
    let dr := (d.get! i).toFloat; let dg := (d.get! (i+1)).toFloat; let db := (d.get! (i+2)).toFloat
    let dA := da.toFloat / K.c255
    let k := dA * (K.one - α)
    let oa := α + k
    if oa ≤ K.zero then
      (((d.set! i 0).set! (i+1) 0).set! (i+2) 0).set! (i+3) 0
    else
      let inv := K.one / oa
      (((d.set! i ((sr * α + dr * k) * inv + K.half).toUInt8).set! (i+1) ((sg * α + dg * k) * inv + K.half).toUInt8).set!
        (i+2) ((sb * α + db * k) * inv + K.half).toUInt8).set! (i+3) (oa * K.c255 + K.half).toUInt8

theorem size_blendPx (d : ByteArray) (i : Nat) (sr sg sb α : Float) :
    (blendPx d i sr sg sb α).size = d.size := by
  unfold blendPx; dsimp only
  repeat' split
  all_goals simp

namespace Canvas

variable {w h : Nat}

/-- Byte offset of pixel `(x, y)`. -/
@[inline] def offset (w x y : Nat) : Nat := 4 * (y * w + x)

/-- `n` copies of the 4-byte pattern by doubling (O(log n) `memcpy`s). -/
def repeat4 (p : Px) (n : Nat) : ByteArray :=
  let unit : ByteArray := ⟨#[p.r, p.g, p.b, p.a]⟩
  let target := 4 * n
  let rec grow (b : ByteArray) (fuel : Nat) : ByteArray :=
    match fuel with
    | 0 => b
    | f + 1 =>
      if 2 * b.size ≤ target then grow (b ++ b) f
      else b.copySlice 0 b b.size (target - b.size) false
  if n == 0 then .empty else grow unit 64

/-- A canvas filled with one colour. -/
def fill (w h : Nat) (c : RGBA) : Canvas w h :=
  let d := repeat4 (pxOf c) (w * h)
  if hd : d.size = 4 * (w * h) then ⟨d, hd⟩
  else ⟨⟨Array.replicate (4 * (w * h)) 0⟩, by simp [ByteArray.size]⟩

/-- A fully transparent canvas. -/
def transparent (w h : Nat) : Canvas w h := fill w h .transparent

/-- Read pixel `(x, y)` (zero outside the canvas). -/
def get (c : Canvas w h) (x y : Nat) : Px :=
  if x < w && y < h then
    let i := offset w x y
    ⟨c.data.get! i, c.data.get! (i+1), c.data.get! (i+2), c.data.get! (i+3)⟩
  else ⟨0, 0, 0, 0⟩

/-- Write one byte. -/
@[inline] def setByte (c : Canvas w h) (i : Nat) (v : UInt8) : Canvas w h :=
  ⟨c.data.set! i v, by simp [c.size_eq]⟩

/-- Overwrite pixel `(x, y)` (ignored outside the canvas). -/
def set (c : Canvas w h) (x y : Nat) (p : Px) : Canvas w h :=
  if x < w && y < h then
    let i := offset w x y
    (((c.setByte i p.r).setByte (i+1) p.g).setByte (i+2) p.b).setByte (i+3) p.a
  else c

/-- Src-over blend of straight colour `(sr, sg, sb)` (byte units) with opacity
`α` at byte offset `i` (a multiple of 4 below `4*w*h`). -/
@[inline] def blend (c : Canvas w h) (i : Nat) (sr sg sb α : Float) : Canvas w h :=
  ⟨blendPx c.data i sr sg sb α, by rw [size_blendPx]; exact c.size_eq⟩

/-- Src-over blend of `col` scaled by coverage `cov` at pixel `(x, y)`. -/
def blendAt (c : Canvas w h) (x y : Nat) (col : RGBA) (cov : Float := K.one) : Canvas w h :=
  if x < w && y < h then
    c.blend (offset w x y) (clamp01 col.r * K.c255) (clamp01 col.g * K.c255) (clamp01 col.b * K.c255)
      (clamp01 col.a * clamp01 cov)
  else c

/-- Number of pixels. -/
def numPixels (_ : Canvas w h) : Nat := w * h

end Canvas

end LeanPlot.Raster
