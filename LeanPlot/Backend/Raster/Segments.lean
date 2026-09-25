import LeanPlot.Backend.Raster.Stroke

/-
`segments` op: many independent straight segments that share a width, each
with its own RGBA8 colour (colour-mapped lines, `linesegments`, streamlines).

Each segment is deposited as one offset rectangle plus its two caps. Runs of
consecutive segments with the same colour are accumulated together and
resolved in one sweep, so a single-colour run behaves like one stroke: no
double blending where neighbours overlap. Each sweep visits only the touched
cells, so cost is proportional to the painted area.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Colour `i` of an RGBA8 array, reusing the last colour when the array is
short (opaque black when empty). Returns `r g b a` in byte units packed as
`((r*256+g)*256+b)*256+a`. -/
@[inline] def rgbaAt (rgba : ByteArray) (i : Nat) : UInt32 :=
  let n := rgba.size / 4
  if n == 0 then 0x000000FF else
  let j := 4 * (if i < n then i else n - 1)
  ((rgba.get! j).toUInt32 <<< 24) ||| ((rgba.get! (j+1)).toUInt32 <<< 16) |||
    ((rgba.get! (j+2)).toUInt32 <<< 8) ||| (rgba.get! (j+3)).toUInt32

/-- Painter for a packed RGBA8 colour. -/
@[inline] def packedPainter {w h : Nat} (c : UInt32) : Painter w h :=
  solidPainter ((c >>> 24) &&& 0xFF).toFloat ((c >>> 16) &&& 0xFF).toFloat ((c >>> 8) &&& 0xFF).toFloat
    ((c &&& 0xFF).toFloat / K.c255)

namespace Accum

/-- Deposit segment `(x0,y0)–(x1,y1)` with caps. -/
@[inline] def segment (b : FloatArray) (w h : Nat) (cl : Clip) (g : StrokeGeom) (x0 y0 x1 y1 : Float) : FloatArray :=
  if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite) then b else
  let dx := x1 - x0; let dy := y1 - y0
  let len := (dx * dx + dy * dy).sqrt
  if len < Stroker.minSeg then Stroker.dot b w h cl g x0 y0 else
  let ux := dx / len; let uy := dy / len
  let b := Stroker.cap b w h cl g x0 y0 (-ux) (-uy)
  let b := Stroker.segQuad b w h cl g.hw x0 y0 x1 y1 ux uy
  Stroker.cap b w h cl g x1 y1 ux uy

end Accum

/-- Render segments `[i, n)`; `cur` is the packed colour of the pending run
(already deposited into `acc`). -/
def segmentsLoop {w h : Nat} (acc : Accum) (cv : Canvas w h) (cl : Clip) (g : StrokeGeom)
    (xs ys : FloatArray) (rgba : ByteArray) (i n : Nat) (cur : UInt32) : Accum × Canvas w h :=
  if i < n then
    let c := rgbaAt rgba i
    let (acc, cv) := if c != cur && i > 0 then acc.sweep cv cl false (packedPainter cur) else (acc, cv)
    let buf := Accum.segment acc.buf w h cl g (xs.get! (2*i)) (ys.get! (2*i)) (xs.get! (2*i+1)) (ys.get! (2*i+1))
    segmentsLoop { acc with buf } cv cl g xs ys rgba (i + 1) n c
  else if n > 0 then acc.sweep cv cl false (packedPainter cur)
  else (acc, cv)
termination_by n - i

/-- Render a `segments` op. -/
def renderSegments {w h : Nat} (acc : Accum) (cv : Canvas w h) (cl : Clip) (xs ys : FloatArray)
    (rgba : ByteArray) (width : Float) (cap : LineCap) : Accum × Canvas w h :=
  if !(width > K.zero) || cl.isEmpty || acc.w != w || acc.h != h then (acc, cv) else
  let g : StrokeGeom := { hw := K.half * width, cap, join := .miter, miterLimit := K.four }
  let n := (min xs.size ys.size) / 2
  segmentsLoop acc cv cl g xs ys rgba 0 n (rgbaAt rgba 0)

end LeanPlot.Raster
