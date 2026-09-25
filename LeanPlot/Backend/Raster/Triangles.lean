import LeanPlot.Backend.Raster.Coverage

/-
`triangles` op: Gouraud-shaded triangle meshes with antialiased outer edges
and no seams.

Rendering each triangle separately with AA edges leaves visible seams. Two
50 %-covered pixels on a shared edge composite to 75 %, so the background
leaks through (the classic "conflation" artifact). Instead:

1. **Coverage.** All triangles of the op are deposited, with one orientation,
   into a single accumulation. Shared interior edges cancel exactly, so the
   sweep sees the union of the mesh with exact-area AA on its boundary only.
2. **Colour.** Each triangle is also rasterised *without* AA, sampling pixel
   centres with a top-left rule (an exact partition of the plane), into an
   RGBA8 colour buffer over the op's bounding box. Painter's order holds:
   later triangles overwrite earlier ones. The colour is the affine
   (barycentric) interpolation of the vertex colours at the pixel centre.
   Each triangle also "weakly" writes its vertex and centroid pixels, so
   sub-pixel slivers still get a colour.
3. **Composite.** The sweep blends the buffered colour with the union
   coverage. A covered pixel whose centre no triangle hit (a boundary sliver)
   takes the colour of a written 8-neighbour.

Vertex colours are interpolated in straight (non-premultiplied) RGBA.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Little-endian `UInt32` at byte `4k` of `idx`, as `Nat`. -/
@[inline] def u32At (idx : ByteArray) (k : Nat) : Nat :=
  let i := 4 * k
  (idx.get! i).toNat ||| ((idx.get! (i+1)).toNat <<< 8) ||| ((idx.get! (i+2)).toNat <<< 16) |||
    ((idx.get! (i+3)).toNat <<< 24)

/-- Affine colour plane over a triangle: `c(x, y) = c₀ + gx·(x − x₀) + gy·(y − y₀)`
for each of R, G, B, A (byte units). -/
structure Plane where
  x0 : Float
  y0 : Float
  r : Float
  rx : Float
  ry : Float
  g : Float
  gx : Float
  gy : Float
  b : Float
  bx : Float
  by_ : Float
  a : Float
  ax : Float
  ay : Float

/-- Plane through three vertex values; `none` when the triangle is degenerate. -/
def Plane.make? (x0 y0 x1 y1 x2 y2 : Float) (c0 c1 c2 : UInt32) : Option Plane :=
  let det := (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
  if det.abs < K.eps12 then none else
  let ch (c : UInt32) (s : UInt32) : Float := ((c >>> s) &&& 0xFF).toFloat
  let grad (v0 v1 v2 : Float) : Float × Float :=
    (((v1 - v0) * (y2 - y0) - (v2 - v0) * (y1 - y0)) / det,
     ((v2 - v0) * (x1 - x0) - (v1 - v0) * (x2 - x0)) / det)
  let r0 := ch c0 24; let g0 := ch c0 16; let b0 := ch c0 8; let a0 := ch c0 0
  let (rx, ry) := grad r0 (ch c1 24) (ch c2 24)
  let (gx, gy) := grad g0 (ch c1 16) (ch c2 16)
  let (bx, by_) := grad b0 (ch c1 8) (ch c2 8)
  let (ax, ay) := grad a0 (ch c1 0) (ch c2 0)
  some { x0, y0, r := r0, rx, ry, g := g0, gx, gy, b := b0, bx, by_, a := a0, ax, ay }

/-- Byte from a float channel value, clamped to `[0, 255]`. -/
@[inline] def chanByte (v : Float) : UInt8 := (v + K.half).toUInt8

/-- The colour buffer of a `triangles` op: RGBA8 over the box
`[bx0, bx0 + bw) × [by0, by0 + bh)` plus a priority mask (0 none, 1 weak,
2 centre-sampled). -/
structure TriBuf where
  bx0 : Nat
  by0 : Nat
  bw : Nat
  bh : Nat
  col : ByteArray
  mask : ByteArray

namespace TriBuf

/-- Write the packed colour at canvas pixel `(x, y)` with priority `p`
(overwrites if `p = 2` or nothing is there yet). -/
@[inline] def put (tb : TriBuf) (x y : Nat) (c : UInt32) (p : UInt8) : TriBuf :=
  if x < tb.bx0 || y < tb.by0 || x ≥ tb.bx0 + tb.bw || y ≥ tb.by0 + tb.bh then tb else
  let k := (y - tb.by0) * tb.bw + (x - tb.bx0)
  if p < 2 && tb.mask.get! k != 0 then tb else
  let i := 4 * k
  { tb with
    col := (((tb.col.set! i ((c >>> 24) &&& 0xFF).toUInt8).set! (i+1) ((c >>> 16) &&& 0xFF).toUInt8).set!
      (i+2) ((c >>> 8) &&& 0xFF).toUInt8).set! (i+3) (c &&& 0xFF).toUInt8,
    mask := tb.mask.set! k p }

/-- Mask value at canvas pixel `(x, y)` (0 outside the box). -/
@[inline] def maskAt (tb : TriBuf) (x y : Int) : UInt8 :=
  if x < tb.bx0 || y < tb.by0 || x ≥ tb.bx0 + tb.bw || y ≥ tb.by0 + tb.bh then 0 else
  tb.mask.get! ((y.toNat - tb.by0) * tb.bw + (x.toNat - tb.bx0))

/-- Byte offset in `col` of canvas pixel `(x, y)` (inside the box). -/
@[inline] def colOffset (tb : TriBuf) (x y : Nat) : Nat := 4 * ((y - tb.by0) * tb.bw + (x - tb.bx0))

end TriBuf

/-- Centre-sample columns `[c, cEnd)` of row `y` into the buffer. -/
def triCols (tb : TriBuf) (pl : Plane) (y : Nat) (yc : Float) (c cEnd : Nat) : TriBuf :=
  if c < cEnd then
    let xc := natF c + K.half
    let dx := xc - pl.x0; let dy := yc - pl.y0
    let v (base gx gy : Float) : UInt32 := (chanByte (base + gx * dx + gy * dy)).toUInt32
    let col := (v pl.r pl.rx pl.ry <<< 24) ||| (v pl.g pl.gx pl.gy <<< 16) ||| (v pl.b pl.bx pl.by_ <<< 8) |||
      v pl.a pl.ax pl.ay
    triCols (tb.put c y col 2) pl y yc (c + 1) cEnd
  else tb
termination_by cEnd - c

/-- `⌈v − ½⌉` clamped to `[lo, hi]` (pixel whose centre is the first ≥ `v`). -/
@[inline] def centreCeil (v : Float) (lo hi : Nat) : Nat :=
  let t := (v - K.half).ceil
  if !(t > natF lo) then lo else if t ≥ natF hi then hi else t.toUInt64.toNat

/-- Centre-sample rows `[y, yEnd)` of a triangle with vertices sorted by `y`. -/
def triRows (tb : TriBuf) (pl : Plane) (ax ay bx by_ cx cy : Float) (y yEnd : Nat) : TriBuf :=
  if y < yEnd then
    let yc := natF y + K.half
    let xl := if cy > ay then ax + (yc - ay) * (cx - ax) / (cy - ay) else ax
    let xs := if yc < by_ then (if by_ > ay then ax + (yc - ay) * (bx - ax) / (by_ - ay) else ax)
      else (if cy > by_ then bx + (yc - by_) * (cx - bx) / (cy - by_) else bx)
    let lo := min xl xs; let hi := max xl xs
    let x0 := tb.bx0; let x1 := tb.bx0 + tb.bw
    let tb := triCols tb pl y yc (centreCeil lo x0 x1) (centreCeil hi x0 x1)
    triRows tb pl ax ay bx by_ cx cy (y + 1) yEnd
  else tb
termination_by yEnd - y

/-- Rasterise one triangle's colour (centre sampling + weak vertex/centroid writes). -/
def triColour (tb : TriBuf) (x0 y0 x1 y1 x2 y2 : Float) (c0 c1 c2 : UInt32) : TriBuf :=
  let fl (v : Float) : Nat := if v > K.zero then v.floor.toUInt64.toNat else 0
  let ok (v : Float) : Bool := v ≥ K.zero
  let weak (tb : TriBuf) (x y : Float) (c : UInt32) : TriBuf := if ok x && ok y then tb.put (fl x) (fl y) c 1 else tb
  let tb := weak (weak (weak tb x0 y0 c0) x1 y1 c1) x2 y2 c2
  let mx := (x0 + x1 + x2) / K.three; let my := (y0 + y1 + y2) / K.three
  match Plane.make? x0 y0 x1 y1 x2 y2 c0 c1 c2 with
  | none => tb
  | some pl =>
    let dx := mx - pl.x0; let dy := my - pl.y0
    let v (base gx gy : Float) : UInt32 := (chanByte (base + gx * dx + gy * dy)).toUInt32
    let tb := weak tb mx my ((v pl.r pl.rx pl.ry <<< 24) ||| (v pl.g pl.gx pl.gy <<< 16) |||
      (v pl.b pl.bx pl.by_ <<< 8) ||| v pl.a pl.ax pl.ay)
    -- sort vertices by y
    let (ax, ay, bx, by_, cx, cy) :=
      let (p, q, r) := ((x0, y0), (x1, y1), (x2, y2))
      let (p, q) := if q.2 < p.2 then (q, p) else (p, q)
      let (q, r) := if r.2 < q.2 then (r, q) else (q, r)
      let (p, q) := if q.2 < p.2 then (q, p) else (p, q)
      (p.1, p.2, q.1, q.2, r.1, r.2)
    let y0' := centreCeil ay tb.by0 (tb.by0 + tb.bh)
    let y1' := centreCeil cy tb.by0 (tb.by0 + tb.bh)
    triRows tb pl ax ay bx by_ cx cy y0' y1'

/-- Painter reading the triangle colour buffer, with an 8-neighbour fallback. -/
@[inline] def triPainter {w h : Nat} (tb : TriBuf) : Painter w h := fun cv x y cov =>
  let xi : Int := x; let yi : Int := y
  let pick : Option (Nat × Nat) :=
    if tb.maskAt xi yi != 0 then some (x, y) else
    let cands : List (Int × Int) := [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,-1), (1,-1), (-1,1)]
    match cands.find? (fun (dx, dy) => tb.maskAt (xi + dx) (yi + dy) == 2) with
    | some (dx, dy) => some ((xi + dx).toNat, (yi + dy).toNat)
    | none => match cands.find? (fun (dx, dy) => tb.maskAt (xi + dx) (yi + dy) != 0) with
      | some (dx, dy) => some ((xi + dx).toNat, (yi + dy).toNat)
      | none => none
  match pick with
  | none => cv
  | some (sx, sy) =>
    let i := tb.colOffset sx sy
    cv.blend (Canvas.offset w x y) (tb.col.get! i).toFloat (tb.col.get! (i+1)).toFloat (tb.col.get! (i+2)).toFloat
      ((tb.col.get! (i+3)).toFloat / K.c255 * cov)

/-- Deposit every triangle's edges (canonical orientation) and rasterise its colour. -/
def trianglesLoop (buf : FloatArray) (w h : Nat) (cl : Clip) (tb : TriBuf) (xs ys : FloatArray) (rgba idx : ByteArray)
    (nv : Nat) (t nt : Nat) : FloatArray × TriBuf :=
  if t < nt then
    let i0 := u32At idx (3*t); let i1 := u32At idx (3*t+1); let i2 := u32At idx (3*t+2)
    if i0 ≥ nv || i1 ≥ nv || i2 ≥ nv then trianglesLoop buf w h cl tb xs ys rgba idx nv (t + 1) nt else
    let x0 := xs.get! i0; let y0 := ys.get! i0
    let x1 := xs.get! i1; let y1 := ys.get! i1
    let x2 := xs.get! i2; let y2 := ys.get! i2
    if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite && x2.isFinite && y2.isFinite) then
      trianglesLoop buf w h cl tb xs ys rgba idx nv (t + 1) nt else
    let cr := (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0)
    let buf :=
      if cr < K.zero then
        Accum.lineRaw (Accum.lineRaw (Accum.lineRaw buf w h cl x0 y0 x1 y1) w h cl x1 y1 x2 y2) w h cl x2 y2 x0 y0
      else
        Accum.lineRaw (Accum.lineRaw (Accum.lineRaw buf w h cl x0 y0 x2 y2) w h cl x2 y2 x1 y1) w h cl x1 y1 x0 y0
    let tb := triColour tb x0 y0 x1 y1 x2 y2 (rgbaAt' rgba i0) (rgbaAt' rgba i1) (rgbaAt' rgba i2)
    trianglesLoop buf w h cl tb xs ys rgba idx nv (t + 1) nt
  else (buf, tb)
termination_by nt - t
where
  /-- Vertex colour `k` (opaque black if missing). -/
  rgbaAt' (rgba : ByteArray) (k : Nat) : UInt32 :=
    if 4 * k + 3 < rgba.size then
      ((rgba.get! (4*k)).toUInt32 <<< 24) ||| ((rgba.get! (4*k+1)).toUInt32 <<< 16) |||
        ((rgba.get! (4*k+2)).toUInt32 <<< 8) ||| (rgba.get! (4*k+3)).toUInt32
    else 0x000000FF

/-- Bounding box (min x, min y, max x, max y) of the valid triangles. -/
def trianglesBBox (xs ys : FloatArray) (idx : ByteArray) (nv nt : Nat) : Float × Float × Float × Float := Id.run do
  let mut x0 := K.huge; let mut y0 := K.huge; let mut x1 := -K.huge; let mut y1 := -K.huge
  for t in [0:nt] do
    for j in [0:3] do
      let i := u32At idx (3*t + j)
      if i < nv then
        let x := xs.get! i; let y := ys.get! i
        if x.isFinite && y.isFinite then
          x0 := min x0 x; y0 := min y0 y; x1 := max x1 x; y1 := max y1 y
  return (x0, y0, x1, y1)

/-- Render a `triangles` op. -/
def renderTriangles {w h : Nat} (acc : Accum) (cv : Canvas w h) (cl : Clip) (xs ys : FloatArray)
    (rgba idx : ByteArray) : Accum × Canvas w h :=
  let nv := min xs.size ys.size
  let nt := idx.size / 12
  if nt == 0 || cl.isEmpty || acc.w != w || acc.h != h then (acc, cv) else
  let (fx0, fy0, fx1, fy1) := trianglesBBox xs ys idx nv nt
  if fx0 > fx1 then (acc, cv) else
  let fl (v : Float) (lo hi : Nat) : Nat :=
    let t := v.floor
    if !(t > natF lo) then lo else if t ≥ natF hi then hi else t.toUInt64.toNat
  let bx0 := fl fx0 cl.ix0 cl.ix1; let bx1 := max bx0 (fl (fx1 + K.one) cl.ix0 cl.ix1)
  let by0 := fl fy0 cl.iy0 cl.iy1; let by1 := max by0 (fl (fy1 + K.one) cl.iy0 cl.iy1)
  let bw := bx1 - bx0; let bh := by1 - by0
  if bw == 0 || bh == 0 then (acc, cv) else
  let tb : TriBuf := { bx0, by0, bw, bh, col := zeroBytes (4 * bw * bh), mask := zeroBytes (bw * bh) }
  let (buf, tb) := trianglesLoop acc.buf w h cl tb xs ys rgba idx nv 0 nt
  Accum.sweep { acc with buf } cv cl false (triPainter tb)

end LeanPlot.Raster
