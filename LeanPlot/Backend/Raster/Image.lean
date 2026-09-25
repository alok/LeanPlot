import LeanPlot.Backend.Raster.Coverage

/-
`image` op: blit a `w × h` RGBA8 image (row-major, top row first) into a
destination rectangle, with nearest or bilinear sampling.

Each canvas pixel whose area meets `dst ∩ clip` samples the source at its
centre. The source coordinate is `u = (px + ½ − dst.x) / dst.w · w`, so a
negative `dst.w` or `dst.h` flips the image. Nearest picks the texel
containing `u`, which is Makie's `interpolate = false` heatmap look. Bilinear
interpolates premultiplied RGBA between texel centres, clamping at the
borders. The destination's fractional edges are antialiased by the pixel
overlap.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Index clamp to `[0, n-1]` from a float coordinate. -/
@[inline] def clampIdx (v : Float) (n : Nat) : Nat :=
  if !(v > K.zero) then 0 else
  let k := v.toUInt64.toNat
  if k ≥ n then n - 1 else k

/-- Image sampling parameters (read-only during the blit). -/
structure Blit where
  iw : Nat
  ih : Nat
  src : ByteArray
  dx : Float
  dy : Float
  dw : Float
  dh : Float
  /-- overlap region (fractional), already intersected with the clip -/
  ox0 : Float
  oy0 : Float
  ox1 : Float
  oy1 : Float
  linear : Bool

/-- Sample and blend pixel `(px, py)` with area factor `f`. -/
@[inline] def blitPx {w h : Nat} (b : Blit) (cv : Canvas w h) (px py : Nat) (f : Float) : Canvas w h :=
  let u := (natF px + K.half - b.dx) / b.dw * natF b.iw
  let v := (natF py + K.half - b.dy) / b.dh * natF b.ih
  let o := Canvas.offset w px py
  if !b.linear then
    let sx := clampIdx u b.iw; let sy := clampIdx v b.ih
    let i := 4 * (sy * b.iw + sx)
    let a := (b.src.get! (i+3)).toFloat / K.c255 * f
    if a ≤ K.zero then cv else
    cv.blend o (b.src.get! i).toFloat (b.src.get! (i+1)).toFloat (b.src.get! (i+2)).toFloat a
  else
    let fu := u - K.half; let fv := v - K.half
    let x0f := fu.floor; let y0f := fv.floor
    let tx := fu - x0f; let ty := fv - y0f
    let x0 := clampIdx x0f b.iw; let x1 := clampIdx (x0f + K.one) b.iw
    let y0 := clampIdx y0f b.ih; let y1 := clampIdx (y0f + K.one) b.ih
    let tex (sx sy : Nat) : Nat := 4 * (sy * b.iw + sx)
    let i00 := tex x0 y0; let i10 := tex x1 y0; let i01 := tex x0 y1; let i11 := tex x1 y1
    let w00 := (K.one - tx) * (K.one - ty); let w10 := tx * (K.one - ty)
    let w01 := (K.one - tx) * ty; let w11 := tx * ty
    let al (i : Nat) : Float := (b.src.get! (i+3)).toFloat
    let a00 := al i00; let a10 := al i10; let a01 := al i01; let a11 := al i11
    let A := w00 * a00 + w10 * a10 + w01 * a01 + w11 * a11
    if A ≤ K.zero then cv else
    let ch (c : Nat) : Float :=
      (w00 * a00 * (b.src.get! (i00 + c)).toFloat + w10 * a10 * (b.src.get! (i10 + c)).toFloat +
       w01 * a01 * (b.src.get! (i01 + c)).toFloat + w11 * a11 * (b.src.get! (i11 + c)).toFloat) / A
    cv.blend o (ch 0) (ch 1) (ch 2) (A / K.c255 * f)

/-- Blit columns `[x, xEnd)` of row `y` with row factor `fy`. -/
def blitCols {w h : Nat} (b : Blit) (cv : Canvas w h) (y : Nat) (fy : Float) (x xEnd : Nat) : Canvas w h :=
  if x < xEnd then
    let f := fy * Clip.overlap x b.ox0 b.ox1
    blitCols b (if f > Accum.covEps then blitPx b cv x y f else cv) y fy (x + 1) xEnd
  else cv
termination_by xEnd - x

/-- Blit rows `[y, yEnd)`. -/
def blitRows {w h : Nat} (b : Blit) (cv : Canvas w h) (x0 x1 y yEnd : Nat) : Canvas w h :=
  if y < yEnd then
    let fy := Clip.overlap y b.oy0 b.oy1
    blitRows b (if fy > Accum.covEps then blitCols b cv y fy x0 x1 else cv) x0 x1 (y + 1) yEnd
  else cv
termination_by yEnd - y

/-- Render an `image` op. -/
def renderImage {w h : Nat} (cv : Canvas w h) (cl : Clip) (iw ih : Nat) (rgba : ByteArray) (dst : Rect)
    (interp : Interp) : Canvas w h :=
  if iw == 0 || ih == 0 || rgba.size < 4 * (iw * ih) || cl.isEmpty then cv else
  if !(dst.x.isFinite && dst.y.isFinite && dst.w.isFinite && dst.h.isFinite) || dst.w == K.zero || dst.h == K.zero then cv else
  let rx0 := min dst.x (dst.x + dst.w); let rx1 := max dst.x (dst.x + dst.w)
  let ry0 := min dst.y (dst.y + dst.h); let ry1 := max dst.y (dst.y + dst.h)
  let ox0 := max rx0 cl.x0; let ox1 := min rx1 cl.x1
  let oy0 := max ry0 cl.y0; let oy1 := min ry1 cl.y1
  if !(ox0 < ox1 && oy0 < oy1) then cv else
  let b : Blit := { iw, ih, src := rgba, dx := dst.x, dy := dst.y, dw := dst.w, dh := dst.h,
                    ox0, oy0, ox1, oy1, linear := interp == .linear }
  let x0 := Clip.toNatFloor ox0; let x1 := min w (Clip.toNatFloor ox1.ceil)
  let y0 := Clip.toNatFloor oy0; let y1 := min h (Clip.toNatFloor oy1.ceil)
  blitRows b cv x0 x1 y0 y1

end LeanPlot.Raster
