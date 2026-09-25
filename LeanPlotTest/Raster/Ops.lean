import LeanPlotTest.Raster.Util

/-!
Tests for the non-path ops: `segments` (per-segment colours, caps, runs),
`triangles` (seamless Gouraud meshes), `image` (nearest, bilinear, flips,
clip, AA edges), `text` (stub and custom outliner), plus scene-level
behaviour (background, painter's order, PNG export).
-/

namespace LeanPlotTest.Raster.OpsTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Pixel equality with a per-channel tolerance. -/
def pxNear (p : Px) (r g b : Nat) (tol : Nat := 0) : Bool :=
  let d (u : UInt8) (v : Nat) : Nat := if u.toNat ≥ v then u.toNat - v else v - u.toNat
  d p.r r ≤ tol && d p.g g ≤ tol && d p.b b ≤ tol

/-- All op checks. -/
def tests : T Unit := do
  -- segments: three horizontal segments in three colours, width 3, pixel aligned
  let xs : FloatArray := ⟨#[10, 90, 10, 90, 10, 90]⟩
  let ys : FloatArray := ⟨#[20.5, 20.5, 40.5, 40.5, 60.5, 60.5]⟩
  let cols : ByteArray := ⟨#[255, 0, 0, 255,  0, 255, 0, 255,  0, 0, 255, 255]⟩
  let cv := paint 100 80 #[.segments xs ys cols 3 .butt none]
  check "segment 0 colour" (pxNear (cv.get 50 20) 255 0 0) s!"{repr (cv.get 50 20)}"
  check "segment 1 colour" (pxNear (cv.get 50 41) 0 255 0)
  check "segment 2 colour" (pxNear (cv.get 50 59) 0 0 255)
  check "segment butt end" (pxNear (cv.get 9 20) 255 255 255 && pxNear (cv.get 90 20) 255 255 255)
  checkNear "segment width integral" (colDark cv 50) (3.0 * 3.0 * (1.0 - 1.0 / 3.0)) 0.05
  let cvr := paint 100 80 #[.segments xs ys cols 3 .round none]
  check "segment round cap reaches past end" (!(pxNear (cvr.get 90 20) 255 255 255))
  -- short colour array: the last colour is reused
  let cv1 := paint 100 80 #[.segments xs ys ⟨#[10, 20, 30, 255]⟩ 3 .butt none]
  check "short colour array reuses last" (pxNear (cv1.get 50 60) 10 20 30)
  -- a run of equal colours overlapping at alpha ½ is blended once
  let xs2 : FloatArray := ⟨#[10, 60, 40, 90]⟩
  let ys2 : FloatArray := ⟨#[30, 30, 30, 30]⟩
  let half : ByteArray := ⟨#[0, 0, 0, 128, 0, 0, 0, 128]⟩
  let cvh := paint 100 60 #[.segments xs2 ys2 half 4 .butt none]
  check "equal-colour run blended once" ((cvh.get 50 30).r == (cvh.get 20 30).r) s!"{(cvh.get 50 30).r} vs {(cvh.get 20 30).r}"
  check "empty segments" ((paint 10 10 #[.segments {} {} {} 3 .butt none]).data == (Canvas.fill 10 10 .white).data)
  -- triangles: a quad split along its diagonal in one colour has no seam
  let tx : FloatArray := ⟨#[10.3, 90.6, 90.6, 10.3]⟩
  let ty : FloatArray := ⟨#[10.2, 10.2, 70.7, 70.7]⟩
  let red : ByteArray := ⟨#[200, 30, 30, 255, 200, 30, 30, 255, 200, 30, 30, 255, 200, 30, 30, 255]⟩
  let quad := packIdx [0, 1, 2, 0, 2, 3]
  let cvq := paint 100 80 #[.triangles tx ty red quad none]
  let mut seamOk := true
  for x in [12:89] do
    -- the diagonal runs from (10.3,10.2) to (90.6,70.7)
    let y := (10.2 + (x.toFloat + 0.5 - 10.3) * (60.5 / 80.3)).floor.toUInt64.toNat
    if !(pxNear (cvq.get x y) 200 30 30) then seamOk := false
  check "mesh has no seam on the shared edge" seamOk
  let areaQ := Id.run do
    let mut s := 0.0
    for i in [0:100*80] do s := s + (255.0 - (cvq.data.get! (4*i+1)).toFloat) / (255.0 - 30.0)
    return s
  checkNear "mesh union area" areaQ (80.3 * 60.5) 1.0
  -- Gouraud: the colour at the centroid is the vertex average
  let gx : FloatArray := ⟨#[10, 90, 50]⟩
  let gy : FloatArray := ⟨#[70, 70, 10]⟩
  let gc : ByteArray := ⟨#[255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255]⟩
  let cvg := paint 100 80 #[.triangles gx gy gc (packIdx [0, 1, 2]) none]
  -- centroid (50, 50) → (85, 85, 85); pixel centre (50.5, 50.5) is within 1 px
  check "gouraud centroid" (pxNear (cvg.get 50 50) 85 85 85 4) s!"{repr (cvg.get 50 50)}"
  check "gouraud near vertex 0" (pxNear (cvg.get 13 68) 240 10 5 25) s!"{repr (cvg.get 13 68)}"
  -- a 64-triangle fan (disc) in one colour: every interior pixel is exact
  let n := 64
  let fx : FloatArray := Id.run do
    let mut a : FloatArray := ⟨#[50.0]⟩
    for i in [0:n] do a := a.push (50.0 + 35.0 * Float.cos (6.283185307179586 * i.toFloat / n.toFloat))
    return a
  let fy : FloatArray := Id.run do
    let mut a : FloatArray := ⟨#[40.0]⟩
    for i in [0:n] do a := a.push (40.0 + 35.0 * Float.sin (6.283185307179586 * i.toFloat / n.toFloat))
    return a
  let fc : ByteArray := Id.run do
    let mut b : ByteArray := {}
    for _ in [0:n+1] do b := (((b.push 20).push 120).push 220).push 255
    return b
  let fidx := packIdx ((List.range n).flatMap fun i => [0, i + 1, (i + 1) % n + 1])
  let cvf := paint 100 80 #[.triangles fx fy fc fidx none]
  let mut fanOk := true
  for y in [10:70] do
    for x in [20:80] do
      let dx := x.toFloat + 0.5 - 50.0; let dy := y.toFloat + 0.5 - 40.0
      if dx * dx + dy * dy < 33.0 * 33.0 && !(pxNear (cvf.get x y) 20 120 220) then fanOk := false
  check "fan interior exact (no seams)" fanOk
  -- invalid indices are skipped
  let cvbad := paint 50 50 #[.triangles gx gy gc (packIdx [0, 1, 7]) none]
  check "invalid triangle skipped" (cvbad.data == (Canvas.fill 50 50 .white).data)
  -- image: 2×2 nearest into an integer rect replicates texels exactly
  let img : ByteArray := ⟨#[255, 0, 0, 255,  0, 255, 0, 255,  0, 0, 255, 255,  255, 255, 0, 255]⟩
  let cvi := paint 100 100 #[.image 2 2 img ⟨10, 20, 60, 40⟩ .nearest none]
  check "nearest quadrant TL" (pxNear (cvi.get 10 20) 255 0 0 && pxNear (cvi.get 39 39) 255 0 0)
  check "nearest quadrant TR" (pxNear (cvi.get 40 20) 0 255 0 && pxNear (cvi.get 69 39) 0 255 0)
  check "nearest quadrant BL" (pxNear (cvi.get 10 40) 0 0 255)
  check "nearest quadrant BR" (pxNear (cvi.get 69 59) 255 255 0)
  check "nearest outside untouched" (pxNear (cvi.get 70 30) 255 255 255 && pxNear (cvi.get 9 30) 255 255 255)
  -- flipped destination (negative width) mirrors the image
  let cvflip := paint 100 100 #[.image 2 2 img ⟨70, 20, -60, 40⟩ .nearest none]
  check "flipped image" (pxNear (cvflip.get 10 20) 0 255 0 && pxNear (cvflip.get 69 20) 255 0 0)
  -- bilinear: black→white ramp, midpoint grey, monotone
  let ramp : ByteArray := ⟨#[0, 0, 0, 255, 255, 255, 255, 255]⟩
  let cvb := paint 200 10 #[.image 2 1 ramp ⟨0, 0, 200, 10⟩ .linear none]
  check "bilinear clamps at the ends" (pxNear (cvb.get 10 5) 0 0 0 && pxNear (cvb.get 190 5) 255 255 255)
  check "bilinear midpoint" (pxNear (cvb.get 100 5) 128 128 128 2) s!"{repr (cvb.get 100 5)}"
  let mut mono := true
  for x in [0:199] do
    if (cvb.get (x+1) 5).r < (cvb.get x 5).r then mono := false
  check "bilinear monotone" mono
  -- premultiplied bilinear: a transparent texel does not darken its neighbour
  let tp : ByteArray := ⟨#[255, 0, 0, 255, 0, 0, 0, 0]⟩
  let cvt := paint 200 10 #[.image 2 1 tp ⟨0, 0, 200, 10⟩ .linear none]
  let pm := cvt.get 100 5
  check "premultiplied interpolation keeps hue" (pm.r == 255 && pm.g == pm.b && pm.g > 100) s!"{repr pm}"
  -- fractional destination edges are antialiased
  let cvfr := paint 20 20 #[.image 1 1 ⟨#[0, 0, 0, 255]⟩ ⟨2.5, 2.0, 5.0, 5.0⟩ .nearest none]
  check "image AA edge" (pxNear (cvfr.get 2 3) 128 128 128 1 && pxNear (cvfr.get 7 3) 128 128 128 1)
  checkNear "image area" (darkArea cvfr) 25.0 0.05
  -- clip
  let cvcl := paint 100 100 #[.image 2 2 img ⟨10, 20, 60, 40⟩ .nearest (some ⟨0, 0, 30, 100⟩)]
  check "image clipped" (pxNear (cvcl.get 29 25) 255 0 0 && pxNear (cvcl.get 30 25) 255 255 255)
  check "short image buffer ignored" ((paint 10 10 #[.image 4 4 ⟨#[0, 0, 0, 255]⟩ ⟨0, 0, 10, 10⟩ .nearest none]).data ==
    (Canvas.fill 10 10 .white).data)
  -- text: a text op is exactly the fill of its outline; these checks hold for
  -- the stub outliner and for the font module's once it is wired in
  let txt : DrawOp := .text 10 20 "Hello" { color := .black } none
  let txtPath := defaultTextOutliner { color := .black } "Hello" 10 20
  check "text op = filled default outline"
    ((paint 50 50 #[txt]).data == (paint 50 50 #[.path txtPath (some { color := .black }) none none]).data)
  check (if txtPath.verbs.isEmpty then "text stub draws nothing" else "wired outliner draws ink")
    (if txtPath.verbs.isEmpty then (paint 50 50 #[txt]).data == (Canvas.fill 50 50 .white).data
     else darkArea (paint 50 50 #[txt]) > 10.0)
  let boxOutliner : TextOutliner := fun st s x y => Path.rect ⟨x, y - st.size, 6.0 * s.length.toFloat, st.size⟩
  let sc : Scene := { width := 50, height := 50, ops := #[txt] }
  let cvtext := sc.toCanvas boxOutliner
  checkNear "custom outliner filled" (darkArea cvtext) (30.0 * 12.0) 1.0e-6
  check "renderText = text op" ((renderText { color := .black } "x" 5 5 (Canvas.fill 20 20 .white)).data ==
    (paint 20 20 #[.text 5 5 "x" { color := .black } none]).data)
  -- scene: background and painter's order
  let ops2 : Array DrawOp := #[.path (Path.rect ⟨0, 0, 10, 10⟩) (some { color := ⟨1, 0, 0, 1⟩ }) none none,
    .path (Path.rect ⟨5, 5, 10, 10⟩) (some { color := ⟨0, 0, 1, 1⟩ }) none none]
  let sc2 : Scene := { width := 20, height := 20, background := ⟨0.1, 0.2, 0.3, 1⟩, ops := ops2 }
  let cvs := sc2.toCanvas
  check "background" (pxNear (cvs.get 18 2) 26 51 77)
  check "painter's order" (pxNear (cvs.get 7 7) 0 0 255 && pxNear (cvs.get 2 2) 255 0 0)
  -- fill then stroke on one path op: the stroke is on top
  let fs := paint 40 40 #[.path (Path.rect ⟨10, 10, 20, 20⟩) (some { color := ⟨1, 0, 0, 1⟩ }) (some { color := .black, width := 2 }) none]
  check "stroke over fill" (pxNear (fs.get 10 20) 0 0 0 && pxNear (fs.get 20 20) 255 0 0)
  -- PNG export decodes to the canvas bytes
  check "scene png round trip" (match PNG.decodeRGBA sc2.toPNG with
    | .ok (w, h, px) => w == 20 && h == 20 && px == cvs.data | _ => false)
  let tr := (Canvas.transparent 8 8).drawOps #[.path (Path.rect ⟨0, 0, 4, 4⟩) (some { color := ⟨1, 0, 0, 0.5⟩ }) none none]
  check "transparent canvas exports RGBA" (match PNG.decode tr.toPNG with
    | .ok img => img.colorType == .rgba8 && img.pixels == tr.data | _ => false)
  check "empty scene" ((({ width := 3, height := 2 } : Scene).toCanvas).data.size == 24)
  check "zero-size scene" ((({ width := 0, height := 5 } : Scene).toCanvas).data.size == 0)
  let allOps : Array DrawOp := #[.path (Path.rect ⟨0, 0, 5, 5⟩) (some {}) (some {}) none,
    .segments xs ys cols 3 .round none, .triangles gx gy gc (packIdx [0, 1, 2]) none,
    .image 2 2 img ⟨0, 0, 5, 5⟩ .linear none, txt]
  check "zero-width scene with ops" ((({ width := 0, height := 5, ops := allOps } : Scene).toCanvas).data.size == 0)
  check "zero-height scene with ops" ((({ width := 7, height := 0, ops := allOps } : Scene).toCanvas).data.size == 0)
  check "1×1 scene with ops" ((({ width := 1, height := 1, ops := allOps } : Scene).toCanvas).data.size == 4)

/-- Run the ops suite. -/
def run : IO (Nat × Nat) := runSuite "ops" tests

end LeanPlotTest.Raster.OpsTests
