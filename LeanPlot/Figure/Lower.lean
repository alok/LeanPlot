import LeanPlot.Scene.Mark
import LeanPlot.Figure.Text

/-!
# Lowering marks to draw ops

Given a `Projector` (data point ↦ device pixel plus depth), every `Mark` becomes a list of
device-space `DrawOp`s, following how CairoMakie draws the corresponding Makie plot:

* `lines`: one path per mark; NaN points lift the pen; a run whose last point is
  (approximately) its first is closed (CairoMakie `draw_single_lines`). Per-vertex colours
  become a `segments` op (one colour per segment: the mean of its endpoint colours, where
  Cairo uses a two-stop gradient).
* `segments`: point pairs, one path or one `segments` op for per-element colours.
* `scatter`: marker outlines from `MarkerShape` scaled by the marker size, filled, with an
  optional stroke; consecutive opaque markers of equal colour share one path.
* `band`: one polygon per NaN-free run (`lower` forwards, `upper` backwards).
* `poly`: filled rings with an optional outline.
* `text`: a `text` op per string at the projected point plus the pixel offset.
* `heatmap`: an `image` op (nearest) when the projected cell edges are uniform and
  axis-aligned, otherwise flat-coloured quads as `triangles`.
* `image`: an `image` op (bilinear) into the projected rectangle.
* `mesh`: a `triangles` op with per-vertex colours; with `depthSort` the triangles are
  painted back to front (CairoMakie's `draw_mesh3D`).
* `arrows`: Makie `arrows2d` geometry in pixel space (shaft rectangle, triangular tip,
  scaled down when the arrow is shorter than its parts, plus the `strokemask` outline).
-/

namespace LeanPlot.Lower

open LeanPlot LeanPlot.Num

/-- Maps data points to device pixels. `project p = (x, y, depth)`, y down; `depth` is only
meaningful for 3D axes (larger is farther). -/
structure Projector where
  project : Vec3 → Vec3
  /-- Clip rectangle for everything drawn (the axis viewport). -/
  clip : Option Rect
  /-- The projection is an axis-aligned scaling (2D axes with linear scales): heatmaps can be
  drawn as images. -/
  axisAligned : Bool := false
  /-- Paint mesh triangles back to front. -/
  depthSort : Bool := false

/-- Project positions; non-finite inputs become NaN. Returns device `xs`, `ys`, depths. -/
def projectPos (pr : Projector) (p : Pos) : FloatArray × FloatArray × FloatArray :=
  let n := p.size
  let rec go (i : Nat) (ox oy oz : FloatArray) : FloatArray × FloatArray × FloatArray :=
    if i < n then
      let v := p.get3 i
      if v.isFinite then
        let q := pr.project v
        if q.x.isFinite && q.y.isFinite then go (i + 1) (ox.push q.x) (oy.push q.y) (oz.push q.z)
        else go (i + 1) (ox.push nan) (oy.push nan) (oz.push nan)
      else go (i + 1) (ox.push nan) (oy.push nan) (oz.push nan)
    else (ox, oy, oz)
  termination_by n - i
  go 0 (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n) (FloatArray.emptyWithCapacity n)

/-- Julia `isapprox` for 2D points (default `rtol = √eps(Float32)`, norm-based). -/
@[inline] def approxPt (ax ay bx by_ : Float) : Bool :=
  let d := Float.sqrt ((ax - bx) * (ax - bx) + (ay - by_) * (ay - by_))
  let na := Float.sqrt (ax * ax + ay * ay)
  let nb := Float.sqrt (by_ * by_ + bx * bx)
  d ≤ 0.00034526698 * (if na > nb then na else nb)

/-- The polyline path of NaN-separated runs; a run ending (approximately) where it started is
closed. -/
def polylinePath (xs ys : FloatArray) : Path :=
  let n := min xs.size ys.size
  let rec go (i : Nat) (p : Path) (inRun : Bool) (sx sy lx ly : Float) (runLen : Nat) : Path :=
    if i < n then
      let x := xs[i]!
      let y := ys[i]!
      if x.isNaN || y.isNaN then
        let p := if inRun && runLen > 2 && approxPt lx ly sx sy then p.close else p
        go (i + 1) p false sx sy lx ly 0
      else if inRun then go (i + 1) (p.lineTo x y) true sx sy x y (runLen + 1)
      else go (i + 1) (p.moveTo x y) true x y x y 1
    else if inRun && runLen > 2 && approxPt lx ly sx sy then p.close else p
  termination_by n - i
  go 0 {} false 0 0 0 0 0

/-- Segment pairs as one path (pairs with a NaN end are skipped). -/
def segmentsPath (xs ys : FloatArray) : Path :=
  let n := min xs.size ys.size / 2
  let rec go (k : Nat) (p : Path) : Path :=
    if k < n then
      let x0 := xs[2 * k]!
      let y0 := ys[2 * k]!
      let x1 := xs[2 * k + 1]!
      let y1 := ys[2 * k + 1]!
      if x0.isNaN || y0.isNaN || x1.isNaN || y1.isNaN then go (k + 1) p
      else go (k + 1) ((p.moveTo x0 y0).lineTo x1 y1)
    else p
  termination_by n - k
  go 0 {}

/-- The device stroke of a line style with colour `c`. -/
def strokeOf (s : LineSpec) (c : RGBA) : Stroke :=
  { color := c, width := s.width, cap := s.cap, join := s.join, miterLimit := s.miterLimit
    dash := s.style.dashArray s.width }

/-- Mean of two RGBA8 colours stored at elements `i` and `j`. -/
@[inline] def pushMean (acc src : ByteArray) (i j : Nat) : ByteArray :=
  let m (c : Nat) : UInt8 := ((src.get! (4 * i + c)).toNat + (src.get! (4 * j + c)).toNat + 1) / 2 |>.toUInt8
  (((acc.push (m 0)).push (m 1)).push (m 2)).push (m 3)

/-- Segments `i → i+1` of a polyline with per-vertex colours (skipping NaN ends). -/
def coloredPolylineSegments (xs ys : FloatArray) (rgba : ByteArray) : FloatArray × FloatArray × ByteArray :=
  let n := min xs.size ys.size
  let rec go (i : Nat) (ox oy : FloatArray) (oc : ByteArray) : FloatArray × FloatArray × ByteArray :=
    if i + 1 < n then
      let x0 := xs[i]!
      let y0 := ys[i]!
      let x1 := xs[i + 1]!
      let y1 := ys[i + 1]!
      if x0.isNaN || y0.isNaN || x1.isNaN || y1.isNaN then go (i + 1) ox oy oc
      else go (i + 1) ((ox.push x0).push x1) ((oy.push y0).push y1) (pushMean oc rgba i (i + 1))
    else (ox, oy, oc)
  termination_by n - i
  go 0 .empty .empty .empty

/-- Segment pairs with per-vertex (`perVertex`) or per-segment colours. -/
def coloredSegments (xs ys : FloatArray) (rgba : ByteArray) (perVertex : Bool) :
    FloatArray × FloatArray × ByteArray :=
  let n := min xs.size ys.size / 2
  let rec go (k : Nat) (ox oy : FloatArray) (oc : ByteArray) : FloatArray × FloatArray × ByteArray :=
    if k < n then
      let x0 := xs[2 * k]!
      let y0 := ys[2 * k]!
      let x1 := xs[2 * k + 1]!
      let y1 := ys[2 * k + 1]!
      if x0.isNaN || y0.isNaN || x1.isNaN || y1.isNaN then go (k + 1) ox oy oc
      else
        let oc := if perVertex then pushMean oc rgba (2 * k) (2 * k + 1) else pushMean oc rgba k k
        go (k + 1) ((ox.push x0).push x1) ((oy.push y0).push y1) oc
    else (ox, oy, oc)
  termination_by n - k
  go 0 .empty .empty .empty

/-- `lines`. -/
def lines (pr : Projector) (p : Pos) (s : LineSpec) : Array DrawOp :=
  let (xs, ys, _) := projectPos pr p
  match s.color with
  | .solid c => #[.path (polylinePath xs ys) none (some (strokeOf s c)) pr.clip]
  | col =>
    let rgba := col.resolve p.size
    let (sx, sy, sc) := coloredPolylineSegments xs ys rgba
    #[.segments sx sy sc s.width s.cap pr.clip]

/-- `linesegments`: per-vertex colours when there is one colour per point, per-segment
colours when there is one per pair. -/
def segments (pr : Projector) (p : Pos) (s : LineSpec) : Array DrawOp :=
  let (xs, ys, _) := projectPos pr p
  match s.color with
  | .solid c => #[.path (segmentsPath xs ys) none (some (strokeOf s c)) pr.clip]
  | col =>
    let nPts := p.size
    let count := match col with
      | .perElement b => b.size / 4
      | .values vs _ => vs.size
      | .solid _ => nPts
    let perVertex := count ≥ nPts
    let rgba := col.resolve (if perVertex then nPts else nPts / 2)
    let (sx, sy, sc) := coloredSegments xs ys rgba perVertex
    #[.segments sx sy sc s.width s.cap pr.clip]

/-- `scatter`: markers, grouped into one path per run of equal opaque colours. -/
def scatter (pr : Projector) (p : Pos) (s : MarkerSpec) : Array DrawOp :=
  let (xs, ys, _) := projectPos pr p
  let n := xs.size
  let rgba := s.color.resolve n
  let stroke : Option Stroke :=
    if s.strokeWidth > 0 && s.strokeColor.a > 0 then some { color := s.strokeColor, width := s.strokeWidth } else none
  let flush (ops : Array DrawOp) (path : Path) (c : RGBA) : Array DrawOp :=
    if path.verbs.isEmpty then ops else ops.push (.path path (some { color := c }) stroke pr.clip)
  let rec go (i : Nat) (ops : Array DrawOp) (path : Path) (cur : RGBA) : Array DrawOp :=
    if i < n then
      let x := xs[i]!
      let y := ys[i]!
      let sz := s.sizeAt i
      if x.isNaN || y.isNaN || !(sz > 0) || sz.isNaN then go (i + 1) ops path cur else
      let c := RGBA.ofRGBA8At rgba i
      if c.a == 0 && stroke.isNone then go (i + 1) ops path cur else
      -- opaque markers of the same colour can share one path; translucent ones are separate
      if c == cur && c.a == 1 && !path.verbs.isEmpty then
        go (i + 1) ops (s.shape.appendPath sz x y path) cur
      else
        let ops := flush ops path cur
        go (i + 1) ops (s.shape.toPath sz x y) c
    else flush ops path cur
  termination_by n - i
  go 0 #[] {} RGBA.transparent

/-- Closed polygon path through projected points (NaN points skipped). -/
def ringPath (xs ys : FloatArray) (p : Path := {}) : Path :=
  let n := min xs.size ys.size
  let rec go (i : Nat) (p : Path) (started : Bool) : Path :=
    if i < n then
      let x := xs[i]!
      let y := ys[i]!
      if x.isNaN || y.isNaN then go (i + 1) p started
      else go (i + 1) (if started then p.lineTo x y else p.moveTo x y) true
    else if started then p.close else p
  termination_by n - i
  go 0 p false

/-- `band`: one polygon per NaN-free run of `(lower[k], upper[k])` pairs. -/
def band (pr : Projector) (lo hi : Pos) (fill : ColorSpec) : Array DrawOp :=
  let (lx, ly, _) := projectPos pr lo
  let (ux, uy, _) := projectPos pr hi
  let n := min lx.size ux.size
  let valid (k : Nat) : Bool := !(lx[k]!.isNaN || ly[k]!.isNaN || ux[k]!.isNaN || uy[k]!.isNaN)
  -- polygon of the run [a, b)
  let runPath (a b : Nat) (p : Path) : Path :=
    let rec fwd (k : Nat) (p : Path) : Path :=
      if k < b then fwd (k + 1) (if k == a then p.moveTo lx[k]! ly[k]! else p.lineTo lx[k]! ly[k]!) else p
    termination_by b - k
    let rec bwd (j : Nat) (p : Path) : Path :=
      match j with
      | 0 => p
      | j + 1 => if j + 1 > a then bwd j (p.lineTo ux[j]! uy[j]!) else p
    termination_by j
    (bwd b (fwd a p)).close
  let rec scan (k : Nat) (start : Option Nat) (p : Path) : Path :=
    if k < n then
      if valid k then scan (k + 1) (start.orElse fun _ => some k) p
      else match start with
        | some a => scan (k + 1) none (if k - a ≥ 2 then runPath a k p else p)
        | none => scan (k + 1) none p
    else match start with
      | some a => if n - a ≥ 2 then runPath a n p else p
      | none => p
  termination_by n - k
  let path := scan 0 none {}
  if path.verbs.isEmpty then #[] else
  match fill with
  | .solid c => #[.path path (some { color := c }) none pr.clip]
  | col =>
    -- per-vertex colours: two triangles per quad between consecutive valid pairs
    let rgbaLo := col.resolve n
    let rgbaHi := match col with
      | .perElement b => if b.size ≥ 8 * n then b.extract (4 * n) (8 * n) else rgbaLo
      | .values vs m => if vs.size ≥ 2 * n then
          let (lo', hi') := m.rangeFor vs
          m.toRGBA8 lo' hi' ⟨vs.data.extract n (2 * n)⟩
        else rgbaLo
      | _ => rgbaLo
    let xs : FloatArray := ⟨lx.data ++ ux.data⟩
    let ys : FloatArray := ⟨ly.data ++ uy.data⟩
    let rgba := rgbaLo ++ rgbaHi
    let pushTri (idx : ByteArray) (a b c : Nat) : ByteArray :=
      let pushU32 (acc : ByteArray) (v : Nat) : ByteArray :=
        (((acc.push v.toUInt8).push (v >>> 8).toUInt8).push (v >>> 16).toUInt8).push (v >>> 24).toUInt8
      pushU32 (pushU32 (pushU32 idx a) b) c
    let rec tris (k : Nat) (idx : ByteArray) : ByteArray :=
      if k + 1 < n then
        if valid k && valid (k + 1) then
          tris (k + 1) (pushTri (pushTri idx k (k + 1) (n + k)) (k + 1) (n + k + 1) (n + k))
        else tris (k + 1) idx
      else idx
    termination_by n - k
    #[.triangles xs ys rgba (tris 0 .empty) pr.clip]

/-- `poly`: each ring filled with its colour (`perElement`/`values` give one colour per
ring), then outlined. -/
def poly (pr : Projector) (rings : Array Pts2) (s : PolySpec) : Array DrawOp :=
  let n := rings.size
  let rgba := s.color.resolve n
  let stroke : Option Stroke :=
    if s.strokeWidth > 0 && s.strokeColor.a > 0 then
      some { color := s.strokeColor, width := s.strokeWidth, miterLimit := 2.0
             dash := s.strokeStyle.dashArray s.strokeWidth }
    else none
  (Array.range n).filterMap fun k =>
    let (xs, ys, _) := projectPos pr (.xy rings[k]!)
    let path := ringPath xs ys
    if path.verbs.isEmpty then none else
    let c := RGBA.ofRGBA8At rgba k
    some (.path path (if c.a > 0 then some { color := c } else none) stroke pr.clip)

/-- `text`: one op per string, at the projected point plus the pixel offset (y up). -/
def text (pr : Projector) (p : Pos) (strs : Array String) (s : TextSpec) : Array DrawOp :=
  let (xs, ys, _) := projectPos pr p
  let style := s.toStyle
  (Array.range (min xs.size strs.size)).filterMap fun i =>
    let x := xs[i]!
    let y := ys[i]!
    if x.isNaN || y.isNaN || FigText.isBlank strs[i]! then none
    else some (.text (x + s.offset.1) (y - s.offset.2) strs[i]! style pr.clip)

/-- Little-endian `UInt32` append. -/
@[inline] def pushU32 (acc : ByteArray) (v : Nat) : ByteArray :=
  (((acc.push v.toUInt8).push (v >>> 8).toUInt8).push (v >>> 16).toUInt8).push (v >>> 24).toUInt8

/-- Are the entries (at least 2) evenly spaced (relative tolerance 1e-6 of the span)? -/
def uniform (xs : FloatArray) : Bool :=
  let n := xs.size
  if n < 2 then false else
  let span := xs[n - 1]! - xs[0]!
  let step := span / Num.ofInt ((n : Int) - 1)
  let tol := span.abs * 1.0e-6
  let rec go (i : Nat) : Bool :=
    if i < n then
      if (xs[i]! - (xs[0]! + step * Num.ofInt i)).abs ≤ tol then go (i + 1) else false
    else true
  termination_by n - i
  span.isFinite && span != 0 && go 0

/-- `heatmap`. -/
def heatmap (pr : Projector) (h : HeatmapData) : Array DrawOp :=
  let nx := h.z.nx
  let ny := h.z.ny
  if nx == 0 || ny == 0 || h.xs.size != nx + 1 || h.ys.size != ny + 1 then #[] else
  let (lo, hi) := h.mapping.rangeFor h.z.grid.z
  let colors := h.mapping.toRGBA8 lo hi h.z.grid.z
  -- projected edges along the axes (2D)
  let ex := h.xs.foldl (init := FloatArray.emptyWithCapacity (nx + 1)) fun acc x => acc.push (pr.project ⟨x, h.ys[0]!, 0⟩).x
  let ey := h.ys.foldl (init := FloatArray.emptyWithCapacity (ny + 1)) fun acc y => acc.push (pr.project ⟨h.xs[0]!, y, 0⟩).y
  if pr.axisAligned && uniform ex && uniform ey then
    -- image rows top first: device y increases downward
    let x0 := ex[0]!
    let x1 := ex[nx]!
    let y0 := ey[0]!
    let y1 := ey[ny]!
    let flipX := x1 < x0
    let flipY := y1 > y0 -- data j increasing goes down on screen (reversed y axis)
    let rec rowsGo (k : Nat) (acc : ByteArray) : ByteArray :=
      if k < nx * ny then
        let row := k / nx
        let col := k % nx
        let j := if flipY then row else ny - 1 - row
        let i := if flipX then nx - 1 - col else col
        let o := 4 * (i + nx * j)
        rowsGo (k + 1) ((((acc.push (colors.get! o)).push (colors.get! (o + 1))).push (colors.get! (o + 2))).push (colors.get! (o + 3)))
      else acc
    termination_by nx * ny - k
    let img := rowsGo 0 (ByteArray.emptyWithCapacity (4 * nx * ny))
    let dst : Rect := Rect.ofCorners x0 y0 x1 y1
    #[.image nx ny img dst .nearest pr.clip]
  else
    -- flat quads: 4 vertices per cell
    let rec cells (k : Nat) (xs ys : FloatArray) (rgba idx : ByteArray) (v : Nat) :
        FloatArray × FloatArray × ByteArray × ByteArray :=
      if k < nx * ny then
        let i := k % nx
        let j := k / nx
        let o := 4 * k
        let a := colors.get! (o + 3)
        if a == 0 then cells (k + 1) xs ys rgba idx v else
        let c0 := pr.project ⟨h.xs[i]!, h.ys[j]!, 0⟩
        let c1 := pr.project ⟨h.xs[i + 1]!, h.ys[j]!, 0⟩
        let c2 := pr.project ⟨h.xs[i + 1]!, h.ys[j + 1]!, 0⟩
        let c3 := pr.project ⟨h.xs[i]!, h.ys[j + 1]!, 0⟩
        if !(c0.x.isFinite && c0.y.isFinite && c1.x.isFinite && c1.y.isFinite &&
             c2.x.isFinite && c2.y.isFinite && c3.x.isFinite && c3.y.isFinite) then
          cells (k + 1) xs ys rgba idx v
        else
        let col (acc : ByteArray) : ByteArray :=
          (((acc.push (colors.get! o)).push (colors.get! (o + 1))).push (colors.get! (o + 2))).push a
        cells (k + 1) ((((xs.push c0.x).push c1.x).push c2.x).push c3.x)
          ((((ys.push c0.y).push c1.y).push c2.y).push c3.y) (col (col (col (col rgba))))
          (pushU32 (pushU32 (pushU32 (pushU32 (pushU32 (pushU32 idx v) (v + 1)) (v + 2)) v) (v + 2)) (v + 3)) (v + 4)
      else (xs, ys, rgba, idx)
    termination_by nx * ny - k
    let (xs, ys, rgba, idx) := cells 0 .empty .empty .empty .empty 0
    if idx.isEmpty then #[] else #[.triangles xs ys rgba idx pr.clip]

/-- `image`: an RGBA raster into the projected data rectangle (bilinear by default, like
Makie's `image`). -/
def image (pr : Projector) (x0 x1 y0 y1 : Float) (w h : Nat) (rgba : ByteArray) (interp : Interp) : Array DrawOp :=
  let a := pr.project ⟨x0, y0, 0⟩
  let b := pr.project ⟨x1, y1, 0⟩
  if !(a.x.isFinite && a.y.isFinite && b.x.isFinite && b.y.isFinite) || w == 0 || h == 0 then #[] else
  -- the raster's top row is at y1; flip when the axis direction reverses it
  let flipX := b.x < a.x
  let flipY := b.y > a.y
  let data :=
    if !flipX && !flipY then rgba else
    let rec go (k : Nat) (acc : ByteArray) : ByteArray :=
      if k < w * h then
        let row := k / w
        let col := k % w
        let r := if flipY then h - 1 - row else row
        let c := if flipX then w - 1 - col else col
        let o := 4 * (r * w + c)
        go (k + 1) ((((acc.push (rgba.get! o)).push (rgba.get! (o + 1))).push (rgba.get! (o + 2))).push (rgba.get! (o + 3)))
      else acc
    termination_by w * h - k
    go 0 (ByteArray.emptyWithCapacity (4 * w * h))
  #[.image w h data (Rect.ofCorners a.x a.y b.x b.y) interp pr.clip]

/-- Sort triangle indices back to front by mean depth (largest depth first, as CairoMakie
draws `reverse(sortperm(average_zs))`). -/
def depthOrder (zs : FloatArray) (tri : Array UInt32) : Array Nat :=
  let nt := tri.size / 3
  let key (t : Nat) : Float :=
    (zs.get! tri[3 * t]!.toNat + zs.get! tri[3 * t + 1]!.toNat + zs.get! tri[3 * t + 2]!.toNat) / 3
  let keyed := (Array.range nt).map fun t => (key t, t)
  -- stable descending by depth
  (keyed.qsort fun a b => a.1 > b.1 || (a.1 == b.1 && a.2 < b.2)).map (·.2)

/-- `mesh`: triangles with per-vertex colours (`colors` overrides the mark's colour, e.g.
after shading). -/
def mesh (pr : Projector) (m : MeshData) (colors : Option ByteArray := none) : Array DrawOp :=
  let pos := Pos.xyz m.mesh.pos
  let (xs, ys, zs) := projectPos pr pos
  let nv := xs.size
  let rgba := colors.getD (m.color.resolve nv)
  let tri := m.mesh.tri
  let nt := tri.size / 3
  let order : Array Nat := if pr.depthSort then depthOrder zs tri else Array.range nt
  let idx := order.foldl (init := ByteArray.emptyWithCapacity (12 * nt)) fun acc t =>
    let a := tri[3 * t]!.toNat
    let b := tri[3 * t + 1]!.toNat
    let c := tri[3 * t + 2]!.toNat
    if xs[a]!.isNaN || xs[b]!.isNaN || xs[c]!.isNaN || ys[a]!.isNaN || ys[b]!.isNaN || ys[c]!.isNaN then acc
    else pushU32 (pushU32 (pushU32 acc a) b) c
  if idx.isEmpty then #[] else #[.triangles xs ys rgba idx pr.clip]

/-- Makie `arrows2d` metrics for a pixel-space arrow of length `len`:
`(taillength, tailwidth, shaftlength, shaftwidth, tiplength, tipwidth)`. -/
def arrowMetrics (s : ArrowSpec) (len : Float) : Float × Float × Float × Float × Float × Float :=
  let shaft := clamp (len - s.taillength - s.tiplength) s.minshaftlength s.maxshaftlength
  let total := shaft + s.taillength + s.tiplength
  let k := if total > 0 then len / total else 0
  (k * s.taillength, k * s.tailwidth, k * shaft, k * s.shaftwidth, k * s.tiplength, k * s.tipwidth)

/-- `arrows2d`: start points `o - align·d'` with `d' = lengthscale · (normalize ? d/|d| : d)`,
drawn in pixel space: a shaft rectangle and a triangular tip (widths reduced by the stroke
mask), each filled and outlined with `strokemask` width in the arrow colour. -/
def arrows (pr : Projector) (origins dirs : Pos) (s : ArrowSpec) : Array DrawOp :=
  let n := min origins.size dirs.size
  let rgba := s.color.resolve n
  let rec go (i : Nat) (ops : Array DrawOp) : Array DrawOp :=
    if i < n then
      let p := origins.get3 i
      let d := dirs.get3 i
      let d := if s.normalize then Vec3.normalize d else d
      let d := Vec3.smul s.lengthscale d
      let st := p.sub (Vec3.smul s.align d)
      let en := st.add d
      if !(st.isFinite && en.isFinite) then go (i + 1) ops else
      let a := pr.project st
      let b := pr.project en
      let dx := b.x - a.x
      let dy := b.y - a.y
      let len := Float.sqrt (dx * dx + dy * dy)
      if !(len > 0) || !len.isFinite then go (i + 1) ops else
      let (tl, tw, sl, sw, pl, pw) := arrowMetrics s len
      let ux := dx / len
      let uy := dy / len
      -- perpendicular (device space)
      let vx := -uy
      let vy := ux
      let c := RGBA.ofRGBA8At rgba i
      let mask := s.strokemask
      let quad (off l w : Float) (p : Path) : Path :=
        let w := max 0 (w - mask) / 2
        let x0 := a.x + ux * off
        let y0 := a.y + uy * off
        let x1 := x0 + ux * l
        let y1 := y0 + uy * l
        ((((p.moveTo (x0 + vx * w) (y0 + vy * w)).lineTo (x1 + vx * w) (y1 + vy * w)).lineTo
          (x1 - vx * w) (y1 - vy * w)).lineTo (x0 - vx * w) (y0 - vy * w)).close
      let tip (off l w : Float) (p : Path) : Path :=
        let w := max 0 (w - mask) / 2
        let x0 := a.x + ux * off
        let y0 := a.y + uy * off
        (((p.moveTo (x0 + vx * w) (y0 + vy * w)).lineTo (x0 + ux * l) (y0 + uy * l)).lineTo
          (x0 - vx * w) (y0 - vy * w)).close
      let stroke : Option Stroke := if mask > 0 then some { color := c, width := mask, miterLimit := 2.0 } else none
      let fill : Option Fill := some { color := c }
      let ops := if tl > 0 && tw > 0 then ops.push (.path (quad 0 tl tw {}) fill stroke pr.clip) else ops
      let ops := if sw > 0 then ops.push (.path (quad tl sl sw {}) fill stroke pr.clip) else ops
      let ops := if pl > 0 && pw > 0 then ops.push (.path (tip (tl + sl) pl pw {}) fill stroke pr.clip) else ops
      go (i + 1) ops
    else ops
  termination_by n - i
  go 0 #[]

/-- Lower one mark. -/
def mark (pr : Projector) : Mark → Array DrawOp
  | .lines p s => lines pr p s
  | .segments p s => segments pr p s
  | .scatter p s => scatter pr p s
  | .band lo hi c => band pr lo hi c
  | .poly rs s => poly pr rs s
  | .text p strs s => text pr p strs s
  | .heatmap h => heatmap pr h
  | .image x0 x1 y0 y1 w h rgba interp => image pr x0 x1 y0 y1 w h rgba interp
  | .mesh m => mesh pr m
  | .arrows o d s => arrows pr o d s
  | .hlines .. | .vlines .. => #[]

end LeanPlot.Lower
