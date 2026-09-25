import LeanPlotTest.Raster.Util

/-!
Performance smoke tests. Each workload runs three times and the minimum wall
time is reported. A check fails only beyond 5× its target, so CI noise does
not flake the suite; the printed numbers are what `docs` records.

Targets (task spec): a 10⁵-segment polyline stroke on 1000×1000 < ~200 ms;
a typical 800×600 plot < 30 ms.
-/

namespace LeanPlotTest.Raster.PerfTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Minimum wall time over `k` runs of `f` (the result is forced by reading a pixel). -/
def bench {w h : Nat} (k : Nat) (f : Unit → Canvas w h) : IO (Float × Canvas w h) := do
  let mut best := 1.0e30
  let mut last : Canvas w h := Canvas.transparent w h
  for i in [0:k] do
    let (cv, ms) ← timeMs (IO.lazyPure fun _ => f ())
    -- touch the result so the work cannot be skipped
    if (cv.get (i % (max w 1)) 0).a == 7 then IO.println ""
    best := min best ms
    last := cv
  return (best, last)

/-- 10⁵-point sine sweep across a 1000 px canvas (≈32 periods, steep slopes). -/
def sineSweep (n : Nat) : Path := Id.run do
  let mut xs := FloatArray.emptyWithCapacity n
  let mut ys := FloatArray.emptyWithCapacity n
  for i in [0:n] do
    let x := 1000.0 * i.toFloat / n.toFloat
    xs := xs.push x
    ys := ys.push (500.0 + 400.0 * Float.sin (x / 5.0))
  return Path.polyline xs ys

/-- 10⁵-point random walk with ~3 px steps. -/
def randomWalk (n : Nat) : Path := Id.run do
  let mut xs := FloatArray.emptyWithCapacity n
  let mut ys := FloatArray.emptyWithCapacity n
  let mut x := 500.0; let mut y := 500.0
  let mut s : UInt64 := 12345
  for _ in [0:n] do
    s := s * 6364136223846793005 + 1442695040888963407
    let a := ((s >>> 11).toFloat / 9007199254740992.0) * 6.283185307179586
    x := max 5.0 (min 995.0 (x + 3.0 * Float.cos a))
    y := max 5.0 (min 995.0 (y + 3.0 * Float.sin a))
    xs := xs.push x; ys := ys.push y
  return Path.polyline xs ys

/-- A 10⁵-vertex star polygon. -/
def bigStar (n : Nat) : Path := Id.run do
  let mut p : Path := {}
  for i in [0:n] do
    let t := 6.283185307179586 * i.toFloat / n.toFloat
    let r := if i % 2 == 0 then 480.0 else 300.0
    let x := 500.0 + r * Float.cos t; let y := 500.0 + r * Float.sin t
    p := if i == 0 then p.moveTo x y else p.lineTo x y
  return p.close

/-- A filled band under a noisy 10⁵-point curve (a realistic large fill). -/
def band (n : Nat) : Path := Id.run do
  let mut p : Path := ({} : Path).moveTo 0 900
  for i in [0:n] do
    let x := 1000.0 * i.toFloat / n.toFloat
    p := p.lineTo x (500.0 + 300.0 * Float.sin (x / 40.0) + 20.0 * Float.sin (x * 7.3))
  return (p.lineTo 1000 900).close

/-- A typical 800×600 plot: frame, 11+11 grid lines, 5 series × 1000 points,
200 circle markers with outlines, a 500-segment colour-mapped line, a 64×48
heatmap, and a legend box. -/
def typicalPlot : Scene := Id.run do
  let mut s : Scene := { width := 800, height := 600 }
  let fr : Rect := ⟨70.5, 40.5, 690, 500⟩
  let grid : Stroke := { color := ⟨0.92, 0.92, 0.92, 1⟩, width := 1 }
  for k in [0:11] do
    let x := fr.x + fr.w * k.toFloat / 10.0
    let y := fr.y + fr.h * k.toFloat / 10.0
    s := s.push (.path (polyline [(x, fr.y), (x, fr.y + fr.h)]) none (some grid) none)
    s := s.push (.path (polyline [(fr.x, y), (fr.x + fr.w, y)]) none (some grid) none)
  -- heatmap inset
  let hm : ByteArray := Id.run do
    let mut b := ByteArray.emptyWithCapacity (64 * 48 * 4)
    for j in [0:48] do
      for i in [0:64] do
        b := (((b.push (i * 4).toUInt8).push (j * 5).toUInt8).push 160).push 255
    return b
  s := s.push (.image 64 48 hm ⟨600, 60, 128, 96⟩ .nearest (some fr))
  let colors : Array RGBA := #[⟨0, 0.447, 0.698, 1⟩, ⟨0.902, 0.624, 0, 1⟩, ⟨0, 0.620, 0.451, 1⟩,
                              ⟨0.8, 0.475, 0.655, 1⟩, ⟨0.337, 0.706, 0.914, 1⟩]
  for c in [0:5] do
    let mut xs := FloatArray.emptyWithCapacity 1000; let mut ys := FloatArray.emptyWithCapacity 1000
    for i in [0:1000] do
      let t := i.toFloat / 999.0
      xs := xs.push (fr.x + fr.w * t)
      ys := ys.push (fr.y + fr.h * (0.5 + 0.4 * Float.sin (7.0 * t + c.toFloat) * Float.exp (-t)))
    s := s.push (.path (Path.polyline xs ys) none (some { color := colors[c]!, width := 1.5 }) (some fr))
  -- colour-mapped segments
  let mut sx := FloatArray.emptyWithCapacity 1000; let mut sy := FloatArray.emptyWithCapacity 1000
  let mut sc := ByteArray.emptyWithCapacity 2000
  for i in [0:500] do
    let t0 := i.toFloat / 500.0; let t1 := (i + 1).toFloat / 500.0
    sx := (sx.push (fr.x + fr.w * t0)).push (fr.x + fr.w * t1)
    sy := (sy.push (fr.y + fr.h * (0.8 - 0.3 * t0 * t0))).push (fr.y + fr.h * (0.8 - 0.3 * t1 * t1))
    sc := (((sc.push (i / 2).toUInt8).push 80).push (255 - i / 2).toUInt8).push 255
  s := s.push (.segments sx sy sc 2.0 .round (some fr))
  -- markers
  for i in [0:200] do
    let t := i.toFloat / 199.0
    let p := circlePath (fr.x + fr.w * t) (fr.y + fr.h * (0.5 + 0.3 * Float.cos (11.0 * t))) 3.5
    s := s.push (.path p (some { color := ⟨0.9, 0.4, 0.1, 0.8⟩ }) (some { color := .black, width := 0.75 }) (some fr))
  -- frame and legend
  s := s.push (.path (Path.rect fr) none (some { color := .black, width := 1 }) none)
  s := s.push (.path (Path.rect ⟨620, 450, 120, 70⟩) (some { color := ⟨1, 1, 1, 0.9⟩ })
    (some { color := ⟨0.3, 0.3, 0.3, 1⟩, width := 1 }) none)
  return s

/-- `n` circle markers of radius 3.5 with outlines, one `path` op each,
scattered over a 1000×1000 canvas. -/
def markers (n : Nat) : Array DrawOp := Id.run do
  let mut ops := Array.emptyWithCapacity n
  let mut s : UInt64 := 777
  for _ in [0:n] do
    s := s * 6364136223846793005 + 1442695040888963407
    let x := 10.0 + 980.0 * ((s >>> 11).toFloat / 9007199254740992.0)
    s := s * 6364136223846793005 + 1442695040888963407
    let y := 10.0 + 980.0 * ((s >>> 11).toFloat / 9007199254740992.0)
    ops := ops.push (.path (circlePath x y 3.5) (some { color := ⟨0.9, 0.4, 0.1, 0.8⟩ })
      (some { color := .black, width := 0.75 }) none)
  return ops

/-- A `k × k`-vertex grid over a 1000×1000 canvas as one Gouraud `triangles`
op (`2 (k−1)²` triangles), coloured by a smooth RGB field. -/
def meshGrid (k : Nat) : DrawOp := Id.run do
  let mut xs := FloatArray.emptyWithCapacity (k * k)
  let mut ys := FloatArray.emptyWithCapacity (k * k)
  let mut cs := ByteArray.emptyWithCapacity (4 * k * k)
  let step := 1000.0 / (k - 1).toFloat
  for j in [0:k] do
    for i in [0:k] do
      let x := i.toFloat * step; let y := j.toFloat * step
      -- a gentle warp so edges are not axis-aligned
      xs := xs.push (x + 2.0 * Float.sin (y / 37.0)); ys := ys.push (y + 2.0 * Float.cos (x / 41.0))
      let u := i.toFloat / (k - 1).toFloat; let v := j.toFloat / (k - 1).toFloat
      cs := (((cs.push (255.0 * u).toUInt8).push (255.0 * v).toUInt8).push
        (127.0 + 127.0 * Float.sin (9.0 * u * v)).toUInt8).push 255
  let mut idx := ByteArray.emptyWithCapacity (24 * (k - 1) * (k - 1))
  let push32 (b : ByteArray) (v : Nat) : ByteArray :=
    (((b.push v.toUInt8).push (v >>> 8).toUInt8).push (v >>> 16).toUInt8).push (v >>> 24).toUInt8
  for j in [0:k-1] do
    for i in [0:k-1] do
      let a := j * k + i
      idx := push32 (push32 (push32 idx a) (a + 1)) (a + k)
      idx := push32 (push32 (push32 idx (a + 1)) (a + k + 1)) (a + k)
  return .triangles xs ys cs idx none

/-- A `segments` op along the 10⁵-segment sine sweep, every segment with its
own colour (the worst case: one sweep per segment). -/
def colourSegments (n : Nat) : DrawOp := Id.run do
  let mut xs := FloatArray.emptyWithCapacity (2 * n)
  let mut ys := FloatArray.emptyWithCapacity (2 * n)
  let mut cs := ByteArray.emptyWithCapacity (4 * n)
  for i in [0:n] do
    let x0 := 1000.0 * i.toFloat / n.toFloat; let x1 := 1000.0 * (i + 1).toFloat / n.toFloat
    xs := (xs.push x0).push x1
    ys := (ys.push (500.0 + 400.0 * Float.sin (x0 / 5.0))).push (500.0 + 400.0 * Float.sin (x1 / 5.0))
    cs := (((cs.push (i % 256).toUInt8).push 80).push (255 - i % 256).toUInt8).push 255
  return .segments xs ys cs 1.5 .butt none

/-- The perf suite. -/
def tests : T Unit := do
  let sweep := sineSweep 100000
  let walk := randomWalk 100000
  let star := bigStar 100000
  let bnd := band 100000
  let st : Stroke := { color := ⟨0.1, 0.2, 0.6, 1⟩, width := 1.5 }
  let (tSweep, _) ← bench 3 fun _ => paint 1000 1000 #[.path sweep none (some st) none]
  let (tSweepRound, _) ← bench 3 fun _ => paint 1000 1000 #[.path sweep none (some { st with join := .round, cap := .round }) none]
  let (tDash, _) ← bench 3 fun _ => paint 1000 1000 #[.path sweep none (some { st with dash := #[6, 3] }) none]
  let (tWalk, _) ← bench 3 fun _ => paint 1000 1000 #[.path walk none (some st) none]
  let (tStar, _) ← bench 3 fun _ => paint 1000 1000 #[.path star (some { color := .black }) none none]
  let (tBand, _) ← bench 3 fun _ => paint 1000 1000 #[.path bnd (some { color := ⟨0.2, 0.4, 0.8, 0.5⟩ }) none none]
  let (tEmpty, _) ← bench 3 fun _ => paint 1000 1000 #[.path (Path.rect ⟨0, 0, 1, 1⟩) (some { color := .black }) none none]
  let cseg := colourSegments 100000
  let (tSegs, _) ← bench 3 fun _ => paint 1000 1000 #[cseg]
  let mk := markers 10000
  let (tMarkers, _) ← bench 3 fun _ => paint 1000 1000 mk
  let mesh := meshGrid 200
  let nTri := match mesh with | .triangles _ _ _ idx _ => idx.size / 12 | _ => 0
  let (tMesh, _) ← bench 3 fun _ => paint 1000 1000 #[mesh]
  let plot := typicalPlot
  let (tPlot, plotCv) ← bench 5 fun _ => plot.toCanvas
  let (png, tPng) ← timeMs (IO.lazyPure fun _ => plotCv.toPNG)
  let (pngFast, tPngFast) ← timeMs (IO.lazyPure fun _ => plotCv.toPNG { level := .fast })
  IO.println s!"  perf: 1000×1000, 10⁵-segment sine stroke (miter)     {tSweep} ms"
  IO.println s!"  perf: 1000×1000, 10⁵-segment sine stroke (round)     {tSweepRound} ms"
  IO.println s!"  perf: 1000×1000, 10⁵-segment sine stroke (dashed)    {tDash} ms"
  IO.println s!"  perf: 1000×1000, 10⁵-segment random-walk stroke      {tWalk} ms"
  IO.println s!"  perf: 1000×1000, 10⁵-vertex star polygon fill         {tStar} ms"
  IO.println s!"  perf: 1000×1000, 10⁵-vertex band fill (area plot)     {tBand} ms"
  IO.println s!"  perf: 1000×1000, canvas + accumulator setup           {tEmpty} ms"
  IO.println s!"  perf: 1000×1000, 10⁵ segments, one colour each        {tSegs} ms"
  IO.println s!"  perf: 1000×1000, 10⁴ markers (fill + outline)         {tMarkers} ms"
  IO.println s!"  perf: 1000×1000, {nTri}-triangle Gouraud mesh        {tMesh} ms"
  IO.println s!"  perf: 800×600 typical plot ({plot.ops.size} ops)          {tPlot} ms"
  IO.println s!"  perf: 800×600 PNG encode default: {tPng} ms, {png.size} B; fast: {tPngFast} ms, {pngFast.size} B"
  check "10⁵-segment stroke < 5× target (200 ms)" (tSweep < 1000.0) s!"{tSweep} ms"
  check "10⁵-segment dashed stroke < 5× target" (tDash < 1000.0) s!"{tDash} ms"
  check "10⁵-segment random walk < 5× target" (tWalk < 1000.0) s!"{tWalk} ms"
  check "10⁵-vertex star fill < 1 s" (tStar < 1000.0) s!"{tStar} ms"
  check "10⁵-vertex band fill < 5× 200 ms" (tBand < 1000.0) s!"{tBand} ms"
  check "10⁵ coloured segments < 1 s" (tSegs < 1000.0) s!"{tSegs} ms"
  check "10⁴ markers < 1 s" (tMarkers < 1000.0) s!"{tMarkers} ms"
  check "80k-triangle mesh < 1 s" (tMesh < 1000.0) s!"{tMesh} ms"
  check "typical plot < 5× target (30 ms)" (tPlot < 150.0) s!"{tPlot} ms"
  check "plot PNG < 1 s" (tPng < 1000.0) s!"{tPng} ms"

/-- Run the perf suite. -/
def run : IO (Nat × Nat) := runSuite "perf" tests

end LeanPlotTest.Raster.PerfTests
