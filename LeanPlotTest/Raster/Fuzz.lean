import LeanPlotTest.Raster.Util

/-!
Randomised robustness test. Seeded random scenes mix every op kind with
hostile inputs: NaN and ±∞ coordinates, coordinates up to ±10⁶, near-degenerate
curves, random clips (including negative extents), stroke widths from 10⁻⁷ to
10⁷ px, dash patterns with zero-length elements, out-of-range triangle
indices, and short or empty image buffers.

Each scene must render within a time bound. This guards against hangs and
blow-ups; the dashing work budget came out of this. The canvas must keep its
size. Out-of-bounds accesses would print `PANIC` on stderr, and running the
suite under `LEAN_ABORT_ON_NONLINEAR=1` additionally catches accidental
buffer copies.
-/

namespace LeanPlotTest.Raster.FuzzTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Tiny LCG. -/
structure Rng where
  s : UInt64

/-- Next uniform float in `[0, 1)`. -/
def Rng.next (r : Rng) : Rng × Float :=
  let s := r.s * 6364136223846793005 + 1442695040888963407
  (⟨s⟩, (s >>> 11).toFloat / 9007199254740992.0)

/-- A coordinate: mostly on canvas, sometimes far away, sometimes non-finite. -/
def Rng.coord (r : Rng) : Rng × Float :=
  let (r, u) := r.next
  let (r, v) := r.next
  let x := if u < 0.7 then v * 220.0 - 10.0 else if u < 0.85 then (v - 0.5) * 2.0e6
    else if u < 0.9 then Float.nan else if u < 0.93 then Float.inf else if u < 0.96 then -Float.inf
    else (v - 0.5) * 1.0e-3 + 50.0
  (r, x)

/-- One random op. -/
def randomOp (r : Rng) (it : Nat) : Rng × DrawOp := Id.run do
  let mut r := r
  let (r1, u) := r.next; r := r1
  let (r1, n) := r.next; r := r1
  let npts := (n * 12.0).toUInt64.toNat + 1
  let mut p : Path := {}
  for k in [0:npts] do
    let (r1, x) := r.coord; r := r1
    let (r1, y) := r.coord; r := r1
    let (r1, v) := r.next; r := r1
    p := if k == 0 || v < 0.1 then p.moveTo x y
      else if v < 0.6 then p.lineTo x y
      else if v < 0.75 then p.quadTo (x + 5) (y - 7) (x * 0.9) (y + 3)
      else if v < 0.9 then p.cubicTo x (y + 10) (x - 10) y (x + 3) (y + 3)
      else p.close
  let (r1, a) := r.next; r := r1
  let (r1, b) := r.next; r := r1
  let (r1, c) := r.next; r := r1
  let clip : Option Rect :=
    if a < 0.5 then none else some ⟨b * 200 - 20, c * 150 - 20, (a - 0.5) * 400 - 50, (b - 0.3) * 300⟩
  let cap : LineCap := if a < 0.33 then .butt else if a < 0.66 then .round else .square
  let join : LineJoin := if b < 0.33 then .miter else if b < 0.66 then .round else .bevel
  let dash : Array Float := if c < 0.3 then #[] else if c < 0.6 then #[a * 10, b * 5] else #[0, c * 7, a]
  let width := if a < 0.05 then 1.0e7 * b else if a > 0.97 then 1.0e-7 else b * 12.0 - 1.0
  let st : Stroke := { color := ⟨a, b, c, a⟩, width, cap, join, miterLimit := c * 20, dash,
                       dashOffset := (a - 0.5) * 100 }
  let op : DrawOp :=
    if u < 0.5 then
      let rule : FillRule := if b < 0.5 then .nonzero else .evenOdd
      let fill : Option Fill := if a < 0.7 then some { color := ⟨c, b, a, 0.8⟩, rule } else none
      .path p fill (if b < 0.7 then some st else none) clip
    else if u < 0.7 then
      .segments p.coords ⟨p.coords.data.reverse⟩ ⟨#[1, 2, 3, 200, 9, 8, 7, 100]⟩ (a * 6.0 - 1.0) cap clip
    else if u < 0.85 then
      let nv := p.coords.size
      let idx := packIdx ((List.range (3 * (npts + 2))).map fun k => (k * 7919) % (nv + 2))
      .triangles p.coords ⟨p.coords.data.reverse⟩ ⟨#[255, 0, 0, 255, 0, 255, 0, 128]⟩ idx clip
    else
      let iw := (a * 5.0).toUInt64.toNat; let ih := (b * 5.0).toUInt64.toNat
      let img := randBytes (if c < 0.1 then 3 else 4 * iw * ih) it.toUInt64
      .image iw ih img ⟨(a - 0.2) * 300, b * 150, (c - 0.3) * 300, (a - 0.3) * 200⟩
        (if c < 0.5 then .nearest else .linear) clip
  return (r, op)

/-- Render `iters` random scenes of 4 ops each; check sizes and time. -/
def tests (iters : Nat := 300) (seed : UInt64 := 42) : T Unit := do
  let mut r : Rng := ⟨seed⟩
  let mut worst := 0.0
  let mut sizesOk := true
  let t0 ← IO.monoNanosNow
  for it in [0:iters] do
    let mut ops : Array DrawOp := #[]
    for _ in [0:4] do
      let (r1, op) := randomOp r it; r := r1
      ops := ops.push op
    let (cv, ms) ← timeMs (IO.lazyPure fun _ => (Canvas.fill 201 150 .white).drawOps ops)
    worst := max worst ms
    if cv.data.size != 4 * 201 * 150 then sizesOk := false
  let t1 ← IO.monoNanosNow
  let total := (t1 - t0).toFloat / 1.0e6
  IO.println s!"  fuzz: {iters} scenes in {total} ms, worst {worst} ms"
  check "fuzz canvas sizes preserved" sizesOk
  check "fuzz worst scene < 2 s" (worst < 2000.0) s!"{worst} ms"

/-- Run the fuzz suite. -/
def run : IO (Nat × Nat) := runSuite "fuzz" (tests)

end LeanPlotTest.Raster.FuzzTests
