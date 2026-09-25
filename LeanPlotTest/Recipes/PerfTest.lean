import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo

/-!
Performance smoke tests for the recipe algorithms. Each workload runs three
times and the minimum wall time is printed. A check fails only beyond 5× its
budget, so CI noise does not flake the suite. Budgets are about twice the
Julia/Makie times for the same workloads, measured in the oracle environment on
the reference machine (Apple Silicon): `streamplot_impl` 0.39 ms (2D 32×32) and
1.07 ms (3D 16³), `Contour.contours` + `contourlines` 0.49 ms, Makie
`calculate_contourf_polys!` 15.4 ms, `surface2mesh` 1.08 ms.
-/

namespace LeanPlotTest.Recipes.PerfTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- A `0.0` the compiler cannot see through (workloads add it to an input so
that closed terms are not hoisted out of the timing loop). -/
def runtimeZero : IO Float := do
  let v ← IO.getEnv "LEANPLOT_PERF_NEVER_SET"
  return if v.isSome then 1.0 else 0.0

/-- Minimum wall time (ms) over `k` runs; `size` forces the result. -/
def bench {α : Type} (k : Nat) (f : Float → α) (size : α → Nat) : IO (Float × Nat) := do
  let mut best := 1.0e30
  let mut n := 0
  for _ in [0:k] do
    let z ← runtimeZero
    let t0 ← IO.monoNanosNow
    let r ← IO.lazyPure fun _ => f z
    n := size r
    let t1 ← IO.monoNanosNow
    best := min best ((t1 - t0).toFloat / 1.0e6)
  return (best, n)

/-- Record a timing against a budget (fails beyond 5×). The workload receives a
runtime `0.0`. -/
def timed {α : Type} (name : String) (budgetMs : Float) (f : Float → α) (size : α → Nat) : TestM Unit := do
  let (ms, n) ← bench 3 f size
  IO.println s!"  perf {name}: {ms} ms (size {n}, budget {budgetMs} ms)"
  check s!"perf {name}" (ms ≤ 5 * budgetMs) (fun _ => s!"{ms} ms > 5 × {budgetMs} ms")

/-- `0.2` as a global (float literals inside closures are re-parsed on every call
by the current code generator, see `LeanPlot.Num`'s note on hot constants). -/
def c02 : Float := 0.2
/-- `0.3` as a global. -/
def c03 : Float := 0.3
/-- `0.1` as a global. -/
def c01 : Float := 0.1

/-- A smooth test field on `[-3, 3]²`. -/
def field (x y : Float) : Float := Float.sin (1.3 * x) * Float.cos (0.9 * y) + 0.1 * x * y

/-- Run the suite. -/
def suite : TestM Unit := do
  -- streamplot: 32×32 cells, default step and maxsteps
  timed "streamplot 2D 32x32" 1 (fun z =>
    Stream.streamplot2 (fun p => ⟨-p.y - c02 * p.x, p.x - c02 * p.y⟩) (z - 2) (-2) 4 4)
    (fun r => r.linePoints.size)
  timed "streamplot 3D 16^3" 3 (fun z =>
    Stream.streamplot3 (fun p => ⟨-p.y, p.x, c03 - c01 * p.z⟩) ⟨z - 1, -1, -1⟩ ⟨2, 2, 2⟩ { gridsize := #[16, 16, 16] })
    (fun r => r.linePoints.size)
  -- contour / contourf on a 256×256 grid (Makie defaults: 5 lines, 10 bands)
  let n := 256
  let xs := LeanPlot.Num.range (-3) 3 n
  let g := Grid2.sample field xs xs
  let shift (z : Float) : Grid2 xs.size xs.size := g.map (· + z)
  timed "contour 256x256 x5" 2 (fun z => Contour.makieContour (.rect xs xs) (shift z)) (fun r => r.lines.size)
  timed "contourf 256x256 x10" 30 (fun z => Isoband.makieContourf xs xs (shift z)) (fun r => r.polys.size)
  -- surface mesh with normals
  timed "surface 256x256" 4 (fun z => Surface.surfaceMesh xs xs (shift z)) (fun r => r.mesh.numTriangles)
  -- 10⁴ 2D arrows
  let m := 10000
  let sx : FloatArray := ⟨(Array.range m).map fun i => (i % 100).toFloat * 8⟩
  let sy : FloatArray := ⟨(Array.range m).map fun i => (i / 100).toFloat * 8⟩
  let sz : FloatArray := ⟨Array.replicate m 0⟩
  let dx : FloatArray := ⟨(Array.range m).map fun i => F32.r32 (20 * Float.cos i.toFloat)⟩
  let dy : FloatArray := ⟨(Array.range m).map fun i => F32.r32 (20 * Float.sin i.toFloat)⟩
  timed "arrows2d 1e4" 10 (fun z => Arrows.shapes2d {} sx sy (sz.push z) dx dy) (fun r => r.count)

end LeanPlotTest.Recipes.PerfTest
