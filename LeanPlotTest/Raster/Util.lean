import LeanPlot.Backend.Raster

/-!
Tiny test harness for the raster/PNG suites: a counter of passed and failed
checks, threaded through `StateT` over `IO`. Failures print a line with a
detail message and are counted, so a suite never aborts early.
-/

namespace LeanPlotTest.Raster

open LeanPlot LeanPlot.Raster

/-- Pass/fail counters. -/
structure Tally where
  passed : Nat := 0
  failed : Nat := 0

/-- Test monad. -/
abbrev T := StateT Tally IO

/-- Record one check. -/
def check (name : String) (ok : Bool) (detail : String := "") : T Unit := do
  if ok then modify fun t => { t with passed := t.passed + 1 }
  else
    IO.println s!"  FAIL {name}{if detail.isEmpty then "" else ": " ++ detail}"
    modify fun t => { t with failed := t.failed + 1 }

/-- `|a - b| ≤ tol`. -/
def near (a b tol : Float) : Bool := (a - b).abs ≤ tol

/-- Check `|actual - expected| ≤ tol`. -/
def checkNear (name : String) (actual expected tol : Float) : T Unit :=
  check name (near actual expected tol) s!"got {actual}, expected {expected} ± {tol}"

/-- Run a suite and return `(passed, failed)`. -/
def runSuite (label : String) (t : T Unit) : IO (Nat × Nat) := do
  let ((), r) ← t.run {}
  IO.println s!"[raster] {label}: {r.passed} passed, {r.failed} failed"
  return (r.passed, r.failed)

/-- Time an IO action in milliseconds (the result is forced by the caller). -/
def timeMs {α : Type} (act : IO α) : IO (α × Float) := do
  let t0 ← IO.monoNanosNow
  let r ← act
  let t1 ← IO.monoNanosNow
  return (r, (t1 - t0).toFloat / 1.0e6)

/-- Coverage of pixel `(x, y)` when black was painted on white: `1 - r/255`. -/
def darkness {w h : Nat} (cv : Canvas w h) (x y : Nat) : Float :=
  1.0 - (cv.get x y).r.toFloat / 255.0

/-- Σ darkness over the canvas: the painted area for black-on-white. -/
def darkArea {w h : Nat} (cv : Canvas w h) : Float := Id.run do
  let mut s := 0.0
  for i in [0:w*h] do
    s := s + (1.0 - (cv.data.get! (4*i)).toFloat / 255.0)
  return s

/-- Σ darkness over row `y`. -/
def rowDark {w h : Nat} (cv : Canvas w h) (y : Nat) : Float := Id.run do
  let mut s := 0.0
  for x in [0:w] do s := s + darkness cv x y
  return s

/-- Σ darkness over column `x`. -/
def colDark {w h : Nat} (cv : Canvas w h) (x : Nat) : Float := Id.run do
  let mut s := 0.0
  for y in [0:h] do s := s + darkness cv x y
  return s

/-- Render ops on a white `w × h` canvas. -/
def paint (w h : Nat) (ops : Array DrawOp) : Canvas w h :=
  (Canvas.fill w h .white).drawOps ops

/-- Closed polygon through the points. -/
def polygon (pts : List (Float × Float)) : Path :=
  match pts with
  | [] => {}
  | (x, y) :: rest => (rest.foldl (fun p (x, y) => p.lineTo x y) (({} : Path).moveTo x y)).close

/-- Open polyline through the points. -/
def polyline (pts : List (Float × Float)) : Path :=
  match pts with
  | [] => {}
  | (x, y) :: rest => rest.foldl (fun p (x, y) => p.lineTo x y) (({} : Path).moveTo x y)

/-- Circle approximated by four cubic arcs (κ = 0.5522847498). -/
def circlePath (cx cy r : Float) : Path :=
  let k := 0.5522847498 * r
  (((((({} : Path).moveTo (cx + r) cy).cubicTo (cx + r) (cy + k) (cx + k) (cy + r) cx (cy + r)).cubicTo
      (cx - k) (cy + r) (cx - r) (cy + k) (cx - r) cy).cubicTo (cx - r) (cy - k) (cx - k) (cy - r) cx (cy - r)).cubicTo
      (cx + k) (cy - r) (cx + r) (cy - k) (cx + r) cy).close

/-- Deterministic pseudo-random bytes (LCG). -/
def randBytes (n : Nat) (seed : UInt64) (mask : UInt64 := 255) : ByteArray := Id.run do
  let mut s := seed
  let mut b := ByteArray.emptyWithCapacity n
  for _ in [0:n] do
    s := s * 6364136223846793005 + 1442695040888963407
    b := b.push ((s >>> 33) &&& mask).toUInt8
  return b

/-- Little-endian `UInt32` indices packed into bytes. -/
def packIdx (is : List Nat) : ByteArray :=
  is.foldl (fun b v => (((b.push v.toUInt8).push (v >>> 8).toUInt8).push (v >>> 16).toUInt8).push (v >>> 24).toUInt8) .empty

end LeanPlotTest.Raster
