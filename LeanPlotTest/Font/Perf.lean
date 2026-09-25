import LeanPlotTest.Font.Harness

/-!
Performance budget of the text engine (compiled code):

* laying out a 40-character label: < 20 µs;
* building the outlines of 1000 tick labels (`textPath`): < 20 ms.

Inputs are built at run time (so nothing is hoisted as a closed term) and each measurement
is the best of several repetitions.
-/

namespace LeanPlotTest.Font.Perf

open LeanPlot LeanPlot.Font

/-- A 40-character label that varies with `i`. -/
def label40 (i : Nat) : String :=
  let s := s!"Velocity ‖v‖ of sample {i} at t = {i % 97}.5 s ±"
  let s := if s.length < 40 then s ++ String.ofList (List.replicate (40 - s.length) 'x') else s
  String.ofList (s.toList.take 40)

/-- A Makie-style tick label (U+2212 minus). -/
def tick (i : Nat) : String :=
  let v := (Float.ofNat i - 500) / 8
  let neg := v < 0
  let a := Float.abs v
  let ip := a.floor.toUInt64.toNat
  let fp := ((a - a.floor) * 1000).round.toUInt64.toNat
  let body := if fp == 0 then toString ip else s!"{ip}.{fp}"
  if neg then "−" ++ body else body

/-- Time `f` over `inputs`, best of `reps`; returns (nanoseconds per call, checksum). -/
def timeEach (inputs : Array String) (reps : Nat) (f : String → Nat) : IO (Float × Nat) := do
  let mut best := (1e30 : Float)
  let mut sum := 0
  for _ in [0:reps] do
    let t0 ← IO.monoNanosNow
    let s := inputs.foldl (fun acc x => acc + f x) 0
    let t1 ← IO.monoNanosNow
    sum := s
    let dt := Float.ofNat (t1 - t0) / Float.ofNat inputs.size
    if dt < best then best := dt
  return (best, sum)

/-- Time building all tick labels as paths, best of `reps`; returns (ms, checksum). -/
def timeBatch (inputs : Array String) (reps : Nat) : IO (Float × Nat) := do
  let mut best := (1e30 : Float)
  let mut sum := 0
  let style : TextStyle := { size := 14, halign := .center, valign := .top }
  for _ in [0:reps] do
    let t0 ← IO.monoNanosNow
    let (_, s) := inputs.foldl (fun (y, acc) x =>
      let p := textPath style x 100 y
      (y + 1, acc + p.coords.size)) ((0 : Float), 0)
    let t1 ← IO.monoNanosNow
    sum := s
    let dt := Float.ofNat (t1 - t0) / 1e6
    if dt < best then best := dt
  return (best, sum)

/-- Run the performance checks; prints the measurements. -/
def run : IO (Nat × Nat) := do
  let labels := (Array.range 2000).map label40
  let ticks := (Array.range 1000).map tick
  -- warm up (decodes the faces)
  let _ := (layout Font.regular "warm up").codes.size
  let (nsLayout, c1) ← timeEach labels 5 fun s => (layout Font.regular s .center .top).codes.size
  let (nsPath, c2) ← timeEach labels 5 fun s => (textPath { size := 12 } s 0 0).verbs.size
  let (msTicks, c3) ← timeBatch ticks 5
  let (nsMeasure, c4) ← timeEach ticks 5 fun s => (measure { size := 14 } s).advance.toUInt64.toNat
  IO.println s!"  font/perf: layout 40-char label {fmt (nsLayout / 1000) 3} µs; textPath 40-char label {fmt (nsPath / 1000) 3} µs; 1000 tick-label paths {fmt msTicks 3} ms; measure tick label {fmt (nsMeasure / 1000) 3} µs (checksums {c1} {c2} {c3} {c4})"
  let t : Tally := {}
  let t := t.check (nsLayout < 20000) s!"layout of a 40-char label took {nsLayout} ns (budget 20 µs)"
  let t := t.check (msTicks < 20) s!"1000 tick labels took {msTicks} ms (budget 20 ms)"
  t.report "perf"

end LeanPlotTest.Font.Perf
