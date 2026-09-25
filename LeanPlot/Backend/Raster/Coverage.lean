import LeanPlot.Backend.Raster.Canvas

/-
Exact-area coverage accumulation (font-rs / stb_truetype v2 style).

Every edge of a closed outline deposits its *signed area* into an
accumulation buffer, one cell per pixel plus two guard cells per row. A
prefix sum along each row then gives, for each pixel, the integral of the
winding number over that pixel. The coverage is `min 1 |acc|` for the nonzero
rule and a triangle wave of `acc` (period 2) for even-odd. Pixels fully
inside or outside are exact. Only partially covered pixels that several edges
cross are approximated.

Edges are order-independent, so a shape may be deposited as a union of
same-orientation pieces (the stroker does this). A shared edge traversed in
opposite directions cancels exactly.

Layout of the single `FloatArray` (width `W`, height `H`, stride `S = W + 2`):

* `[0, H·S)`: cells.
* `H·S + 2r`, `H·S + 2r + 1`: first and last touched cell of row `r`
  (`spanNone`/`-1` when untouched), so a sweep visits only touched spans and
  fills the interior between spans at constant coverage.
* `H·S + 2H`, `H·S + 2H + 1`: first and one-past-last touched row.

Clipping: edges are clipped in `y` to the clip rows and split at the clip
columns. Parts left of the clip become vertical edges on its left boundary,
which preserves the accumulated winding. Parts right of it are dropped.
Fractional clip boundaries are antialiased by scaling coverage with the
pixel/clip overlap.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Sentinel for an untouched row span. -/
def spanNone : Float := K.e18

/-- A clip region: fractional bounds (already intersected with the canvas) and
the integer pixel box that contains them. Empty when `ix0 ≥ ix1` or `iy0 ≥ iy1`. -/
structure Clip where
  x0 : Float
  y0 : Float
  x1 : Float
  y1 : Float
  ix0 : Nat
  iy0 : Nat
  ix1 : Nat
  iy1 : Nat
  deriving Repr, Inhabited

namespace Clip

/-- Non-negative float → Nat (floor), NaN/negative ↦ 0. -/
@[inline] def toNatFloor (v : Float) : Nat := v.floor.toUInt64.toNat

/-- Clip to `r ∩ [0,w]×[0,h]` (whole canvas when `r = none`). -/
def make (w h : Nat) (r : Option Rect) : Clip :=
  let W := natF w; let H := natF h
  let (x0, y0, x1, y1) := match r with
    | none => (K.zero, K.zero, W, H)
    | some r =>
      -- normalise negative extents
      let xa := if r.w < K.zero then r.x + r.w else r.x
      let ya := if r.h < K.zero then r.y + r.h else r.y
      (xa, ya, xa + r.w.abs, ya + r.h.abs)
  let ok (v : Float) := !v.isNaN
  let x0 := if ok x0 then max K.zero (min W x0) else K.zero
  let y0 := if ok y0 then max K.zero (min H y0) else K.zero
  let x1 := if ok x1 then max x0 (min W x1) else x0
  let y1 := if ok y1 then max y0 (min H y1) else y0
  { x0, y0, x1, y1, ix0 := toNatFloor x0, iy0 := toNatFloor y0,
    ix1 := min w (toNatFloor x1.ceil), iy1 := min h (toNatFloor y1.ceil) }

/-- Is the clip region empty? -/
def isEmpty (c : Clip) : Bool := c.ix0 ≥ c.ix1 || c.iy0 ≥ c.iy1

/-- Intersection of two clips (both for the same canvas). -/
def inter (a b : Clip) : Clip :=
  let x0 := max a.x0 b.x0; let y0 := max a.y0 b.y0
  let x1 := max x0 (min a.x1 b.x1); let y1 := max y0 (min a.y1 b.y1)
  { x0, y0, x1, y1, ix0 := max a.ix0 b.ix0, iy0 := max a.iy0 b.iy0,
    ix1 := max (max a.ix0 b.ix0) (min a.ix1 b.ix1), iy1 := max (max a.iy0 b.iy0) (min a.iy1 b.iy1) }

/-- Fraction of the unit interval `[c, c+1]` inside `[lo, hi]`. -/
@[inline] def overlap (c : Nat) (lo hi : Float) : Float :=
  let cf := natF c
  let v := (min (cf + K.one) hi) - (max cf lo)
  if v ≥ K.one then K.one else if v > K.zero then v else K.zero

end Clip

/-- Coverage accumulator for a `w × h` canvas (see the module docstring for
the layout). The dimensions are type indices, so an accumulator can only be
swept onto a canvas of its own size. The structure has a single runtime
field, so at runtime it *is* the `FloatArray`: there is no wrapper to
allocate and no struct update that could alias the buffer. -/
structure Accum (w h : Nat) where
  buf : FloatArray

namespace Accum

/-- Row stride (`w + 2` cells). -/
@[inline] def strideOf (w : Nat) : Nat := w + 2

/-- Offset of the span table. -/
@[inline] def spanBase (w h : Nat) : Nat := h * strideOf w

/-- A zeroed accumulator. -/
def new (w h : Nat) : Accum w h :=
  let S := strideOf w
  let cells := h * S
  let rec zeros (k : Nat) (a : FloatArray) : FloatArray :=
    match k with
    | 0 => a
    | k + 1 => zeros k (a.push K.zero)
  let rec spans (k : Nat) (a : FloatArray) : FloatArray :=
    match k with
    | 0 => a
    | k + 1 => spans k ((a.push spanNone).push (-K.one))
  let a := zeros cells (FloatArray.emptyWithCapacity (cells + 2 * h + 2))
  let a := spans h a
  ⟨(a.push spanNone).push (-K.one)⟩

/-- Make the buffer unique (copying it if shared, e.g. an accumulator hoisted
to a closed term) and mark it linear. With `LEAN_ABORT_ON_NONLINEAR=1`, any
later accidental copy then panics. `Canvas.drawOps` marks its accumulator
this way; `new` itself does not, because a marked closed term would trip the
check on its first legitimate copy. -/
def markLinear {w h : Nat} (acc : Accum w h) : Accum w h := ⟨acc.buf.markLinear⟩

/-- `a[i] += v`. -/
@[inline] def addAt (a : FloatArray) (i : Nat) (v : Float) : FloatArray :=
  a.set! i (a.get! i + v)

/-- Record that row `row` touched cells `[c0, c1]`. -/
@[inline] def touch (a : FloatArray) (sb row : Nat) (c0 c1 : Float) : FloatArray :=
  let k := sb + 2 * row
  let lo := a.get! k
  let hi := a.get! (k + 1)
  let a := if c0 < lo then a.set! k c0 else a
  if c1 > hi then a.set! (k + 1) c1 else a

/-- Record that rows `[r0, r1)` were touched. -/
@[inline] def touchRows (a : FloatArray) (sb h : Nat) (r0 r1 : Float) : FloatArray :=
  let k := sb + 2 * h
  let a := if r0 < a.get! k then a.set! k r0 else a
  if r1 > a.get! (k + 1) then a.set! (k + 1) r1 else a

/-- `a[base + c] += v` for `c ∈ [c0, c1)`. -/
def addRun (a : FloatArray) (base c c1 : Nat) (v : Float) : FloatArray :=
  if c < c1 then addRun (addAt a (base + c) v) base (c + 1) c1 v else a
termination_by c1 - c

/-- Deposit the part of an edge within one row: from `x` to `xn` (any order)
over a vertical extent whose signed height is `d`. -/
@[inline] def cellRow (a : FloatArray) (sb base row : Nat) (x xn d : Float) : FloatArray :=
  let x0 := min x xn
  let x1 := max x xn
  let x0f := x0.floor
  let x0i := x0f.toUInt64.toNat
  let x1c := x1.ceil
  let x1i := x1c.toUInt64.toNat
  if x1i ≤ x0i + 1 then
    let xmf := K.half * (x + xn) - x0f
    let a := addAt a (base + x0i) (d - d * xmf)
    let a := addAt a (base + x0i + 1) (d * xmf)
    touch a sb row x0f (x0f + K.one)
  else
    let s := K.one / (x1 - x0)
    let x0r := x0 - x0f
    let a0 := K.half * s * (K.one - x0r) * (K.one - x0r)
    let x1r := x1 - x1c + K.one
    let am := K.half * s * x1r * x1r
    let a := addAt a (base + x0i) (d * a0)
    let a :=
      if x1i == x0i + 2 then addAt a (base + x0i + 1) (d * (K.one - a0 - am))
      else
        let a1 := s * (K.c1_5 - x0r)
        let a := addAt a (base + x0i + 1) (d * (a1 - a0))
        let a := addRun a base (x0i + 2) (x1i - 1) (d * s)
        let a2 := a1 + natF (x1i - x0i - 3) * s
        addAt a (base + x1i - 1) (d * (K.one - a2 - am))
    let a := addAt a (base + x1i) (d * am)
    touch a sb row x0f x1c

/-- Walk rows `row, row+1, …` of an edge piece spanning `[ys, ye]` (`ys < ye`)
whose `x` at `max row ys` is `x`, with slope `dxdy`, clamped to `[lo, hi]`. -/
def rows (a : FloatArray) (S sb : Nat) (row : Nat) (ys ye x dxdy dir lo hi : Float) (fuel : Nat) : FloatArray :=
  match fuel with
  | 0 => a
  | fuel + 1 =>
    let rf := natF row
    if rf ≥ ye then a else
    let top := max rf ys
    let bot := min (rf + K.one) ye
    let dy := bot - top
    let xn := x + dxdy * dy
    let xc := max lo (min hi x)
    let xnc := max lo (min hi xn)
    let a := cellRow a sb (row * S) row xc xnc (dy * dir)
    rows a S sb (row + 1) ys ye xn dxdy dir lo hi fuel

/-- Deposit one monotone piece `(xs, ys) → (xe, ye)` with `ys < ye` inside the
clip rows. Pieces left of the clip collapse onto its left edge; pieces right
of it are dropped. -/
@[inline] def piece (a : FloatArray) (S sb h : Nat) (cl : Clip) (xa ya dxdy dir ys ye : Float) : FloatArray :=
  if ye - ys ≤ K.zero then a else
  let lo := natF cl.ix0
  let hi := natF cl.ix1
  let xs := xa + (ys - ya) * dxdy
  let xe := xa + (ye - ya) * dxdy
  let xm := K.half * (xs + xe)
  if xm ≥ hi then a else
  let (xs, dxdy) := if xm ≤ lo then (lo, K.zero) else (xs, dxdy)
  let r0 := ys.floor
  let r1 := ye.ceil
  let a := touchRows a sb h r0 r1
  rows a S sb r0.toUInt64.toNat ys ye xs dxdy dir lo hi (r1 - r0 + K.one).toUInt64.toNat

/-- Deposit the edge `(x0, y0) → (x1, y1)` (device pixels) into buffer `a` of an
accumulator for a `w × h` canvas, restricted to `cl`. -/
def lineRaw (a : FloatArray) (w h : Nat) (cl : Clip) (x0 y0 x1 y1 : Float) : FloatArray :=
  if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite) || y0 == y1 then a else
  let S := strideOf w
  let sb := spanBase w h
  let dir := if y0 < y1 then K.one else -K.one
  let xa := if y0 < y1 then x0 else x1
  let ya := if y0 < y1 then y0 else y1
  let xb := if y0 < y1 then x1 else x0
  let yb := if y0 < y1 then y1 else y0
  let cy0 := natF cl.iy0
  let cy1 := natF cl.iy1
  if yb ≤ cy0 || ya ≥ cy1 || cl.isEmpty then a else
  let dxdy := (xb - xa) / (yb - ya)
  let ys := max ya cy0
  let ye := min yb cy1
  -- split where the edge crosses the clip columns
  let lo := natF cl.ix0
  let hi := natF cl.ix1
  let xs := xa + (ys - ya) * dxdy
  let xe := xa + (ye - ya) * dxdy
  let s0 := if (xs - lo) * (xe - lo) < K.zero then ya + (lo - xa) / dxdy else ye
  let s1 := if (xs - hi) * (xe - hi) < K.zero then ya + (hi - xa) / dxdy else ye
  let m0 := max ys (min ye (min s0 s1))
  let m1 := max m0 (min ye (max s0 s1))
  let a := piece a S sb h cl xa ya dxdy dir ys m0
  let a := piece a S sb h cl xa ya dxdy dir m0 m1
  piece a S sb h cl xa ya dxdy dir m1 ye

/-- Deposit an edge. -/
@[inline] def line {w h : Nat} (acc : Accum w h) (cl : Clip) (x0 y0 x1 y1 : Float) : Accum w h :=
  ⟨lineRaw acc.buf w h cl x0 y0 x1 y1⟩

/-- Coverage from an accumulated winding integral. -/
@[inline] def coverage (evenOdd : Bool) (acc : Float) : Float :=
  let v := acc.abs
  if evenOdd then
    let t := v - K.two * (K.half * v).floor
    if t > K.one then K.two - t else t
  else if v > K.one then K.one else v

end Accum

/-- Something the sweep can paint: given the canvas, pixel `(x, y)` and
coverage (already scaled by the clip overlap), blend into the canvas. -/
abbrev Painter (w h : Nat) := Canvas w h → Nat → Nat → Float → Canvas w h

/-- Solid-colour painter: straight colour in byte units and colour alpha. -/
@[inline] def solidPainter {w h : Nat} (r g b alpha : Float) : Painter w h :=
  fun c x y cov => c.blend (Canvas.offset w x y) r g b (alpha * cov)

namespace Accum

/-- Minimum coverage worth painting (≈ ¼ of a byte step). -/
def covEps : Float := K.one / K.c1024

/-- Sweep one touched span of a row: cells `[c, smax]` are accumulated,
cleared and painted; returns the running accumulator. -/
@[specialize] def spanCells {w h : Nat} (paint : Painter w h) (evenOdd : Bool) (cl : Clip) (fy : Float)
    (base y : Nat) (c smax : Nat) (acc : Float) (a : FloatArray) (cv : Canvas w h) (fuel : Nat) :
    Float × FloatArray × Canvas w h :=
  match fuel with
  | 0 => (acc, a, cv)
  | fuel + 1 =>
    if c > smax then (acc, a, cv) else
    let acc := acc + a.get! (base + c)
    let a := a.set! (base + c) K.zero
    let cv :=
      if c < cl.ix1 then
        let cov := coverage evenOdd acc * fy * Clip.overlap c cl.x0 cl.x1
        if cov > covEps then paint cv c y cov else cv
      else cv
    spanCells paint evenOdd cl fy base y (c + 1) smax acc a cv fuel

/-- Paint `[c, stop)` of row `y` at constant coverage `cov0` (before the clip
overlap factor). -/
@[specialize] def run {w h : Nat} (paint : Painter w h) (cl : Clip) (fy cov0 : Float) (y c stop : Nat)
    (cv : Canvas w h) : Canvas w h :=
  if c < stop then
    let cov := cov0 * fy * Clip.overlap c cl.x0 cl.x1
    run paint cl fy cov0 y (c + 1) stop (if cov > covEps then paint cv c y cov else cv)
  else cv
termination_by stop - c

/-- Sweep rows `[y, yEnd)`: accumulate, paint, and reset the buffer. -/
@[specialize] def sweepRows {w h : Nat} (paint : Painter w h) (evenOdd : Bool) (cl : Clip)
    (y yEnd : Nat) (a : FloatArray) (cv : Canvas w h) : FloatArray × Canvas w h :=
  if y < yEnd then
    let S := strideOf w
    let sb := spanBase w h
    let k := sb + 2 * y
    let lo := a.get! k
    let hi := a.get! (k + 1)
    if lo > hi then sweepRows paint evenOdd cl (y + 1) yEnd a cv else
    let a := (a.set! k spanNone).set! (k + 1) (-K.one)
    let smin := lo.toUInt64.toNat
    let smax := hi.toUInt64.toNat
    let fy := Clip.overlap y cl.y0 cl.y1
    let (acc, a, cv) := spanCells paint evenOdd cl fy (y * S) y smin smax K.zero a cv (smax + 1 - smin + 1)
    let cov0 := coverage evenOdd acc
    let cv := if cov0 > covEps && smax + 1 < cl.ix1 then run paint cl fy cov0 y (smax + 1) cl.ix1 cv else cv
    sweepRows paint evenOdd cl (y + 1) yEnd a cv
  else (a, cv)
termination_by yEnd - y

/-- Resolve everything deposited so far onto the canvas with `paint`, then
reset the accumulator to zero. -/
@[specialize] def sweep {w h : Nat} (acc : Accum w h) (cv : Canvas w h) (cl : Clip) (evenOdd : Bool)
    (paint : Painter w h) : Accum w h × Canvas w h :=
  let a := acc.buf
  let k := spanBase w h + 2 * h
  let r0 := a.get! k
  let r1 := a.get! (k + 1)
  let a := (a.set! k spanNone).set! (k + 1) (-K.one)
  if r0 ≥ r1 then (⟨a⟩, cv)
  else
    let (a, cv) := sweepRows paint evenOdd cl (r0.toUInt64.toNat) (min h r1.toUInt64.toNat) a cv
    (⟨a⟩, cv)

/-- Sweep with a solid colour. -/
def sweepSolid {w h : Nat} (acc : Accum w h) (cv : Canvas w h) (cl : Clip) (rule : FillRule) (col : RGBA) :
    Accum w h × Canvas w h :=
  sweep acc cv cl (rule == .evenOdd)
    (solidPainter (clamp01 col.r * K.c255) (clamp01 col.g * K.c255) (clamp01 col.b * K.c255) (clamp01 col.a))

end Accum

end LeanPlot.Raster
