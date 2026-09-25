import LeanPlot.Backend.Raster.Coverage

/-
Path flattening: `Path` (verbs + coords, with quadratic and cubic Béziers) →
`Polylines` (structure-of-arrays points plus subpath boundaries).

Béziers are subdivided uniformly into `n` pieces, where `n` comes from the
second-difference bound on the chord error:

* quadratic: `err ≤ |P₀ − 2P₁ + P₂| / (4n²)`,
* cubic: `err ≤ 3·max(|P₀ − 2P₁ + P₂|, |P₁ − 2P₂ + P₃|) / (4n²)`,

so `n = ⌈√(bound / tol)⌉` meets the tolerance (default 0.25 px). The piece
count therefore adapts to curvature and size. Points are evaluated directly
at `t = i/n`, so there is no forward-difference drift.

Consecutive duplicate points are dropped. A non-finite coordinate (NaN or
±∞) ends the current subpath, and the next finite point starts a new one,
following Makie's NaN-break convention for lines.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Flattened subpaths. Subpath `k` is the points `[starts[k], starts[k+1])`
(the last one runs to `xs.size`). `closed[k]` records an explicit `close`. -/
structure Polylines where
  xs : FloatArray := .empty
  ys : FloatArray := .empty
  starts : Array Nat := #[]
  closed : Array Bool := #[]
  deriving Inhabited

namespace Polylines

/-- Number of subpaths. -/
def count (p : Polylines) : Nat := p.starts.size

/-- Point range `[a, b)` of subpath `k`. -/
@[inline] def range (p : Polylines) (k : Nat) : Nat × Nat :=
  (p.starts[k]!, if k + 1 < p.starts.size then p.starts[k+1]! else p.xs.size)

/-- Total number of points. -/
def numPoints (p : Polylines) : Nat := p.xs.size

end Polylines

/-- Default flattening tolerance in pixels. -/
def flattenTol : Float := K.quarter

/-- Maximum number of pieces per curve. -/
def maxCurvePieces : Nat := 1000

/-- Pieces needed for a curve whose chord-error bound is `bound / n²`. -/
@[inline] def curvePieces (bound tol : Float) : Nat :=
  let n := (bound / tol).sqrt.ceil
  if n.isNaN || n < K.one then 1 else min maxCurvePieces n.toUInt64.toNat

/-- Flattener state. The point buffers are threaded as separate fields so
pushes are in place. -/
structure FlatSt where
  xs : FloatArray
  ys : FloatArray
  starts : Array Nat
  closed : Array Bool
  /-- current point -/
  cx : Float
  cy : Float
  /-- start of the current subpath -/
  sx : Float
  sy : Float
  /-- is a subpath open (has its first point)? -/
  open_ : Bool

namespace FlatSt

/-- Begin a new subpath at `(x, y)`. -/
@[inline] def begin (s : FlatSt) (x y : Float) : FlatSt :=
  { s with xs := s.xs.push x, ys := s.ys.push y, starts := s.starts.push s.xs.size,
           closed := s.closed.push false, cx := x, cy := y, sx := x, sy := y, open_ := true }

/-- Append `(x, y)` to the current subpath (starting one at the current point
if needed), dropping exact duplicates; non-finite points break the path. -/
@[inline] def lineTo (s : FlatSt) (x y : Float) : FlatSt :=
  if !(x.isFinite && y.isFinite) then { s with open_ := false, cx := x, cy := y } else
  let s := if s.open_ then s else
    if s.cx.isFinite && s.cy.isFinite then s.begin s.cx s.cy else s.begin x y
  if x == s.cx && y == s.cy then s
  else { s with xs := s.xs.push x, ys := s.ys.push y, cx := x, cy := y }

/-- Uniformly subdivided quadratic Bézier from the current point. -/
def quadTo (s : FlatSt) (x1 y1 x y tol : Float) : FlatSt :=
  let x0 := s.cx; let y0 := s.cy
  let dx := x0 - K.two * x1 + x; let dy := y0 - K.two * y1 + y
  let n := curvePieces ((dx * dx + dy * dy).sqrt / K.four) tol
  let nf := natF n
  let rec go (i : Nat) (s : FlatSt) (fuel : Nat) : FlatSt :=
    match fuel with
    | 0 => s
    | fuel + 1 =>
      if i > n then s else
      let t := natF i / nf
      let u := K.one - t
      let px := u * u * x0 + K.two * u * t * x1 + t * t * x
      let py := u * u * y0 + K.two * u * t * y1 + t * t * y
      -- land exactly on the end point
      let (px, py) := if i == n then (x, y) else (px, py)
      go (i + 1) (s.lineTo px py) fuel
  go 1 s (n + 1)

/-- Uniformly subdivided cubic Bézier from the current point. -/
def cubicTo (s : FlatSt) (x1 y1 x2 y2 x y tol : Float) : FlatSt :=
  let x0 := s.cx; let y0 := s.cy
  let ax := x0 - K.two * x1 + x2; let ay := y0 - K.two * y1 + y2
  let bx := x1 - K.two * x2 + x; let by_ := y1 - K.two * y2 + y
  let m := max (ax * ax + ay * ay).sqrt (bx * bx + by_ * by_).sqrt
  let n := curvePieces (K.c0_75 * m) tol
  let nf := natF n
  let rec go (i : Nat) (s : FlatSt) (fuel : Nat) : FlatSt :=
    match fuel with
    | 0 => s
    | fuel + 1 =>
      if i > n then s else
      let t := natF i / nf
      let u := K.one - t
      let c0 := u * u * u; let c1 := K.three * u * u * t; let c2 := K.three * u * t * t; let c3 := t * t * t
      let px := c0 * x0 + c1 * x1 + c2 * x2 + c3 * x
      let py := c0 * y0 + c1 * y1 + c2 * y2 + c3 * y
      let (px, py) := if i == n then (x, y) else (px, py)
      go (i + 1) (s.lineTo px py) fuel
  go 1 s (n + 1)

/-- If no subpath is open but the pen is at a finite point, open one there. -/
@[inline] def ensureOpen (s : FlatSt) : FlatSt :=
  if !s.open_ && s.cx.isFinite && s.cy.isFinite then s.begin s.cx s.cy else s

/-- `close`: mark the subpath closed; the pen returns to its start. -/
@[inline] def close (s : FlatSt) : FlatSt :=
  if s.open_ then
    let k := s.closed.size - 1
    { s with closed := s.closed.set! k true, cx := s.sx, cy := s.sy, open_ := false }
  else { s with cx := s.sx, cy := s.sy }

end FlatSt

/-- Drop the duplicated closing point of closed subpaths (the closing segment
is implicit) and subpaths with no points. -/
def Polylines.normalize (p : Polylines) : Polylines := Id.run do
  -- fast path: nothing to drop
  let needs := (List.range p.count).any fun k =>
    let (a, b) := p.range k
    b ≤ a || (p.closed[k]! && b > a + 1 && p.xs[b-1]! == p.xs[a]! && p.ys[b-1]! == p.ys[a]!)
  if !needs then return p
  let mut xs : FloatArray := .emptyWithCapacity p.xs.size
  let mut ys : FloatArray := .emptyWithCapacity p.ys.size
  let mut starts : Array Nat := #[]
  let mut closed : Array Bool := #[]
  for k in [0:p.count] do
    let (a, b) := p.range k
    let cl := p.closed[k]!
    let mut e := b
    -- a closed subpath ending on its start point
    if cl && e > a + 1 && p.xs[e-1]! == p.xs[a]! && p.ys[e-1]! == p.ys[a]! then e := e - 1
    if e > a then
      starts := starts.push xs.size
      closed := closed.push cl
      for i in [a:e] do
        xs := xs.push p.xs[i]!
        ys := ys.push p.ys[i]!
  return { xs, ys, starts, closed }

/-- Flatten a path to polylines with tolerance `tol` pixels. -/
def flatten (p : Path) (tol : Float := flattenTol) : Polylines :=
  let cs := p.coords
  let c (i : Nat) : Float := cs.get! i
  let rec go (vi ci : Nat) (s : FlatSt) : FlatSt :=
    if h : vi < p.verbs.size then
      let v := Verb.ofUInt8 (p.verbs.get vi h)
      if ci + v.arity > cs.size then s else
      let s := match v with
        | .moveTo =>
          let x := c ci; let y := c (ci+1)
          if x.isFinite && y.isFinite then s.begin x y else { s with open_ := false, cx := x, cy := y }
        | .lineTo => s.lineTo (c ci) (c (ci+1))
        | .quadTo =>
          let s := s.ensureOpen
          if !s.open_ then s.lineTo (c (ci+2)) (c (ci+3))
          else if (c ci).isFinite && (c (ci+1)).isFinite && (c (ci+2)).isFinite && (c (ci+3)).isFinite then
            s.quadTo (c ci) (c (ci+1)) (c (ci+2)) (c (ci+3)) tol
          else s.lineTo Float.nan Float.nan
        | .cubicTo =>
          let s := s.ensureOpen
          if !s.open_ then s.lineTo (c (ci+4)) (c (ci+5))
          else if (c ci).isFinite && (c (ci+1)).isFinite && (c (ci+2)).isFinite && (c (ci+3)).isFinite &&
              (c (ci+4)).isFinite && (c (ci+5)).isFinite then
            s.cubicTo (c ci) (c (ci+1)) (c (ci+2)) (c (ci+3)) (c (ci+4)) (c (ci+5)) tol
          else s.lineTo Float.nan Float.nan
        | .close => s.close
      go (vi + 1) (ci + v.arity) s
    else s
  termination_by p.verbs.size - vi
  let s : FlatSt := { xs := .emptyWithCapacity (cs.size / 2 + 4), ys := .emptyWithCapacity (cs.size / 2 + 4),
                      starts := #[], closed := #[], cx := K.zero, cy := K.zero, sx := K.zero, sy := K.zero, open_ := false }
  let s := go 0 0 s
  Polylines.normalize { xs := s.xs, ys := s.ys, starts := s.starts, closed := s.closed }

/-- Polylines straight from coordinate arrays (one open subpath, NaN breaks). -/
def Polylines.ofPoints (xs ys : FloatArray) (closed : Bool := false) : Polylines :=
  let n := min xs.size ys.size
  let rec go (i : Nat) (s : FlatSt) : FlatSt :=
    if i < n then go (i + 1) (s.lineTo (xs.get! i) (ys.get! i)) else s
  termination_by n - i
  let s : FlatSt := { xs := .emptyWithCapacity n, ys := .emptyWithCapacity n, starts := #[], closed := #[],
                      cx := Float.nan, cy := Float.nan, sx := K.zero, sy := K.zero, open_ := false }
  let s := go 0 s
  let s := if closed then s.close else s
  Polylines.normalize { xs := s.xs, ys := s.ys, starts := s.starts, closed := s.closed }

namespace Accum

/-- Deposit the edges of points `[i, b)` (a polyline), then its closing edge
back to `a`. -/
def fillSub (buf : FloatArray) (w h : Nat) (cl : Clip) (xs ys : FloatArray) (a i b : Nat) : FloatArray :=
  if i + 1 < b then
    let buf := lineRaw buf w h cl (xs.get! i) (ys.get! i) (xs.get! (i+1)) (ys.get! (i+1))
    fillSub buf w h cl xs ys a (i + 1) b
  else if b > a + 1 then
    lineRaw buf w h cl (xs.get! (b-1)) (ys.get! (b-1)) (xs.get! a) (ys.get! a)
  else buf
termination_by b - i

/-- Deposit every subpath of `p` as a closed polygon. -/
def fillPolylines (acc : Accum) (cl : Clip) (p : Polylines) : Accum :=
  let rec go (k : Nat) (buf : FloatArray) : FloatArray :=
    if k < p.count then
      let (a, b) := p.range k
      go (k + 1) (fillSub buf acc.w acc.h cl p.xs p.ys a a b)
    else buf
  termination_by p.count - k
  { acc with buf := go 0 acc.buf }

end Accum

end LeanPlot.Raster
