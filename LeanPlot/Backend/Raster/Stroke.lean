import LeanPlot.Backend.Raster.Flatten

/-
Stroker: polylines → a union of same-orientation polygons that the coverage
accumulator fills with the nonzero rule.

Each segment contributes its offset rectangle. Each vertex contributes the
outer join wedge: a bevel triangle, a miter quad (subject to the miter limit)
or a round fan. Each open end contributes a cap: nothing for butt, a half
square for square, a half disc for round. All pieces are emitted with the
same orientation. Pieces that share an edge therefore cancel on it exactly,
and overlaps on the inner side of a turn simply have winding > 1, which the
nonzero rule clamps. Because the stroke is resolved in one sweep, a
translucent stroke is never blended twice where its pieces overlap.

Dashes (`dashPolylines`) cut the flattened polylines into open pieces before
stroking. The pattern restarts on every subpath, as in SVG.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Resolved stroke geometry. -/
structure StrokeGeom where
  /-- half width -/
  hw : Float
  cap : LineCap
  join : LineJoin
  miterLimit : Float
  /-- arc flattening tolerance (px) -/
  tol : Float := K.tenth
  /-- culling box: pieces entirely outside `[bx0, bx1] × [by0, by1]` are not
  emitted (each piece is closed, so an invisible one contributes nothing) -/
  bx0 : Float := -K.huge
  by0 : Float := -K.huge
  bx1 : Float := K.huge
  by1 : Float := K.huge
  deriving Repr, Inhabited

namespace StrokeGeom

/-- How far a stroke's geometry can reach from its centre line: miter tips,
square caps and round joins all stay within this distance. -/
def reach (g : StrokeGeom) : Float := g.hw * max g.miterLimit K.two + K.two

/-- Cull everything more than `reach` outside the clip. -/
def withClip (g : StrokeGeom) (cl : Clip) : StrokeGeom :=
  let m := g.reach
  { g with bx0 := cl.x0 - m, by0 := cl.y0 - m, bx1 := cl.x1 + m, by1 := cl.y1 + m }

/-- Is the point inside the culling box? -/
@[inline] def ptVis (g : StrokeGeom) (x y : Float) : Bool :=
  x ≥ g.bx0 && x ≤ g.bx1 && y ≥ g.by0 && y ≤ g.by1

/-- Does the segment's bounding box meet the culling box? -/
@[inline] def segVis (g : StrokeGeom) (x0 y0 x1 y1 : Float) : Bool :=
  !(max x0 x1 < g.bx0 || min x0 x1 > g.bx1 || max y0 y1 < g.by0 || min y0 y1 > g.by1)

end StrokeGeom

namespace Stroker

/-- Segments shorter than this are treated as zero-length. -/
def minSeg : Float := K.eps9

/-- An edge. -/
@[inline] def edge (b : FloatArray) (w h : Nat) (cl : Clip) (x0 y0 x1 y1 : Float) : FloatArray :=
  Accum.lineRaw b w h cl x0 y0 x1 y1

/-- Closed quad `A → B → C → D → A` (caller guarantees the orientation). -/
@[inline] def quad (b : FloatArray) (w h : Nat) (cl : Clip) (ax ay bx by_ cx cy dx dy : Float) : FloatArray :=
  let b := edge b w h cl ax ay bx by_
  let b := edge b w h cl bx by_ cx cy
  let b := edge b w h cl cx cy dx dy
  edge b w h cl dx dy ax ay

/-- Rectangle of half width `hw` around the segment `P → Q` with unit
direction `(ux, uy)` (the canonical orientation used by every piece). -/
@[inline] def segQuad (b : FloatArray) (w h : Nat) (cl : Clip) (hw px py qx qy ux uy : Float) : FloatArray :=
  let nx := -uy * hw; let ny := ux * hw
  quad b w h cl (px + nx) (py + ny) (qx + nx) (qy + ny) (qx - nx) (qy - ny) (px - nx) (py - ny)

/-- Triangle `P, A, B`, emitted with the canonical (negative) orientation. -/
@[inline] def tri (b : FloatArray) (w h : Nat) (cl : Clip) (px py ax ay bx by_ : Float) : FloatArray :=
  let cr := (ax - px) * (by_ - py) - (ay - py) * (bx - px)
  if cr < K.zero then
    edge (edge (edge b w h cl px py ax ay) w h cl ax ay bx by_) w h cl bx by_ px py
  else
    edge (edge (edge b w h cl px py bx by_) w h cl bx by_ ax ay) w h cl ax ay px py

/-- Arc steps for radius `r`, sweep `|sweep|`, tolerance `tol`. -/
@[inline] def arcSteps (r sweep tol : Float) : Nat :=
  let step := if r > tol then K.two * Float.acos (K.one - tol / r) else K.halfPi
  let n := (sweep.abs / step).ceil
  if n.isNaN || n < K.one then 1 else min 1024 n.toUInt64.toNat

/-- Arc points `i = 1‥n` of the arc centred at `(px, py)` from angle `a0`
sweeping `sw`: interior points at radius `rin`, the last at `r`; the
previous point is `(x, y)`. -/
def arcEdges (b : FloatArray) (w h : Nat) (cl : Clip) (px py r rin a0 sw : Float) (n i : Nat) (x y : Float)
    (fuel : Nat) : FloatArray × Float × Float :=
  match fuel with
  | 0 => (b, x, y)
  | fuel + 1 =>
    if i > n then (b, x, y) else
    let t := a0 + sw * (natF i / natF n)
    let rr := if i == n then r else rin
    let nx := px + rr * Float.cos t
    let ny := py + rr * Float.sin t
    arcEdges (edge b w h cl x y nx ny) w h cl px py r rin a0 sw n (i + 1) nx ny fuel

/-- Radius for the interior vertices of an `n`-step fan of radius `r` and step
angle `θ` whose end points stay on the circle, chosen so that the fan's area
equals the sector's. The triangles have area `½ sin θ · (2 r r' + (n−2) r'²)`,
which must equal `½ r² n θ`. -/
def fanRadius (r θ : Float) (n : Nat) : Float :=
  if n ≤ 1 || !(θ > K.eps9) then r else
  let q := natF n * θ / Float.sin θ
  if n == 2 then r * q / K.two
  else
    let m := natF (n - 2)
    r * ((K.one + m * q).sqrt - K.one) / m

/-- Pie slice centred at `P`: `P → arc(a0, a0 + sw) → P` (orientation follows
`sw`). The polygon is area-exact (see `fanRadius`). -/
def fan (b : FloatArray) (w h : Nat) (cl : Clip) (px py r a0 sw tol : Float) : FloatArray :=
  let n := arcSteps r sw tol
  let rin := fanRadius r (sw.abs / natF n) n
  let sx := px + r * Float.cos a0
  let sy := py + r * Float.sin a0
  let b := edge b w h cl px py sx sy
  let (b, ex, ey) := arcEdges b w h cl px py r rin a0 sw n 1 sx sy (n + 1)
  edge b w h cl ex ey px py

/-- Cap at an end point `P` of a stroke leaving in direction `(ux, uy)`. -/
def cap (b : FloatArray) (w h : Nat) (cl : Clip) (g : StrokeGeom) (px py ux uy : Float) : FloatArray :=
  match g.cap with
  | .butt => b
  | .square => segQuad b w h cl g.hw px py (px + ux * g.hw) (py + uy * g.hw) ux uy
  | .round =>
    -- half disc on the forward side: from the left normal, sweeping −π
    let an := Float.atan2 ux (-uy)
    fan b w h cl px py g.hw an (-K.pi) g.tol

/-- Join at vertex `P` between incoming direction `u` and outgoing `v`. -/
def join (b : FloatArray) (w h : Nat) (cl : Clip) (g : StrokeGeom) (px py ux uy vx vy : Float) : FloatArray :=
  let cr := ux * vy - uy * vx
  let dt := ux * vx + uy * vy
  if cr.abs < K.eps12 then
    if dt > K.zero then b
    -- full reversal: only a round join shows anything (a half disc ahead)
    else if g.join == .round then cap b w h cl { g with cap := .round } px py ux uy
    else b
  else
  let hw := g.hw
  let sg := if cr > K.zero then -K.one else K.one
  -- outer offset points
  let ax := px + sg * (-uy) * hw; let ay := py + sg * ux * hw
  let bx := px + sg * (-vy) * hw; let by_ := py + sg * vx * hw
  match g.join with
  | .bevel => tri b w h cl px py ax ay bx by_
  | .miter =>
    let ratio := (K.two / (K.one + dt)).sqrt
    if K.one + dt > K.eps12 && ratio ≤ g.miterLimit then
      let k := sg * hw / (K.one + dt)
      let mx := px + k * (-uy - vy)
      let my := py + k * (ux + vx)
      let o := (ax - px) * (by_ - py) - (ay - py) * (bx - px)
      if o < K.zero then quad b w h cl px py ax ay mx my bx by_
      else quad b w h cl px py bx by_ mx my ax ay
    else tri b w h cl px py ax ay bx by_
  | .round =>
    let a0 := Float.atan2 (ay - py) (ax - px)
    let sw := Float.atan2 ((ax - px) * (by_ - py) - (ay - py) * (bx - px)) ((ax - px) * (bx - px) + (ay - py) * (by_ - py))
    -- orientation: forward if the wedge is negative, else walk it backwards
    if sw < K.zero then fan b w h cl px py hw a0 sw g.tol
    else fan b w h cl px py hw (a0 + sw) (-sw) g.tol

/-- A zero-length subpath: a dot for round/square caps. -/
def dot (b : FloatArray) (w h : Nat) (cl : Clip) (g : StrokeGeom) (px py : Float) : FloatArray :=
  match g.cap with
  | .butt => b
  | _ => cap (cap b w h cl g px py K.one K.zero) w h cl g px py (-K.one) K.zero

/-- Stroke points `[i, e)` of a subpath whose first point is index `a`.
`(qx, qy)`: last accepted point; `(ux, uy)`: direction of the last segment;
`(fx, fy)`: direction of the first segment (valid when `has`). -/
def walk (b : FloatArray) (w h : Nat) (cl : Clip) (g : StrokeGeom) (xs ys : FloatArray) (closed : Bool)
    (a i e : Nat) (qx qy ux uy fx fy : Float) (has : Bool) : FloatArray :=
  if i < e then
    let x := xs.get! i; let y := ys.get! i
    let dx := x - qx; let dy := y - qy
    let len := (dx * dx + dy * dy).sqrt
    if len < minSeg then walk b w h cl g xs ys closed a (i + 1) e qx qy ux uy fx fy has else
    let vx := dx / len; let vy := dy / len
    let b := if !g.ptVis qx qy then b
      else if has then join b w h cl g qx qy ux uy vx vy
      else if closed then b else cap b w h cl g qx qy (-vx) (-vy)
    let b := if g.segVis qx qy x y then segQuad b w h cl g.hw qx qy x y vx vy else b
    let fx' := if has then fx else vx
    let fy' := if has then fy else vy
    walk b w h cl g xs ys closed a (i + 1) e x y vx vy fx' fy' true
  else
    let x0 := xs.get! a; let y0 := ys.get! a
    if !has then (if g.ptVis x0 y0 then dot b w h cl g x0 y0 else b)
    else if !closed then (if g.ptVis qx qy then cap b w h cl g qx qy ux uy else b)
    else
      -- closing segment back to the first point, then the join there
      let dx := x0 - qx; let dy := y0 - qy
      let len := (dx * dx + dy * dy).sqrt
      if len < minSeg then (if g.ptVis x0 y0 then join b w h cl g x0 y0 ux uy fx fy else b)
      else
        let vx := dx / len; let vy := dy / len
        let b := if g.ptVis qx qy then join b w h cl g qx qy ux uy vx vy else b
        let b := if g.segVis qx qy x0 y0 then segQuad b w h cl g.hw qx qy x0 y0 vx vy else b
        if g.ptVis x0 y0 then join b w h cl g x0 y0 vx vy fx fy else b
termination_by e - i

/-- Deposit the stroke of every subpath of `pl`. -/
def strokeAll (b : FloatArray) (w h : Nat) (cl : Clip) (g : StrokeGeom) (pl : Polylines) (k : Nat) : FloatArray :=
  if k < pl.count then
    let (a, e) := pl.range k
    let b := if e > a then
      walk b w h cl g pl.xs pl.ys pl.closed[k]! a (a + 1) e (pl.xs.get! a) (pl.ys.get! a) K.zero K.zero K.zero K.zero false
      else b
    strokeAll b w h cl g pl (k + 1)
  else b
termination_by pl.count - k

end Stroker

/-- Stroke geometry from a `Stroke` style (width clamped at 0). -/
def StrokeGeom.ofStroke (s : Stroke) : StrokeGeom :=
  { hw := K.half * (if s.width > K.zero then s.width else K.zero), cap := s.cap, join := s.join,
    miterLimit := if s.miterLimit ≥ K.one then s.miterLimit else K.one }

namespace Accum

/-- Deposit the stroke outline of `pl` (already dashed, if dashing applies). -/
def strokePolylines {w h : Nat} (acc : Accum w h) (cl : Clip) (pl : Polylines) (g : StrokeGeom) : Accum w h :=
  if g.hw ≤ K.zero then acc else
  ⟨Stroker.strokeAll acc.buf w h cl (g.withClip cl) pl 0⟩

end Accum

/-! ## Dashing -/

/-- Dash state while walking a subpath. -/
structure DashSt where
  xs : FloatArray
  ys : FloatArray
  starts : Array Nat
  /-- index into the pattern -/
  idx : Nat
  /-- length left in the current pattern element -/
  rem : Float
  /-- is the current element "on"? -/
  on : Bool
  /-- is a dash currently open in the output? -/
  inDash : Bool
  /-- a dash has begun at `(sx, sy)` but has no point yet: it is only written
  out when its first point arrives, so a dash that would begin exactly at the
  end of a subpath never appears -/
  pend : Bool := false
  sx : Float := 0.0
  sy : Float := 0.0

namespace DashSt

/-- Begin a dash at `(x, y)` (deferred until its first point). -/
@[inline] def startDash (s : DashSt) (x y : Float) : DashSt :=
  { s with pend := true, sx := x, sy := y, inDash := true }

/-- Append a point to the open dash, materialising a pending start (whose
index is recorded before the pushes, as in `FlatSt.begin`, so the buffers
are never copied). -/
@[inline] def addPoint (s : DashSt) (x y : Float) : DashSt :=
  if s.pend then
    let s := { s with starts := s.starts.push s.xs.size, pend := false }
    { s with xs := (s.xs.push s.sx).push x, ys := (s.ys.push s.sy).push y }
  else { s with xs := s.xs.push x, ys := s.ys.push y }

/-- Advance to the next pattern element at point `(x, y)`. -/
@[inline] def toggle (s : DashSt) (pat : Array Float) (x y : Float) : DashSt :=
  let idx := (s.idx + 1) % pat.size
  let on := !s.on
  let s := { s with idx, rem := pat[idx]!, on, inDash := false, pend := false }
  if on then s.startDash x y else s

/-- `advance`'s element loop: consume elements while `d ≥ rem` (at most
`fuel` of them), then leave `rem - d` of the current one. -/
def advanceLoop (s : DashSt) (pat : Array Float) (d rem : Float) (idx : Nat) (on : Bool) (fuel : Nat) : DashSt :=
  match fuel with
  | 0 => { s with idx, on, rem := rem - d }
  | fuel + 1 =>
    if d ≥ rem then
      let idx := (idx + 1) % pat.size
      advanceLoop s pat (d - rem) pat[idx]! idx (!on) fuel
    else { s with idx, on, rem := rem - d }

/-- Advance the pattern by arc length `d` without emitting anything
(`pat` has even length, so whole periods can be skipped). -/
def advance (s : DashSt) (pat : Array Float) (total d : Float) : DashSt :=
  if d < s.rem then { s with rem := s.rem - d } else
  let d := d - s.rem
  let idx := (s.idx + 1) % pat.size
  advanceLoop s pat (d - total * (d / total).floor) pat[idx]! idx (!s.on) (pat.size + 1)

/-- Walk the segment `(px, py) → (qx, qy)` of length `len` starting at arc
position `pos`. -/
def seg (s : DashSt) (pat : Array Float) (px py qx qy len pos : Float) (fuel : Nat) : DashSt :=
  match fuel with
  | 0 => s
  | fuel + 1 =>
    if pos ≥ len then s else
    let step := min s.rem (len - pos)
    let pos' := pos + step
    let t := pos' / len
    let x := px + (qx - px) * t
    let y := py + (qy - py) * t
    let s := if s.on then s.addPoint x y else s
    let s := { s with rem := s.rem - step }
    if s.rem ≤ K.eps12 then seg (s.toggle pat x y) pat px py qx qy len pos' fuel
    else seg s pat px py qx qy len pos' fuel

end DashSt

/-- Normalise a dash pattern (SVG rules): `none` means "solid" (empty, any
negative entry, or a non-positive total); odd-length patterns are repeated. -/
def dashPattern? (d : Array Float) : Option (Array Float) :=
  if d.isEmpty || d.any (fun v => v < K.zero || !v.isFinite) then none else
  let p := if d.size % 2 == 1 then d ++ d else d
  if p.foldl (· + ·) K.zero ≤ K.zero then none else some p

/-- Liang–Barsky lower bound update for the constraint `p·t ≤ q`: raises `t`
when `p < 0`, and empties the interval (`t = 2`) when `p = 0` and `q < 0`. -/
@[inline] def lbLo (t p q : Float) : Float :=
  if p < K.zero then max t (q / p) else if p == K.zero && q < K.zero then K.two else t

/-- Liang–Barsky upper bound update for `p·t ≤ q`: lowers `t` when `p > 0`. -/
@[inline] def lbHi (t p q : Float) : Float := if p > K.zero then min t (q / p) else t

/-- Parameter interval `[t0, t1] ⊆ [0, 1]` of the segment `P → Q` inside the
box (Liang–Barsky); empty when `t0 > t1`. -/
@[inline] def clipParam (px py qx qy bx0 by0 bx1 by1 : Float) : Float × Float :=
  let dx := qx - px; let dy := qy - py
  let t0 := lbLo (lbLo (lbLo (lbLo K.zero (-dx) (px - bx0)) dx (bx1 - px)) (-dy) (py - by0)) dy (by1 - py)
  let t1 := lbHi (lbHi (lbHi (lbHi K.one (-dx) (px - bx0)) dx (bx1 - px)) (-dy) (py - by0)) dy (by1 - py)
  (t0, t1)

/-- Visible length of the segments `i ∈ [i, stop)` of a subpath with points
`[a, e)` (segment `i` ends at `i + 1`, or wraps to `a`), added to `acc`. -/
def visibleLengthSegs (xs ys : FloatArray) (a e : Nat) (bx0 by0 bx1 by1 : Float) (i stop : Nat) (acc : Float) :
    Float :=
  if i < stop then
    let i2 := if i + 1 < e then i + 1 else a
    let px := xs.get! i; let py := ys.get! i
    let qx := xs.get! i2; let qy := ys.get! i2
    let (t0, t1) := clipParam px py qx qy bx0 by0 bx1 by1
    let acc := if t0 < t1 then acc + (t1 - t0) * ((qx - px) * (qx - px) + (qy - py) * (qy - py)).sqrt else acc
    visibleLengthSegs xs ys a e bx0 by0 bx1 by1 (i + 1) stop acc
  else acc
termination_by stop - i

/-- `visibleLength` over subpaths `[k, count)`, added to `acc`. -/
def visibleLengthFrom (pl : Polylines) (bx0 by0 bx1 by1 : Float) (k : Nat) (acc : Float) : Float :=
  if k < pl.count then
    let (a, e) := pl.range k
    let acc := if e ≤ a then acc else
      let segs := if pl.closed[k]! then e - a else e - a - 1
      visibleLengthSegs pl.xs pl.ys a e bx0 by0 bx1 by1 a (a + segs) acc
    visibleLengthFrom pl bx0 by0 bx1 by1 (k + 1) acc
  else acc
termination_by pl.count - k

/-- Total length of the parts of `pl`'s segments inside the box. -/
def visibleLength (pl : Polylines) (bx0 by0 bx1 by1 : Float) : Float :=
  visibleLengthFrom pl bx0 by0 bx1 by1 0 K.zero

/-- Pattern phase at arc length `off` from the start of a subpath: consume
whole elements while `off ≥ rem` (at most `fuel` of them). -/
def DashSt.phase (s : DashSt) (pat : Array Float) (off rem : Float) (idx : Nat) (on : Bool) (fuel : Nat) : DashSt :=
  match fuel with
  | 0 => { s with idx, rem, on }
  | fuel + 1 =>
    if off > K.zero then
      if off ≥ rem then
        let idx := (idx + 1) % pat.size
        DashSt.phase s pat (off - rem) pat[idx]! idx (!on) fuel
      else { s with idx, rem := rem - off, on }
    else { s with idx, rem, on }

/-- Break the open dash, skip `d` of arc length, and reopen a dash at `(x, y)`
if the pattern is then "on". -/
@[inline] def DashSt.skip (s : DashSt) (pat : Array Float) (total d x y : Float) : DashSt :=
  let s := ({ s with inDash := false, pend := false } : DashSt).advance pat total d
  if s.on then s.startDash x y else s

/-- Dash one segment `P → Q` of length `len > 0`, of which only the parameter
interval `[t0, t1]` is visible. -/
@[inline] def DashSt.segment (s : DashSt) (pat : Array Float) (total px py qx qy len t0 t1 : Float) : DashSt :=
  if t0 > t1 then s.skip pat total len qx qy else
  -- invisible prefix
  let s := if t0 > K.zero then s.skip pat total (t0 * len) (px + (qx - px) * t0) (py + (qy - py) * t0) else s
  let vis := (t1 - t0) * len
  let s := if vis > K.zero then
      -- bound the steps: every step either finishes the segment or an element
      let fuel := ((vis / total).ceil.toUInt64.toNat + 2) * pat.size + 4
      s.seg pat (px + (qx - px) * t0) (py + (qy - py) * t0) (px + (qx - px) * t1) (py + (qy - py) * t1) vis K.zero fuel
    else s
  -- invisible suffix
  if t1 < K.one then s.skip pat total ((K.one - t1) * len) qx qy else s

/-- Dash segments `i ∈ [i, stop)` of a subpath with points `[a, e)`. With
`culled`, only the part of each segment inside the box is cut into dashes. -/
def dashSegs (pat : Array Float) (total : Float) (culled : Bool) (bx0 by0 bx1 by1 : Float) (xs ys : FloatArray)
    (a e i stop : Nat) (s : DashSt) : DashSt :=
  if i < stop then
    let i2 := if i + 1 < e then i + 1 else a
    let px := xs.get! i; let py := ys.get! i
    let qx := xs.get! i2; let qy := ys.get! i2
    let len := ((qx - px) * (qx - px) + (qy - py) * (qy - py)).sqrt
    let s := if len > K.zero then
        let (t0, t1) := if culled then clipParam px py qx qy bx0 by0 bx1 by1 else (K.zero, K.one)
        s.segment pat total px py qx qy len t0 t1
      else s
    dashSegs pat total culled bx0 by0 bx1 by1 xs ys a e (i + 1) stop s
  else s
termination_by stop - i

/-- Dash subpaths `[k, count)`; every subpath starts at the pattern phase
`ph` (only its `idx`, `rem` and `on` are read). -/
def dashSubpaths (pl : Polylines) (pat : Array Float) (total : Float) (ph : DashSt) (culled : Bool)
    (bx0 by0 bx1 by1 : Float) (k : Nat) (s : DashSt) : DashSt :=
  if k < pl.count then
    let (a, e) := pl.range k
    if e ≤ a then dashSubpaths pl pat total ph culled bx0 by0 bx1 by1 (k + 1) s else
    let s := { s with idx := ph.idx, rem := ph.rem, on := ph.on, inDash := false, pend := false }
    let s := if ph.on then s.startDash (pl.xs.get! a) (pl.ys.get! a) else s
    let segs := if pl.closed[k]! then e - a else e - a - 1
    let s := dashSegs pat total culled bx0 by0 bx1 by1 pl.xs pl.ys a e a (a + segs) s
    dashSubpaths pl pat total ph culled bx0 by0 bx1 by1 (k + 1) { s with inDash := false, pend := false }
  else s
termination_by pl.count - k

/-- Cut polylines into dashes. Every output subpath is open. Segment parts
outside `box` (x0, y0, x1, y1), when given, only advance the pattern
analytically; no dashes are produced there. The work is then bounded by the
visible length, even for a far-off path with a fine pattern. -/
def dashPolylines (pl : Polylines) (dash : Array Float) (offset : Float := K.zero)
    (box : Option (Float × Float × Float × Float) := none) : Polylines :=
  match dashPattern? dash with
  | none => pl
  | some pat =>
    let total := pat.foldl (· + ·) K.zero
    let off0 := if offset.isFinite then offset - total * (offset / total).floor else K.zero
    let s : DashSt := { xs := .emptyWithCapacity pl.xs.size, ys := .emptyWithCapacity pl.ys.size,
                        starts := #[], idx := 0, rem := pat[0]!, on := true, inDash := false }
    -- the phase at the start of every subpath (the offset consumed); built
    -- from its own empty state so that `s`'s buffers stay unshared
    let ph := ({ xs := .empty, ys := .empty, starts := #[], idx := 0, rem := pat[0]!, on := true,
                 inDash := false } : DashSt).phase pat off0 pat[0]! 0 true 100000
    let (culled, bx0, by0, bx1, by1) := match box with
      | none => (false, K.zero, K.zero, K.zero, K.zero)
      | some (bx0, by0, bx1, by1) => (true, bx0, by0, bx1, by1)
    let s := dashSubpaths pl pat total ph culled bx0 by0 bx1 by1 0 s
    { xs := s.xs, ys := s.ys, starts := s.starts, closed := Array.replicate s.starts.size false }

end LeanPlot.Raster
