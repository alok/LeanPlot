import LeanPlotTest.Font.Harness

/-!
Outline sanity: every glyph of every embedded face is a sequence of closed contours
(`moveTo … close`), its coordinates are exactly consumed by its verbs, the box of its control
points equals the stored control box, and the exact extrema of its curves reproduce the
tight bounds fontTools computed (`bounds` in `metrics.json`). Also checks contour orientation
(outer vs counter) and that `textPath` places outlines exactly where the layout says.
-/

namespace LeanPlotTest.Font.Paths

open LeanPlot LeanPlot.Font

/-- Structural check of one glyph: `none` if fine, else a description. -/
def structure? (f : Face) (g : Nat) : Option String := Id.run do
  let v0 := f.verbStart[g]!
  let v1 := f.verbStart[g + 1]!
  let mut open_ := false
  let mut n := 0
  for i in [v0:v1] do
    let v := Verb.ofUInt8 (f.verbs.get! i)
    match v with
    | .moveTo =>
      if open_ then return some s!"moveTo inside an open contour at verb {i - v0}"
      open_ := true
    | .close =>
      if !open_ then return some s!"close without contour at verb {i - v0}"
      open_ := false
    | _ => if !open_ then return some s!"segment before moveTo at verb {i - v0}"
    n := n + v.arity
  if open_ then return some "last contour not closed"
  if n != f.coordStart[g + 1]! - f.coordStart[g]! then return some "coordinate count mismatch"
  return none

/-- Extend a `(lo, hi)` range with `x`. -/
@[inline] def ext (r : Float × Float) (x : Float) : Float × Float :=
  (if x < r.1 then x else r.1, if x > r.2 then x else r.2)

/-- Extend with the extrema of a quadratic Bézier coordinate. -/
def quadExt (r : Float × Float) (p0 p1 p2 : Float) : Float × Float :=
  let r := ext (ext r p0) p2
  let den := p0 - 2 * p1 + p2
  if den == 0 then r else
  let t := (p0 - p1) / den
  if t > 0 && t < 1 then ext r ((1 - t) * (1 - t) * p0 + 2 * t * (1 - t) * p1 + t * t * p2) else r

/-- Extend with the extrema of a cubic Bézier coordinate. -/
def cubicExt (r : Float × Float) (p0 p1 p2 p3 : Float) : Float × Float :=
  let r := ext (ext r p0) p3
  let at_ (t : Float) : Float :=
    let u := 1 - t
    u * u * u * p0 + 3 * u * u * t * p1 + 3 * u * t * t * p2 + t * t * t * p3
  let d0 := p1 - p0
  let d1 := p2 - p1
  let d2 := p3 - p2
  let a := d0 - 2 * d1 + d2
  let b := 2 * (d1 - d0)
  let c := d0
  let add (r : Float × Float) (t : Float) := if t > 0 && t < 1 then ext r (at_ t) else r
  if Float.abs a < 1e-12 then
    if b == 0 then r else add r (-c / b)
  else
    let disc := b * b - 4 * a * c
    if disc < 0 then r else
    let sq := Float.sqrt disc
    add (add r ((-b + sq) / (2 * a))) ((-b - sq) / (2 * a))

/-- Control-point box and exact tight box of a path: `((cx0,cx1),(cy0,cy1)), ((tx0,tx1),(ty0,ty1))`. -/
def boxes (p : Path) : (Float × Float) × (Float × Float) × (Float × Float) × (Float × Float) := Id.run do
  let big := 1e300
  let mut cx := (big, -big)
  let mut cy := (big, -big)
  let mut tx := (big, -big)
  let mut ty := (big, -big)
  let mut ci := 0
  let mut px := 0.0
  let mut py := 0.0
  let c := p.coords
  for i in [0:p.verbs.size] do
    match Verb.ofUInt8 (p.verbs.get! i) with
    | .moveTo | .lineTo =>
      px := c[ci]!; py := c[ci + 1]!
      cx := ext cx px; cy := ext cy py; tx := ext tx px; ty := ext ty py
      ci := ci + 2
    | .quadTo =>
      for k in [0:2] do cx := ext cx c[ci + 2 * k]!; cy := ext cy c[ci + 2 * k + 1]!
      tx := quadExt tx px c[ci]! c[ci + 2]!
      ty := quadExt ty py c[ci + 1]! c[ci + 3]!
      px := c[ci + 2]!; py := c[ci + 3]!
      ci := ci + 4
    | .cubicTo =>
      for k in [0:3] do cx := ext cx c[ci + 2 * k]!; cy := ext cy c[ci + 2 * k + 1]!
      tx := cubicExt tx px c[ci]! c[ci + 2]! c[ci + 4]!
      ty := cubicExt ty py c[ci + 1]! c[ci + 3]! c[ci + 5]!
      px := c[ci + 4]!; py := c[ci + 5]!
      ci := ci + 6
    | .close => pure ()
  return (cx, cy, tx, ty)

/-- Signed area (shoelace over on-curve points and control polygon) of each contour. -/
def contourAreas (p : Path) : Array Float := Id.run do
  let mut out : Array Float := #[]
  let mut acc := 0.0
  let mut sx := 0.0
  let mut sy := 0.0
  let mut px := 0.0
  let mut py := 0.0
  let mut ci := 0
  let c := p.coords
  for i in [0:p.verbs.size] do
    let v := Verb.ofUInt8 (p.verbs.get! i)
    match v with
    | .moveTo =>
      sx := c[ci]!; sy := c[ci + 1]!; px := sx; py := sy; acc := 0
    | .close =>
      acc := acc + (px * sy - sx * py)
      out := out.push (acc / 2)
    | _ =>
      for k in [0:v.arity / 2] do
        let x := c[ci + 2 * k]!
        let y := c[ci + 2 * k + 1]!
        acc := acc + (px * y - x * py)
        px := x; py := y
    ci := ci + v.arity
  return out

/-- Check every glyph of a face. -/
def checkFace (t : Tally) (key : String) (f : Face) (ref : Json) : Tally := Id.run do
  let mut t := t
  -- glyph id → reference record (for tight bounds)
  let mut byGid : Array Json := Array.replicate f.numGlyphs .null
  for (_, g) in (ref.get "glyphs").obj! do
    let gid := (g.get "gid").num!.toUInt64.toNat
    if gid < byGid.size then byGid := byGid.set! gid g
  for g in [0:f.numGlyphs] do
    match structure? f g with
    | some e => t := t.check false s!"{key} glyph {g}: {e}"
    | none => t := t.check true ""
    let p := f.glyphPath g
    if p.verbs.size == 0 then continue
    let (cx, cy, tx, ty) := boxes p
    let (x0, y0, x1, y1) := f.glyphBox g
    t := t.check' (cx.1 == x0 && cx.2 == x1 && cy.1 == y0 && cy.2 == y1) fun _ =>
      s!"{key} glyph {g}: control box {cx} {cy} vs ({x0}, {y0}, {x1}, {y1})"
    let r := byGid[g]!
    unless r.isNull do
      let b := r.get "bounds"
      let tol := 1e-4
      t := t.check' (near tx.1 (b.idx 0).num! tol && near ty.1 (b.idx 1).num! tol &&
          near tx.2 (b.idx 2).num! tol && near ty.2 (b.idx 3).num! tol) fun _ =>
        s!"{key} glyph {g} ({(r.get "name").str!}): tight box {tx} {ty} vs fontTools {(b.idx 0).num!} {(b.idx 1).num!} {(b.idx 2).num!} {(b.idx 3).num!}"
  return t

/-- Outer contour and counter of `O`/`o`/`0` wind in opposite directions. -/
def checkOrientation (t : Tally) : Tally := Id.run do
  let mut t := t
  for (key, f) in [("HerosRegular", herosRegular), ("HerosBold", herosBold), ("DejaVuSans", dejaVuSans)] do
    for c in ['O', 'o', '0', 'D'] do
      let g := f.glyphIndex c.toNat
      if g == 0 then continue
      let a := contourAreas (f.glyphPath g)
      t := t.check (a.size == 2 && a[0]! * a[1]! < 0) s!"{key} {c}: contour areas {a}"
  return t

/-- `textPath` places each glyph's outline at the laid-out origin, scaled and flipped: its
control box equals the run's ink box mapped to device space (rotation 0), and a rotation by
π/2 maps it onto the rotated box. -/
def checkTextPath (t : Tally) : Tally := Id.run do
  let mut t := t
  for s in ["Hello, World", "v₁₂ ∧ w₃ ≈ ∫ f", "x", "Å gjpqy\n−1.5"] do
    for (ha, va) in [(HAlign.left, VAlign.baseline), (.center, .middle), (.right, .top), (.left, .bottom)] do
      let style : TextStyle := { size := 20, halign := ha, valign := va }
      let run := layoutStyled style s
      let p := textPath style s 100 50
      let expectVerbs := run.codes.foldl (fun n code =>
        let f := run.font.faceOf code
        n + (f.verbStart[code / 2 + 1]! - f.verbStart[code / 2]!)) 0
      t := t.check (p.verbs.size == expectVerbs) s!"textPath {repr s}: verb count"
      let (cx, cy, _, _) := boxes p
      let ink := run.inkBox.toDevice 20 0 100 50
      let tol := 1e-9
      t := t.check' (near cx.1 ink.x tol && near cy.1 ink.y tol && near cx.2 (ink.x + ink.w) tol &&
          near cy.2 (ink.y + ink.h) tol) fun _ => s!"textPath {repr s}: box {cx} {cy} vs ink {repr ink}"
      -- rotated by π/2 (reads bottom to top): device box is the transposed ink box
      let pr := textPath { style with rotation := pi / 2 } s 100 50
      let (rx, ry, _, _) := boxes pr
      let inkR := run.inkBox.toDevice 20 (pi / 2) 100 50
      let tol := 1e-6
      t := t.check' (near rx.1 inkR.x tol && near ry.1 inkR.y tol && near rx.2 (inkR.x + inkR.w) tol &&
          near ry.2 (inkR.y + inkR.h) tol) fun _ => s!"textPath rot {repr s}: box {rx} {ry} vs {repr inkR}"
      t := t.check (near inkR.w ink.h tol && near inkR.h ink.w tol) s!"rotated box is transposed {repr s}"
  return t

/-- Run the path checks. -/
def run : IO (Nat × Nat) := do
  let j ← Json.readFile (dataPath "metrics.json")
  let faces := j.get "faces"
  let mut t : Tally := {}
  t := checkFace t "HerosRegular" herosRegular (faces.get "HerosRegular")
  t := checkFace t "HerosBold" herosBold (faces.get "HerosBold")
  t := checkFace t "DejaVuSans" dejaVuSans (faces.get "DejaVuSans")
  t := checkOrientation t
  t := checkTextPath t
  t.report "paths"

end LeanPlotTest.Font.Paths
