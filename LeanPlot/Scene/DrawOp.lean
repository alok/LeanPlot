/-
Device-space drawing IR: the contract between layout/recipes and backends.

Everything above this layer (figures, axes, recipes) lowers to a `Scene`, a
list of `DrawOp`s in device pixels (origin top-left, y down). Everything
below it (SVG writer, raster canvas + PNG) consumes a `Scene`. Backends must
not know about plots; recipes must not know about output formats.

Bulk geometry is structure-of-arrays (`FloatArray`, `ByteArray`), never
`Array (Float × Float)`: a 10⁵-point line costs two unboxed buffers rather
than 3·10⁵ heap cells.
-/

namespace LeanPlot

/-- Straight (non-premultiplied) sRGB colour with alpha, each channel in `[0, 1]`. -/
structure RGBA where
  r : Float
  g : Float
  b : Float
  a : Float := 1.0
  deriving Repr, Inhabited, BEq

namespace RGBA
def black : RGBA := ⟨0, 0, 0, 1⟩
def white : RGBA := ⟨1, 1, 1, 1⟩
def transparent : RGBA := ⟨0, 0, 0, 0⟩
/-- Same colour with alpha replaced. -/
def withAlpha (c : RGBA) (a : Float) : RGBA := { c with a }
end RGBA

/-- Axis-aligned rectangle in device pixels. -/
structure Rect where
  x : Float
  y : Float
  w : Float
  h : Float
  deriving Repr, Inhabited, BEq

/-- Path verbs. Coordinates live in `Path.coords`; each verb consumes a fixed
number of them: `moveTo`/`lineTo` 2, `quadTo` 4, `cubicTo` 6, `close` 0. -/
inductive Verb where
  | moveTo | lineTo | quadTo | cubicTo | close
  deriving Repr, Inhabited, BEq, DecidableEq

namespace Verb
/-- Encoding used in `Path.verbs`. -/
def toUInt8 : Verb → UInt8
  | moveTo => 0 | lineTo => 1 | quadTo => 2 | cubicTo => 3 | close => 4
def ofUInt8 : UInt8 → Verb
  | 0 => moveTo | 1 => lineTo | 2 => quadTo | 3 => cubicTo | _ => close
/-- Number of coordinates (x and y each count once) the verb consumes. -/
def arity : Verb → Nat
  | moveTo => 2 | lineTo => 2 | quadTo => 4 | cubicTo => 6 | close => 0
end Verb

/-- A vector path in device pixels, stored as a verb stream plus a flat
coordinate buffer (`x₀ y₀ x₁ y₁ …`). -/
structure Path where
  verbs : ByteArray := .empty
  coords : FloatArray := .empty
  deriving Inhabited

namespace Path
@[inline] def push (p : Path) (v : Verb) : Path := { p with verbs := p.verbs.push v.toUInt8 }
@[inline] def moveTo (p : Path) (x y : Float) : Path :=
  { verbs := p.verbs.push 0, coords := (p.coords.push x).push y }
@[inline] def lineTo (p : Path) (x y : Float) : Path :=
  { verbs := p.verbs.push 1, coords := (p.coords.push x).push y }
@[inline] def quadTo (p : Path) (x1 y1 x y : Float) : Path :=
  { verbs := p.verbs.push 2, coords := ((((p.coords.push x1).push y1).push x).push y) }
@[inline] def cubicTo (p : Path) (x1 y1 x2 y2 x y : Float) : Path :=
  { verbs := p.verbs.push 3, coords := ((((((p.coords.push x1).push y1).push x2).push y2).push x).push y) }
@[inline] def close (p : Path) : Path := { p with verbs := p.verbs.push 4 }
/-- Polyline through the given points (open). -/
def polyline (xs ys : FloatArray) : Path := Id.run do
  let n := min xs.size ys.size
  let mut p : Path := {}
  for i in [0:n] do
    p := if i == 0 then p.moveTo xs[i]! ys[i]! else p.lineTo xs[i]! ys[i]!
  return p
/-- Axis-aligned rectangle as a closed path. -/
def rect (r : Rect) : Path :=
  ((((({} : Path).moveTo r.x r.y).lineTo (r.x + r.w) r.y).lineTo (r.x + r.w) (r.y + r.h)).lineTo r.x (r.y + r.h)).close
end Path

inductive LineCap where | butt | round | square
  deriving Repr, Inhabited, BEq, DecidableEq
inductive LineJoin where | miter | round | bevel
  deriving Repr, Inhabited, BEq, DecidableEq
inductive FillRule where | nonzero | evenOdd
  deriving Repr, Inhabited, BEq, DecidableEq

/-- Stroke style. `width` in device pixels; `dash` is an on/off pattern in pixels (empty = solid). -/
structure Stroke where
  color : RGBA := .black
  width : Float := 1.0
  cap : LineCap := .butt
  join : LineJoin := .miter
  miterLimit : Float := 4.0
  dash : Array Float := #[]
  dashOffset : Float := 0.0
  deriving Repr, Inhabited

/-- Fill style. -/
structure Fill where
  color : RGBA := .black
  rule : FillRule := .nonzero
  deriving Repr, Inhabited

inductive HAlign where | left | center | right
  deriving Repr, Inhabited, BEq, DecidableEq
inductive VAlign where | top | middle | baseline | bottom
  deriving Repr, Inhabited, BEq, DecidableEq

/-- Text style. `size` is the font size in pixels (em height). -/
structure TextStyle where
  size : Float := 12.0
  color : RGBA := .black
  halign : HAlign := .left
  valign : VAlign := .baseline
  /-- Rotation in radians, counter-clockwise in *visual* terms (y-down device space). -/
  rotation : Float := 0.0
  /-- Font family hint for SVG; the raster backend uses its embedded font. -/
  family : String := "sans-serif"
  bold : Bool := false
  deriving Repr, Inhabited

/-- Image sampling when scaling a raster into `dst`. -/
inductive Interp where | nearest | linear
  deriving Repr, Inhabited, BEq, DecidableEq

/-- One device-space drawing operation. Ops are painted in order (painter's
algorithm); `clip`, when present, restricts painting to that rectangle. -/
inductive DrawOp where
  /-- Fill and/or stroke a path. -/
  | path (p : Path) (fill : Option Fill) (stroke : Option Stroke) (clip : Option Rect)
  /-- Many independent straight segments sharing a width, each with its own
  colour: segment `i` goes from `(xs[2i], ys[2i])` to `(xs[2i+1], ys[2i+1])` and
  has colour `rgba[4i..4i+4)` (RGBA8). Used for colour-mapped lines. -/
  | segments (xs ys : FloatArray) (rgba : ByteArray) (width : Float) (cap : LineCap) (clip : Option Rect)
  /-- Triangles with per-vertex colours (Gouraud shading). Vertex `k` is
  `(xs[k], ys[k])` with colour `rgba[4k..4k+4)`; triangle `t` uses vertices
  `idx[3t], idx[3t+1], idx[3t+2]` (little-endian `UInt32` stored as 4 bytes each
  in `idx`). Backends may approximate Gouraud by flat shading. -/
  | triangles (xs ys : FloatArray) (rgba : ByteArray) (idx : ByteArray) (clip : Option Rect)
  /-- A `w × h` RGBA8 image (row-major, top row first) drawn into `dst`. -/
  | image (w h : Nat) (rgba : ByteArray) (dst : Rect) (interp : Interp) (clip : Option Rect)
  /-- Text anchored at `(x, y)`. -/
  | text (x y : Float) (s : String) (style : TextStyle) (clip : Option Rect)
  deriving Inhabited

/-- A complete device-space scene. -/
structure Scene where
  width : Nat
  height : Nat
  background : RGBA := .white
  ops : Array DrawOp := #[]
  deriving Inhabited

namespace Scene
@[inline] def push (s : Scene) (op : DrawOp) : Scene := { s with ops := s.ops.push op }
end Scene

end LeanPlot
