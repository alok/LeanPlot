import LeanPlot.Scene.DrawOp
import LeanPlot.Core.Num

/-
Geometry: unboxed small vectors, 4×4 matrices, rectangles and clipping.

* `Vec2`, `Vec3`, `Vec4`: structures of `Float` fields (stored unboxed inside
  the constructor object); arithmetic instances are provided in
  `namespace LeanPlot`, not at the root.
* `Mat4`: a 4×4 matrix with 16 unboxed fields, column-major like Makie's
  `Mat{4}` constructor; products follow StaticArrays' unrolled `muladd` chains.
* `Rect` (from `LeanPlot.Scene.DrawOp`, `x y w h`) operations, `Rect3` boxes.
* Clipping: Cohen–Sutherland outcodes, Liang–Barsky segment clipping,
  polyline clipping (NaN-separated, Makie's line-break convention) and
  Sutherland–Hodgman polygon clipping against a rectangle.
-/

namespace LeanPlot

open LeanPlot.Num

/-! ## Vectors -/

/-- A 2D vector or point. -/
structure Vec2 where
  x : Float
  y : Float
  deriving Repr, Inhabited, BEq

/-- A 3D vector or point. -/
structure Vec3 where
  x : Float
  y : Float
  z : Float
  deriving Repr, Inhabited, BEq

/-- A homogeneous 4-vector. -/
structure Vec4 where
  x : Float
  y : Float
  z : Float
  w : Float
  deriving Repr, Inhabited, BEq

namespace Vec2
/-- The zero vector. -/
def zero : Vec2 := ⟨0, 0⟩
/-- Sum. -/
@[inline] def add (a b : Vec2) : Vec2 := ⟨a.x + b.x, a.y + b.y⟩
/-- Difference. -/
@[inline] def sub (a b : Vec2) : Vec2 := ⟨a.x - b.x, a.y - b.y⟩
/-- Negation. -/
@[inline] def neg (a : Vec2) : Vec2 := ⟨-a.x, -a.y⟩
/-- Scaling. -/
@[inline] def smul (k : Float) (a : Vec2) : Vec2 := ⟨k * a.x, k * a.y⟩
/-- Dot product. -/
@[inline] def dot (a b : Vec2) : Float := a.x * b.x + a.y * b.y
/-- Scalar cross product `a.x b.y - a.y b.x`. -/
@[inline] def cross (a b : Vec2) : Float := a.x * b.y - a.y * b.x
/-- Squared length. -/
@[inline] def normSq (a : Vec2) : Float := a.x * a.x + a.y * a.y
/-- Euclidean length. -/
@[inline] def norm (a : Vec2) : Float := Float.sqrt a.normSq
/-- Unit vector (zero stays zero). -/
def normalize (a : Vec2) : Vec2 := let n := a.norm; if n == 0 then a else ⟨a.x / n, a.y / n⟩
/-- Counter-clockwise perpendicular `(-y, x)`. -/
@[inline] def perp (a : Vec2) : Vec2 := ⟨-a.y, a.x⟩
/-- Linear interpolation. -/
@[inline] def lerp (a b : Vec2) (t : Float) : Vec2 := ⟨Num.lerp a.x b.x t, Num.lerp a.y b.y t⟩
/-- Distance. -/
@[inline] def dist (a b : Vec2) : Float := (a.sub b).norm
/-- Rotate counter-clockwise by `θ` radians (in a y-up frame). -/
def rotate (a : Vec2) (θ : Float) : Vec2 :=
  let c := Float.cos θ
  let s := Float.sin θ
  ⟨c * a.x - s * a.y, s * a.x + c * a.y⟩
/-- Both coordinates finite. -/
@[inline] def isFinite (a : Vec2) : Bool := a.x.isFinite && a.y.isFinite
instance : Add Vec2 := ⟨add⟩
instance : Sub Vec2 := ⟨sub⟩
instance : Neg Vec2 := ⟨neg⟩
instance : HMul Float Vec2 Vec2 := ⟨smul⟩
end Vec2

namespace Vec3
/-- The zero vector. -/
def zero : Vec3 := ⟨0, 0, 0⟩
/-- Sum. -/
@[inline] def add (a b : Vec3) : Vec3 := ⟨a.x + b.x, a.y + b.y, a.z + b.z⟩
/-- Difference. -/
@[inline] def sub (a b : Vec3) : Vec3 := ⟨a.x - b.x, a.y - b.y, a.z - b.z⟩
/-- Negation. -/
@[inline] def neg (a : Vec3) : Vec3 := ⟨-a.x, -a.y, -a.z⟩
/-- Scaling. -/
@[inline] def smul (k : Float) (a : Vec3) : Vec3 := ⟨k * a.x, k * a.y, k * a.z⟩
/-- Componentwise product. -/
@[inline] def hmul (a b : Vec3) : Vec3 := ⟨a.x * b.x, a.y * b.y, a.z * b.z⟩
/-- Dot product. -/
@[inline] def dot (a b : Vec3) : Float := a.x * b.x + a.y * b.y + a.z * b.z
/-- Cross product (StaticArrays order of operations). -/
@[inline] def cross (a b : Vec3) : Vec3 :=
  ⟨a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x⟩
/-- Squared length. -/
@[inline] def normSq (a : Vec3) : Float := a.x * a.x + a.y * a.y + a.z * a.z
/-- Euclidean length. -/
@[inline] def norm (a : Vec3) : Float := Float.sqrt a.normSq
/-- Unit vector (zero stays zero). -/
def normalize (a : Vec3) : Vec3 :=
  let n := a.norm
  if n == 0 then a else ⟨a.x / n, a.y / n, a.z / n⟩
/-- Linear interpolation. -/
@[inline] def lerp (a b : Vec3) (t : Float) : Vec3 := ⟨Num.lerp a.x b.x t, Num.lerp a.y b.y t, Num.lerp a.z b.z t⟩
/-- All coordinates finite. -/
@[inline] def isFinite (a : Vec3) : Bool := a.x.isFinite && a.y.isFinite && a.z.isFinite
instance : Add Vec3 := ⟨add⟩
instance : Sub Vec3 := ⟨sub⟩
instance : Neg Vec3 := ⟨neg⟩
instance : HMul Float Vec3 Vec3 := ⟨smul⟩
end Vec3

/-! ## 4×4 matrices -/

/-- A 4×4 matrix; `mRC` is row `R`, column `C` (0-based). The anonymous
constructor takes the 16 entries **column-major**, like Makie's `Mat{4}(…)`. -/
structure Mat4 where
  m00 : Float
  m10 : Float
  m20 : Float
  m30 : Float
  m01 : Float
  m11 : Float
  m21 : Float
  m31 : Float
  m02 : Float
  m12 : Float
  m22 : Float
  m32 : Float
  m03 : Float
  m13 : Float
  m23 : Float
  m33 : Float
  deriving Repr, Inhabited, BEq

namespace Mat4

/-- Identity. -/
def identity : Mat4 := ⟨1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1⟩

/-- Entry at row `r`, column `c` (0-based; out of range gives 0). -/
def get (m : Mat4) (r c : Nat) : Float :=
  match r, c with
  | 0, 0 => m.m00 | 1, 0 => m.m10 | 2, 0 => m.m20 | 3, 0 => m.m30
  | 0, 1 => m.m01 | 1, 1 => m.m11 | 2, 1 => m.m21 | 3, 1 => m.m31
  | 0, 2 => m.m02 | 1, 2 => m.m12 | 2, 2 => m.m22 | 3, 2 => m.m32
  | 0, 3 => m.m03 | 1, 3 => m.m13 | 2, 3 => m.m23 | 3, 3 => m.m33
  | _, _ => 0

/-- The 16 entries in column-major order. -/
def toColumnMajor (m : Mat4) : Array Float :=
  #[m.m00, m.m10, m.m20, m.m30, m.m01, m.m11, m.m21, m.m31,
    m.m02, m.m12, m.m22, m.m32, m.m03, m.m13, m.m23, m.m33]

/-- Build from 16 column-major entries (missing entries are 0). -/
def ofColumnMajor (a : Array Float) : Mat4 :=
  let g (i : Nat) : Float := a.getD i 0
  ⟨g 0, g 1, g 2, g 3, g 4, g 5, g 6, g 7, g 8, g 9, g 10, g 11, g 12, g 13, g 14, g 15⟩

/-- One dot product `Σ aₖ bₖ` evaluated as StaticArrays does:
`muladd(a₄, b₄, muladd(a₃, b₃, muladd(a₂, b₂, a₁ b₁)))`. -/
@[inline] private def dot4 (a1 b1 a2 b2 a3 b3 a4 b4 : Float) : Float :=
  Float.fma a4 b4 (Float.fma a3 b3 (Float.fma a2 b2 (a1 * b1)))

/-- Matrix product `a * b`. -/
def mul (a b : Mat4) : Mat4 :=
  let e (r0 r1 r2 r3 c0 c1 c2 c3 : Float) := dot4 r0 c0 r1 c1 r2 c2 r3 c3
  ⟨e a.m00 a.m01 a.m02 a.m03 b.m00 b.m10 b.m20 b.m30, e a.m10 a.m11 a.m12 a.m13 b.m00 b.m10 b.m20 b.m30,
   e a.m20 a.m21 a.m22 a.m23 b.m00 b.m10 b.m20 b.m30, e a.m30 a.m31 a.m32 a.m33 b.m00 b.m10 b.m20 b.m30,
   e a.m00 a.m01 a.m02 a.m03 b.m01 b.m11 b.m21 b.m31, e a.m10 a.m11 a.m12 a.m13 b.m01 b.m11 b.m21 b.m31,
   e a.m20 a.m21 a.m22 a.m23 b.m01 b.m11 b.m21 b.m31, e a.m30 a.m31 a.m32 a.m33 b.m01 b.m11 b.m21 b.m31,
   e a.m00 a.m01 a.m02 a.m03 b.m02 b.m12 b.m22 b.m32, e a.m10 a.m11 a.m12 a.m13 b.m02 b.m12 b.m22 b.m32,
   e a.m20 a.m21 a.m22 a.m23 b.m02 b.m12 b.m22 b.m32, e a.m30 a.m31 a.m32 a.m33 b.m02 b.m12 b.m22 b.m32,
   e a.m00 a.m01 a.m02 a.m03 b.m03 b.m13 b.m23 b.m33, e a.m10 a.m11 a.m12 a.m13 b.m03 b.m13 b.m23 b.m33,
   e a.m20 a.m21 a.m22 a.m23 b.m03 b.m13 b.m23 b.m33, e a.m30 a.m31 a.m32 a.m33 b.m03 b.m13 b.m23 b.m33⟩

instance : Mul Mat4 := ⟨mul⟩

/-- Matrix–vector product. -/
def mulVec (m : Mat4) (v : Vec4) : Vec4 :=
  ⟨dot4 m.m00 v.x m.m01 v.y m.m02 v.z m.m03 v.w, dot4 m.m10 v.x m.m11 v.y m.m12 v.z m.m13 v.w,
   dot4 m.m20 v.x m.m21 v.y m.m22 v.z m.m23 v.w, dot4 m.m30 v.x m.m31 v.y m.m32 v.z m.m33 v.w⟩

/-- Transform a point (`w = 1`), returning the homogeneous result. -/
@[inline] def mulPoint (m : Mat4) (p : Vec3) : Vec4 := m.mulVec ⟨p.x, p.y, p.z, 1⟩

/-- Transpose. -/
def transpose (m : Mat4) : Mat4 :=
  ⟨m.m00, m.m01, m.m02, m.m03, m.m10, m.m11, m.m12, m.m13,
   m.m20, m.m21, m.m22, m.m23, m.m30, m.m31, m.m32, m.m33⟩

/-- Makie `scalematrix(s)`. -/
def scale (s : Vec3) : Mat4 := ⟨s.x, 0, 0, 0, 0, s.y, 0, 0, 0, 0, s.z, 0, 0, 0, 0, 1⟩

/-- Makie `translationmatrix(t)`. -/
def translation (t : Vec3) : Mat4 := ⟨1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, t.x, t.y, t.z, 1⟩

/-- Makie `transformationmatrix(translation, scale)` (scale, then translate). -/
def transformation (t s : Vec3) : Mat4 := ⟨s.x, 0, 0, 0, 0, s.y, 0, 0, 0, 0, s.z, 0, t.x, t.y, t.z, 1⟩

/-- Makie `frustum(left, right, bottom, top, znear, zfar)` (OpenGL convention). -/
def frustum (left right bottom top znear zfar : Float) : Mat4 :=
  if right == left || bottom == top || znear == zfar then identity else
  ⟨2 * znear / (right - left), 0, 0, 0,
   0, 2 * znear / (top - bottom), 0, 0,
   (right + left) / (right - left), (top + bottom) / (top - bottom), -(zfar + znear) / (zfar - znear), -1,
   0, 0, (-2 * znear * zfar) / (zfar - znear), 0⟩

/-- Makie `perspectiveprojection(fovy°, aspect, znear, zfar)`. -/
def perspective (fovy aspect znear zfar : Float) : Mat4 :=
  let h := Float.tan (fovy / 360.0 * Num.pi) * znear
  let w := h * aspect
  frustum (-w) w (-h) h znear zfar

/-- Makie `lookat(eye, target, up)`. -/
def lookat (eye target up : Vec3) : Mat4 :=
  let zaxis := (eye.sub target).normalize
  let xaxis := (up.cross zaxis).normalize
  let yaxis := (zaxis.cross xaxis).normalize
  let basis : Mat4 :=
    ⟨xaxis.x, yaxis.x, zaxis.x, 0, xaxis.y, yaxis.y, zaxis.y, 0, xaxis.z, yaxis.z, zaxis.z, 0, 0, 0, 0, 1⟩
  basis * translation eye.neg

end Mat4

/-! ## Rectangles -/

namespace Rect
/-- Left edge `x`. -/
@[inline] def left (r : Rect) : Float := r.x
/-- Right edge `x + w`. -/
@[inline] def right (r : Rect) : Float := r.x + r.w
/-- Top edge `y` (device space, y down). -/
@[inline] def top (r : Rect) : Float := r.y
/-- Bottom edge `y + h` (device space, y down). -/
@[inline] def bottom (r : Rect) : Float := r.y + r.h
/-- Centre. -/
def center (r : Rect) : Vec2 := ⟨r.x + r.w / 2, r.y + r.h / 2⟩
/-- From two corners (any order). -/
def ofCorners (x0 y0 x1 y1 : Float) : Rect :=
  ⟨min x0 x1, min y0 y1, (x1 - x0).abs, (y1 - y0).abs⟩
/-- No area (or negative/NaN size). -/
def isEmpty (r : Rect) : Bool := !(r.w > 0 && r.h > 0)
/-- Point inside (closed). -/
def contains (r : Rect) (px py : Float) : Bool :=
  r.x ≤ px && px ≤ r.x + r.w && r.y ≤ py && py ≤ r.y + r.h
/-- Smallest rectangle containing both. -/
def union (a b : Rect) : Rect :=
  ofCorners (min a.x b.x) (min a.y b.y) (max a.right b.right) (max a.bottom b.bottom)
/-- Intersection, or `none` when the rectangles do not overlap. -/
def intersect? (a b : Rect) : Option Rect :=
  let x0 := max a.x b.x
  let y0 := max a.y b.y
  let x1 := min a.right b.right
  let y1 := min a.bottom b.bottom
  if x0 ≤ x1 && y0 ≤ y1 then some ⟨x0, y0, x1 - x0, y1 - y0⟩ else none
/-- Grow by `d` on every side (shrink for negative `d`). -/
def inflate (r : Rect) (d : Float) : Rect := ⟨r.x - d, r.y - d, r.w + 2 * d, r.h + 2 * d⟩
/-- Shrink by per-side insets. -/
def inset (r : Rect) (left right top bottom : Float) : Rect :=
  ⟨r.x + left, r.y + top, r.w - left - right, r.h - top - bottom⟩
/-- Bounding box of a point cloud given as SoA (NaN/Inf skipped); `none` if
there is no finite point. -/
def bounds? (xs ys : FloatArray) : Option Rect :=
  let n := min xs.size ys.size
  let rec go (i : Nat) (found : Bool) (x0 y0 x1 y1 : Float) : Option Rect :=
    if i < n then
      let x := xs[i]!
      let y := ys[i]!
      if x.isFinite && y.isFinite then
        if found then go (i + 1) true (if x < x0 then x else x0) (if y < y0 then y else y0)
                         (if x > x1 then x else x1) (if y > y1 then y else y1)
        else go (i + 1) true x y x y
      else go (i + 1) found x0 y0 x1 y1
    else if found then some ⟨x0, y0, x1 - x0, y1 - y0⟩ else none
  termination_by n - i
  go 0 false 0 0 0 0
end Rect

/-- An axis-aligned 3D box, Makie `Rect3d` (origin plus widths). -/
structure Rect3 where
  origin : Vec3
  widths : Vec3
  deriving Repr, Inhabited, BEq

namespace Rect3
/-- Minimum corner. -/
def lo (r : Rect3) : Vec3 := r.origin
/-- Maximum corner. -/
def hi (r : Rect3) : Vec3 := r.origin.add r.widths
/-- Box from min/max corners. -/
def ofBounds (lo hi : Vec3) : Rect3 := ⟨lo, hi.sub lo⟩
/-- The 8 corners, x varying fastest. -/
def corners (r : Rect3) : Array Vec3 :=
  let o := r.origin
  let w := r.widths
  #[⟨o.x, o.y, o.z⟩, ⟨o.x + w.x, o.y, o.z⟩, ⟨o.x, o.y + w.y, o.z⟩, ⟨o.x + w.x, o.y + w.y, o.z⟩,
    ⟨o.x, o.y, o.z + w.z⟩, ⟨o.x + w.x, o.y, o.z + w.z⟩, ⟨o.x, o.y + w.y, o.z + w.z⟩,
    ⟨o.x + w.x, o.y + w.y, o.z + w.z⟩]
end Rect3

/-! ## Clipping -/

namespace Clip

/-- Cohen–Sutherland outcode of a point against `r`: bit 0 left, 1 right,
2 top (y < r.y), 3 bottom (y > r.y + r.h). 0 means inside. -/
@[inline] def outcode (r : Rect) (x y : Float) : UInt8 :=
  (if x < r.x then 1 else 0) ||| (if x > r.x + r.w then 2 else 0) |||
    (if y < r.y then 4 else 0) ||| (if y > r.y + r.h then 8 else 0)

/-- Trivial rejection: both endpoints on the same outside side. -/
@[inline] def triviallyOutside (r : Rect) (x0 y0 x1 y1 : Float) : Bool :=
  (outcode r x0 y0 &&& outcode r x1 y1) != 0

/-- Liang–Barsky: the parameter interval `[t0, t1] ⊆ [0, 1]` of the segment
`(x0,y0)→(x1,y1)` inside `r`, or `none` if it misses. Degenerate (point)
segments are kept when the point is inside. -/
def segmentParams (r : Rect) (x0 y0 x1 y1 : Float) : Option (Float × Float) :=
  let dx := x1 - x0
  let dy := y1 - y0
  -- each boundary: p·t ≤ q
  let step (acc : Option (Float × Float)) (p q : Float) : Option (Float × Float) :=
    match acc with
    | none => none
    | some (t0, t1) =>
      if p == 0 then (if q < 0 then none else some (t0, t1))
      else
        let t := q / p
        if p < 0 then (if t > t1 then none else some (max t0 t, t1))
        else (if t < t0 then none else some (t0, min t1 t))
  let acc := some (0.0, 1.0)
  let acc := step acc (-dx) (x0 - r.x)
  let acc := step acc dx (r.x + r.w - x0)
  let acc := step acc (-dy) (y0 - r.y)
  step acc dy (r.y + r.h - y0)

/-- Liang–Barsky segment clipping: the clipped endpoints, or `none`. -/
def segment (r : Rect) (x0 y0 x1 y1 : Float) : Option (Vec2 × Vec2) :=
  if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite) then none else
  (segmentParams r x0 y0 x1 y1).map fun (t0, t1) =>
    (⟨x0 + t0 * (x1 - x0), y0 + t0 * (y1 - y0)⟩, ⟨x0 + t1 * (x1 - x0), y0 + t1 * (y1 - y0)⟩)

/-- State of `polyline`: output SoA, whether the last emitted piece is still
open, and the last emitted point. -/
structure PolyState where
  ox : FloatArray
  oy : FloatArray
  isOpen : Bool
  lx : Float
  ly : Float

/-- Process the segment `(x0,y0)→(x1,y1)` of a polyline being clipped. -/
def polylineStep (r : Rect) (st : PolyState) (x0 y0 x1 y1 : Float) : PolyState :=
  match segment r x0 y0 x1 y1 with
  | none => { st with isOpen := false }
  | some (a, b) =>
    let continues := st.isOpen && a.x == st.lx && a.y == st.ly
    let (ox, oy) :=
      if continues then (st.ox, st.oy)
      else
        let (ox, oy) := if st.ox.size > 0 then (st.ox.push nan, st.oy.push nan) else (st.ox, st.oy)
        (ox.push a.x, oy.push a.y)
    -- a piece stays open only if the segment ended inside the rectangle
    ⟨ox.push b.x, oy.push b.y, b.x == x1 && b.y == y1, b.x, b.y⟩

/-- Clip a polyline (SoA) to `r`. The result is one SoA polyline in which the
visible pieces are separated by a `(NaN, NaN)` point (Makie's line-break
convention); non-finite input points also break the line. No break markers
are emitted at the ends. -/
def polyline (r : Rect) (xs ys : FloatArray) : FloatArray × FloatArray :=
  let n := min xs.size ys.size
  if n == 1 then
    let x := xs[0]!
    let y := ys[0]!
    if x.isFinite && y.isFinite && r.contains x y then (FloatArray.mk #[x], FloatArray.mk #[y]) else (.empty, .empty)
  else
    let rec go (i : Nat) (st : PolyState) : PolyState :=
      if i + 1 < n then go (i + 1) (polylineStep r st xs[i]! ys[i]! xs[i + 1]! ys[i + 1]!) else st
    termination_by n - i
    let st := go 0 ⟨.empty, .empty, false, 0, 0⟩
    (st.ox, st.oy)

/-- Sutherland–Hodgman clipping of a closed polygon (SoA, implicit closing
edge) against `r`. Returns the clipped polygon (possibly empty). -/
def polygon (r : Rect) (xs ys : FloatArray) : FloatArray × FloatArray :=
  let n := min xs.size ys.size
  let pts : Array Vec2 := (Array.range n).map fun i => ⟨xs[i]!, ys[i]!⟩
  -- clip against one half-plane `inside p` with boundary intersection `cut a b`
  let clipEdge (poly : Array Vec2) (inside : Vec2 → Bool) (cut : Vec2 → Vec2 → Vec2) : Array Vec2 := Id.run do
    if poly.isEmpty then return poly
    let mut out : Array Vec2 := #[]
    let mut prev := poly[poly.size - 1]!
    for cur in poly do
      if inside cur then
        if !inside prev then out := out.push (cut prev cur)
        out := out.push cur
      else if inside prev then
        out := out.push (cut prev cur)
      prev := cur
    return out
  let atX (x : Float) (a b : Vec2) : Vec2 := ⟨x, a.y + (b.y - a.y) * (x - a.x) / (b.x - a.x)⟩
  let atY (y : Float) (a b : Vec2) : Vec2 := ⟨a.x + (b.x - a.x) * (y - a.y) / (b.y - a.y), y⟩
  let x0 := r.x
  let x1 := r.x + r.w
  let y0 := r.y
  let y1 := r.y + r.h
  let p := clipEdge pts (fun q => q.x ≥ x0) (atX x0)
  let p := clipEdge p (fun q => q.x ≤ x1) (atX x1)
  let p := clipEdge p (fun q => q.y ≥ y0) (atY y0)
  let p := clipEdge p (fun q => q.y ≤ y1) (atY y1)
  (⟨p.map (·.x)⟩, ⟨p.map (·.y)⟩)

end Clip

end LeanPlot
