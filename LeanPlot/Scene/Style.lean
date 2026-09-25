import LeanPlot.Scene.DrawOp
import LeanPlot.Core.Color
import LeanPlot.Core.Colormap
import LeanPlot.Core.Data

/-!
# Plot styles

The data-space styling vocabulary of marks (`LeanPlot.Mark`), with Makie's defaults:

* `Pos`: 2D (`Pts2`) or 3D (`Pts3`) positions;
* `LineStyle`: Makie's `:solid/:dash/:dot/:dashdot/:dashdotdot` patterns (in units of the
  line width, `line_diff_pattern` with `:normal` gaps) or a custom on/off pattern;
* `MarkerShape`: the default marker map (`Makie.DEFAULT_MARKER_MAP`), unit-size outlines
  scaled by `markersize` exactly as CairoMakie draws them;
* `ColorMapping` / `ColorSpec`: a solid colour, one colour per element, or numbers mapped
  through a colormap (`colorrange`, `colorscale`, `lowclip`/`highclip`, `nan_color`,
  `alpha`), resolved with `Makie.numbers_to_colors` semantics;
* `LineSpec`, `MarkerSpec`, `PolySpec`, `TextSpec`, `ArrowSpec`: per-mark styles.
-/

namespace LeanPlot

open LeanPlot.Num

/-! ## Positions -/

/-- 2D or 3D positions. -/
inductive Pos where
  | xy (p : Pts2)
  | xyz (p : Pts3)
  deriving Inhabited

namespace Pos

/-- Number of points. -/
def size : Pos → Nat
  | xy p => p.size
  | xyz p => p.size

/-- x coordinates. -/
def xs : Pos → FloatArray
  | xy p => p.xs
  | xyz p => p.xs

/-- y coordinates. -/
def ys : Pos → FloatArray
  | xy p => p.ys
  | xyz p => p.ys

/-- z coordinates (`none` for 2D positions). -/
def zs? : Pos → Option FloatArray
  | xy _ => none
  | xyz p => some p.zs

/-- `true` for 3D positions. -/
def is3D : Pos → Bool
  | xy _ => false
  | xyz _ => true

/-- Point `i` (z = 0 for 2D positions, NaN out of range). -/
def get3 (p : Pos) (i : Nat) : Vec3 :=
  match p with
  | xy q => let v := q.get! i; ⟨v.x, v.y, 0⟩
  | xyz q => q.get! i

/-- The xy projection. -/
def toPts2 : Pos → Pts2
  | xy p => p
  | xyz p => p.xy

/-- 3D positions (z = 0 for 2D input). -/
def toPts3 : Pos → Pts3
  | xy p => Pts3.ofArrays p.xs p.ys ⟨Array.replicate p.size 0⟩
  | xyz p => p

/-- Finite bounding box (`Rect3`, z extent zero for 2D positions). -/
def bounds? : Pos → Option Rect3
  | xy p => p.bounds?.map fun r => ⟨⟨r.x, r.y, 0⟩, ⟨r.w, r.h, 0⟩⟩
  | xyz p => p.bounds?

/-- The empty 2D position set. -/
def empty : Pos := .xy Pts2.empty

end Pos

/-- 2D points are positions. -/
instance : Coe Pts2 Pos := ⟨Pos.xy⟩
/-- 3D points are positions. -/
instance : Coe Pts3 Pos := ⟨Pos.xyz⟩

/-! ## Line styles -/

/-- Makie line styles. Patterns are in units of the line width. -/
inductive LineStyle where
  | solid
  /-- `:dash`: dash 3, gap 3. -/
  | dash
  /-- `:dot`: dot 1, gap 2. -/
  | dot
  /-- `:dashdot`: 3, 3, 1, 3. -/
  | dashdot
  /-- `:dashdotdot`: 3, 3, 1, 2, 1, 3. -/
  | dashdotdot
  /-- Custom on/off lengths in units of the line width (Cairo convention, not cumulative). -/
  | pattern (onOff : Array Float)
  deriving Repr, Inhabited, BEq

namespace LineStyle

/-- The on/off pattern in units of the line width (empty for solid lines). -/
def unitPattern : LineStyle → Array Float
  | solid => #[]
  | dash => #[3, 3]
  | dot => #[1, 2]
  | dashdot => #[3, 3, 1, 3]
  | dashdotdot => #[3, 3, 1, 2, 1, 3]
  | pattern p => p

/-- The Cairo dash array in pixels for a stroke of width `w` (`diff(linestyle) .* w`, padded
to an even length). -/
def dashArray (s : LineStyle) (w : Float) : Array Float :=
  let p := s.unitPattern.map (· * w)
  if p.size % 2 == 1 then p.push 0 else p

/-- Parse Makie's names (`"solid"`, `"dash"`, `"dot"`, `"dashdot"`, `"dashdotdot"`, or a
string of `-` and `.` as in `linestyle = "-.."`). -/
def ofString? (s : String) : Option LineStyle :=
  match s with
  | "solid" => some solid | "dash" => some dash | "dot" => some dot
  | "dashdot" => some dashdot | "dashdotdot" => some dashdotdot
  | _ =>
    let cs := s.toList
    if cs.isEmpty || !cs.all (fun c => c == '-' || c == '.') then none else
    let n := cs.length
    let arr := cs.toArray
    let pat := (List.range n).foldl (init := (#[] : Array Float)) fun acc i =>
      let c := arr[i]!
      let next := arr[(i + 1) % n]!
      let acc := acc.push (if c == '-' then 3 else 1)
      acc.push (if c == '.' && next == '.' then 2 else 3)
    some (pattern pat)

end LineStyle

/-! ## Markers -/

/-- Makie's built-in marker shapes (`Makie.DEFAULT_MARKER_MAP`). -/
inductive MarkerShape where
  | circle | rect | diamond | hexagon | octagon | pentagon
  | utriangle | dtriangle | ltriangle | rtriangle
  | cross | xcross | star4 | star5 | star6 | star8 | hline | vline
  deriving Repr, Inhabited, BEq, DecidableEq

namespace MarkerShape

/-- Makie symbol name. -/
def name : MarkerShape → String
  | circle => "circle" | rect => "rect" | diamond => "diamond" | hexagon => "hexagon"
  | octagon => "octagon" | pentagon => "pentagon" | utriangle => "utriangle"
  | dtriangle => "dtriangle" | ltriangle => "ltriangle" | rtriangle => "rtriangle"
  | cross => "cross" | xcross => "xcross" | star4 => "star4" | star5 => "star5"
  | star6 => "star6" | star8 => "star8" | hline => "hline" | vline => "vline"

/-- All shapes in Makie's palette order first (`:circle, :utriangle, :cross, :rect,
:diamond, :dtriangle, :pentagon, :xcross`), then the rest. -/
def all : Array MarkerShape :=
  #[circle, utriangle, cross, rect, diamond, dtriangle, pentagon, xcross,
    ltriangle, rtriangle, hexagon, octagon, star4, star5, star6, star8, hline, vline]

/-- Makie's default marker palette (`palette.marker`). -/
def palette : Array MarkerShape := #[circle, utriangle, cross, rect, diamond, dtriangle, pentagon, xcross]

/-- Parse a Makie marker symbol (`"circle"`, `"rect"`, `"+"`, `"x"`, …). -/
def ofString? (s : String) : Option MarkerShape :=
  match s with
  | "+" => some cross
  | "x" => some xcross
  | s => all.find? (·.name == s)

/-- Polygon vertices of the unit marker (y up, centred at the origin), as a flat
`x₀ y₀ x₁ y₁ …` list; the circle is handled separately (radius `circleRadius`). These are
the exact `DEFAULT_MARKER_MAP` coordinates of Makie 0.24. -/
def unitPolygon : MarkerShape → Array Float
  | circle => #[]
  | rect => #[0.315718342192545, -0.315718342192545, 0.315718342192545, 0.315718342192545,
              -0.315718342192545, 0.315718342192545, -0.315718342192545, -0.315718342192545]
  | diamond => #[0.4464931614186469, 0, 0, 0.4464931614186469, -0.4464931614186469, 0,
                 0, -0.4464931614186469]
  | hexagon => #[0, 0.375, -0.32475952059030533, 0.1875, -0.32475952059030533, -0.1875,
                 0, -0.375, 0.32475952059030533, -0.1875, 0.32475952059030533, 0.1875]
  | octagon => #[0, 0.375, -0.2651650384068489, 0.2651650384068489, -0.375, 0,
                 -0.2651650384068489, -0.2651650384068489, 0, -0.375,
                 0.2651650384068489, -0.2651650384068489, 0.375, 0, 0.2651650384068489, 0.2651650384068489]
  | pentagon => #[0, 0.375, -0.35664620250463486, 0.11588137596845627,
                  -0.22041946649551392, -0.30338137596845627, 0.22041946649551392, -0.30338137596845627,
                  0.35664620250463486, 0.11588137596845627]
  | utriangle => #[0, 0.485, -0.36375, -0.24250000000000002, 0.36375, -0.24250000000000002]
  | dtriangle => #[0, -0.485, 0.36375, 0.24250000000000002, -0.36375, 0.24250000000000002]
  | ltriangle => #[-0.485, 0, 0.2425, -0.36375, 0.24250000000000005, 0.36375]
  | rtriangle => #[0.485, 0, -0.24249999999999994, 0.36375, -0.2425000000000001, -0.36374999999999996]
  | cross => #[0.1245, 0.375, 0.1245, 0.1245, 0.375, 0.1245, 0.375, -0.1245, 0.1245, -0.1245,
               0.1245, -0.375, -0.1245, -0.375, -0.1245, -0.1245, -0.375, -0.1245, -0.375, 0.1245,
               -0.1245, 0.1245, -0.1245, 0.375]
  | xcross => #[-0.1771302486872301, 0.35319983720268056, 0, 0.17606958851545035,
                0.17713024868723018, 0.3531998372026805, 0.3531998372026805, 0.17713024868723012,
                0.17606958851545035, 0, 0.3531998372026805, -0.17713024868723015,
                0.17713024868723015, -0.3531998372026805, 0, -0.17606958851545035,
                -0.17713024868723015, -0.3531998372026805, -0.35319983720268044, -0.17713024868723018,
                -0.17606958851545035, 0, -0.3531998372026805, 0.1771302486872301]
  | star4 => #[0, 0.45, -0.13258251920342445, 0.13258251920342445, -0.45, 0,
               -0.13258251920342445, -0.13258251920342445, 0, -0.45,
               0.13258251920342445, -0.13258251920342445, 0.45, 0, 0.13258251920342445, 0.13258251920342445]
  | star5 => #[0, 0.45, -0.12343490123748782, 0.16989357054233553, -0.4279754430055618, 0.13905765116214752,
               -0.19972187340259556, -0.06489357054233552, -0.2645033597946167, -0.3640576511621475,
               0, -0.21000000000000002, 0.2645033597946167, -0.3640576511621475,
               0.19972187340259556, -0.06489357054233552, 0.4279754430055618, 0.13905765116214752,
               0.12343490123748782, 0.16989357054233553]
  | star6 => #[0, 0.45, -0.1125, 0.1948557123541832, -0.3897114247083664, 0.225, -0.225, 0,
               -0.3897114247083664, -0.225, -0.1125, -0.1948557123541832, 0, -0.45,
               0.1125, -0.1948557123541832, 0.3897114247083664, -0.225, 0.225, 0,
               0.3897114247083664, 0.225, 0.1125, 0.1948557123541832]
  | star8 => #[0, 0.45, -0.09471414797008038, 0.2286601772904396, -0.31819804608821867, 0.31819804608821867,
               -0.2286601772904396, 0.09471414797008038, -0.45, 0, -0.2286601772904396, -0.09471414797008038,
               -0.31819804608821867, -0.31819804608821867, -0.09471414797008038, -0.2286601772904396,
               0, -0.45, 0.09471414797008038, -0.2286601772904396, 0.31819804608821867, -0.31819804608821867,
               0.2286601772904396, -0.09471414797008038, 0.45, 0, 0.2286601772904396, 0.09471414797008038,
               0.31819804608821867, 0.31819804608821867, 0.09471414797008038, 0.2286601772904396]
  | hline => #[0.315718342192545, -0.063143668438509, 0.315718342192545, 0.063143668438509,
               -0.315718342192545, 0.063143668438509, -0.315718342192545, -0.063143668438509]
  | vline => #[0.063143668438509, -0.315718342192545, 0.063143668438509, 0.315718342192545,
               -0.063143668438509, 0.315718342192545, -0.063143668438509, -0.315718342192545]

/-- Radius of Makie's unit `:circle` marker (`EllipticalArc` of radius 0.3525). -/
def circleRadius : Float := 0.3525

/-- Cubic-Bézier handle factor for a quarter circle, `4/3 (√2 - 1)`. -/
def kappa : Float := 0.5522847498307936

/-- Append the marker outline of size `s` pixels centred at device point `(cx, cy)`
(y down) to a path. -/
def appendPath (m : MarkerShape) (s cx cy : Float) (p : Path) : Path :=
  match m with
  | circle =>
    let r := s * circleRadius
    let k := r * kappa
    -- four cubic arcs, counter-clockwise on screen from the right-most point
    (((((p.moveTo (cx + r) cy).cubicTo (cx + r) (cy - k) (cx + k) (cy - r) cx (cy - r)).cubicTo
      (cx - k) (cy - r) (cx - r) (cy - k) (cx - r) cy).cubicTo
      (cx - r) (cy + k) (cx - k) (cy + r) cx (cy + r)).cubicTo
      (cx + k) (cy + r) (cx + r) (cy + k) (cx + r) cy).close
  | m =>
    let v := m.unitPolygon
    let n := v.size / 2
    let rec go (i : Nat) (p : Path) : Path :=
      if i < n then
        let x := cx + s * v[2 * i]!
        let y := cy - s * v[2 * i + 1]!
        go (i + 1) (if i == 0 then p.moveTo x y else p.lineTo x y)
      else p.close
    termination_by n - i
    go 0 p

/-- Append the marker outline rotated by `θ` radians (counter-clockwise on screen, Makie's
`rotation`). The circle is rotation invariant. -/
def appendPathRotated (m : MarkerShape) (s θ cx cy : Float) (p : Path) : Path :=
  if θ == 0 || m == circle then m.appendPath s cx cy p else
  let c := Float.cos θ
  let sn := Float.sin θ
  let v := m.unitPolygon
  let n := v.size / 2
  let rec go (i : Nat) (p : Path) : Path :=
    if i < n then
      let u := v[2 * i]!
      let w := v[2 * i + 1]!
      -- rotate in the y-up marker frame, then flip into device space
      let x := cx + s * (u * c - w * sn)
      let y := cy - s * (u * sn + w * c)
      go (i + 1) (if i == 0 then p.moveTo x y else p.lineTo x y)
    else p.close
  termination_by n - i
  go 0 p

/-- The marker outline as a path (see `appendPath`). -/
@[inline] def toPath (m : MarkerShape) (s cx cy : Float) : Path := m.appendPath s cx cy {}

end MarkerShape

/-! ## Colours -/

/-- How numbers become colours (Makie's `ColorMapping`): colormap, colour range (automatic
when `none`: the finite extrema of the values, `distinct_extrema_nan`), colour scale,
clip colours, the NaN colour and a global alpha. -/
structure ColorMapping where
  colormap : Colormap := Colormap.viridis
  colorrange : Option (Float × Float) := none
  colorscale : Scale := .identity
  lowclip : Option RGBA := none
  highclip : Option RGBA := none
  nanColor : RGBA := RGBA.transparent
  alpha : Float := 1
  deriving Inhabited

namespace ColorMapping

/-- The colour range used for the values `vs` (automatic range when unset). -/
def rangeFor (m : ColorMapping) (vs : FloatArray) : Float × Float :=
  match m.colorrange with
  | some r => r
  | none => Colormap.autoRange vs

/-- `numbers_to_colors` options of this mapping. -/
def options (m : ColorMapping) : Colormap.MapOptions :=
  { scale := m.colorscale, lowclip := m.lowclip, highclip := m.highclip, nanColor := m.nanColor }

/-- Colormap with the mapping's alpha applied. -/
def effectiveColormap (m : ColorMapping) : Colormap :=
  if m.alpha == 1 then m.colormap else m.colormap.withAlpha m.alpha

/-- Map values to RGBA8 bytes (4 per value) over the range `(lo, hi)`. -/
def toRGBA8 (m : ColorMapping) (lo hi : Float) (vs : FloatArray) : ByteArray :=
  m.effectiveColormap.mapToRGBA8 lo hi m.options vs

/-- Map one value over the range `(lo, hi)`. -/
def mapValue (m : ColorMapping) (lo hi : Float) (v : Float) : RGBA :=
  m.effectiveColormap.mapValue lo hi m.options v

end ColorMapping

/-- The colour of a mark: one colour, one colour per element (RGBA8, 4 bytes each), or
one number per element mapped through a colormap. -/
inductive ColorSpec where
  | solid (c : RGBA)
  | perElement (rgba : ByteArray)
  | values (vs : FloatArray) (mapping : ColorMapping)
  deriving Inhabited

namespace ColorSpec

/-- Black. -/
def black : ColorSpec := .solid RGBA.black

/-- `true` for numbers mapped through a colormap. -/
def isMapped : ColorSpec → Bool
  | values .. => true
  | _ => false

/-- The colour range a mapped colour spec uses (`none` for plain colours). -/
def colorRange? : ColorSpec → Option (Float × Float)
  | values vs m => some (m.rangeFor vs)
  | _ => none

/-- The colour mapping, if any. -/
def mapping? : ColorSpec → Option ColorMapping
  | values _ m => some m
  | _ => none

/-- Resolve to RGBA8 bytes for `n` elements: solid colours are repeated, per-element
buffers and mapped values are used as given (missing entries become transparent). -/
def resolve (c : ColorSpec) (n : Nat) : ByteArray :=
  match c with
  | solid col =>
    let r := RGBA.to8 col.r
    let g := RGBA.to8 col.g
    let b := RGBA.to8 col.b
    let a := RGBA.to8 col.a
    let rec rep (i : Nat) (acc : ByteArray) : ByteArray :=
      if i < n then rep (i + 1) ((((acc.push r).push g).push b).push a) else acc
    termination_by n - i
    rep 0 (ByteArray.emptyWithCapacity (4 * n))
  | perElement rgba => pad rgba
  | values vs m =>
    let (lo, hi) := m.rangeFor vs
    pad (m.toRGBA8 lo hi vs)
where
  /-- Truncate or pad with transparent black to exactly `4 n` bytes. -/
  pad (b : ByteArray) : ByteArray :=
    if b.size == 4 * n then b
    else if b.size > 4 * n then b.extract 0 (4 * n)
    else
      let rec fill (k : Nat) (acc : ByteArray) : ByteArray :=
        if k < 4 * n then fill (k + 1) (acc.push 0) else acc
      termination_by 4 * n - k
      fill b.size b

/-- Element `i` as a colour (solid colours ignore `i`). -/
def get (c : ColorSpec) (i : Nat) : RGBA :=
  match c with
  | solid col => col
  | perElement rgba => RGBA.ofRGBA8At rgba i
  | values vs m =>
    let (lo, hi) := m.rangeFor vs
    m.mapValue lo hi (vs.get! i)

/-- A single representative colour (for legends): the solid colour, or the first element. -/
def representative (c : ColorSpec) : RGBA :=
  match c with
  | solid col => col
  | _ => c.get 0

end ColorSpec

/-! ## Mark styles -/

/-- Stroke style of `lines` and `linesegments`. Makie defaults: width 1.5, butt caps, miter
joins with Cairo miter limit 2 (`miter_limit = π/3`). -/
structure LineSpec where
  color : ColorSpec := .black
  width : Float := 1.5
  style : LineStyle := .solid
  cap : LineCap := .butt
  join : LineJoin := .miter
  miterLimit : Float := 2.0
  deriving Inhabited

/-- Scatter style. Makie defaults: `:circle`, markersize 9, no stroke. `sizes` (one per
point) overrides `size`. -/
structure MarkerSpec where
  shape : MarkerShape := .circle
  size : Float := 9
  sizes : Option FloatArray := none
  color : ColorSpec := .black
  strokeColor : RGBA := RGBA.black
  strokeWidth : Float := 0
  /-- Per-marker rotation in radians (counter-clockwise on screen), `none` for upright. -/
  rotations : Option FloatArray := none
  /-- Rotate each marker to point along the *projected* direction `dirs[i]` plus `offset`
  radians (Makie's `register_projected_rotations_2d!`; streamplot arrowheads use
  `:utriangle` with `offset = -π/2`). Overrides `rotations`. -/
  alongDirections : Option (Pos × Float) := none
  deriving Inhabited

namespace MarkerSpec
/-- Marker size of point `i`. -/
@[inline] def sizeAt (m : MarkerSpec) (i : Nat) : Float :=
  match m.sizes with
  | some s => s.get! i
  | none => m.size
/-- Rotation of marker `i`. -/
@[inline] def rotationAt (m : MarkerSpec) (i : Nat) : Float :=
  match m.rotations with
  | some r => r.get! i
  | none => 0
end MarkerSpec

/-- Polygon style: fill colour (per polygon for `perElement`/`values`) and outline. -/
structure PolySpec where
  color : ColorSpec := .solid (RGBA.rgb 0.4 0.4 0.4)
  strokeColor : RGBA := RGBA.black
  strokeWidth : Float := 0
  strokeStyle : LineStyle := .solid
  deriving Inhabited

/-- Text mark style. Makie defaults: fontsize 14, black, `align = (:left, :bottom)`. The
`offset` is in pixels (x right, y up, like Makie's `offset`). -/
structure TextSpec where
  size : Float := 14
  color : RGBA := RGBA.black
  halign : HAlign := .left
  valign : VAlign := .bottom
  rotation : Float := 0
  bold : Bool := false
  offset : Float × Float := (0, 0)
  deriving Inhabited

namespace TextSpec
/-- The device `TextStyle` of this spec. -/
def toStyle (t : TextSpec) : TextStyle :=
  { size := t.size, color := t.color, halign := t.halign, valign := t.valign
    rotation := t.rotation, bold := t.bold }
end TextSpec

/-- The labels of a labelled contour plot (Makie `contour(...; labels = true)`,
`basic_recipes/contours.jl:292-386`): one label per traced line, anchored at the line's middle
vertex (`label_info`), rotated along the line in screen space and kept upright, centred, in its
line's colour. `dir` holds the data-space directions `p₃ - p₁` of the vertices around the anchor. -/
structure ContourLabels where
  /-- Label anchors (Makie `text_positions`). -/
  pos : Pos
  /-- Line directions at the anchors (`p₃ - p₁`). -/
  dir : Pos
  /-- Label texts (`labelformatter` of the line levels). -/
  strings : Array String
  /-- One colour per label (`labelcolor`, else the level colour). -/
  colors : ColorSpec
  /-- Makie `labelsize` (10). -/
  size : Float := 10
  /-- Makie `labelfont = :bold`. -/
  bold : Bool := false
  deriving Inhabited

/-- A volume drawn by ray casting (Makie `volume`, see `Recipes.Algo.Volume`): `ray front back
light` is the straight-alpha colour of the ray segment from `front` to `back` through the unit
cube of the volume's box (texture coordinates, `front` nearest to the eye), lit from the
world-space direction `light`. `mapping` feeds colorbars. -/
structure VolumeSpec where
  ray : Vec3 → Vec3 → Vec3 → RGBA
  mapping : Option (ColorMapping × Float × Float) := none

instance : Inhabited VolumeSpec := ⟨{ ray := fun _ _ _ => RGBA.transparent }⟩

/-- Arrow style (Makie `arrows2d`; pixel-space metrics, `markerspace = :pixel`).
`align` is `0` (tail at the point), `0.5` (centre) or `1` (tip at the point). -/
structure ArrowSpec where
  color : ColorSpec := .black
  shaftwidth : Float := 3
  tipwidth : Float := 14
  tiplength : Float := 8
  tailwidth : Float := 14
  taillength : Float := 0
  minshaftlength : Float := 10
  maxshaftlength : Float := Num.inf
  strokemask : Float := 0.75
  align : Float := 0
  lengthscale : Float := 1
  normalize : Bool := false
  deriving Inhabited

end LeanPlot
