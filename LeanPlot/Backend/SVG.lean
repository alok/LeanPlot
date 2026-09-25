import LeanPlot.Scene.DrawOp
import LeanPlot.Core.Format
import LeanPlot.Core.Color

/-
SVG backend: `Scene → String`.

The output is deterministic (byte-stable across runs and platforms):

* numbers go through `LeanPlot.Num.fmtFixed` (≤ `precision` decimals, trailing
  zeros trimmed, `-0 ↦ 0`, non-finite ↦ `0`);
* colours are `#rrggbb` plus `fill-opacity`/`stroke-opacity` when alpha < 1;
* every distinct clip rectangle becomes one `<clipPath id="cN">` in `<defs>`,
  numbered in order of first use; consecutive ops with the same clip share a
  `<g clip-path="url(#cN)">`;
* `path` ops become `<path>`; non-finite coordinates lift the pen (Makie's
  NaN line breaks);
* `segments` ops become one `<path>` per run of consecutive equal colours;
* `triangles` ops are flat-shaded with the average vertex colour (runs of equal
  colours merged), or approximated with one `linearGradient` per triangle;
* `image` ops become `<image … xlink:href="data:image/png;base64,…">` using the
  caller-supplied `encodePNG w h rgba` (the PNG encoder lives in the raster
  backend); without an encoder, small images fall back to one `<rect>` per pixel;
* `text` ops go through the `svgText` hook (default: an SVG `<text>` element;
  the glyph-outline renderer replaces it).
-/

namespace LeanPlot.Backend.SVG

open LeanPlot LeanPlot.Num

/-- How `DrawOp.triangles` (Gouraud) is approximated in SVG. -/
inductive TriangleMode where
  /-- One flat colour per triangle: the average of its vertex colours. -/
  | flat
  /-- A two-stop `linearGradient` per triangle along the gradient of the
  (affine) colour field; exact when all channels vary along one direction. -/
  | gradient
  deriving Repr, Inhabited, BEq, DecidableEq

/-- XML-escape text or an attribute value. -/
def escape (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    match c with
    | '&' => acc ++ "&amp;"
    | '<' => acc ++ "&lt;"
    | '>' => acc ++ "&gt;"
    | '"' => acc ++ "&quot;"
    | '\'' => acc ++ "&apos;"
    | c => acc.push c

/-- Standard base64 (RFC 4648, with padding). -/
def base64 (bytes : ByteArray) : String :=
  let tbl := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toList.toArray
  let enc (v : Nat) : Char := tbl[v % 64]!
  let n := bytes.size
  let rec go (i : Nat) (acc : String) : String :=
    if i + 2 < n then
      let v := bytes[i]!.toNat * 65536 + bytes[i + 1]!.toNat * 256 + bytes[i + 2]!.toNat
      go (i + 3) ((((acc.push (enc (v / 262144))).push (enc (v / 4096))).push (enc (v / 64))).push (enc v))
    else if i + 2 == n then
      let v := bytes[i]!.toNat * 65536 + bytes[i + 1]!.toNat * 256
      (((acc.push (enc (v / 262144))).push (enc (v / 4096))).push (enc (v / 64))).push '='
    else if i + 1 == n then
      let v := bytes[i]!.toNat * 65536
      (((acc.push (enc (v / 262144))).push (enc (v / 4096))).push '=').push '='
    else acc
  termination_by n - i
  go 0 ""

/-- Default text renderer: an SVG `<text>` element anchored at `(x, y)`.
Rotation (`TextStyle.rotation`, counter-clockwise on screen) becomes
`rotate(-deg, x, y)`. -/
def defaultText (fmt : Float → String) (st : TextStyle) (s : String) (x y : Float) : String :=
  let anchor := match st.halign with
    | .left => "" | .center => " text-anchor=\"middle\"" | .right => " text-anchor=\"end\""
  let baseline := match st.valign with
    | .baseline => "" | .top => " dominant-baseline=\"text-before-edge\""
    | .middle => " dominant-baseline=\"central\"" | .bottom => " dominant-baseline=\"text-after-edge\""
  let rot := if st.rotation == 0 then "" else
    s!" transform=\"rotate({fmt (-st.rotation * 180 / Num.pi)} {fmt x} {fmt y})\""
  let opacity := if st.color.a < 1 then s!" fill-opacity=\"{fmt st.color.a}\"" else ""
  let weight := if st.bold then " font-weight=\"bold\"" else ""
  s!"<text x=\"{fmt x}\" y=\"{fmt y}\" font-family=\"{escape st.family}\" font-size=\"{fmt st.size}\"{weight} fill=\"{RGBA.toHexRGB st.color}\"{opacity}{anchor}{baseline}{rot}>{escape s}</text>"

/-- Writer options. -/
structure Options where
  /-- Maximum number of decimals for coordinates. -/
  precision : Nat := 3
  /-- Gouraud approximation for `triangles`. -/
  triangleMode : TriangleMode := .flat
  /-- Stroke width (px) drawn around flat triangles in their own colour to hide
  anti-aliasing seams between neighbours; `0` disables it. -/
  seamStroke : Float := 0
  /-- Emit `<?xml version="1.0" encoding="UTF-8"?>` first. -/
  xmlDeclaration : Bool := false
  /-- Largest image (in pixels) drawn as per-pixel `<rect>`s when no PNG encoder
  is supplied; larger images become one rectangle of their mean colour. -/
  maxRectImage : Nat := 4096

/-- Rendering context. -/
structure Ctx where
  opts : Options
  encodePNG : Option (Nat → Nat → ByteArray → ByteArray)
  svgText : TextStyle → String → Float → Float → String

/-- Format a coordinate. -/
@[inline] def Ctx.f (c : Ctx) (x : Float) : String := fmtFixed c.opts.precision x

/-- ` fill="#rrggbb"` plus opacity attribute, or ` fill="none"`. -/
def fillAttrs (c : Ctx) (col : RGBA) : String :=
  if col.a ≤ 0 then " fill=\"none\""
  else if col.a < 1 then s!" fill=\"{RGBA.toHexRGB col}\" fill-opacity=\"{c.f col.a}\""
  else s!" fill=\"{RGBA.toHexRGB col}\""

/-- Stroke attributes (colour, width, cap, join, miter limit, dashes). -/
def strokeAttrs (c : Ctx) (s : Stroke) : String :=
  let col := s!" stroke=\"{RGBA.toHexRGB s.color}\""
  let op := if s.color.a < 1 then s!" stroke-opacity=\"{c.f s.color.a}\"" else ""
  let w := if s.width == 1 then "" else s!" stroke-width=\"{c.f s.width}\""
  let cap := match s.cap with | .butt => "" | .round => " stroke-linecap=\"round\"" | .square => " stroke-linecap=\"square\""
  let join := match s.join with | .miter => "" | .round => " stroke-linejoin=\"round\"" | .bevel => " stroke-linejoin=\"bevel\""
  let ml := if s.join == .miter && s.miterLimit != 4 then s!" stroke-miterlimit=\"{c.f s.miterLimit}\"" else ""
  let dash := if s.dash.isEmpty then "" else
    s!" stroke-dasharray=\"{",".intercalate (s.dash.toList.map c.f)}\"" ++
      (if s.dashOffset == 0 then "" else s!" stroke-dashoffset=\"{c.f s.dashOffset}\"")
  col ++ op ++ w ++ cap ++ join ++ ml ++ dash

/-- The `d` attribute of a path. Non-finite points lift the pen: the next
finite point starts a new subpath (`M`). Malformed trailing verbs are ignored. -/
def pathData (c : Ctx) (p : Path) : String :=
  let cs := p.coords
  let g (k : Nat) : Float := cs[k]!
  let rec go (fuel i k : Nat) (pen : Bool) (acc : String) : String :=
    match fuel with
    | 0 => acc
    | fuel + 1 =>
      if h : i < p.verbs.size then
        let v := Verb.ofUInt8 p.verbs[i]
        if k + v.arity > cs.size then acc else
        match v with
        | .moveTo =>
          let x := g k
          let y := g (k + 1)
          if x.isFinite && y.isFinite then go fuel (i + 1) (k + 2) true (acc ++ s!"M{c.f x} {c.f y}")
          else go fuel (i + 1) (k + 2) false acc
        | .lineTo =>
          let x := g k
          let y := g (k + 1)
          if x.isFinite && y.isFinite then
            go fuel (i + 1) (k + 2) true (acc ++ s!"{if pen then "L" else "M"}{c.f x} {c.f y}")
          else go fuel (i + 1) (k + 2) false acc
        | .quadTo =>
          let ok := (List.range 4).all fun j => (g (k + j)).isFinite
          if ok && pen then
            go fuel (i + 1) (k + 4) true (acc ++ s!"Q{c.f (g k)} {c.f (g (k+1))} {c.f (g (k+2))} {c.f (g (k+3))}")
          else if ok then go fuel (i + 1) (k + 4) true (acc ++ s!"M{c.f (g (k+2))} {c.f (g (k+3))}")
          else go fuel (i + 1) (k + 4) false acc
        | .cubicTo =>
          let ok := (List.range 6).all fun j => (g (k + j)).isFinite
          if ok && pen then
            go fuel (i + 1) (k + 6) true
              (acc ++ s!"C{c.f (g k)} {c.f (g (k+1))} {c.f (g (k+2))} {c.f (g (k+3))} {c.f (g (k+4))} {c.f (g (k+5))}")
          else if ok then go fuel (i + 1) (k + 6) true (acc ++ s!"M{c.f (g (k+4))} {c.f (g (k+5))}")
          else go fuel (i + 1) (k + 6) false acc
        | .close => go fuel (i + 1) k pen (if pen then acc ++ "Z" else acc)
      else acc
  go (p.verbs.size + 1) 0 0 false ""

/-- Render a `path` op. -/
def renderPath (c : Ctx) (p : Path) (fill : Option Fill) (stroke : Option Stroke) : String :=
  let d := pathData c p
  if d.isEmpty then "" else
  let fa := match fill with
    | some f => fillAttrs c f.color ++ (if f.rule == .evenOdd && f.color.a > 0 then " fill-rule=\"evenodd\"" else "")
    | none => " fill=\"none\""
  let sa := match stroke with
    | some s => if s.color.a > 0 && s.width > 0 then strokeAttrs c s else ""
    | none => ""
  if fa == " fill=\"none\"" && sa.isEmpty then "" else
  s!"<path d=\"{d}\"{fa}{sa}/>\n"

/-- RGBA8 colour of entry `i` of a byte buffer (missing bytes are 0). -/
@[inline] def colorAt (rgba : ByteArray) (i : Nat) : UInt8 × UInt8 × UInt8 × UInt8 :=
  (rgba.get! (4 * i), rgba.get! (4 * i + 1), rgba.get! (4 * i + 2), rgba.get! (4 * i + 3))

/-- Colour attributes from 8-bit channels (`kind` is `fill` or `stroke`). -/
def rgba8Attrs (c : Ctx) (kind : String) (col : UInt8 × UInt8 × UInt8 × UInt8) : String :=
  let (r, g, b, a) := col
  let hex := RGBA.toHexRGB (RGBA.ofRGB8 r g b)
  if a == 255 then s!" {kind}=\"{hex}\""
  else s!" {kind}=\"{hex}\" {kind}-opacity=\"{c.f (a.toNat.toFloat / 255)}\""

/-- Render a `segments` op: one `<path>` per run of equal colours. -/
def renderSegments (c : Ctx) (xs ys : FloatArray) (rgba : ByteArray) (width : Float) (cap : LineCap) : String :=
  let n := (min xs.size ys.size) / 2
  let capA := match cap with | .butt => "" | .round => " stroke-linecap=\"round\"" | .square => " stroke-linecap=\"square\""
  let wA := if width == 1 then "" else s!" stroke-width=\"{c.f width}\""
  let flush (acc : String) (d : String) (col : UInt8 × UInt8 × UInt8 × UInt8) : String :=
    if d.isEmpty then acc else acc ++ s!"<path d=\"{d}\" fill=\"none\"{rgba8Attrs c "stroke" col}{wA}{capA}/>\n"
  let rec go (i : Nat) (acc d : String) (col : UInt8 × UInt8 × UInt8 × UInt8) : String :=
    if i < n then
      let x0 := xs[2 * i]!
      let y0 := ys[2 * i]!
      let x1 := xs[2 * i + 1]!
      let y1 := ys[2 * i + 1]!
      let ci := colorAt rgba i
      if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite) || ci.2.2.2 == 0 then go (i + 1) acc d col
      else
        let seg := s!"M{c.f x0} {c.f y0}L{c.f x1} {c.f y1}"
        if ci == col then go (i + 1) acc (d ++ seg) col
        else go (i + 1) (flush acc d col) seg ci
    else flush acc d col
  termination_by n - i
  go 0 "" "" (0, 0, 0, 0)

/-- Little-endian `UInt32` at byte offset `4 k` of an index buffer. -/
@[inline] def indexAt (idx : ByteArray) (k : Nat) : Nat :=
  (idx.get! (4 * k)).toNat + (idx.get! (4 * k + 1)).toNat * 256 +
    (idx.get! (4 * k + 2)).toNat * 65536 + (idx.get! (4 * k + 3)).toNat * 16777216

/-- Rounded average of three bytes. -/
@[inline] private def avg3 (a b c : UInt8) : UInt8 := ((2 * (a.toNat + b.toNat + c.toNat) + 3) / 6).toUInt8

/-- Vertex indices of triangle `t` if all are `< nv`. -/
@[inline] def triIndices (idx : ByteArray) (nv t : Nat) : Option (Nat × Nat × Nat) :=
  let i0 := indexAt idx (3 * t)
  let i1 := indexAt idx (3 * t + 1)
  let i2 := indexAt idx (3 * t + 2)
  if i0 < nv && i1 < nv && i2 < nv then some (i0, i1, i2) else none

/-- Flat colour and path data of one triangle, or `none` if it is invalid,
non-finite or fully transparent. -/
def flatTriangle (c : Ctx) (xs ys : FloatArray) (rgba : ByteArray) (idx : ByteArray) (t : Nat) :
    Option (String × (UInt8 × UInt8 × UInt8 × UInt8)) := do
  let (i0, i1, i2) ← triIndices idx (min xs.size ys.size) t
  let x0 := xs[i0]!
  let y0 := ys[i0]!
  let x1 := xs[i1]!
  let y1 := ys[i1]!
  let x2 := xs[i2]!
  let y2 := ys[i2]!
  if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite && x2.isFinite && y2.isFinite) then none
  let c0 := colorAt rgba i0
  let c1 := colorAt rgba i1
  let c2 := colorAt rgba i2
  let col := (avg3 c0.1 c1.1 c2.1, avg3 c0.2.1 c1.2.1 c2.2.1, avg3 c0.2.2.1 c1.2.2.1 c2.2.2.1,
              avg3 c0.2.2.2 c1.2.2.2 c2.2.2.2)
  if col.2.2.2 == 0 then none
  some (s!"M{c.f x0} {c.f y0}L{c.f x1} {c.f y1}L{c.f x2} {c.f y2}Z", col)

/-- Render a `triangles` op flat-shaded (runs of equal colours share a path). -/
def renderTrianglesFlat (c : Ctx) (xs ys : FloatArray) (rgba : ByteArray) (idx : ByteArray) : String :=
  let nt := idx.size / 12
  let seam := c.opts.seamStroke
  let flush (acc d : String) (col : UInt8 × UInt8 × UInt8 × UInt8) : String :=
    if d.isEmpty then acc else
    let sa := if seam > 0 then rgba8Attrs c "stroke" col ++ s!" stroke-width=\"{c.f seam}\" stroke-linejoin=\"round\"" else ""
    acc ++ s!"<path d=\"{d}\"{rgba8Attrs c "fill" col}{sa}/>\n"
  let rec go (fuel t : Nat) (acc d : String) (col : UInt8 × UInt8 × UInt8 × UInt8) : String :=
    match fuel with
    | 0 => flush acc d col
    | fuel + 1 =>
      match flatTriangle c xs ys rgba idx t with
      | none => go fuel (t + 1) acc d col
      | some (tri, ci) =>
        if ci == col then go fuel (t + 1) acc (d ++ tri) col
        else go fuel (t + 1) (flush acc d col) tri ci
  go nt 0 "" "" (0, 0, 0, 0)

/-- One triangle with its own two-stop `linearGradient` (see
`renderTrianglesGradient`); `none` if invalid or non-finite. -/
def gradientTriangle (c : Ctx) (xs ys : FloatArray) (rgba : ByteArray) (idx : ByteArray) (t gid : Nat) :
    Option String := do
  let (i0, i1, i2) ← triIndices idx (min xs.size ys.size) t
  let x0 := xs[i0]!
  let y0 := ys[i0]!
  let x1 := xs[i1]!
  let y1 := ys[i1]!
  let x2 := xs[i2]!
  let y2 := ys[i2]!
  if !(x0.isFinite && y0.isFinite && x1.isFinite && y1.isFinite && x2.isFinite && y2.isFinite) then none
  let ch (col : UInt8 × UInt8 × UInt8 × UInt8) : Array Float :=
    #[col.1.toNat.toFloat, col.2.1.toNat.toFloat, col.2.2.1.toNat.toFloat, col.2.2.2.toNat.toFloat]
  let c0 := ch (colorAt rgba i0)
  let c1 := ch (colorAt rgba i1)
  let c2 := ch (colorAt rgba i2)
  -- affine field: value(p) = v0 + gx (x - x0) + gy (y - y0)
  let det := (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
  let grad (k : Nat) : Float × Float :=
    let v0 := c0[k]!
    let v1 := c1[k]!
    let v2 := c2[k]!
    if det == 0 then (0, 0) else
    (((v1 - v0) * (y2 - y0) - (v2 - v0) * (y1 - y0)) / det,
     ((x1 - x0) * (v2 - v0) - (x2 - x0) * (v1 - v0)) / det)
  let w : Array Float := #[0.299, 0.587, 0.114, 0.5]
  let (dx, dy) := (List.range 4).foldl (init := (0.0, 0.0)) fun acc k =>
    let g := grad k
    (acc.1 + w[k]! * g.1, acc.2 + w[k]! * g.2)
  let len := Float.sqrt (dx * dx + dy * dy)
  let ux := if len > 1e-12 then dx / len else 1.0
  let uy := if len > 1e-12 then dy / len else 0.0
  let proj (x y : Float) := (x - x0) * ux + (y - y0) * uy
  let s1 := proj x1 y1
  let s2 := proj x2 y2
  let smin := min 0 (min s1 s2)
  let smax := max 0 (max s1 s2)
  let colAt (s : Float) : RGBA :=
    let v (k : Nat) := let g := grad k; clamp01 ((c0[k]! + (g.1 * ux + g.2 * uy) * s) / 255)
    ⟨v 0, v 1, v 2, v 3⟩
  let stop (off : String) (col : RGBA) : String :=
    let op := if col.a < 1 then s!" stop-opacity=\"{c.f col.a}\"" else ""
    s!"<stop offset=\"{off}\" stop-color=\"{RGBA.toHexRGB col}\"{op}/>"
  let gdef := s!"<defs><linearGradient id=\"g{gid}\" gradientUnits=\"userSpaceOnUse\" x1=\"{c.f (x0 + smin * ux)}\" y1=\"{c.f (y0 + smin * uy)}\" x2=\"{c.f (x0 + smax * ux)}\" y2=\"{c.f (y0 + smax * uy)}\">{stop "0" (colAt smin)}{stop "1" (colAt smax)}</linearGradient></defs>\n"
  some (gdef ++ s!"<path d=\"M{c.f x0} {c.f y0}L{c.f x1} {c.f y1}L{c.f x2} {c.f y2}Z\" fill=\"url(#g{gid})\"/>\n")

/-- Render a `triangles` op with one two-stop `linearGradient` per triangle.
The colour field over a triangle is affine; the gradient runs along the
direction of steepest change of its luminance-weighted sum, between the two
extreme projections of the vertices, with the field's exact colours there.
Returns the markup and the next free gradient id. -/
def renderTrianglesGradient (c : Ctx) (xs ys : FloatArray) (rgba : ByteArray) (idx : ByteArray)
    (gid : Nat) : String × Nat :=
  let nt := idx.size / 12
  let rec go (fuel t : Nat) (acc : String) (gid : Nat) : String × Nat :=
    match fuel with
    | 0 => (acc, gid)
    | fuel + 1 =>
      match gradientTriangle c xs ys rgba idx t gid with
      | none => go fuel (t + 1) acc gid
      | some m => go fuel (t + 1) (acc ++ m) (gid + 1)
  go nt 0 "" gid

/-- Mean colour of an RGBA8 image. -/
def meanColor (rgba : ByteArray) (n : Nat) : UInt8 × UInt8 × UInt8 × UInt8 :=
  if n == 0 then (0, 0, 0, 0) else
  let rec go (i : Nat) (r g b a : Nat) : Nat × Nat × Nat × Nat :=
    if i < n then
      let (r', g', b', a') := colorAt rgba i
      go (i + 1) (r + r'.toNat) (g + g'.toNat) (b + b'.toNat) (a + a'.toNat)
    else (r, g, b, a)
  termination_by n - i
  let (r, g, b, a) := go 0 0 0 0 0
  ((r / n).toUInt8, (g / n).toUInt8, (b / n).toUInt8, (a / n).toUInt8)

/-- Render an `image` op. -/
def renderImage (c : Ctx) (w h : Nat) (rgba : ByteArray) (dst : Rect) (interp : Interp) : String :=
  if w == 0 || h == 0 then "" else
  let style := match interp with | .nearest => " style=\"image-rendering:pixelated\"" | .linear => ""
  match c.encodePNG with
  | some enc =>
    s!"<image x=\"{c.f dst.x}\" y=\"{c.f dst.y}\" width=\"{c.f dst.w}\" height=\"{c.f dst.h}\" preserveAspectRatio=\"none\"{style} xlink:href=\"data:image/png;base64,{base64 (enc w h rgba)}\"/>\n"
  | none =>
    if w * h > c.opts.maxRectImage then
      s!"<rect x=\"{c.f dst.x}\" y=\"{c.f dst.y}\" width=\"{c.f dst.w}\" height=\"{c.f dst.h}\"{rgba8Attrs c "fill" (meanColor rgba (w * h))}/>\n"
    else
      let pw := dst.w / w.toFloat
      let ph := dst.h / h.toFloat
      (List.range (w * h)).foldl (init := "") fun acc k =>
        let col := colorAt rgba k
        if col.2.2.2 == 0 then acc else
        let i := k % w
        let j := k / w
        acc ++ s!"<rect x=\"{c.f (dst.x + i.toFloat * pw)}\" y=\"{c.f (dst.y + j.toFloat * ph)}\" width=\"{c.f pw}\" height=\"{c.f ph}\"{rgba8Attrs c "fill" col}/>\n"

/-- The clip rectangle of an op. -/
def opClip : DrawOp → Option Rect
  | .path _ _ _ clip => clip
  | .segments _ _ _ _ _ clip => clip
  | .triangles _ _ _ _ clip => clip
  | .image _ _ _ _ _ clip => clip
  | .text _ _ _ _ clip => clip

/-- Formatted key of a rectangle (clip paths are deduplicated by it). -/
def rectKey (c : Ctx) (r : Rect) : String := s!"{c.f r.x} {c.f r.y} {c.f r.w} {c.f r.h}"

/-- Distinct clip rectangles in order of first use. -/
def collectClips (c : Ctx) (ops : Array DrawOp) : Array String :=
  ops.foldl (init := #[]) fun acc op =>
    match opClip op with
    | some r => let k := rectKey c r; if acc.contains k then acc else acc.push k
    | none => acc

/-- Render one op (no clip handling). Returns the markup and the next gradient id. -/
def renderOp (c : Ctx) (op : DrawOp) (gid : Nat) : String × Nat :=
  match op with
  | .path p fill stroke _ => (renderPath c p fill stroke, gid)
  | .segments xs ys rgba w cap _ => (renderSegments c xs ys rgba w cap, gid)
  | .triangles xs ys rgba idx _ =>
    match c.opts.triangleMode with
    | .flat => (renderTrianglesFlat c xs ys rgba idx, gid)
    | .gradient => renderTrianglesGradient c xs ys rgba idx gid
  | .image w h rgba dst interp _ => (renderImage c w h rgba dst interp, gid)
  | .text x y s st _ =>
    let t := c.svgText st s x y
    (if t.isEmpty then "" else t ++ "\n", gid)

/-- Does the scene contain an image (so `xmlns:xlink` is needed)? -/
def hasImage (ops : Array DrawOp) : Bool :=
  ops.any fun | .image .. => true | _ => false

/-- Render a scene to an SVG document.

`encodePNG w h rgba` must return the PNG file bytes of a `w × h` RGBA8 image
(row-major, top row first); pass `none` to fall back to per-pixel rectangles.
`svgText style s x y` renders one text op (default `defaultText`). -/
def render (scene : Scene) (encodePNG : Option (Nat → Nat → ByteArray → ByteArray) := none)
    (opts : Options := {}) (svgText : Option (TextStyle → String → Float → Float → String) := none) : String :=
  let fmt := fmtFixed opts.precision
  let c : Ctx := { opts, encodePNG, svgText := svgText.getD (defaultText fmt) }
  let W := toString scene.width
  let H := toString scene.height
  let clips := collectClips c scene.ops
  let xlink := if hasImage scene.ops then " xmlns:xlink=\"http://www.w3.org/1999/xlink\"" else ""
  let header := (if opts.xmlDeclaration then "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" else "") ++
    s!"<svg xmlns=\"http://www.w3.org/2000/svg\"{xlink} width=\"{W}\" height=\"{H}\" viewBox=\"0 0 {W} {H}\">\n"
  let defs := if clips.isEmpty then "" else
    "<defs>\n" ++ (clips.mapIdx fun i k =>
      match k.splitOn " " with
      | [x, y, w, h] => s!"<clipPath id=\"c{i}\"><rect x=\"{x}\" y=\"{y}\" width=\"{w}\" height=\"{h}\"/></clipPath>\n"
      | _ => "").foldl (· ++ ·) "" ++ "</defs>\n"
  let bg := if scene.background.a ≤ 0 then "" else
    s!"<rect width=\"{W}\" height=\"{H}\"{fillAttrs c scene.background}/>\n"
  -- body: group consecutive ops that share a clip
  let (body, openClip, _) := scene.ops.foldl (init := ("", (none : Option Nat), 0)) fun (acc, cur, gid) op =>
    let want : Option Nat := (opClip op).bind fun r => clips.idxOf? (rectKey c r)
    let (markup, gid) := renderOp c op gid
    let acc :=
      if want == cur then acc
      else
        let acc := if cur.isSome then acc ++ "</g>\n" else acc
        match want with
        | some i => acc ++ s!"<g clip-path=\"url(#c{i})\">\n"
        | none => acc
    (acc ++ markup, want, gid)
  let body := if openClip.isSome then body ++ "</g>\n" else body
  header ++ defs ++ bg ++ body ++ "</svg>\n"

end LeanPlot.Backend.SVG

namespace LeanPlot.Scene

/-- Render to SVG (see `LeanPlot.Backend.SVG.render`). -/
def toSVG (s : Scene) (encodePNG : Option (Nat → Nat → ByteArray → ByteArray) := none)
    (opts : Backend.SVG.Options := {}) (svgText : Option (TextStyle → String → Float → Float → String) := none) : String :=
  Backend.SVG.render s encodePNG opts svgText

end LeanPlot.Scene
