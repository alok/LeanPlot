import LeanPlot.Backend.Raster.Stroke
import LeanPlot.Backend.Raster.Segments
import LeanPlot.Backend.Raster.Triangles
import LeanPlot.Backend.Raster.Image
import LeanPlot.Backend.Raster.Text

/-
`Scene` → `Canvas`: paints the draw ops in order (painter's algorithm) onto a
canvas cleared to the scene background.

One coverage accumulator is allocated per scene and reused by every op. Each
op leaves it zeroed, because a sweep clears the cells it visits.
-/

namespace LeanPlot.Raster

open LeanPlot

/-- Rendering options. -/
structure RenderOptions where
  /-- Glyph outliner for `text` ops (STUB default: draws nothing). -/
  text : TextOutliner := defaultTextOutliner
  /-- Bézier flattening tolerance in pixels. -/
  tol : Float := flattenTol

/-- Fill flattened subpaths. -/
def fillPolys {w h : Nat} (acc : Accum w h) (cv : Canvas w h) (cl : Clip) (pl : Polylines) (f : Fill) :
    Accum w h × Canvas w h :=
  if !(f.color.a > K.zero) then (acc, cv) else
  (acc.fillPolylines cl pl).sweepSolid cv cl f.rule f.color

/-- Stroke flattened subpaths (dashing first when a pattern is set). -/
def strokePolys {w h : Nat} (acc : Accum w h) (cv : Canvas w h) (cl : Clip) (pl : Polylines) (s : Stroke) :
    Accum w h × Canvas w h :=
  if !(s.width > K.zero) || !(s.color.a > K.zero) then (acc, cv) else
  let pl := if s.dash.isEmpty then pl else dashPolylines pl s.dash s.dashOffset
  (acc.strokePolylines cl pl (StrokeGeom.ofStroke s)).sweepSolid cv cl .nonzero s.color

/-- Paint one op. -/
def renderOp {w h : Nat} (opts : RenderOptions) (acc : Accum w h) (cv : Canvas w h) (op : DrawOp) : Accum w h × Canvas w h :=
  match op with
  | .path p fill stroke clip =>
    let cl := Clip.make w h clip
    if cl.isEmpty || p.verbs.isEmpty then (acc, cv) else
    let pl := flatten p opts.tol
    let (acc, cv) := match fill with
      | some f => fillPolys acc cv cl pl f
      | none => (acc, cv)
    match stroke with
    | some s => strokePolys acc cv cl pl s
    | none => (acc, cv)
  | .segments xs ys rgba width cap clip => renderSegments acc cv (Clip.make w h clip) xs ys rgba width cap
  | .triangles xs ys rgba idx clip => renderTriangles acc cv (Clip.make w h clip) xs ys rgba idx
  | .image iw ih rgba dst interp clip => (acc, renderImage cv (Clip.make w h clip) iw ih rgba dst interp)
  | .text x y s style clip => textInto opts.text acc cv (Clip.make w h clip) style s x y

/-- Paint ops `[i, n)`. -/
def renderOps {w h : Nat} (opts : RenderOptions) (ops : Array DrawOp) (i : Nat) (acc : Accum w h) (cv : Canvas w h) :
    Canvas w h :=
  if hi : i < ops.size then
    let (acc, cv) := renderOp opts acc cv ops[i]
    renderOps opts ops (i + 1) acc cv
  else cv
termination_by ops.size - i

/-- Paint `ops` onto an existing canvas. -/
def Canvas.drawOps {w h : Nat} (cv : Canvas w h) (ops : Array DrawOp) (opts : RenderOptions := {}) : Canvas w h :=
  if ops.isEmpty then cv else renderOps opts ops 0 (Accum.new w h) cv.markLinear

/-- Paint one op onto an existing canvas (allocates a fresh accumulator; for
many ops prefer `drawOps`). -/
def Canvas.drawOp {w h : Nat} (cv : Canvas w h) (op : DrawOp) (opts : RenderOptions := {}) : Canvas w h :=
  cv.drawOps #[op] opts

/-- Fill a path onto a canvas. -/
def Canvas.fillPath {w h : Nat} (cv : Canvas w h) (p : Path) (f : Fill) (clip : Option Rect := none) : Canvas w h :=
  cv.drawOp (.path p (some f) none clip)

/-- Stroke a path onto a canvas. -/
def Canvas.strokePath {w h : Nat} (cv : Canvas w h) (p : Path) (s : Stroke) (clip : Option Rect := none) : Canvas w h :=
  cv.drawOp (.path p none (some s) clip)

/-- Render a scene: clear to the background, then paint every op in order. -/
def render (s : Scene) (opts : RenderOptions := {}) : Canvas s.width s.height :=
  (Canvas.fill s.width s.height s.background).drawOps s.ops opts

end LeanPlot.Raster

namespace LeanPlot.Scene

/-- Rasterise a scene to an RGBA8 canvas. `text` turns `text` ops into glyph
outlines; the default is a STUB that draws no text until the font module is
wired in. -/
def toCanvas (s : Scene) (text : Raster.TextOutliner := Raster.defaultTextOutliner) :
    Raster.Canvas s.width s.height :=
  Raster.render s { text }

end LeanPlot.Scene
