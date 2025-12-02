import LeanPlot.Render.Bitmap
import LeanPlot.Render.Rasterize
import LeanPlot.Render.PNG.Encode
import LeanPlot.Graphic

/-!
# LeanPlot.Render.Export - PNG and SVG Export

This module provides functions to export `Graphic` values to PNG and SVG files.
-/

namespace LeanPlot.Render

open LeanPlot PNG

/-! ## Export Configuration -/

/-- Configuration for PNG export -/
structure PngConfig where
  /-- Image width in pixels -/
  width : Nat := 800
  /-- Image height in pixels -/
  height : Nat := 600
  /-- Background color -/
  background : RGB := RGB.white
  /-- Margin in pixels -/
  margin : Nat := 60
  /-- Number of samples for functions -/
  samples : Nat := 200

/-! ## Graphic Rendering -/

/-- Render a single Graphic layer to bitmap -/
partial def renderGraphicLayer (bmp : Bitmap) (g : Graphic) (t : CoordTransform)
    (colorIdx : Nat := 0) : Bitmap × Nat := Id.run do
  let colors := #[
    RGB.fromHex "#b5de2b" |>.getD RGB.green,    -- yellowGreen
    RGB.fromHex "#1f9e89" |>.getD RGB.cyan,     -- greenTurquoise
    RGB.fromHex "#3e4a89" |>.getD RGB.blue,     -- bluePurple
    RGB.fromHex "#31688e" |>.getD RGB.blue,     -- blue
    RGB.fromHex "#26828e" |>.getD RGB.cyan,     -- turquoise
    RGB.fromHex "#FF0000" |>.getD RGB.red       -- red
  ]
  let color := colors[colorIdx % colors.size]!

  match g.tag with
  | GraphicTag.fn =>
    let result := bmp.plotFunction g.func t g.opts.samples color
    (result, colorIdx + 1)
  | GraphicTag.area =>
    let result := bmp.plotFunction g.func t g.opts.samples color
    (result, colorIdx + 1)
  | GraphicTag.points =>
    -- Draw scatter points as small filled circles (just pixels for now)
    let mut result := bmp
    for (x, y) in g.pts do
      let px := t.toPixelX x
      let py := t.toPixelY y
      -- Draw a 3x3 dot
      for dyi in [:3] do
        for dxi in [:3] do
          let dx : Int := (dxi : Int) - 1
          let dy : Int := (dyi : Int) - 1
          if px + dx >= 0 && py + dy >= 0 then
            result := result.setPixel (px + dx).toNat (py + dy).toNat color
    (result, colorIdx + 1)
  | GraphicTag.bars =>
    -- Draw bars
    let mut result := bmp
    let barWidth := ((t.pixelRight - t.pixelLeft) / (g.pts.size * 2)).max 1
    for (x, y) in g.pts do
      let px := t.toPixelX x
      let py := t.toPixelY y
      let py0 := t.toPixelY 0
      let top := min py py0
      let bottom := max py py0
      result := result.fillRect
        ((px - (barWidth : Int) / 2).toNat.max 0)
        top.toNat
        barWidth
        ((bottom - top).toNat.max 1)
        color
    (result, colorIdx + 1)
  | GraphicTag.overlay =>
    match g.child1, g.child2 with
    | some g1, some g2 =>
      let (bmp1, idx1) := renderGraphicLayer bmp g1 t colorIdx
      renderGraphicLayer bmp1 g2 t idx1
    | some g1, none => renderGraphicLayer bmp g1 t colorIdx
    | none, some g2 => renderGraphicLayer bmp g2 t colorIdx
    | none, none => (bmp, colorIdx)
  | GraphicTag.styled =>
    match g.child1 with
    | some inner => renderGraphicLayer bmp inner t colorIdx
    | none => (bmp, colorIdx)
  | GraphicTag.facetH => (bmp, colorIdx)  -- Not supported in single PNG
  | GraphicTag.facetV => (bmp, colorIdx)  -- Not supported in single PNG

/-- Helper for Float min -/
private def fmin (a b : Float) : Float := if a < b then a else b

/-- Helper for Float max -/
private def fmax (a b : Float) : Float := if a > b then a else b

/-- Compute data bounds from a Graphic -/
partial def getGraphicBounds (g : Graphic) : Option (Float × Float × Float × Float) :=
  match g.tag with
  | GraphicTag.fn =>
    let (lo, hi) := g.opts.domain.getD (0.0, 1.0)
    let samples := g.opts.samples
    let step := (hi - lo) / samples.toFloat
    let ys := (List.range (samples + 1)).map fun i =>
      g.func (lo + i.toFloat * step)
    let yMin := ys.foldl fmin (ys.head?.getD 0)
    let yMax := ys.foldl fmax (ys.head?.getD 0)
    some (lo, hi, yMin, yMax)
  | GraphicTag.area =>
    let (lo, hi) := g.opts.domain.getD (0.0, 1.0)
    let samples := g.opts.samples
    let step := (hi - lo) / samples.toFloat
    let ys := (List.range (samples + 1)).map fun i =>
      g.func (lo + i.toFloat * step)
    let yMin := ys.foldl fmin (ys.head?.getD 0)
    let yMax := ys.foldl fmax (ys.head?.getD 0)
    some (lo, hi, yMin, yMax)
  | GraphicTag.points | GraphicTag.bars =>
    if g.pts.isEmpty then none
    else
      let xs := g.pts.map Prod.fst
      let ys := g.pts.map Prod.snd
      let xMin := xs.foldl fmin (xs[0]!)
      let xMax := xs.foldl fmax (xs[0]!)
      let yMin := ys.foldl fmin (ys[0]!)
      let yMax := ys.foldl fmax (ys[0]!)
      some (xMin, xMax, yMin, yMax)
  | GraphicTag.overlay =>
    match g.child1, g.child2 with
    | some g1, some g2 =>
      match getGraphicBounds g1, getGraphicBounds g2 with
      | some (x1, x2, y1, y2), some (x3, x4, y3, y4) =>
        some (min x1 x3, max x2 x4, min y1 y3, max y2 y4)
      | some b, none => some b
      | none, some b => some b
      | none, none => none
    | some g1, none => getGraphicBounds g1
    | none, some g2 => getGraphicBounds g2
    | none, none => none
  | GraphicTag.styled =>
    match g.child1 with
    | some inner => getGraphicBounds inner
    | none => none
  | GraphicTag.facetH | GraphicTag.facetV => none

/-! ## Export Functions -/

/-- Render a Graphic to a Bitmap -/
def renderToBitmap (g : Graphic) (config : PngConfig := {}) : Bitmap := Id.run do
  -- Create bitmap with background
  let mut bmp := Bitmap.fill config.width config.height config.background

  -- Get data bounds
  let (xMin, xMax, yMin, yMax) := getGraphicBounds g |>.getD (0, 1, 0, 1)

  -- Add some padding to bounds
  let xPad := (xMax - xMin) * 0.1
  let yPad := (yMax - yMin) * 0.1
  let xMin' := xMin - xPad
  let xMax' := xMax + xPad
  let yMin' := yMin - yPad
  let yMax' := yMax + yPad

  -- Create coordinate transform
  let transform := CoordTransform.create bmp xMin' xMax' yMin' yMax' config.margin

  -- Draw axes
  bmp := bmp.drawAxes transform

  -- Render graphic layers
  let (result, _) := renderGraphicLayer bmp g transform
  result

/-- Save a Graphic as a PNG file -/
def savePNG (path : System.FilePath) (g : Graphic) (config : PngConfig := {}) : IO Unit := do
  let bmp := renderToBitmap g config
  PNG.writePNG path bmp

/-- Save a Graphic as an SVG file (basic implementation) -/
def saveSVG (path : System.FilePath) (g : Graphic) (width : Nat := 800) (height : Nat := 600) : IO Unit := do
  -- Get data bounds
  let (xMin, xMax, yMin, yMax) := getGraphicBounds g |>.getD (0, 1, 0, 1)
  let margin := 60.0

  -- Helper to transform coordinates
  let toSvgX (x : Float) : Float :=
    margin + (x - xMin) / (xMax - xMin) * (width.toFloat - 2 * margin)
  let toSvgY (y : Float) : Float :=
    height.toFloat - margin - (y - yMin) / (yMax - yMin) * (height.toFloat - 2 * margin)

  -- Generate SVG content
  let mut svg := s!"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  svg := svg ++ s!"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\">\n"
  svg := svg ++ s!"  <rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n"

  -- Draw axes
  svg := svg ++ s!"  <line x1=\"{toSvgX xMin}\" y1=\"{toSvgY 0}\" x2=\"{toSvgX xMax}\" y2=\"{toSvgY 0}\" stroke=\"gray\" stroke-width=\"1\"/>\n"
  svg := svg ++ s!"  <line x1=\"{toSvgX 0}\" y1=\"{toSvgY yMin}\" x2=\"{toSvgX 0}\" y2=\"{toSvgY yMax}\" stroke=\"gray\" stroke-width=\"1\"/>\n"

  -- Generate path for function
  if g.tag == GraphicTag.fn then
    let (lo, hi) := g.opts.domain.getD (xMin, xMax)
    let samples := g.opts.samples
    let step := (hi - lo) / samples.toFloat
    let mut pathData := ""
    for i in [:samples + 1] do
      let x := lo + i.toFloat * step
      let y := g.func x
      let cmd := if i == 0 then "M" else "L"
      pathData := pathData ++ s!" {cmd} {toSvgX x} {toSvgY y}"
    svg := svg ++ s!"  <path d=\"{pathData}\" fill=\"none\" stroke=\"#3e4a89\" stroke-width=\"2\"/>\n"

  svg := svg ++ "</svg>\n"

  IO.FS.writeFile path svg

end LeanPlot.Render

/-! ## Convenience Functions -/

namespace LeanPlot

/-- Save a Graphic to a PNG file -/
def Graphic.savePNG (g : Graphic) (path : String) (width : Nat := 800) (height : Nat := 600) : IO Unit :=
  Render.savePNG path g { width, height }

/-- Save a Graphic to an SVG file -/
def Graphic.saveSVG (g : Graphic) (path : String) (width : Nat := 800) (height : Nat := 600) : IO Unit :=
  Render.saveSVG path g width height

end LeanPlot
