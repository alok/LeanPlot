import LeanPlot.Render.Bitmap

/-!
# LeanPlot.Render.Rasterize - Line Drawing Primitives

This module provides rasterization functions for drawing plots to bitmaps.
Includes Bresenham's line algorithm and anti-aliased line drawing.
-/

namespace LeanPlot.Render

open Bitmap

/-! ## Float conversion helpers -/

private def floatToInt (x : Float) : Int :=
  if x >= 0 then
    x.toUInt64.toNat
  else
    -((-x).toUInt64.toNat)

/-! ## Bresenham's Line Algorithm -/

/-- Draw a line using Bresenham's algorithm -/
def Bitmap.drawLine (bmp : Bitmap) (x0 y0 x1 y1 : Int) (color : RGB) : Bitmap := Id.run do
  let mut result := bmp
  let dx : Int := (x1 - x0).natAbs
  let dy : Int := (y1 - y0).natAbs
  let sx : Int := if x0 < x1 then 1 else -1
  let sy : Int := if y0 < y1 then 1 else -1
  let mut err : Int := (if dx > dy then dx else -dy) / 2
  let mut x := x0
  let mut y := y0

  for _ in [:(dx + dy + 1).toNat] do
    if x >= 0 && y >= 0 then
      result := result.setPixel x.toNat y.toNat color
    if x == x1 && y == y1 then break
    let e2 := err
    if e2 > -dx then
      err := err - dy
      x := x + sx
    if e2 < dy then
      err := err + dx
      y := y + sy

  result

/-- Draw a thick line by drawing multiple parallel lines -/
def Bitmap.drawThickLine (bmp : Bitmap) (x0 y0 x1 y1 : Int) (thickness : Nat) (color : RGB) : Bitmap := Id.run do
  let mut result := bmp
  let half := thickness / 2

  -- Determine line direction to decide offset direction
  let dx := x1 - x0
  let dy := y1 - y0
  let isMoreHorizontal := dx.natAbs > dy.natAbs

  for offset in [:thickness] do
    let off := (offset : Int) - (half : Int)
    if isMoreHorizontal then
      result := result.drawLine x0 (y0 + off) x1 (y1 + off) color
    else
      result := result.drawLine (x0 + off) y0 (x1 + off) y1 color

  result

/-! ## Plot-Specific Drawing -/

/-- Coordinate transform: map data coordinates to pixel coordinates -/
structure CoordTransform where
  /-- Data x range -/
  dataXMin : Float
  dataXMax : Float
  /-- Data y range -/
  dataYMin : Float
  dataYMax : Float
  /-- Pixel bounds (with margins) -/
  pixelLeft : Nat
  pixelRight : Nat
  pixelTop : Nat
  pixelBottom : Nat

namespace CoordTransform

/-- Create transform for a bitmap with margins -/
def create (bmp : Bitmap) (xMin xMax yMin yMax : Float) (margin : Nat := 40) : CoordTransform :=
  { dataXMin := xMin
    dataXMax := xMax
    dataYMin := yMin
    dataYMax := yMax
    pixelLeft := margin
    pixelRight := bmp.width - margin
    pixelTop := margin
    pixelBottom := bmp.height - margin }

/-- Convert data x to pixel x -/
def toPixelX (t : CoordTransform) (x : Float) : Int :=
  let range := t.dataXMax - t.dataXMin
  if range == 0 then (t.pixelLeft : Int)
  else
    let normalized := (x - t.dataXMin) / range
    let result := t.pixelLeft.toFloat + normalized * (t.pixelRight - t.pixelLeft).toFloat
    floatToInt result.round

/-- Convert data y to pixel y (note: y is inverted for screen coords) -/
def toPixelY (t : CoordTransform) (y : Float) : Int :=
  let range := t.dataYMax - t.dataYMin
  if range == 0 then (t.pixelBottom : Int)
  else
    let normalized := (y - t.dataYMin) / range
    let result := t.pixelBottom.toFloat - normalized * (t.pixelBottom - t.pixelTop).toFloat
    floatToInt result.round

end CoordTransform

/-- Draw axes on a bitmap -/
def Bitmap.drawAxes (bmp : Bitmap) (t : CoordTransform) (axisColor : RGB := RGB.gray) : Bitmap := Id.run do
  let mut result := bmp

  -- X axis (at y = 0 if in range, otherwise at bottom)
  let yAxisPos := if t.dataYMin <= 0 && 0 <= t.dataYMax
                  then t.toPixelY 0
                  else (t.pixelBottom : Int)
  result := result.drawLine (t.pixelLeft : Int) yAxisPos (t.pixelRight : Int) yAxisPos axisColor

  -- Y axis (at x = 0 if in range, otherwise at left)
  let xAxisPos := if t.dataXMin <= 0 && 0 <= t.dataXMax
                  then t.toPixelX 0
                  else (t.pixelLeft : Int)
  result := result.drawLine xAxisPos (t.pixelTop : Int) xAxisPos (t.pixelBottom : Int) axisColor

  result

/-- Draw a polyline (connected line segments) -/
def Bitmap.drawPolyline (bmp : Bitmap) (points : Array (Float × Float)) (t : CoordTransform) (color : RGB) : Bitmap := Id.run do
  let mut result := bmp

  for i in [:points.size - 1] do
    let (x0, y0) := points[i]!
    let (x1, y1) := points[i + 1]!
    let px0 := t.toPixelX x0
    let py0 := t.toPixelY y0
    let px1 := t.toPixelX x1
    let py1 := t.toPixelY y1
    result := result.drawThickLine px0 py0 px1 py1 2 color

  result

/-- Sample a function and draw it -/
def Bitmap.plotFunction (bmp : Bitmap) (f : Float → Float) (t : CoordTransform) (samples : Nat) (color : RGB) : Bitmap :=
  let step := (t.dataXMax - t.dataXMin) / samples.toFloat
  let points := (List.range (samples + 1)).toArray.map fun i =>
    let x := t.dataXMin + i.toFloat * step
    (x, f x)
  bmp.drawPolyline points t color

end LeanPlot.Render
