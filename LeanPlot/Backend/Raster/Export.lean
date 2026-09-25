import LeanPlot.Backend.Raster.Render
import LeanPlot.Backend.PNG.Encode

/-
Canvas / scene → PNG bytes and files.
-/

namespace LeanPlot.Raster.Canvas

open LeanPlot

variable {w h : Nat}

/-- Encode as PNG. Opaque canvases are written as RGB8, others as RGBA8. -/
def toPNG (cv : Canvas w h) (opts : PNG.Options := {}) : ByteArray :=
  PNG.encodeAuto w h cv.data opts

/-- Encode as RGBA8 PNG regardless of opacity. -/
def toPNGRGBA (cv : Canvas w h) (opts : PNG.Options := {}) : ByteArray :=
  PNG.encodeRGBA w h cv.data opts

/-- Write a PNG file. -/
def writePNG (cv : Canvas w h) (path : System.FilePath) (opts : PNG.Options := {}) : IO Unit :=
  IO.FS.writeBinFile path (cv.toPNG opts)

/-- `data:image/png;base64,…` URI of the canvas. -/
def toDataURI (cv : Canvas w h) (opts : PNG.Options := {}) : String :=
  PNG.dataURI w h cv.data opts

end LeanPlot.Raster.Canvas

namespace LeanPlot.Scene

/-- Rasterise and encode a scene as PNG bytes. -/
def toPNG (s : Scene) (text : Raster.TextOutliner := Raster.defaultTextOutliner) (opts : PNG.Options := {}) :
    ByteArray :=
  (s.toCanvas text).toPNG opts

/-- Rasterise a scene and write it to a PNG file. -/
def writePNG (s : Scene) (path : System.FilePath) (text : Raster.TextOutliner := Raster.defaultTextOutliner)
    (opts : PNG.Options := {}) : IO Unit :=
  IO.FS.writeBinFile path (s.toPNG text opts)

end LeanPlot.Scene
