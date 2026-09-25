import LeanPlot.Backend.SVG
import LeanPlot.Backend.Raster
import LeanPlot.Font

/-!
# Output

The one-stop entry points for turning a `Scene` into files. Both backends draw text as glyph
outlines from the embedded fonts, so SVG and PNG output agree and layout metrics are exact.
-/

namespace LeanPlot.Scene

/-- SVG with text as glyph paths and images embedded as base64 PNG. -/
def renderSVG (s : Scene) (opts : Backend.SVG.Options := {}) : String :=
  s.toSVG (encodePNG := some (fun w h rgba => PNG.encodeRGBA w h rgba)) (opts := opts)
    (svgText := some (Backend.SVG.glyphText Font.textPath))

/-- Anti-aliased PNG bytes. -/
def renderPNG (s : Scene) (opts : PNG.Options := {}) : ByteArray :=
  s.toPNG (opts := opts)

/-- Write the scene to `path`, choosing the format from the extension (`.svg` or `.png`). -/
def save (s : Scene) (path : System.FilePath) : IO Unit := do
  match path.extension with
  | some "svg" => IO.FS.writeFile path s.renderSVG
  | some "png" => IO.FS.writeBinFile path s.renderPNG
  | _ => throw (IO.userError s!"Scene.save: unsupported extension in {path} (use .svg or .png)")

end LeanPlot.Scene
