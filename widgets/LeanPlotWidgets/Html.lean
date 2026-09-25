import LeanPlot
import ProofWidgets.Component.HtmlDisplay

/-!
# Figures as ProofWidgets HTML

`Scene.toHtml` (and `Figure.toHtml`, `Axis2.toHtml`, `Axis3.toHtml`) turns a LeanPlot figure into
a `ProofWidgets.Html` tree. By default the tree is a single
`<img src="data:image/svg+xml;base64,…">` holding exactly the SVG that `Scene.save "x.svg"`
writes, so what the infoview shows is byte-for-byte what CI renders.

The HTML is plain data: splice it into any other widget, e.g.
`<div>{fig.toHtml}{other}</div>` with `open scoped ProofWidgets.Jsx`, or show it with
ProofWidgets' `#html fig` (the `HtmlEval` instances below).

Formats (`Format`):

* `svg` (default): an `<img>` with an SVG data URI. The image is isolated from the page, so
  several figures never share `id`s or styles.
* `png`: an `<img>` with the anti-aliased raster (`Scene.renderPNG`). Rasterizing is slower
  than writing SVG (see `docs/WIDGETS.md`), but the page then holds one bitmap instead of many
  vector paths.
* `inline`: the SVG markup inlined into the DOM (`dangerouslySetInnerHTML`), with every `id`
  prefixed by a hash of the document so figures on one page cannot capture each other's clip
  paths or gradients. It is a fallback for hosts that block `data:` images.
-/

namespace LeanPlot.Widgets

open Lean ProofWidgets

/-- How a figure is embedded in HTML. -/
inductive Format where
  /-- `<img src="data:image/svg+xml;base64,…">` (the SVG backend's output). -/
  | svg
  /-- `<img src="data:image/png;base64,…">` (the raster backend's output). -/
  | png
  /-- Inline SVG markup with document-unique `id`s. -/
  | inline
  deriving Repr, Inhabited, BEq, DecidableEq

namespace Format

/-- Parse `"svg"`, `"png"` or `"inline"`. -/
def ofString? : String → Option Format
  | "svg" => some .svg
  | "png" => some .png
  | "inline" => some .inline
  | _ => none

end Format

/-- Display options. -/
structure Options where
  /-- Embedding format. -/
  format : Format := .svg
  /-- Display width in CSS pixels; `none` keeps the figure's own width. The height follows
  the aspect ratio. -/
  width : Option Nat := none
  /-- Shrink the figure to the width of its container (`max-width: 100%`), keeping the
  aspect ratio. -/
  fit : Bool := true
  /-- Alternative text of the image. -/
  alt : String := "LeanPlot figure"
  /-- A caption shown above the figure. -/
  caption : Option String := none
  /-- SVG writer options (`svg` and `inline` formats). -/
  svg : Backend.SVG.Options := {}

/-- `data:image/svg+xml;base64,…` URI of an SVG document. -/
def svgDataUri (svg : String) : String :=
  "data:image/svg+xml;base64," ++ Base64.encode svg.toUTF8

/-- `data:image/png;base64,…` URI of PNG bytes. -/
def pngDataUri (png : ByteArray) : String :=
  "data:image/png;base64," ++ Base64.encode png

/-- Display size in CSS pixels: the figure size, or `width` with the height scaled to keep the
aspect ratio (rounded to the nearest pixel). -/
def displaySize (w h : Nat) (width : Option Nat) : Nat × Nat :=
  match width with
  | none => (w, h)
  | some dw => if w == 0 then (dw, h) else (dw, (2 * h * dw + w) / (2 * w))

/-- The `style` object shared by all formats. -/
def style (opts : Options) : Json :=
  if opts.fit then Json.mkObj [("maxWidth", "100%"), ("height", "auto")] else Json.mkObj []

/-- Prefix every `id="…"` and `url(#…)` reference of an SVG document with `pre`. The SVG
writer emits user text as glyph outlines, never as characters, so these two patterns occur
only as clip-path and gradient ids. -/
def prefixIds (svg pre : String) : String :=
  (svg.replace "id=\"" ("id=\"" ++ pre)).replace "url(#" ("url(#" ++ pre)

/-- A document-unique `id` prefix for inline SVG (`lp` + a hash of the markup). -/
def idPrefix (svg : String) : String :=
  "lp" ++ String.ofList (Nat.toDigits 16 (hash svg).toNat) ++ "-"

/-- The SVG markup for the `inline` format: ids prefixed, and the root element scaled like an
image (`width`/`height` from `displaySize`, `max-width: 100%` when `fit`). -/
def inlineMarkup (svg : String) (w h : Nat) (opts : Options) : String :=
  let (dw, dh) := displaySize w h opts.width
  let body := prefixIds svg (idPrefix svg)
  let old := s!"width=\"{w}\" height=\"{h}\" viewBox="
  let styleAttr := if opts.fit then " style=\"max-width:100%;height:auto\"" else ""
  body.replace old s!"width=\"{dw}\" height=\"{dh}\"{styleAttr} viewBox="

/-- An `<img>` element of the given source and size. -/
def img (src : String) (w h : Nat) (opts : Options) : Html :=
  let (dw, dh) := displaySize w h opts.width
  .element "img" #[("src", .str src), ("alt", .str opts.alt), ("width", toJson dw), ("height", toJson dh),
    ("style", style opts)] #[]

/-- Wrap `body` in `<figure>` with a `<figcaption>` when a caption is given. -/
def withCaption (caption : Option String) (body : Html) : Html :=
  match caption with
  | none => body
  | some c =>
    let capStyle := Json.mkObj [("fontSize", "13px"), ("fontWeight", "500"), ("marginBottom", "6px"),
      ("fontFamily", "var(--vscode-editor-font-family, monospace)")]
    .element "figure" #[("style", Json.mkObj [("margin", "0")])]
      #[.element "figcaption" #[("style", capStyle)] #[.text c], body]

/-- A scene as HTML. -/
def sceneHtml (s : Scene) (opts : Options := {}) : Html :=
  let body := match opts.format with
    | .svg => img (svgDataUri (s.renderSVG opts.svg)) s.width s.height opts
    | .png => img (pngDataUri s.renderPNG) s.width s.height opts
    | .inline =>
      .element "div" #[("role", .str "img"), ("aria-label", .str opts.alt),
        ("dangerouslySetInnerHTML", Json.mkObj [("__html", .str (inlineMarkup (s.renderSVG opts.svg) s.width s.height opts))])] #[]
  withCaption opts.caption body

/-- Values that are displayed as a figure: a `Scene`, a `Figure`, or a single `Axis2`/`Axis3`
(placed alone in a default `Figure.new`, like Makie's non-mutating plotting functions). -/
class ToScene (α : Type) where
  /-- The device-space scene to display. -/
  toScene : α → Scene

instance : ToScene Scene := ⟨id⟩
instance : ToScene Figure := ⟨Figure.toScene⟩
instance : ToScene Axis2 := ⟨fun a => (Figure.new |>.axis 1 1 a).toScene⟩
instance : ToScene Axis3 := ⟨fun a => (Figure.new |>.axis3 1 1 a).toScene⟩

/-- Any displayable value as HTML. -/
def html {α : Type} [ToScene α] (a : α) (opts : Options := {}) : Html :=
  sceneHtml (ToScene.toScene a) opts

/-- A figure as a structured message (e.g. for `logInfo` from a tactic or command); `alt` is
the plain-text rendering used outside the infoview. -/
def message {α : Type} [ToScene α] (a : α) (opts : Options := {}) (alt : String := "[LeanPlot figure]") :
    CoreM MessageData :=
  MessageData.ofHtml (html a opts) alt

instance : HtmlEval Scene := ⟨fun s => pure (sceneHtml s)⟩
instance : HtmlEval Figure := ⟨fun f => pure (html f)⟩
instance : HtmlEval Axis2 := ⟨fun a => pure (html a)⟩
instance : HtmlEval Axis3 := ⟨fun a => pure (html a)⟩

end LeanPlot.Widgets

namespace LeanPlot

/-- The scene as ProofWidgets HTML (see `LeanPlot.Widgets.Options`). -/
def Scene.toHtml (s : Scene) (opts : Widgets.Options := {}) : ProofWidgets.Html := Widgets.sceneHtml s opts

/-- The figure as ProofWidgets HTML (see `LeanPlot.Widgets.Options`). -/
def Figure.toHtml (f : Figure) (opts : Widgets.Options := {}) : ProofWidgets.Html := Widgets.html f opts

/-- The axis, alone in a default figure, as ProofWidgets HTML. -/
def Axis2.toHtml (a : Axis2) (opts : Widgets.Options := {}) : ProofWidgets.Html := Widgets.html a opts

/-- The 3D axis, alone in a default figure, as ProofWidgets HTML. -/
def Axis3.toHtml (a : Axis3) (opts : Widgets.Options := {}) : ProofWidgets.Html := Widgets.html a opts

end LeanPlot
