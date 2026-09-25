import LeanPlotWidgets

/-!
# Tests: figures as HTML

Checked while this module elaborates (`lake build` / `lake test` in `widgets/`). The infoview
itself cannot be observed from a build, so these tests pin down the HTML trees and data URIs
the widget renders, down to the SVG bytes inside them.
-/

namespace LeanPlotWidgetsTest.Html

open Lean LeanPlot LeanPlot.Widgets ProofWidgets

/-- A small figure (fast to rasterize in the interpreter). -/
def fig : Figure :=
  Figure.new (size := (160, 120)) |>.axis 1 1 (Axis2.new (title := "sin") |>.linesFn Float.sin (Num.range 0 10 21))

/-- Its SVG, as `Figure.save "x.svg"` writes it. -/
def svg : String := fig.toSVG

/-- Tag, attributes and children of an element. -/
def element? : Html → Option (String × Array (String × Json) × Array Html)
  | .element t as cs => some (t, as, cs)
  | _ => none

/-- An attribute of an element. -/
def attr? (h : Html) (k : String) : Option Json := do
  let (_, as, _) ← element? h
  (as.find? (·.1 == k)).map (·.2)

/-- The string value of an attribute. -/
def attrStr? (h : Html) (k : String) : Option String := do
  match ← attr? h k with
  | .str s => some s
  | _ => none

/-- Decode a base64 data URI with the given prefix to bytes. -/
def decodeUri? (pre uri : String) : Option ByteArray :=
  if uri.startsWith pre then Base64.decode (uri.drop pre.length).toString else none

/-- The JSON the infoview receives for an HTML tree. -/
def wire (h : Html) : Json := (Server.rpcEncode h).run' {}

/-- Number of non-overlapping occurrences of `pat` in `s`. -/
def count (s pat : String) : Nat := (s.splitOn pat).length - 1

/-! ## Default format: `<img>` with an SVG data URI -/

#guard (element? fig.toHtml).map (fun (t, _, cs) => (t, cs.size)) == some ("img", 0)
#guard attrStr? fig.toHtml "src" == some (svgDataUri svg)
#guard (decodeUri? "data:image/svg+xml;base64," (svgDataUri svg)).bind String.fromUTF8? == some svg
#guard svg.startsWith "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"160\" height=\"120\" viewBox=\"0 0 160 120\">"
#guard attr? fig.toHtml "width" == some (toJson 160)
#guard attr? fig.toHtml "height" == some (toJson 120)
#guard attrStr? fig.toHtml "alt" == some "LeanPlot figure"
#guard attr? fig.toHtml "style" == some (json% {maxWidth: "100%", height: "auto"})
#guard attr? (fig.toHtml { fit := false }) "style" == some (json% {})
#guard attrStr? (fig.toHtml { alt := "a sine" }) "alt" == some "a sine"

-- the same figure through every entry point
#guard wire fig.toScene.toHtml == wire fig.toHtml
#guard wire (html fig) == wire fig.toHtml
#guard wire (Axis2.new |>.linesFn Float.cos (Num.range 0 1 5)).toHtml ==
  wire (Figure.new |>.axis 1 1 (Axis2.new |>.linesFn Float.cos (Num.range 0 1 5))).toHtml

-- the JSON shape the ProofWidgets renderer expects (`{"element": [tag, attrs, children]}`)
#guard wire fig.toHtml == json% {element: ["img",
  [["src", $(svgDataUri svg)], ["alt", "LeanPlot figure"], ["width", 160], ["height", 120],
   ["style", {maxWidth: "100%", height: "auto"}]], []]}

/-! ## Display size -/

#guard displaySize 600 450 none == (600, 450)
#guard displaySize 600 450 (some 300) == (300, 225)
#guard displaySize 601 450 (some 300) == (300, 225)
#guard displaySize 3 2 (some 2) == (2, 1)      -- 1.33… rounds to 1
#guard displaySize 3 2 (some 4) == (4, 3)      -- 2.67 rounds to 3
#guard displaySize 0 450 (some 300) == (300, 450)
#guard attr? (fig.toHtml { width := some 80 }) "width" == some (toJson 80)
#guard attr? (fig.toHtml { width := some 80 }) "height" == some (toJson 60)

/-! ## Caption -/

#guard match fig.toHtml { caption := some "Sine" } with
  | .element "figure" _ #[.element "figcaption" _ #[.text "Sine"], img] => wire img == wire fig.toHtml
  | _ => false

/-! ## PNG format -/

#guard (attrStr? (fig.toHtml { format := .png }) "src").bind (decodeUri? "data:image/png;base64,") ==
  some fig.toPNG
#guard (fig.toPNG.extract 0 8).data == #[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
#guard attr? (fig.toHtml { format := .png }) "width" == some (toJson 160)

/-! ## Inline format -/

#guard prefixIds "<clipPath id=\"c0\"/><g clip-path=\"url(#c0)\"/>" "p-" ==
  "<clipPath id=\"p-c0\"/><g clip-path=\"url(#p-c0)\"/>"
#guard (idPrefix svg).startsWith "lp" && (idPrefix svg).endsWith "-"
#guard idPrefix svg == idPrefix svg
#guard idPrefix svg != idPrefix (Figure.new (size := (161, 120))).toSVG

/-- The inline markup of `fig`. -/
def inlineSvg : Option String := do
  let .obj o ← attr? (fig.toHtml { format := .inline }) "dangerouslySetInnerHTML" | none
  match o.get? "__html" with
  | some (.str s) => some s
  | _ => none

-- the figure clips its plot area, so it has ids to prefix
#guard count svg "id=\"" > 0
#guard (inlineSvg.map fun s => count s ("id=\"" ++ idPrefix svg)) == some (count svg "id=\"")
#guard (inlineSvg.map fun s => count s ("url(#" ++ idPrefix svg)) == some (count svg "url(#")
#guard inlineSvg.map (·.startsWith
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"160\" height=\"120\" style=\"max-width:100%;height:auto\" viewBox=\"0 0 160 120\">") ==
  some true
#guard inlineSvg == some (inlineMarkup svg 160 120 {})
#guard (inlineMarkup svg 160 120 { width := some 80, fit := false }).startsWith
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"80\" height=\"60\" viewBox=\"0 0 160 120\">"
#guard (element? (fig.toHtml { format := .inline })).map (·.1) == some "div"

/-! ## Options -/

#guard Format.ofString? "svg" == some .svg
#guard Format.ofString? "png" == some .png
#guard Format.ofString? "inline" == some .inline
#guard Format.ofString? "jpeg" == none

/-! ## Other entry points: `#html` and messages -/

/-- info: [LeanPlot figure] -/
#guard_msgs in
run_cmd do logInfo (← Elab.Command.liftCoreM (Widgets.message fig))

#guard_msgs in
#html fig

#guard_msgs in
#html fig.toHtml { format := .png, caption := some "PNG" }

end LeanPlotWidgetsTest.Html
