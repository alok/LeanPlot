/-!
# LeanPlot.SVG - Simple SVG generation for static plot images

This module provides SVG output for documentation and static exports.
-/

namespace LeanPlot.SVG

/-- Float minimum -/
def floatMin (a b : Float) : Float := if a < b then a else b

/-- Float maximum -/
def floatMax (a b : Float) : Float := if a > b then a else b

/-- SVG document dimensions -/
structure Dims where
  /-- Width in pixels -/
  width : Nat := 400
  /-- Height in pixels -/
  height : Nat := 300
  /-- Margin in pixels -/
  margin : Nat := 40
  deriving Repr

/-- A point in the plot coordinate system -/
structure Point where
  /-- X coordinate -/
  x : Float
  /-- Y coordinate -/
  y : Float
  deriving Repr

/-- Generate SVG path data for a line plot -/
def linePath (points : Array (Float × Float)) (dims : Dims) : String :=
  let w := (dims.width - 2 * dims.margin).toFloat
  let h := (dims.height - 2 * dims.margin).toFloat
  let m := dims.margin.toFloat

  -- Find bounds
  let xs := points.map (·.1)
  let ys := points.map (·.2)
  let xMin := xs.foldl floatMin xs[0]!
  let xMax := xs.foldl floatMax xs[0]!
  let yMin := ys.foldl floatMin ys[0]!
  let yMax := ys.foldl floatMax ys[0]!

  let xRange := if xMax - xMin > 0 then xMax - xMin else 1.0
  let yRange := if yMax - yMin > 0 then yMax - yMin else 1.0

  let toSvgX (x : Float) := m + (x - xMin) / xRange * w
  let toSvgY (y : Float) := m + h - (y - yMin) / yRange * h

  let pathData := points.foldl (init := "") fun acc (x, y) =>
    let sx := toSvgX x
    let sy := toSvgY y
    let cmd := if acc.isEmpty then "M" else "L"
    s!"{acc}{cmd}{sx.toString},{sy.toString} "

  pathData

/-- Generate SVG for scatter points -/
def scatterPoints (points : Array (Float × Float)) (dims : Dims) (color : String) : String :=
  let w := (dims.width - 2 * dims.margin).toFloat
  let h := (dims.height - 2 * dims.margin).toFloat
  let m := dims.margin.toFloat

  let xs := points.map (·.1)
  let ys := points.map (·.2)
  let xMin := xs.foldl floatMin xs[0]!
  let xMax := xs.foldl floatMax xs[0]!
  let yMin := ys.foldl floatMin ys[0]!
  let yMax := ys.foldl floatMax ys[0]!

  let xRange := if xMax - xMin > 0 then xMax - xMin else 1.0
  let yRange := if yMax - yMin > 0 then yMax - yMin else 1.0

  let toSvgX (x : Float) := m + (x - xMin) / xRange * w
  let toSvgY (y : Float) := m + h - (y - yMin) / yRange * h

  points.foldl (init := "") fun acc (x, y) =>
    let sx := toSvgX x
    let sy := toSvgY y
    s!"{acc}<circle cx=\"{sx.toString}\" cy=\"{sy.toString}\" r=\"4\" fill=\"{color}\" opacity=\"0.7\"/>\n"

/-- Generate axis labels -/
def axisLabels (xLabel yLabel : String) (dims : Dims) : String :=
  let w := dims.width.toFloat
  let h := dims.height.toFloat
  s!"<text x=\"{w/2}\" y=\"{h - 5}\" text-anchor=\"middle\" font-size=\"12\" fill=\"#666\">{xLabel}</text>
<text x=\"15\" y=\"{h/2}\" text-anchor=\"middle\" font-size=\"12\" fill=\"#666\" transform=\"rotate(-90,15,{h/2})\">{yLabel}</text>"

/-- Generate a simple grid -/
def grid (dims : Dims) : String :=
  let m := dims.margin
  let w := dims.width - m
  let h := dims.height - m
  let gridLines := (List.range 5).foldl (init := "") fun acc i =>
    let y := m + (h - m) * i / 4
    let x := m + (w - m) * i / 4
    s!"{acc}<line x1=\"{m}\" y1=\"{y}\" x2=\"{w}\" y2=\"{y}\" stroke=\"#e5e7eb\" stroke-dasharray=\"3,3\"/>
<line x1=\"{x}\" y1=\"{m}\" x2=\"{x}\" y2=\"{h}\" stroke=\"#e5e7eb\" stroke-dasharray=\"3,3\"/>\n"
  gridLines

/-- Generate complete SVG for a line chart -/
def lineChartSVG (points : Array (Float × Float)) (title : String)
    (color : String := "#2563eb") (dims : Dims := {}) : String :=
  let path := linePath points dims
  s!"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{dims.width}\" height=\"{dims.height}\" viewBox=\"0 0 {dims.width} {dims.height}\">
  <rect width=\"100%\" height=\"100%\" fill=\"white\"/>
  <text x=\"{dims.width/2}\" y=\"20\" text-anchor=\"middle\" font-size=\"14\" font-weight=\"600\" fill=\"#374151\" font-family=\"monospace\">{title}</text>
  {grid dims}
  <rect x=\"{dims.margin}\" y=\"{dims.margin}\" width=\"{dims.width - 2*dims.margin}\" height=\"{dims.height - 2*dims.margin}\" fill=\"none\" stroke=\"#d1d5db\"/>
  <path d=\"{path}\" fill=\"none\" stroke=\"{color}\" stroke-width=\"2\"/>
  {axisLabels "x" "y" dims}
</svg>"

/-- Generate complete SVG for a scatter chart -/
def scatterChartSVG (points : Array (Float × Float)) (title : String)
    (color : String := "#2563eb") (dims : Dims := {}) : String :=
  let pts := scatterPoints points dims color
  s!"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{dims.width}\" height=\"{dims.height}\" viewBox=\"0 0 {dims.width} {dims.height}\">
  <rect width=\"100%\" height=\"100%\" fill=\"white\"/>
  <text x=\"{dims.width/2}\" y=\"20\" text-anchor=\"middle\" font-size=\"14\" font-weight=\"600\" fill=\"#374151\" font-family=\"monospace\">{title}</text>
  {grid dims}
  <rect x=\"{dims.margin}\" y=\"{dims.margin}\" width=\"{dims.width - 2*dims.margin}\" height=\"{dims.height - 2*dims.margin}\" fill=\"none\" stroke=\"#d1d5db\"/>
  {pts}
  {axisLabels "x" "y" dims}
</svg>"

/-- Generate complete SVG for multiple line series -/
def multiLineChartSVG (series : Array (String × Array (Float × Float) × String)) (title : String)
    (dims : Dims := {}) : String :=
  let paths := series.foldl (init := "") fun acc (_, points, color) =>
    let path := linePath points dims
    s!"{acc}<path d=\"{path}\" fill=\"none\" stroke=\"{color}\" stroke-width=\"2\"/>\n"

  -- Simple legend
  let legend := series.foldl (init := (0, "")) fun (i, acc) (name, _, color) =>
    let y := dims.margin + 10 + i * 15
    let x := dims.width - dims.margin - 60
    (i + 1, s!"{acc}<rect x=\"{x}\" y=\"{y}\" width=\"12\" height=\"12\" fill=\"{color}\"/>
<text x=\"{x + 16}\" y=\"{y + 10}\" font-size=\"10\" fill=\"#374151\">{name}</text>\n")

  s!"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{dims.width}\" height=\"{dims.height}\" viewBox=\"0 0 {dims.width} {dims.height}\">
  <rect width=\"100%\" height=\"100%\" fill=\"white\"/>
  <text x=\"{dims.width/2}\" y=\"20\" text-anchor=\"middle\" font-size=\"14\" font-weight=\"600\" fill=\"#374151\" font-family=\"monospace\">{title}</text>
  {grid dims}
  <rect x=\"{dims.margin}\" y=\"{dims.margin}\" width=\"{dims.width - 2*dims.margin}\" height=\"{dims.height - 2*dims.margin}\" fill=\"none\" stroke=\"#d1d5db\"/>
  {paths}
  {legend.2}
  {axisLabels "x" "y" dims}
</svg>"

/-- Sample a function and return points -/
def sampleFn (f : Float → Float) (steps : Nat := 200) (min : Float := 0) (max : Float := 1) : Array (Float × Float) :=
  (List.range (steps + 1)).toArray.map fun i =>
    let x := min + (max - min) * i.toFloat / steps.toFloat
    (x, f x)

end LeanPlot.SVG
