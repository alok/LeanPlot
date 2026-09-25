import LeanPlotTest.Core.Harness
import LeanPlot.Recipes.Basic

/-!
Unit tests of the figure layer's building blocks: the grid solver, autolimit expansion,
heatmap edges, line-style patterns, the marker map, colour specs and palettes, mark bounds,
mark lowering (path shapes, grouping, clipping, arrows) and the stable z-sort.
-/

namespace LeanPlotTest.Figure

open LeanPlot LeanPlot.Layout LeanPlotTest.Core

/-- Grid solver cases (GridLayoutBase semantics). -/
def gridTests : TestM Unit := do
  -- one cell, Outside(16) padding, protrusions: the inner box
  let g : Grid := { nrows := 1, ncols := 1, align := .outside (Sides.uniform 16) }
  let s := g.solve #[{ span := Span.cell 0 0, protrusions := { left := 57.946, bottom := 42.62, top := 20.31 } }] ⟨0, 600, 0, 450⟩
  let c := s.cell (Span.cell 0 0)
  check "grid: 1x1 left" ((c.left - 73.946).abs < 1e-4) (fun _ => s!"{c.left}")
  check "grid: 1x1 right" (c.right == 584) (fun _ => s!"{c.right}")
  check "grid: 1x1 bottom/top" ((c.bottom - 58.62).abs < 1e-4 && (c.top - 413.69).abs < 1e-4) (fun _ => s!"{c.bottom} {c.top}")
  -- two columns, the second with a determined width: gaps = protrusions + colgap
  let g2 : Grid := { nrows := 1, ncols := 2, align := .outside (Sides.uniform 16) }
  let items : Array Item := #[{ span := Span.cell 0 0, protrusions := { left := 10, right := 5 } },
                              { span := Span.cell 0 1, protrusions := { left := 3, right := 20 }, width := some 12 }]
  let s2 := g2.solve items ⟨0, 600, 0, 450⟩
  check "grid: fixed-width column" ((s2.rights[1]! - s2.lefts[1]! - 12).abs < 1e-4) (fun _ => s!"{s2.lefts} {s2.rights}")
  check "grid: gap = right prot + left prot + colgap" ((s2.lefts[1]! - s2.rights[0]! - (5 + 3 + 18)).abs < 1e-4)
  check "grid: right edge = width - padding - protrusion" ((s2.rights[1]! - (600 - 16 - 20)).abs < 1e-4)
  -- relative and fixed sizes
  let g3 : Grid := { nrows := 1, ncols := 3, colSizes := #[.relative 0.5, .fixed 100, .auto], defaultColGap := 0 }
  let s3 := g3.solve #[] ⟨0, 400, 0, 100⟩
  check "grid: relative" ((s3.rights[0]! - s3.lefts[0]! - 200).abs < 1e-4)
  check "grid: fixed" ((s3.rights[1]! - s3.lefts[1]! - 100).abs < 1e-4)
  check "grid: auto takes the rest" ((s3.rights[2]! - s3.lefts[2]! - 100).abs < 1e-4)
  -- aspect: column 0 as wide as row 0 is tall
  let g4 : Grid := { nrows := 1, ncols := 2, colSizes := #[.aspect 0 1, .auto], defaultColGap := 0 }
  let s4 := g4.solve #[] ⟨0, 400, 0, 100⟩
  check "grid: aspect column" ((s4.rights[0]! - s4.lefts[0]! - 100).abs < 1e-4)
  -- determined totals (legend-style content)
  let g5 : Grid := { nrows := 2, ncols := 2, rowGaps := #[3], colGaps := #[5], align := .outside (Sides.uniform 6) }
  let it5 : Array Item := #[{ span := Span.cell 0 0, width := some 20, height := some 20 },
    { span := Span.cell 0 1, width := some 17.892, height := some 16.31 },
    { span := Span.cell 1 0, width := some 20, height := some 20 },
    { span := Span.cell 1 1, width := some 18.676, height := some 16.31 }]
  check "grid: determined width" (((g5.determinedTotal it5 true).getD 0 - 55.676).abs < 1e-4)
  check "grid: determined height" (((g5.determinedTotal it5 false).getD 0 - 55).abs < 1e-4)
  -- placement inside a cell
  let b := place ⟨0, 100, 0, 100⟩ .auto .auto (some 20) (some 10) (some 20) none 1 0
  check "place: right/bottom aligned" (b.left == 80 && b.right == 100 && b.bottom == 0 && b.top == 10)

/-- Autolimits: margins, singular limits, log scales. -/
def limitTests : TestM Unit := do
  let m := margin05
  let (a, b) := Axis2.expandLimits 0 10 m m .identity
  check "limits: 5% margins" (a == -0.5000000074505806 && b == 10.500000007450581) (fun _ => s!"{a} {b}")
  check "limits: singular at 5" (Axis2.expandLimits 5 5 m m .identity == (0, 10))
  check "limits: singular at 0" (Axis2.expandLimits 0 0 m m .identity == (-1, 1))
  let (l, h) := Axis2.expandLimits 1 1000 m m .log10
  check "limits: log10 margins in scaled space" ((Float.log10 l + 0.15).abs < 1e-6 && (Float.log10 h - 3.15).abs < 1e-6) (fun _ => s!"{l} {h}")
  -- an empty axis uses the scale's default limits
  check "limits: empty axis" (Axis2.new.targetLimits == ((0, 10), (0, 10)))
  -- a heatmap removes the margins
  let z : Grid2 2 2 := Grid2.ofFn 2 2 fun i j => Num.ofInt (i + j : Nat)
  let ax := Axis2.new |>.heatmapGrid z
  check "limits: heatmap is tight" (ax.targetLimits == ((0.5, 2.5), (0.5, 2.5))) (fun _ => s!"{ax.targetLimits}")
  -- user limits win per side
  let ax2 : Axis2 := { (Axis2.new |>.lines ⟨#[0, 1]⟩ ⟨#[0, 1]⟩) with xlimits := (some (-3), none) }
  check "limits: partial user limits" (ax2.targetLimits.1.1 == -3 && ax2.targetLimits.1.2 > 1)
  -- hlines only constrain y
  let ax3 := Axis2.new |>.lines ⟨#[0, 1]⟩ ⟨#[0, 1]⟩ |>.hlines #[5]
  check "limits: hlines extend y only" (ax3.targetLimits.2.2 > 5 && ax3.targetLimits.1.2 < 2)

/-- Small helpers of the recipes. -/
def helperTests : TestM Unit := do
  check "edges: centres" ((Recipes.edges ⟨#[1, 2, 3]⟩).data == #[0.5, 1.5, 2.5, 3.5])
  check "edges: single" ((Recipes.edges ⟨#[5]⟩).data == #[4.5, 5.5])
  check "edges: irregular" ((Recipes.edges ⟨#[0, 1, 3]⟩).data == #[-0.5, 0.5, 2, 4])
  check "cellEdges keeps edges" ((Recipes.cellEdges ⟨#[0, 1, 3]⟩ 2).data == #[0, 1, 3])
  check "dash pattern" ((LineStyle.dash.dashArray 2) == #[6, 6])
  check "dot pattern" ((LineStyle.dot.dashArray 1.5) == #[1.5, 3])
  check "dashdot pattern" ((LineStyle.dashdot.dashArray 1) == #[3, 3, 1, 3])
  match LineStyle.ofString? "-.." with
  | some (.pattern p) => check "linestyle string" (p == #[3, 3, 1, 2, 1, 3])
  | _ => check "linestyle string" false
  check "marker names" (MarkerShape.ofString? "x" == some .xcross && MarkerShape.ofString? "+" == some .cross &&
    MarkerShape.ofString? "star5" == some .star5 && MarkerShape.ofString? "nope" == none)
  check "marker polygons" (MarkerShape.all.all fun m => m == .circle || m.unitPolygon.size ≥ 6)
  -- the circle marker of size 9 spans 2 · 0.3525 · 9 px
  let p := MarkerShape.circle.toPath 9 100 100
  let xs := (Array.range (p.coords.size / 2)).map fun i => p.coords[2 * i]!
  let w := xs.foldl max 0 - xs.foldl min 1000
  check "circle marker diameter" ((w - 6.345).abs < 1e-9) (fun _ => s!"{w}")
  -- palettes
  let t : Theme := {}
  let pc := t.patchColor 0
  check "patchcolor palette" ((pc.r - 0.19999999).abs < 1e-6 && (pc.g - 0.557647).abs < 1e-6 && (pc.b - 0.7584314).abs < 1e-6)
    (fun _ => s!"{repr pc}")
  check "palette cycles" (t.color 7 == t.color 0)
  -- colour specs
  check "solid resolve" ((ColorSpec.solid RGBA.black).resolve 3 == ⟨#[0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255]⟩)
  let v := ColorSpec.values ⟨#[0, 1]⟩ {}
  let r := v.resolve 2
  check "mapped resolve ends" (r.size == 8 && r.get! 0 == 68 && r.get! 4 == 253) (fun _ => s!"{r.toList}")
  check "mapped range" (v.colorRange? == some (0, 1))

/-- Marks: bounds and lowering. -/
def markTests : TestM Unit := do
  let p := Pts2.ofArrays ⟨#[0, 1, Num.nan, 2]⟩ ⟨#[0, 2, 5, -1]⟩
  match (Mark.lines (.xy p) {}).bounds? with
  | some r => check "bounds skip NaN" (r.lo.x == 0 && r.hi.x == 2 && r.lo.y == -1 && r.hi.y == 2)
  | none => check "bounds skip NaN" false
  let h : HeatmapData := { xs := ⟨#[0, 1, 3]⟩, ys := ⟨#[0, 2]⟩, z := ⟨2, 1, Grid2.fill 2 1 0⟩ }
  check "heatmap bounds are its edges" ((Mark.heatmap h).bounds?.map (fun r => (r.lo.x, r.hi.x, r.hi.y)) == some (0, 3, 2))
  check "heatmap is tight" (Mark.heatmap h).tight
  -- identity projector: polyline with a NaN break gives two subpaths
  let pr : Lower.Projector := { project := fun v => v, clip := none }
  match Lower.lines pr (.xy p) {} with
  | #[.path path none (some s) none] =>
    check "lines: two subpaths" ((path.verbs.toList.filter (· == 0)).length == 2)
    check "lines: width 1.5" (s.width == 1.5)
  | _ => check "lines: one path op" false
  -- closed runs are closed
  let sq := Pts2.ofArrays ⟨#[0, 10, 10, 0, 0]⟩ ⟨#[0, 0, 10, 10, 0]⟩
  match Lower.lines pr (.xy sq) {} with
  | #[.path path _ _ _] => check "lines: closed loop" (path.verbs.toList.getLast? == some 4)
  | _ => check "lines: closed loop" false
  -- per-vertex colours become a segments op with one colour per segment
  match Lower.lines pr (.xy sq) { color := .values ⟨#[0, 1, 2, 3, 4]⟩ {} } with
  | #[.segments xs _ rgba _ _ _] => check "lines: coloured segments" (xs.size == 8 && rgba.size == 16)
  | _ => check "lines: coloured segments" false
  -- scatter: equal opaque colours share one path, translucent ones do not
  let pts := Pts2.ofArrays ⟨#[0, 1, 2]⟩ ⟨#[0, 1, 2]⟩
  check "scatter: grouped" ((Lower.scatter pr (.xy pts) { color := .solid RGBA.black }).size == 1)
  check "scatter: translucent separate" ((Lower.scatter pr (.xy pts) { color := .solid (RGBA.black.withAlpha 0.5) }).size == 3)
  -- arrows: shaft and tip per arrow; long arrows keep the default metrics
  let (tl, _, sl, sw, pl, pw) := Lower.arrowMetrics {} 100
  check "arrow metrics (long)" (tl == 0 && sl == 92 && sw == 3 && pl == 8 && pw == 14)
  let (_, _, sl2, sw2, pl2, _) := Lower.arrowMetrics {} 9
  check "arrow metrics (short, scaled)" ((sl2 + pl2 - 9).abs < 1e-9 && sw2 < 3) (fun _ => s!"{sl2} {pl2} {sw2}")
  check "arrows: two ops each" ((Lower.arrows pr (.xy pts) (.xy (Pts2.ofArrays ⟨#[50, 50, 50]⟩ ⟨#[0, 0, 0]⟩)) {}).size == 6)
  -- band: one polygon
  match Lower.band pr (.xy pts) (.xy (Pts2.ofArrays ⟨#[0, 1, 2]⟩ ⟨#[1, 2, 3]⟩)) (.solid RGBA.black) with
  | #[.path path (some _) none none] => check "band: 6 vertices, closed" (path.coords.size == 12 && path.verbs.toList.getLast? == some 4)
  | _ => check "band: one filled path" false

/-- Figure-level helpers. -/
def figureUnitTests : TestM Unit := do
  let ops : Array (Float × DrawOp) := #[(20, .text 0 0 "a" {} none), (0, .text 0 0 "b" {} none),
    (-10, .text 0 0 "c" {} none), (0, .text 0 0 "d" {} none)]
  let order := (Figure.sortOps ops).map fun op => match op with | .text _ _ s _ _ => s | _ => ""
  check "stable z-sort" (order == #["c", "b", "d", "a"]) (fun _ => s!"{order}")
  -- legend entries from labelled items only
  let ax := Axis2.new |>.lines ⟨#[0, 1]⟩ ⟨#[0, 1]⟩ (label := "a") |>.scatter ⟨#[0]⟩ ⟨#[0]⟩
    |>.band ⟨#[0, 1]⟩ ⟨#[0, 0]⟩ ⟨#[1, 1]⟩ (label := "b")
  check "legend entries" ((LegendEntry.ofItems ax.items).map (·.label) == #["a", "b"])
  -- colour cycling per plot type
  let ax2 := Axis2.new |>.lines ⟨#[0]⟩ ⟨#[0]⟩ |>.scatter ⟨#[0]⟩ ⟨#[0]⟩ |>.lines ⟨#[0]⟩ ⟨#[0]⟩ (color := RGBA.black)
    |>.lines ⟨#[0]⟩ ⟨#[0]⟩
  let colors := ax2.items.map fun it => match it.mark with
    | .lines _ s | .segments _ s => s.color.representative
    | .scatter _ s => s.color.representative
    | _ => RGBA.transparent
  check "cycle: per plot type, explicit colours skipped" (colors == #[wongColor 0, wongColor 0, RGBA.black, wongColor 1])
  -- a colorbar referring to an axis picks up the heatmap range
  let z : Grid2 2 2 := Grid2.ofFn 2 2 fun i j => Num.ofInt (i + 2 * j : Nat)
  let f := Figure.new |>.axis 1 1 (Axis2.new |>.heatmapGrid z) |>.colorbar 1 2 (1, 1)
  match f.content[1]!.block with
  | .colorbar cb => let (_, lo, hi) := f.colorbarMapping cb; check "colorbar range from axis" (lo == 0 && hi == 3)
  | _ => check "colorbar placed" false
  check "grid size" (f.grid.nrows == 1 && f.grid.ncols == 2)

/-- The unit suite. -/
def unitSuite : TestM Unit := do
  gridTests
  limitTests
  helperTests
  markTests
  figureUnitTests

end LeanPlotTest.Figure
