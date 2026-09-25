import LeanPlot.Recipes.Basic

/-!
The figures of `LeanPlotTest/oracle/figure/figure_oracle.jl`, rebuilt with the same data
through the recipe API. `specs` is keyed by the oracle's figure names.
-/

namespace LeanPlotTest.Figure

open LeanPlot

/-- `collect(range(0, 10, length = 101))`. -/
def xs : FloatArray := Num.range 0 10 101

/-- Pointwise map of a float array. -/
def fmap (f : Float → Float) (a : FloatArray) : FloatArray := a.foldl (init := .empty) fun acc x => acc.push (f x)

/-- Every `k`-th element starting at 0 (`a[1:k:end]`). -/
def every (k : Nat) (a : FloatArray) : FloatArray :=
  (Array.range a.size).foldl (init := .empty) fun acc i => if i % k == 0 then acc.push a[i]! else acc

/-- 1. lines + scatter with a title and axis labels. -/
def basic : Figure :=
  let ax := Axis2.new (title := "Title") (xlabel := "x label") (ylabel := "y label")
    |>.linesFn Float.sin xs
    |>.scatter ⟨#[1, 2, 3]⟩ ⟨#[0.5, -0.5, 0.2]⟩
  Figure.new |>.axis 1 1 ax

/-- 2. an empty axis. -/
def empty : Figure := Figure.new |>.axis 1 1 Axis2.new

/-- 3. two axes side by side; the second has scientific tick labels. -/
def twoaxes : Figure :=
  let a1 := Axis2.new (xlabel := "t") |>.linesFn Float.cos xs
  let a2 := Axis2.new (title := "growth") (ylabel := "count") |>.linesFn (fun x => 2000 * x ^ 2) xs
  Figure.new (size := (800, 400)) |>.axis 1 1 a1 |>.axis 1 2 a2

/-- 4. a 2×2 grid of axes. -/
def grid22 : Figure := Id.run do
  let mut f := Figure.new (size := (700, 500))
  for i in [1:3] do
    for j in [1:3] do
      let fi := Num.ofInt i
      let fj := Num.ofInt j
      let ax := Axis2.new (title := if i == 1 then s!"panel {i}{j}" else "") (xlabel := if i == 2 then "x" else "")
        (ylabel := if j == 1 then "y" else "")
        |>.linesFn (fun x => (fi + fj) * Float.sin (x * fj) + 10 * fi) xs
      f := f.axis i j ax
  return f

/-- 5. explicit limits. -/
def limits : Figure :=
  Figure.new |>.axis 1 1 (Axis2.new |>.limits 0 5 (-2) 2 |>.linesFn Float.sin xs)

/-- The heatmap data `sin(0.5 i) cos(0.3 j)`, `i ∈ 1:20`, `j ∈ 1:15`. -/
def heatZ : Grid2 20 15 := Grid2.ofFn 20 15 fun i j => Float.sin (0.5 * Num.ofInt (i + 1 : Nat)) * Float.cos (0.3 * Num.ofInt (j + 1 : Nat))

/-- 6. a heatmap with a colorbar. -/
def heatmapColorbar : Figure :=
  Figure.new |>.axis 1 1 (Axis2.new |>.heatmap (Recipes.oneTo 20) (Recipes.oneTo 15) heatZ) |>.colorbar 1 2 (1, 1)

/-- 7. a legend in its own column. -/
def legend : Figure :=
  let ax := Axis2.new
    |>.linesFn Float.sin xs (label := "sin")
    |>.linesFn Float.cos xs (label := "cos")
    |>.scatter (every 10 xs) (fmap Float.sin (every 10 xs)) (label := "samples")
  Figure.new |>.axis 1 1 ax |>.legend 1 2 (1, 1)

/-- 8. `axislegend`. -/
def axislegend : Figure :=
  let ax := Axis2.new
    |>.linesFn Float.sin xs (label := "sin")
    |>.linesFn Float.cos xs (label := "cos")
    |>.axislegend
  Figure.new |>.axis 1 1 ax

/-- 9. a log10 y axis. -/
def logy : Figure :=
  let x1 : FloatArray := ⟨xs.data.extract 1 xs.size⟩
  Figure.new |>.axis 1 1 (Axis2.new (yscale := .log10) |>.linesFn Float.exp x1)

/-- 10. `DataAspect()`. -/
def dataaspect : Figure :=
  let ts := Num.range 0 (2 * Num.pi) 101
  Figure.new |>.axis 1 1 (Axis2.new (aspect := .data) |>.lines (fmap Float.cos ts) (fmap Float.sin ts))

/-- 11. band, poly and text. -/
def bandpoly : Figure :=
  let ax := Axis2.new
    |>.band xs (fmap (fun x => Float.sin x - 0.5) xs) (fmap (fun x => Float.sin x + 0.5) xs)
    |>.poly (Pts2.ofArrays ⟨#[2, 4, 3]⟩ ⟨#[2, 2, 3]⟩)
    |>.text 5 2.5 "note"
  Figure.new |>.axis 1 1 ax

/-- 12. a helix in a default `Axis3`. -/
def axis3 : Figure :=
  let ts := Num.range 0 (4 * Num.pi) 101
  Figure.new |>.axis3 1 1 (Axis3.new |>.lines (fmap Float.cos ts) (fmap Float.sin ts) (fmap (· / (4 * Num.pi)) ts))

/-- 13. a shaded surface in a rotated `Axis3` with a title. -/
def surface3 : Figure :=
  let x3 := Num.range (-2) 2 21
  let y3 := Num.range (-1) 1 11
  let z := Grid2.ofFn 21 11 fun i j => Float.exp (-(x3[i]! ^ 2 + y3[j]! ^ 2))
  Figure.new (size := (500, 400)) |>.axis3 1 1
    (Axis3.new (title := "surface") (azimuth := 0.3 * Num.pi) (elevation := 0.2 * Num.pi) |>.surface x3 y3 z)

/-- 14. marker shapes, stroked markers and line styles. -/
def markers : Figure := Id.run do
  let mut ax := Axis2.new (title := "markers")
  let shapes := #[MarkerShape.circle, .rect, .diamond, .utriangle, .dtriangle, .cross, .xcross, .star5]
  for i in [0:shapes.size] do
    ax := ax.scatter ⟨#[Num.ofInt (i + 1 : Nat)]⟩ ⟨#[1]⟩ (marker := shapes[i]!) (markersize := 15)
  ax := ax.scatter ⟨#[1, 2, 3, 4, 5, 6, 7, 8]⟩ ⟨Array.replicate 8 2⟩ (markersize := 12) (color := RGBA.white)
    (strokecolor := RGBA.black) (strokewidth := 1)
  ax := ax.lines ⟨#[0.5, 8.5]⟩ ⟨#[3, 3]⟩ (linestyle := .dash) (color := RGBA.black)
  ax := ax.lines ⟨#[0.5, 8.5]⟩ ⟨#[3.5, 3.5]⟩ (linestyle := .dot) (color := RGBA.black)
  ax := ax.lines ⟨#[0.5, 8.5]⟩ ⟨#[4, 4]⟩ (linestyle := .dashdot) (color := RGBA.black) (linewidth := 2)
  return Figure.new |>.axis 1 1 ax

/-- The 5×5 grid `-1:0.5:1` with x varying fastest (`vec([… for x in g, y in g])`). -/
def arrowGrid : Pts2 × Pts2 :=
  let g : Array Float := #[-1, -0.5, 0, 0.5, 1]
  let pts := (Array.range 25).map fun k => (g[k % 5]!, g[k / 5]!)
  -- `Vec2f(-y, x) * 0.3` promotes to `Float64`
  (Pts2.ofArrays ⟨pts.map (·.1)⟩ ⟨pts.map (·.2)⟩,
   Pts2.ofArrays ⟨pts.map fun (_, y) => -y * 0.3⟩ ⟨pts.map fun (x, _) => x * 0.3⟩)

/-- 15. colour-mapped lines, arrows and an explicit colorbar with a label. -/
def colormapped : Figure :=
  let ts := Num.range 0 (2 * Num.pi) 200
  let (o, d) := arrowGrid
  let ax := Axis2.new
    |>.linesColored (Pts2.ofArrays (fmap Float.cos ts) (fmap Float.sin ts)) ts (linewidth := 4)
    |>.arrows2d o d
  Figure.new |>.axis 1 1 ax |>.colorbarExplicit 1 2 Colormap.viridis 0 (2 * Num.pi) (label := "angle")

/-- 16. an irregular heatmap and an image. -/
def heatmapImage : Figure :=
  let z : Grid2 4 3 := Grid2.ofFn 4 3 fun i j => Num.ofInt (i + j + 2 : Nat)
  let a1 := Axis2.new (title := "irregular") |>.heatmap ⟨#[0, 1, 3, 6, 10]⟩ ⟨#[0, 2, 3, 5]⟩ z (colormap := Colormap.named "inferno")
  let img : ByteArray := (Array.range 12).foldl (init := .empty) fun acc k =>
    RGBA.pushRGBA8 acc (RGBA.toF32 ⟨Num.ofInt (k % 4 + 1 : Nat) / 4, Num.ofInt (k / 4 + 1 : Nat) / 3, 0.5, 1⟩)
  let a2 := Axis2.new (title := "image") (aspect := .data) |>.image 0 4 0 3 4 3 img
  Figure.new (size := (700, 350)) |>.axis 1 1 a1 |>.axis 1 2 a2

/-- 17. a titled horizontal legend below the axis. -/
def legendHorizontal : Figure :=
  let s := every 10 xs
  let ax := Axis2.new
    |>.linesFn Float.sin xs (label := "sin")
    |>.scatter s (fmap Float.cos s) (marker := .rect) (label := "cos")
    |>.band xs (fmap (fun x => Float.sin x - 0.2) xs) (fmap (fun x => Float.sin x + 0.2) xs) (label := "band")
  Figure.new |>.axis 1 1 ax |>.legend 2 1 (1, 1) (title := some "Functions") (orientation := .horizontal)

/-- 18. a spanning title label, log x with minor ticks, reversed x, a horizontal colorbar. -/
def labelLogReversed : Figure :=
  let x := (Array.range 100).foldl (init := FloatArray.empty) fun acc i => acc.push (Num.ofInt (i + 1 : Nat))
  let a1 : Axis2 := Axis2.new (xscale := .log10) |>.lines x (fmap Float.sqrt x)
  let a1 := { a1 with style := { a1.style with x := { a1.style.x with minorticksvisible := true, minorgridvisible := true } } }
  let z : Grid2 10 8 := Grid2.ofFn 10 8 fun i j => Float.sin (Num.ofInt (i + 1 : Nat) / 3) + Float.cos (Num.ofInt (j + 1 : Nat) / 2)
  let a2 : Axis2 := { (Axis2.new (ylabel := "y") |>.heatmap (Recipes.oneTo 10) (Recipes.oneTo 8) z) with xreversed := true }
  Figure.new (size := (700, 450))
    |>.labelSpan 1 1 2 "Super title" (fontsize := 20) (bold := true)
    |>.axis 2 1 a1 |>.axis 2 2 a2
    |>.colorbar 3 2 (2, 2) (label := "value") (vertical := false)

/-- 19. Axis3 with perspective, a wireframe and scatter. -/
def axis3Wire : Figure :=
  let g3 := Num.range (-1) 1 9
  let z := Grid2.ofFn 9 9 fun i j => g3[i]! * g3[j]!
  let ax := Axis3.new (perspectiveness := 0.5)
    |>.wireframe g3 g3 z
    |>.scatter ⟨#[0.5, -0.5, 0]⟩ ⟨#[0.5, 0.5, -0.5]⟩ ⟨#[0.8, 0.2, 0.5]⟩ (markersize := 15)
  Figure.new |>.axis3 1 1 ax

/-- 20. hlines/vlines and `autolimitaspect`. -/
def hvlinesAspect : Figure :=
  let ts := Num.range 0 (2 * Num.pi) 101
  let ax : Axis2 := { (Axis2.new
    |>.lines (fmap Float.cos ts) (fmap (0.5 * Float.sin ·) ts)
    |>.hlines #[0.25, -0.25] (color := ColorSpec.ofName "gray")
    |>.vlines #[0] (color := ColorSpec.ofName "red") (linestyle := .dash)) with autolimitaspect := some 1 }
  Figure.new |>.axis 1 1 ax

/-- 22. rotated tick labels, a clipped colour range with lowclip/highclip triangles. -/
def rotatedClip : Figure :=
  let z : Grid2 12 9 := Grid2.ofFn 12 9 fun i j => Float.sin (Num.ofInt (i + 1 : Nat) / 2) * Float.cos (Num.ofInt (j + 1 : Nat) / 3)
  let ax : Axis2 := Axis2.new (xlabel := "x")
    |>.heatmap (Recipes.oneTo 12) (Recipes.oneTo 9) z (colorrange := some (-0.5, 0.5))
      (lowclip := some (RGBA.rgb 1 0 0)) (highclip := some (RGBA.rgb 0 0 1))
  let ax := { ax with style := { ax.style with x := { ax.style.x with ticklabelrotation := Num.pi / 4 }
                                               y := { ax.style.y with ticklabelrotation := Num.pi / 2 } } }
  Figure.new |>.axis 1 1 ax |>.colorbar 1 2 (1, 1)

/-- 23. a coloured 2D mesh, a stroked polygon, rotated text, colour-mapped scatter and
per-segment colours. -/
def meshPolyText : Figure :=
  let pos := Pts3.ofArrays ⟨#[0, 1, 0.5, 1.5]⟩ ⟨#[0, 0, 1, 1]⟩ ⟨#[0, 0, 0, 0]⟩
  let m := (TriMesh.mk? pos #[0, 1, 2, 1, 3, 2]).getD default
  let segColors : ByteArray := RGBA.pushRGBA8 (RGBA.pushRGBA8 .empty (RGBA.rgb 1 0 0)) (RGBA.rgb 0 0 1)
  let ax := Axis2.new
    |>.mesh m (color := ColorSpec.values ⟨#[1, 2, 3, 4]⟩ {})
    |>.poly (Pts2.ofArrays ⟨#[2, 3, 3, 2]⟩ ⟨#[0, 0, 1, 1]⟩) (color := ColorSpec.ofName "orange") (strokecolor := RGBA.black)
      (strokewidth := 2)
    |>.text 2.5 1.2 "rotated" (fontsize := 18) (halign := .center) (valign := .bottom) (rotation := Num.pi / 6)
    |>.scatter ⟨#[0.5, 1.5, 2.5]⟩ ⟨#[1.5, 1.5, 1.5]⟩ (color := ColorSpec.mapped ⟨#[0, 0.5, 1]⟩ "plasma") (markersize := 20)
    |>.linesegments (Pts2.ofArrays ⟨#[0, 1, 2, 3]⟩ ⟨#[-0.5, -0.5, -0.5, -0.5]⟩) (color := ColorSpec.perElement segColors)
      (linewidth := 3)
  Figure.new |>.axis 1 1 ax

/-- 24. fixed axis sizes, custom ticks and a column gap. -/
def fixedTicks : Figure :=
  let a1 : Axis2 := { (Axis2.new |>.linesFn Float.sin xs) with
    width := .fixed 250, height := .fixed 200, xticks := .values #[0, 2.5, 7]
    yticks := .labeled #[-1, 0, 1] #["low", "mid", "high"] }
  let s := every 5 xs
  let a2 := Axis2.new (title := "gap") |>.scatter s (fmap (· ^ 2) s)
  Figure.new (size := (640, 400)) |>.axis 1 1 a1 |>.axis 1 2 a2 |>.colgap 40

/-- 25. x axis on top, y axis on the right. -/
def flippedAxes : Figure :=
  let ax : Axis2 := { (Axis2.new (title := "flipped") (xlabel := "x") (ylabel := "y") |>.linesFn Float.cos xs) with
    xaxistop := true, yaxisright := true }
  Figure.new |>.axis 1 1 ax

/-- 26. a two-bank legend with a title and styled lines. -/
def legendBanks : Figure := Id.run do
  let mut ax := Axis2.new
  let styles := #[LineStyle.solid, .dash, .dot, .dashdot]
  for k in [0:4] do
    let ph := Num.ofInt (k + 1 : Nat)
    ax := ax.linesFn (fun x => Float.sin (x + ph)) xs (linestyle := styles[k]!) (label := s!"phase {k + 1}")
  return Figure.new |>.axis 1 1 ax
    |>.place 1 2 (.legend { source := .axisAt 1 1, title := some "Phases", style := { nbanks := 2 } })

/-- All oracle figures by name. -/
def specs : Array (String × Figure) := #[
  ("basic", basic), ("empty", empty), ("twoaxes", twoaxes), ("grid22", grid22), ("limits", limits),
  ("heatmap_colorbar", heatmapColorbar), ("legend", legend), ("axislegend", axislegend), ("logy", logy),
  ("dataaspect", dataaspect), ("bandpoly", bandpoly), ("axis3", axis3), ("surface3", surface3),
  ("markers", markers), ("colormapped", colormapped), ("heatmap_image", heatmapImage),
  ("legend_horizontal", legendHorizontal), ("label_log_reversed", labelLogReversed), ("axis3_wire", axis3Wire),
  ("hvlines_aspect", hvlinesAspect), ("rotated_clip", rotatedClip), ("mesh_poly_text", meshPolyText),
  ("fixed_ticks", fixedTicks), ("flipped_axes", flippedAxes),
  ("legend_banks", legendBanks)]

end LeanPlotTest.Figure
