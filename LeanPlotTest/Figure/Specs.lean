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

/-- All oracle figures by name. -/
def specs : Array (String × Figure) := #[
  ("basic", basic), ("empty", empty), ("twoaxes", twoaxes), ("grid22", grid22), ("limits", limits),
  ("heatmap_colorbar", heatmapColorbar), ("legend", legend), ("axislegend", axislegend), ("logy", logy),
  ("dataaspect", dataaspect), ("bandpoly", bandpoly), ("axis3", axis3), ("surface3", surface3)]

end LeanPlotTest.Figure
