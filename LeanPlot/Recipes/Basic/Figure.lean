import LeanPlot.Recipes.Basic.Axis3

/-!
# Figure builders and one-liners

```lean
open LeanPlot in
def fig : Figure :=
  let xs := Num.range 0 10 101
  let ax := Axis2.new (title := "waves") (xlabel := "t")
    |>.linesFn Float.sin xs (label := "sin")
    |>.linesFn Float.cos xs (label := "cos")
  Figure.new (size := (600, 450)) |>.axis 1 1 ax |>.legend 1 2 (1, 1)
```

`Figure.axis`, `axis3`, `colorbar`, `legend`, `label` place blocks (1-based cells, as
`f[i, j]` in Makie); `LeanPlot.Quick.lines`/`scatter`/`heatmap`/`surface` build a whole
figure with one axis, like Makie's non-mutating `lines(xs, ys)`.
-/

namespace LeanPlot

open LeanPlot.Num

namespace Figure

/-- `f[r, c] = ax` for a 2D axis. -/
def axis (f : Figure) (r c : Nat) (ax : Axis2) : Figure := f.place r c (.axis2 ax)

/-- `f[r, c] = ax` for a 3D axis. -/
def axis3 (f : Figure) (r c : Nat) (ax : Axis3) : Figure := f.place r c (.axis3 ax)

/-- `Colorbar(f[r, c], plot)`: a colorbar for the first colour-mapped plot of the axis at
cell `(axRow, axCol)` (or its `index`-th one). -/
def colorbar (f : Figure) (r c : Nat) (ax : Nat × Nat) (label : String := "") (index : Option Nat := none)
    (vertical : Bool := true) : Figure :=
  f.place r c (.colorbar { source := .axisAt ax.1 ax.2 index, label, vertical })

/-- `Colorbar(f[r, c], colormap = …, limits = (lo, hi))`. -/
def colorbarExplicit (f : Figure) (r c : Nat) (colormap : Colormap) (lo hi : Float) (label : String := "")
    (vertical : Bool := true) : Figure :=
  f.place r c (.colorbar { source := .explicit { colormap } lo hi, label, vertical })

/-- `Legend(f[r, c], ax)`: the labelled plots of the axis at cell `ax`. -/
def legend (f : Figure) (r c : Nat) (ax : Nat × Nat) (title : Option String := none)
    (orientation : Orientation := .vertical) : Figure :=
  f.place r c (.legend { source := .axisAt ax.1 ax.2, title, style := { orientation } })

/-- `Label(f[r, c], text)`. -/
def label (f : Figure) (r c : Nat) (text : String) (fontsize : Float := 14) (bold : Bool := false) : Figure :=
  f.place r c (.label { text, size := fontsize, bold })

/-- `Label(f[r, c0:c1], text)` spanning columns. -/
def labelSpan (f : Figure) (r c0 c1 : Nat) (text : String) (fontsize : Float := 14) (bold : Bool := false) : Figure :=
  f.placeSpan r r c0 c1 (.label { text, size := fontsize, bold })

/-- Write frames `0, …, n-1` of an animation (a figure per frame index) as
`dir/frame_0001.ext`, … (Makie's `record` without a video encoder). -/
def saveFrames (frame : Nat → Figure) (n : Nat) (dir : System.FilePath) (ext : String := "png") : IO Unit := do
  IO.FS.createDirAll dir
  for i in [0:n] do
    let num := toString (i + 1)
    let name := "frame_" ++ "".pushn '0' (4 - min 4 num.length) ++ num ++ "." ++ ext
    (frame i).save (dir / name)

end Figure

namespace Quick

/-- `lines(xs, ys)`: a figure with one axis. -/
def lines (xs ys : FloatArray) (size : Nat × Nat := (600, 450)) : Figure :=
  Figure.new size |>.axis 1 1 (Axis2.new |>.lines xs ys)

/-- `scatter(xs, ys)`. -/
def scatter (xs ys : FloatArray) (size : Nat × Nat := (600, 450)) : Figure :=
  Figure.new size |>.axis 1 1 (Axis2.new |>.scatter xs ys)

/-- `heatmap(xs, ys, z)` with a colorbar. -/
def heatmap {nx ny : Nat} (xs ys : FloatArray) (z : Grid2 nx ny) (colormap : Colormap := Colormap.viridis)
    (size : Nat × Nat := (600, 450)) : Figure :=
  Figure.new size |>.axis 1 1 (Axis2.new |>.heatmap xs ys z colormap) |>.colorbar 1 2 (1, 1)

/-- `surface(xs, ys, z)` in an `Axis3`. -/
def surface {nx ny : Nat} (xs ys : FloatArray) (z : Grid2 nx ny) (size : Nat × Nat := (600, 450)) : Figure :=
  Figure.new size |>.axis3 1 1 (Axis3.new |>.surface xs ys z)

end Quick

end LeanPlot
