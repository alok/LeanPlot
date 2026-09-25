import LeanPlot.Figure.Layout
import LeanPlot.Scene.Style

/-!
# Makie's default theme

The numbers here were dumped from Makie 0.24 / CairoMakie in the oracle environment
(`LeanPlotTest/oracle/figure/figure_oracle.jl`, `MAKIE_DEFAULT_THEME` and the resolved
attributes of default `Axis`, `Axis3`, `Colorbar` and `Legend` blocks), not guessed:

* figure: size 600×450, `figure_padding = 16`, `rowgap = colgap = 18`, white background;
* text: TeX Gyre Heros, `fontsize = 14` for everything (titles bold);
* plots: `linewidth = 1.5`, `markersize = 9`, Wong palette, `patchcolor` palette
  `lerp(white, wong, 0.8)`, colormap viridis;
* `Axis`: ticks 5 px long / 1 px wide outside, tick label pads 2 (x) and 4 (y), label
  paddings 3 (x) and 5 (y), title gap 4, spines 1 px black, grid 1 px `RGBAf(0,0,0,0.12)`,
  minor grid `RGBAf(0,0,0,0.05)`, autolimit margins `0.05f0`.
-/

namespace LeanPlot

open LeanPlot.Num

/-- `0.05f0` as a `Float` (Makie's autolimit margin, stored as `Float32`). -/
def margin05 : Float := (0.05 : Float32).toFloat

/-- Figure-wide defaults. -/
structure Theme where
  fontsize : Float := 14
  figurePadding : Layout.Sides := Layout.Sides.uniform 16
  rowgap : Float := 18
  colgap : Float := 18
  backgroundColor : RGBA := RGBA.white
  textColor : RGBA := RGBA.black
  linewidth : Float := 1.5
  markersize : Float := 9
  /-- Categorical palette for lines, scatter, linesegments (`palette.color`). -/
  palette : Array RGBA := wongColors
  colormap : Colormap := Colormap.viridis
  deriving Inhabited

namespace Theme

/-- Makie's `patchcolor` palette: `lerp(background, c, 0.8f0)` for each palette colour. -/
def patchPalette (t : Theme) : Array RGBA :=
  let bg := t.backgroundColor
  let k : Float := (0.8 : Float32).toFloat
  t.palette.map fun c =>
    RGBA.toF32 ⟨bg.r + (c.r - bg.r) * k, bg.g + (c.g - bg.g) * k, bg.b + (c.b - bg.b) * k, bg.a + (c.a - bg.a) * k⟩

/-- Palette colour `i` (cycling). -/
def color (t : Theme) (i : Nat) : RGBA :=
  if t.palette.isEmpty then RGBA.black else t.palette[i % t.palette.size]!

/-- Patch palette colour `i` (cycling). -/
def patchColor (t : Theme) (i : Nat) : RGBA :=
  let p := t.patchPalette
  if p.isEmpty then RGBA.gray 0.4 else p[i % p.size]!

end Theme

/-- Styling of one axis direction (Makie's `LineAxis` attributes of `Axis`). -/
structure LineAxisStyle where
  ticksize : Float := 5
  tickwidth : Float := 1
  tickcolor : RGBA := RGBA.black
  /-- 0 = ticks outside, 1 = inside. -/
  tickalign : Float := 0
  ticksvisible : Bool := true
  ticklabelsize : Float := 14
  ticklabelcolor : RGBA := RGBA.black
  ticklabelpad : Float := 2
  ticklabelsvisible : Bool := true
  ticklabelrotation : Float := 0
  /-- Fixed tick label space (`none` = automatic). -/
  ticklabelspace : Option Float := none
  labelsize : Float := 14
  labelcolor : RGBA := RGBA.black
  labelpadding : Float := 3
  labelvisible : Bool := true
  gridvisible : Bool := true
  gridwidth : Float := 1
  gridcolor : RGBA := MakieTheme.gridColor
  gridstyle : LineStyle := .solid
  minorgridvisible : Bool := false
  minorgridwidth : Float := 1
  minorgridcolor : RGBA := MakieTheme.minorGridColor
  minorticksvisible : Bool := false
  minorticksize : Float := 3
  minortickwidth : Float := 1
  minortickcolor : RGBA := RGBA.black
  minortickalign : Float := 0
  /-- `IntervalsBetween(n)` for minor ticks. -/
  minorIntervals : Nat := 2
  deriving Inhabited

/-- Styling of an `Axis` (Makie defaults). -/
structure AxisStyle where
  x : LineAxisStyle := {}
  y : LineAxisStyle := { ticklabelpad := 4, labelpadding := 5 }
  titlesize : Float := 14
  titlegap : Float := 4
  titlecolor : RGBA := RGBA.black
  titlebold : Bool := true
  titlevisible : Bool := true
  /-- 0 left, 0.5 centre, 1 right. -/
  titlealign : Float := 0.5
  spinewidth : Float := 1
  spinecolor : RGBA := RGBA.black
  leftspinevisible : Bool := true
  rightspinevisible : Bool := true
  bottomspinevisible : Bool := true
  topspinevisible : Bool := true
  backgroundcolor : RGBA := RGBA.white
  deriving Inhabited

end LeanPlot
