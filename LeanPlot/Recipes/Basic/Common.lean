import LeanPlot.Figure.Figure

/-!
# Shared helpers for the basic recipes

* `ColorArg`-style coercions: an `RGBA` or a colour name can be given where a `ColorSpec`
  is expected;
* `edges`: Makie's `edges(v)` (cell centres → cell edges) for heatmaps;
* `surfaceMesh`: a `Grid2` surface as a triangle mesh (Makie `surface2mesh`), plus the
  grid-line segments of `wireframe`;
* `rowsTopDown`: an RGBA grid (`z[i, j]`, `i ↔ x`) as image rows, top row first.
-/

namespace LeanPlot

open LeanPlot.Num

/-- A plain colour is a solid colour spec. -/
instance : Coe RGBA ColorSpec := ⟨.solid⟩

namespace ColorSpec
/-- A solid colour by Makie/Colors.jl name or hex (`"red"`, `"#1f77b4"`); black if unknown. -/
def ofName (s : String) : ColorSpec := .solid ((RGBA.parse? s).getD RGBA.black)
/-- Values mapped through a named colormap (Makie `color = values, colormap = :name`). -/
def mapped (vs : FloatArray) (colormap : String := "viridis") (colorrange : Option (Float × Float) := none) : ColorSpec :=
  .values vs { colormap := Colormap.named colormap, colorrange }
end ColorSpec

namespace Recipes

/-- Makie `edges(v)`: cell edges from cell centres (half a step beyond the ends). -/
def edges (v : FloatArray) : FloatArray :=
  let l := v.size
  if l == 0 then .empty
  else if l == 1 then ⟨#[v[0]! - 0.5, v[0]! + 0.5]⟩
  else
    let b : Array Float := (Array.range (l + 1)).map fun i =>
      let a : Float := v[if i == 0 then 0 else i - 1]!
      let c : Float := v[min (l - 1) i]!
      0.5 * (a + c)
    let b := b.set! 0 (fTwo * b[0]! - b[1]!)
    let b := b.set! l (fTwo * b[l]! - b[l - 1]!)
    ⟨b⟩

/-- Cell edges for a heatmap: given `n` centres or `n + 1` edges. -/
def cellEdges (v : FloatArray) (n : Nat) : FloatArray :=
  if v.size == n + 1 then v else edges v

/-- `1, 2, …, n` as floats. -/
def oneTo (n : Nat) : FloatArray := ⟨(Array.range n).map fun i => Num.ofInt (i + 1 : Nat)⟩

/-- The mesh of a surface over the grid `xs × ys` with heights `z` (two triangles per
cell, `(a, b, c), (a, c, d)` for the cell corners `a = (i, j)`, `b = (i+1, j)`,
`c = (i+1, j+1)`, `d = (i, j+1)`). -/
def surfaceMesh (xs ys : FloatArray) (g : AnyGrid2) : TriMesh :=
  let nx := g.nx
  let ny := g.ny
  let n := nx * ny
  let px := (Array.range n).map fun k => xs.get! (k % nx)
  let py := (Array.range n).map fun k => ys.get! (k / nx)
  let pz := (Array.range n).map fun k => g.grid.get! (k % nx) (k / nx)
  let pos := Pts3.ofArrays ⟨px⟩ ⟨py⟩ ⟨pz⟩
  let tri := (Array.range ((nx - 1) * (ny - 1))).foldl (init := #[]) fun acc c =>
    let i := c % (nx - 1)
    let j := c / (nx - 1)
    let a := (i + nx * j).toUInt32
    let b := (i + 1 + nx * j).toUInt32
    let cc := (i + 1 + nx * (j + 1)).toUInt32
    let d := (i + nx * (j + 1)).toUInt32
    ((((((acc.push a).push b).push cc).push a).push cc).push d)
  (TriMesh.mk? pos tri).getD default

/-- The grid lines of a surface as segment pairs (x-direction lines first). -/
def wireframeSegments (xs ys : FloatArray) (g : AnyGrid2) : Pts3 :=
  let nx := g.nx
  let ny := g.ny
  let pt (i j : Nat) : Vec3 := ⟨xs.get! i, ys.get! j, g.grid.get! i j⟩
  let alongX := (Array.range ((nx - 1) * ny)).foldl (init := #[]) fun acc k =>
    let i := k % (nx - 1)
    let j := k / (nx - 1)
    (acc.push (pt i j)).push (pt (i + 1) j)
  let alongY := (Array.range (nx * (ny - 1))).foldl (init := alongX) fun acc k =>
    let i := k % nx
    let j := k / nx
    (acc.push (pt i j)).push (pt i (j + 1))
  Pts3.ofArrays ⟨alongY.map (·.x)⟩ ⟨alongY.map (·.y)⟩ ⟨alongY.map (·.z)⟩

/-- An `nx × ny` grid of colours (`rgba` in `z[i, j]` order, `i ↔ x`, 4 bytes each) as image
rows with the top row (largest `j`) first. -/
def rowsTopDown (nx ny : Nat) (rgba : ByteArray) : ByteArray :=
  (Array.range (nx * ny)).foldl (init := ByteArray.emptyWithCapacity (4 * nx * ny)) fun acc k =>
    let row := k / nx
    let i := k % nx
    let j := ny - 1 - row
    let o := 4 * (i + nx * j)
    (((acc.push (rgba.get! o)).push (rgba.get! (o + 1))).push (rgba.get! (o + 2))).push (rgba.get! (o + 3))

end Recipes

end LeanPlot
