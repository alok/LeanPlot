import LeanPlotTest.Recipes.Harness
import LeanPlot.Recipes.Algo.Streamplot

/-!
Oracle tests for `LeanPlot.Recipes.Algo.Stream` against `streamplot.json`
(Makie `streamplot_impl` outputs). The field functions mirror
`streamplot_oracle.jl` operation for operation, so traces are compared
bit-exactly: counts, arrow positions/directions, every line point (with the
`NaN` separators) and every colour value.
-/

namespace LeanPlotTest.Recipes.StreamTest

open LeanPlot
open LeanPlot.Recipes.Algo
open LeanPlotTest.Recipes

/-- The oracle's fields, by name (3D fields; 2D ones ignore z). -/
def field (name : String) : Vec3 → Vec3 := fun p =>
  match name with
  | "rotation" => ⟨-p.y, p.x, 0⟩
  | "saddle" => ⟨p.x, -p.y, 0⟩
  | "source" => ⟨p.x, p.y, 0⟩
  | "dipole" =>
    let ax := p.x + 0.5
    let bx := p.x - 0.5
    let y := p.y
    let ra := ax * ax + y * y
    let rb := bx * bx + y * y
    let ka := ra * Float.sqrt ra
    let kb := rb * Float.sqrt rb
    ⟨ax / ka - bx / kb, y / ka - y / kb, 0⟩
  | "f32field" => ⟨-p.y + 0.3 * p.x, p.x, 0⟩
  | "spiral32" => ⟨-p.y - 0.2 * p.x, p.x - 0.2 * p.y, 0⟩
  | "vortex3" | "vortex3pad" => ⟨-p.y, p.x, 0.3⟩
  | "tangent3" => ⟨1.0 - 2.0 * p.y, p.x * p.y + 0.2, 0.0⟩
  | _ => ⟨0, 0, 0⟩

/-- Coordinate arrays of a point set, by dimension index. -/
def coord (p : Pts3) (k : Nat) : FloatArray := if k == 0 then p.xs else if k == 1 then p.ys else p.zs

/-- Run the suite. -/
def suite : TestM Unit := do
  let some j ← loadOracle "streamplot.json" | return
  -- quasi-random sequence coefficients
  let a2 := #[LeanPlot.Num.powInt (Stream.phi 2) (-1), LeanPlot.Num.powInt (Stream.phi 2) (-2)]
  check "acoeff2" (floatsBitEq a2 (j.get "acoeff2").floats) (fun _ => s!"{a2}")
  let a3 := #[LeanPlot.Num.powInt (Stream.phi 3) (-1), LeanPlot.Num.powInt (Stream.phi 3) (-2),
    LeanPlot.Num.powInt (Stream.phi 3) (-3)]
  check "acoeff3" (floatsBitEq a3 (j.get "acoeff3").floats) (fun _ => s!"{a3}")
  check "recipe == impl" (j.get "recipe_matches_impl").boolean
  for c in (j.get "cases").arrD do
    let name := (c.get "name").string
    let dim := (c.get "dim").nat
    let o := (c.get "origin").floats
    let w := (c.get "widths").floats
    let opts : Stream.Options := {
      gridsize := (c.get "gridsize").arrD.map (·.nat)
      stepsize := (c.get "stepsize").float
      maxsteps := (c.get "maxsteps").nat
      density := (c.get "density").float
      limitsF32 := (c.get "f32limits").boolean
      fieldF32 := name == "f32field" }
    let f := field name
    let r := Stream.run dim (fun p => let q := f p; (q.x, q.y, q.z)) ⟨o[0]!, o[1]!, o.getD 2 0⟩ ⟨w[0]!, w[1]!, w.getD 2 0⟩ opts
    check s!"{name} narrows" (r.arrowPos.size == (c.get "narrows").nat)
      (fun _ => s!"got {r.arrowPos.size} want {(c.get "narrows").nat}")
    check s!"{name} npoints" (r.linePoints.size == (c.get "npoints").nat)
      (fun _ => s!"got {r.linePoints.size} want {(c.get "npoints").nat}")
    for k in List.range dim do
      let ap := floatArr ((c.get "arrow_pos").arrD[k]!)
      let ad := floatArr ((c.get "arrow_dir").arrD[k]!)
      let lp := floatArr ((c.get "line_points").arrD[k]!)
      check s!"{name} arrow_pos[{k}]" (faBitEq (coord r.arrowPos k) ap)
        (fun _ => s!"maxdiff {maxAbsDiff (coord r.arrowPos k) ap}")
      check s!"{name} arrow_dir[{k}]" (faBitEq (coord r.arrowDir k) ad)
        (fun _ => s!"maxdiff {maxAbsDiff (coord r.arrowDir k) ad}")
      check s!"{name} line_points[{k}]" (faBitEq (coord r.linePoints k) lp)
        (fun _ => s!"maxdiff {maxAbsDiff (coord r.linePoints k) lp} sizes {(coord r.linePoints k).size} {lp.size}")
    check s!"{name} arrow_colors" (faBitEq r.arrowColors (floatArr (c.get "arrow_colors")))
      (fun _ => s!"maxdiff {maxAbsDiff r.arrowColors (floatArr (c.get "arrow_colors"))}")
    check s!"{name} line_colors" (faBitEq r.lineColors (floatArr (c.get "line_colors")))
      (fun _ => s!"maxdiff {maxAbsDiff r.lineColors (floatArr (c.get "line_colors"))}")

end LeanPlotTest.Recipes.StreamTest
