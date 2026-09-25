import LeanPlotTest.Core.Harness
import LeanPlot.Core.Camera

/-!
Geometry: clipping unit tests and the `Axis3` camera against Makie
(`camera.json`): `calculate_matrices` for ~85 view settings (matrix entries
and projected pixels), and real `Axis3` figures (scene viewport and camera
after layout, projected sample points).
-/

namespace LeanPlotTest.Core.GeometryTest

open LeanPlot LeanPlot.Num

/-- Relative/absolute closeness. -/
def close (tol a b : Float) : Bool := (a - b).abs ≤ tol * (1 + b.abs)

/-- Clipping unit tests. -/
def clipping : TestM Unit := do
  let r : Rect := ⟨0, 0, 10, 10⟩
  check "outcode inside" (Clip.outcode r 5 5 == 0) fun _ => "inside"
  check "outcode left-top" (Clip.outcode r (-1) (-1) == 5) fun _ => s!"{Clip.outcode r (-1) (-1)}"
  check "trivial reject" (Clip.triviallyOutside r (-5) 1 (-1) 9) fun _ => "reject"
  match Clip.segment r (-5) 5 15 5 with
  | some (a, b) => check "LB horizontal" (a.x == 0 && b.x == 10 && a.y == 5 && b.y == 5) fun _ => s!"{a.x},{b.x}"
  | none => check "LB horizontal" false fun _ => "none"
  check "LB miss" (Clip.segment r (-5) (-5) (-1) 20).isNone fun _ => "hit"
  match Clip.segment r (-10) (-10) 20 20 with
  | some (a, b) => check "LB diagonal" (a.x == 0 && a.y == 0 && b.x == 10 && b.y == 10) fun _ => s!"{a.x},{a.y},{b.x},{b.y}"
  | none => check "LB diagonal" false fun _ => "none"
  check "LB NaN" (Clip.segment r nan 1 2 3).isNone fun _ => "NaN accepted"
  -- polyline: in → out → in gives two pieces separated by NaN
  let xs : FloatArray := ⟨#[1, 5, 15, 5, 2]⟩
  let ys : FloatArray := ⟨#[1, 5, 5, 8, 8]⟩
  let (ox, oy) := Clip.polyline r xs ys
  let want : Array Float := #[1, 5, 10, nan, 10, 5, 2]
  check "polyline split" (floatsBitEq ox.data want && oy.size == 7) fun _ => showFloats ox.data
  let (px, _) := Clip.polygon r ⟨#[-5, 5, 15]⟩ ⟨#[5, -5, 5]⟩
  check "polygon clip" (px.size ≥ 3) fun _ => showFloats px.data
  let u := Rect.union ⟨0, 0, 1, 1⟩ ⟨2, 3, 1, 1⟩
  check "rect union" (u.x == 0 && u.y == 0 && u.w == 3 && u.h == 4) fun _ => s!"{u.w} {u.h}"
  check "rect disjoint" (Rect.intersect? ⟨0, 0, 1, 1⟩ ⟨2, 2, 1, 1⟩).isNone fun _ => "intersects"
  match Rect.bounds? ⟨#[1, nan, 3, -2]⟩ ⟨#[0, 5, 4, 1]⟩ with
  | some b => check "bounds skip NaN" (b.x == -2 && b.w == 5 && b.y == 0 && b.h == 4) fun _ => s!"{b.x} {b.w} {b.y} {b.h}"
  | none => check "bounds skip NaN" false fun _ => "none"

/-- Parse the view settings of a matrix case. -/
def viewOf (c : J) : Axis3View :=
  let prot := (c.get "protrusions").floats
  let rev := (c.get "reversed").arrD.map J.boolean
  let off := (c.get "offset").floats
  let aspect : Aspect3 := match c.get "aspect" with
    | .str "data" => .data
    | .str "equal" => .equal
    | j => let a := j.floats; .ratio a[0]! a[1]! a[2]!
  let mode : ViewMode := match (c.get "viewmode").string with
    | "fit" => .fit | "stretch" => .stretch | "free" => .free | _ => .fitzoom
  { azimuth := (c.get "azimuth").float, elevation := (c.get "elevation").float,
    perspectiveness := (c.get "perspectiveness").float, aspect, viewmode := mode,
    protrusions := ⟨prot[0]!, prot[1]!, prot[2]!, prot[3]!⟩,
    xreversed := rev[0]!, yreversed := rev[1]!, zreversed := rev[2]!, offset := ⟨off[0]!, off[1]!⟩ }

/-- Camera oracle. -/
def camera : TestM Unit := do
  let some j ← loadOracle "camera.json" | return
  let mut idx := 0
  for c in (j.get "matrices").arrD do
    let o := (c.get "origin").floats
    let w := (c.get "widths").floats
    let vp := (c.get "viewport").floats
    let lim : Rect3 := ⟨⟨o[0]!, o[1]!, o[2]!⟩, ⟨w[0]!, w[1]!, w[2]!⟩⟩
    let cam := Camera3.ofLimits (viewOf c) lim ⟨0, 0, vp[0]!, vp[1]!⟩
    for (nm, m) in [("model", cam.model), ("view", cam.view), ("proj", cam.proj)] do
      let want := (c.get nm).floats
      let got := m.toColumnMajor
      check s!"camera[{idx}] {nm}" ((Array.range 16).all fun k => close 1e-9 got[k]! want[k]!)
        fun _ => s!"got {showFloats got}, want {showFloats want}"
    let pts := (c.get "points").arrD.map J.floats
    let pix := (c.get "pixels").arrD.map J.floats
    for k in [0:pts.size] do
      let p := pts[k]!
      let q := cam.project ⟨p[0]!, p[1]!, p[2]!⟩
      let want := pix[k]!
      check s!"camera[{idx}] project {k}" (close 1e-7 q.x want[0]! && close 1e-7 q.y want[1]! && close 1e-7 q.z want[2]!)
        fun _ => s!"got ({q.x}, {q.y}, {q.z}), want {showFloats want}"
    idx := idx + 1
  for c in (j.get "figures").arrD do
    let o := (c.get "finallimits_origin").floats
    let w := (c.get "finallimits_widths").floats
    let vp := (c.get "viewport").floats
    let st := c.get "settings"
    let v : Axis3View := {
      azimuth := (st.get? "azimuth").map J.float |>.getD (1.275 * Num.pi),
      elevation := (st.get? "elevation").map J.float |>.getD (Num.pi / 8),
      perspectiveness := (st.get? "perspectiveness").map J.float |>.getD 0,
      aspect := if (st.get? "aspect").map J.string == some "data" then .data else .ratio 1 1 (2 / 3) }
    let lim : Rect3 := ⟨⟨o[0]!, o[1]!, o[2]!⟩, ⟨w[0]!, w[1]!, w[2]!⟩⟩
    let cam := Camera3.ofLimits v lim ⟨vp[0]!, vp[1]!, vp[2]!, vp[3]!⟩
    let pts := (c.get "points").arrD.map J.floats
    let pix := (c.get "pixels").arrD.map J.floats
    for k in [0:pts.size] do
      let p := pts[k]!
      let q := cam.project ⟨p[0]!, p[1]!, p[2]!⟩
      let want := pix[k]!
      check s!"figure {(c.get "size").floats} point {k}" ((q.x - want[0]!).abs ≤ 1e-6 && (q.y - want[1]!).abs ≤ 1e-6)
        fun _ => s!"got ({q.x}, {q.y}), want {showFloats want}"

/-- The geometry suite. -/
def suite : TestM Unit := do
  clipping
  camera

end LeanPlotTest.Core.GeometryTest
