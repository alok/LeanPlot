import LeanPlotTest.Raster.Util

/-!
Coverage-rasterizer invariants: exact coverage for axis-aligned rectangles,
circle area, analytic antialiasing values on edges, fill rules, clipping, and
robustness to NaN or huge coordinates.
-/

namespace LeanPlotTest.Raster.FillTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Expected byte after blending black with coverage `c` over white. -/
def expectByte (c : Float) : Nat := (255.0 - 255.0 * c + 0.5).floor.toUInt64.toNat

/-- Fill `p` in black on a white canvas. -/
def fillBlack (w h : Nat) (p : Path) (rule : FillRule := .nonzero) (clip : Option Rect := none) : Canvas w h :=
  paint w h #[.path p (some { color := .black, rule }) none clip]

/-- All fill checks. -/
def tests : T Unit := do
  -- integer rectangle: every pixel is exactly 0 or 1
  let cv := fillBlack 100 80 (Path.rect ⟨10, 20, 50, 30⟩)
  checkNear "int rect area" (darkArea cv) 1500.0 1.0e-9
  check "int rect inside" ((cv.get 10 20).r == 0 && (cv.get 59 49).r == 0)
  check "int rect outside" ((cv.get 9 20).r == 255 && (cv.get 60 49).r == 255 && (cv.get 10 50).r == 255)
  -- fractional rectangle: edge pixels carry the exact covered fraction
  let cv := fillBlack 100 80 (Path.rect ⟨10.25, 20.5, 50.0, 30.0⟩)
  check "frac rect left edge 0.75" ((cv.get 10 30).r.toNat == expectByte 0.75) s!"{(cv.get 10 30).r}"
  check "frac rect right edge 0.25" ((cv.get 60 30).r.toNat == expectByte 0.25) s!"{(cv.get 60 30).r}"
  check "frac rect top edge 0.5" ((cv.get 30 20).r.toNat == expectByte 0.5)
  check "frac rect bottom edge 0.5" ((cv.get 30 50).r.toNat == expectByte 0.5)
  check "frac rect corner 0.375" ((cv.get 10 20).r.toNat == expectByte 0.375) s!"{(cv.get 10 20).r}"
  checkNear "frac rect area" (darkArea cv) 1500.0 0.5
  -- a rectangle given clockwise and counter-clockwise gives the same result
  let ccw := fillBlack 100 80 (polygon [(10.25, 20.5), (10.25, 50.5), (60.25, 50.5), (60.25, 20.5)])
  check "orientation independent" (ccw.data == cv.data)
  -- circles (four cubics each): area within 0.5 % (1 % for a 2.5 px marker, where
  -- 8-bit rounding of ~20 edge pixels alone is worth 0.2 %)
  for (cx, cy, r, tol) in [(100.3, 75.7, 40.0, 0.005), (61.1, 58.9, 30.3, 0.005), (20.5, 20.25, 7.5, 0.005),
                           (150.0, 100.0, 2.5, 0.01)] do
    let cv := fillBlack 200 150 (circlePath cx cy r)
    let a := darkArea cv
    let exact := 3.141592653589793 * r * r
    check s!"circle r={r} area within {tol * 100.0}%" ((a - exact).abs / exact ≤ tol) s!"{a} vs {exact}"
  -- diagonal edge through pixel corners: exactly half covered
  let cv := fillBlack 120 120 (polygon [(0, 0), (100, 0), (0, 100)])
  let mut diagOk := true
  for x in [5:95] do
    if (cv.get x (99 - x)).r.toNat != expectByte 0.5 then diagOk := false
  check "diagonal edge = 0.5" diagOk
  checkNear "triangle area" (darkArea cv) 5000.0 0.5
  -- shallow slope: pixel coverage equals the trapezoid area under the edge
  -- edge from (0, 10.2) to (100, 10.7): in column x the covered height of row 10 is 1 - (10.2 + 0.005(x+0.5) - 10)
  let cv := fillBlack 120 30 (polygon [(0, 10.2), (100, 10.7), (100, 25), (0, 25)])
  let mut slopeOk := true
  for x in [0:100] do
    let yMid := 10.2 + 0.005 * (x.toFloat + 0.5)
    let cov := 11.0 - yMid
    let got := (cv.get x 10).r.toNat
    let want := expectByte cov
    if got + 1 < want || got > want + 1 then slopeOk := false
  check "sloped edge coverage" slopeOk
  -- fill rules: two overlapping squares drawn in the same direction
  let two := (polygon [(10, 10), (60, 10), (60, 60), (10, 60)]) |> fun p =>
    (((((p.moveTo 40 40).lineTo 90 40).lineTo 90 90).lineTo 40 90).close)
  checkNear "nonzero union" (darkArea (fillBlack 100 100 two .nonzero)) (2500.0 * 2 - 400) 1.0e-9
  checkNear "even-odd xor" (darkArea (fillBlack 100 100 two .evenOdd)) (2500.0 * 2 - 800) 1.0e-9
  -- nested squares in opposite directions make a hole under both rules
  let hole := (polygon [(10, 10), (90, 10), (90, 90), (10, 90)]) |> fun p =>
    (((((p.moveTo 30 30).lineTo 30 70).lineTo 70 70).lineTo 70 30).close)
  checkNear "nonzero hole" (darkArea (fillBlack 100 100 hole .nonzero)) (6400.0 - 1600) 1.0e-9
  checkNear "even-odd hole" (darkArea (fillBlack 100 100 hole .evenOdd)) (6400.0 - 1600) 1.0e-9
  -- winding 3 (three copies of one square) is still coverage 1 under nonzero
  let triple := (polygon [(10, 10), (60, 10), (60, 60), (10, 60)]) |> fun p =>
    ((((((((p.moveTo 10 10).lineTo 60 10).lineTo 60 60).lineTo 10 60).close.moveTo 10 10).lineTo 60 10).lineTo 60 60).lineTo 10 60).close
  checkNear "nonzero clamps winding" (darkArea (fillBlack 100 100 triple .nonzero)) 2500.0 1.0e-9
  checkNear "even-odd odd winding" (darkArea (fillBlack 100 100 triple .evenOdd)) 2500.0 1.0e-9
  -- clipping: integer, fractional and fully outside
  let big := Path.rect ⟨-50, -50, 300, 300⟩
  checkNear "int clip" (darkArea (fillBlack 100 100 big .nonzero (some ⟨10, 20, 30, 40⟩))) 1200.0 1.0e-9
  checkNear "frac clip area" (darkArea (fillBlack 100 100 big .nonzero (some ⟨10.5, 20.25, 30.0, 40.0⟩))) 1200.0 0.5
  let cvc := fillBlack 100 100 big .nonzero (some ⟨10.5, 20.0, 30.0, 40.0⟩)
  check "frac clip edge 0.5" ((cvc.get 10 30).r.toNat == expectByte 0.5 && (cvc.get 40 30).r.toNat == expectByte 0.5)
  checkNear "negative-extent clip" (darkArea (fillBlack 100 100 big .nonzero (some ⟨40, 60, -30, -40⟩))) 1200.0 1.0e-9
  checkNear "clip outside canvas" (darkArea (fillBlack 100 100 big .nonzero (some ⟨200, 200, 10, 10⟩))) 0.0 1.0e-9
  -- geometry left of the canvas still winds correctly
  checkNear "left overhang" (darkArea (fillBlack 100 100 (Path.rect ⟨-1000.0, 10, 1050.5, 20⟩))) (50.5 * 20) 0.5
  checkNear "full overhang" (darkArea (fillBlack 100 100 big)) 10000.0 1.0e-9
  -- robustness: NaN, infinities and huge coordinates
  let nanP := polygon [(10, 10), (Float.nan, 50), (60, 60), (10, 60)]
  let cvn := fillBlack 100 100 nanP
  check "nan path finite" (darkArea cvn ≥ 0.0 && darkArea cvn ≤ 10000.0)
  let (cvh, ms) ← timeMs (IO.lazyPure fun _ => fillBlack 100 100 (polygon [(-1.0e12, -1.0e12), (1.0e12, 5.0), (50, 1.0e12)]))
  check "huge coords bounded" (ms < 500.0 && darkArea cvh ≤ 10000.0) s!"{ms} ms"
  let cvi := fillBlack 100 100 (polygon [(10, 10), (Float.inf, 20), (20, 80)])
  check "inf path no crash" (darkArea cvi ≤ 10000.0)
  checkNear "degenerate zero-area" (darkArea (fillBlack 100 100 (polygon [(10, 10), (50, 50), (90, 90)]))) 0.0 1.0e-9
  checkNear "empty path" (darkArea (fillBlack 100 100 {})) 0.0 1.0e-9
  -- quadratic Béziers: a parabolic segment's area (∫ of the hull triangle × 2/3)
  let q := (((({} : Path).moveTo 10 90).quadTo 50 10 90 90).close)
  checkNear "quad bezier area" (darkArea (fillBlack 100 100 q)) (2.0 / 3.0 * 0.5 * 80.0 * 80.0) 3.0
  -- translucent fill: exact src-over
  let cvt := paint 20 20 #[.path (Path.rect ⟨0, 0, 20, 20⟩) (some { color := ⟨1, 0, 0, 0.5⟩ }) none none]
  let p := cvt.get 5 5
  check "alpha 0.5 red over white" (p.r == 255 && p.g == 128 && p.b == 128 && p.a == 255) s!"{repr p}"
  -- blending onto a transparent canvas keeps straight alpha
  let cvz := (Canvas.transparent 10 10).drawOps #[.path (Path.rect ⟨0, 0, 10, 10⟩) (some { color := ⟨0.2, 0.4, 0.6, 0.5⟩ }) none none]
  let pz := cvz.get 3 3
  check "straight alpha on transparent" (pz.r == 51 && pz.g == 102 && pz.b == 153 && pz.a == 128) s!"{repr pz}"
  let cvz2 := cvz.drawOps #[.path (Path.rect ⟨0, 0, 10, 10⟩) (some { color := ⟨0.2, 0.4, 0.6, 0.5⟩ }) none none]
  let pz2 := cvz2.get 3 3
  -- 0.5 over (stored) 128/255 = 0.75098… → 191.5 → 192
  check "straight alpha composes (0.75)" (pz2.r == 51 && pz2.g == 102 && pz2.b == 153 && pz2.a == 192) s!"{repr pz2}"

/-- Run the fill suite. -/
def run : IO (Nat × Nat) := runSuite "fill" tests

end LeanPlotTest.Raster.FillTests
