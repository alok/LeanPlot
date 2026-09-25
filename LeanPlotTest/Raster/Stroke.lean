import LeanPlotTest.Raster.Util

/-!
Stroker invariants: stroke widths (row and column integrals), cap and join
areas against closed forms, the miter limit, closed paths, dashes, dots, no
double blending of translucent self-overlaps, and zero width.
-/

namespace LeanPlotTest.Raster.StrokeTests

open LeanPlot LeanPlot.Raster LeanPlotTest.Raster

/-- Stroke `p` in black on white. -/
def strokeBlack (w h : Nat) (p : Path) (s : Stroke := {}) : Canvas w h :=
  paint w h #[.path p none (some { s with color := .black }) none]

/-- π -/
def pi : Float := 3.141592653589793

/-- All stroke checks. -/
def tests : T Unit := do
  -- horizontal lines: every column integrates to the width
  for (y, wd) in [(50.5, 1.0), (50.0, 2.0), (50.3, 1.7), (50.5, 3.0), (49.9, 0.5)] do
    let cv := strokeBlack 100 100 (polyline [(10, y), (90, y)]) { width := wd }
    checkNear s!"h-line y={y} w={wd} column integral" (colDark cv 50) wd 0.01
    checkNear s!"h-line y={y} w={wd} area" (darkArea cv) (80.0 * wd) (0.01 * 80.0)
  -- pixel-aligned 1 px line is crisp
  let cv := strokeBlack 100 100 (polyline [(10, 50.5), (90, 50.5)]) { width := 1.0 }
  check "aligned 1px line crisp" ((cv.get 50 50).r == 0 && (cv.get 50 49).r == 255 && (cv.get 50 51).r == 255)
  -- vertical
  let cv := strokeBlack 100 100 (polyline [(30.25, 10), (30.25, 90)]) { width := 2.5 }
  checkNear "v-line row integral" (rowDark cv 50) 2.5 0.01
  -- diagonal: area = length × width (butt caps)
  let cv := strokeBlack 200 200 (polyline [(20, 30), (170, 150)]) { width := 4.0 }
  let len := ((150.0 * 150.0) + (120.0 * 120.0)).sqrt
  check "diagonal area" ((darkArea cv - 4.0 * len).abs / (4.0 * len) ≤ 0.005) s!"{darkArea cv} vs {4.0 * len}"
  -- caps on a 60 px, 10 px wide segment
  let seg := polyline [(40, 50), (100, 50)]
  checkNear "butt cap area" (darkArea (strokeBlack 140 100 seg { width := 10, cap := .butt })) 600.0 0.5
  checkNear "square cap area" (darkArea (strokeBlack 140 100 seg { width := 10, cap := .square })) 700.0 0.5
  let rnd := darkArea (strokeBlack 140 100 seg { width := 10, cap := .round })
  check "round cap area" ((rnd - (600.0 + pi * 25.0)).abs ≤ 0.01 * (600.0 + pi * 25.0)) s!"{rnd}"
  -- joins at a right angle, 20 px wide: the outer wedges differ by closed forms
  let corner := polyline [(20, 30), (100, 30), (100, 110)]
  let area (j : LineJoin) := darkArea (strokeBlack 140 140 corner { width := 20, join := j })
  let bevel := area .bevel; let miter := area .miter; let round := area .round
  checkNear "miter - bevel = hw²/2" (miter - bevel) 50.0 1.0
  checkNear "round - bevel = (π/4 - 1/2)hw²" (round - bevel) ((pi / 4.0 - 0.5) * 100.0) 1.5
  -- the whole L: two 80×20 arms minus the shared 10×10 inner square + miter corner
  checkNear "miter L area" miter (80.0 * 20.0 * 2.0 - 100.0 + 100.0) 1.0
  -- miter limit: a 20° turn has miter ratio 1/sin(10°) ≈ 5.8 > 4 → bevel
  let sharp := polyline [(20, 100), (120, 100), (20, 100 - 100.0 * Float.tan (20.0 * pi / 180.0))]
  let aMiter := darkArea (strokeBlack 160 160 sharp { width := 8, join := .miter, miterLimit := 4 })
  let aBevel := darkArea (strokeBlack 160 160 sharp { width := 8, join := .bevel })
  checkNear "miter limit falls back to bevel" aMiter aBevel 0.05
  let aMiter10 := darkArea (strokeBlack 160 160 sharp { width := 8, join := .miter, miterLimit := 10 })
  check "miter limit 10 keeps the miter" (aMiter10 > aBevel + 5.0) s!"{aMiter10} vs {aBevel}"
  -- closed square stroke: outer² − inner² exactly
  let sq := polygon [(20, 20), (120, 20), (120, 120), (20, 120)]
  checkNear "closed square stroke" (darkArea (strokeBlack 140 140 sq { width := 10 })) (110.0 * 110.0 - 90.0 * 90.0) 1.0e-6
  -- open square (same points, no close): the first corner is left unjoined with butt caps
  let opensq := polyline [(20, 20), (120, 20), (120, 120), (20, 120), (20, 20)]
  checkNear "open square misses one corner" (darkArea (strokeBlack 140 140 opensq { width := 10 }))
    (110.0 * 110.0 - 90.0 * 90.0 - 25.0) 1.0e-6
  -- dashes: 100 px line, [10, 10] → 50 px on
  let line := polyline [(10, 50), (110, 50)]
  checkNear "dash area" (darkArea (strokeBlack 130 100 line { width := 2, dash := #[10, 10] })) 100.0 0.1
  checkNear "dash offset area" (darkArea (strokeBlack 130 100 line { width := 2, dash := #[10, 10], dashOffset := 5 })) 100.0 0.1
  checkNear "odd dash array repeats" (darkArea (strokeBlack 130 100 line { width := 2, dash := #[10] })) 100.0 0.1
  -- the first dash of [10,10] with offset 5 covers x ∈ [10, 15)
  let cvo := strokeBlack 130 100 line { width := 2, dash := #[10, 10], dashOffset := 5 }
  check "dash offset phase" ((cvo.get 12 49).r == 0 && (cvo.get 17 49).r == 255 && (cvo.get 27 49).r == 0)
  checkNear "negative dash = solid" (darkArea (strokeBlack 130 100 line { width := 2, dash := #[10, -1] })) 200.0 0.1
  -- round-capped dashes add a disc each: 5 dashes → 5·π·r²
  let rd := darkArea (strokeBlack 130 100 line { width := 4, dash := #[10, 10], cap := .round })
  check "round dashes" ((rd - (200.0 + 5.0 * pi * 4.0)).abs ≤ 3.0) s!"{rd}"
  -- zero-length subpath: dot with round caps, nothing with butt
  let dot := ({} : Path).moveTo 50.3 50.7 |>.lineTo 50.3 50.7
  let dr := darkArea (strokeBlack 100 100 dot { width := 10, cap := .round })
  check "round dot" ((dr - pi * 25.0).abs ≤ 0.01 * pi * 25.0) s!"{dr}"
  checkNear "square dot" (darkArea (strokeBlack 100 100 dot { width := 10, cap := .square })) 100.0 0.5
  checkNear "butt dot invisible" (darkArea (strokeBlack 100 100 dot { width := 10, cap := .butt })) 0.0 1.0e-9
  -- translucent self-overlap is blended once
  let x := polyline [(10, 10), (90, 90), (90, 10), (10, 90)]
  let cvx := paint 100 100 #[.path x none (some { color := ⟨0, 0, 0, 0.5⟩, width := 6 }) none]
  check "no double blend at crossing" ((cvx.get 50 50).r == (cvx.get 30 30).r) s!"{(cvx.get 50 50).r} vs {(cvx.get 30 30).r}"
  -- zero and negative width draw nothing
  checkNear "zero width" (darkArea (strokeBlack 100 100 line { width := 0 })) 0.0 1.0e-9
  checkNear "negative width" (darkArea (strokeBlack 100 100 line { width := -3 })) 0.0 1.0e-9
  -- a 180° reversal with round join adds a half disc
  let rev := polyline [(20, 50), (80, 50), (40, 50)]
  let arev := darkArea (strokeBlack 100 100 rev { width := 10, join := .round })
  check "reversal round join" ((arev - (600.0 + pi * 12.5)).abs ≤ 3.0) s!"{arev}"
  -- stroke of a curve: a circle outline of radius 30, width 4 → area 2π·30·4
  let ring := darkArea (strokeBlack 100 100 (circlePath 50 50 30) { width := 4 })
  check "circle outline area" ((ring - 2.0 * pi * 30.0 * 4.0).abs ≤ 0.01 * 2.0 * pi * 30.0 * 4.0) s!"{ring}"
  -- clip applies to strokes
  let cvc := paint 100 100 #[.path line none (some { color := .black, width := 10 }) (some ⟨30, 0, 20, 100⟩)]
  checkNear "clipped stroke" (darkArea cvc) 200.0 1.0e-6

/-- Run the stroke suite. -/
def run : IO (Nat × Nat) := runSuite "stroke" tests

end LeanPlotTest.Raster.StrokeTests
