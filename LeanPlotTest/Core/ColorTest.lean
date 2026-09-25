import LeanPlotTest.Core.Harness
import LeanPlot.Core.Colormap

/-!
Colours and colormaps against Makie (`colors.json`, `colormaps.json`): Wong
palette, named colours, hex parsing, `Colors.hex`, `N0f8` rounding; every
built-in colormap's `interpolated_getindex`/`nearest_getindex` samples
(bit-exact `Float32`), `numbers_to_colors` and `resample_cmap`.
-/

namespace LeanPlotTest.Core.ColorTest

open LeanPlot LeanPlot.Num

/-- Decode `[r, g, b, a]`. -/
def rgbaOf (j : J) : RGBA :=
  let xs := j.floats
  ⟨xs[0]!, xs[1]!, xs[2]!, xs[3]!⟩

/-- Bit-exact colour equality. -/
def rgbaEq (a b : RGBA) : Bool := bitEq a.r b.r && bitEq a.g b.g && bitEq a.b b.b && bitEq a.a b.a

/-- Show a colour. -/
def showRGBA (c : RGBA) : String := s!"({showFloat c.r}, {showFloat c.g}, {showFloat c.b}, {showFloat c.a})"

/-- Colour unit tests and `colors.json`. -/
def colors : TestM Unit := do
  -- unit tests
  check "toHex" (RGBA.toHex ⟨1, 0.5, 0, 1⟩ == "#ff8000") fun _ => RGBA.toHex ⟨1, 0.5, 0, 1⟩
  check "toHex alpha" (RGBA.toHex ⟨0, 0, 0, 0.5⟩ == "#00000080") fun _ => RGBA.toHex ⟨0, 0, 0, 0.5⟩
  check "roundtrip hex" ((RGBA.ofHex? "#3e4a89").map RGBA.toHex == some "#3e4a89") fun _ => "roundtrip"
  check "bad hex" ((RGBA.ofHex? "#12345").isNone && (RGBA.ofHex? "xyz").isNone) fun _ => "accepted"
  check "packing" (RGBA.toRGBA8 (RGBA.ofRGBA8 0x12345678) == 0x12345678) fun _ => "pack"
  let bl := RGBA.blend ⟨1, 0, 0, 0.5⟩ RGBA.white
  check "blend" (bl.r == 1 && bl.g == 0.5 && bl.a == 1) fun _ => showRGBA bl
  check "named case/space" ((RGBA.named? "Royal Blue").isSome && (RGBA.named? "nosuchcolor").isNone) fun _ => "named"
  let some j ← loadOracle "colors.json" | return
  let wong := (j.get "wong").arrD.map rgbaOf
  check "wong size" (wong.size == wongColors.size) fun _ => s!"{wong.size}"
  for i in [0:min wong.size wongColors.size] do
    check s!"wong[{i}]" (rgbaEq wongColors[i]! wong[i]!) fun _ => s!"got {showRGBA wongColors[i]!}, want {showRGBA wong[i]!}"
  for c in (j.get "named").arrD do
    let n := (c.get "name").string
    let want := rgbaOf (c.get "rgba")
    match RGBA.named? n with
    | some got => check s!"named {n}" (rgbaEq got want) fun _ => s!"got {showRGBA got}, want {showRGBA want}"
    | none => check s!"named {n}" false fun _ => "not found"
  for c in (j.get "parse").arrD do
    let s := (c.get "s").string
    let want := rgbaOf (c.get "rgba")
    match RGBA.parse? s with
    | some got => check s!"parse {s}" (rgbaEq got want) fun _ => s!"got {showRGBA got}, want {showRGBA want}"
    | none => check s!"parse {s}" false fun _ => "parse failed"
  for c in (j.get "hex").arrD do
    let xs := (c.get "rgb").floats
    let got := RGBA.hexColors ⟨xs[0]!, xs[1]!, xs[2]!, 1⟩
    let want := (c.get "hex").string
    check s!"hex {showFloats xs}" (got == want) fun _ => s!"got {got}, want {want}"
  for c in (j.get "n0f8").arrD do
    let x := (c.get "x").float
    let want := (c.get "u").nat
    let got := (RGBA.to8 x).toNat
    check s!"N0f8({showFloat x})" (got == want) fun _ => s!"got {got}, want {want}"

/-- `colormaps.json`. -/
def colormaps : TestM Unit := do
  -- structural unit tests
  let v := Colormap.viridis
  check "viridis size" (v.size == 256) fun _ => s!"{v.size}"
  check "builtin count" (Colormap.builtinNames.size ≥ 80) fun _ => s!"{Colormap.builtinNames.size}"
  let rv := v.reverse
  check "reverse" (rgbaEq rv.first v.last && rgbaEq rv.last v.first) fun _ => "reverse"
  check "ofColors empty" ((Colormap.ofColors #[]).size == 1) fun _ => "empty"
  let some j ← loadOracle "colormaps.json" | return
  for c in (j.get "maps").arrD do
    let n := (c.get "name").string
    match Colormap.named? n with
    | none => check s!"colormap {n}" false fun _ => "missing"
    | some cm =>
      check s!"colormap {n} size" (cm.size == (c.get "n").nat) fun _ => s!"got {cm.size}"
      for s in (c.get "samples").arrD do
        let t := (s.get "t").float
        let want := rgbaOf (s.get "rgba")
        let got := cm.interpolatedGetIndex t
        check s!"{n}[{showFloat t}]" (rgbaEq got want) fun _ => s!"got {showRGBA got}, want {showRGBA want}"
      for s in (c.get "nearest").arrD do
        let t := (s.get "t").float
        let want := rgbaOf (s.get "rgba")
        let got := cm.nearestGetIndex t
        check s!"{n} nearest[{showFloat t}]" (rgbaEq got want) fun _ => s!"got {showRGBA got}, want {showRGBA want}"
  for c in (j.get "numbers_to_colors").arrD do
    let n := (c.get "name").string
    let cm := Colormap.named n
    let lo := (c.get "lo").float
    let hi := (c.get "hi").float
    let clip := (c.get "clip").boolean
    let scale : Scale := if (c.get "scale").string == "log10" then .log10 else .identity
    let opts : Colormap.MapOptions :=
      { scale, lowclip := if clip then some ⟨1, 0, 0, 1⟩ else none, highclip := if clip then some ⟨0, 0, 1, 1⟩ else none }
    let vals := (c.get "values").floats
    let want := (c.get "rgba").arrD.map rgbaOf
    for i in [0:vals.size] do
      let got := cm.mapValue lo hi opts vals[i]!
      check s!"numbers_to_colors {n} {(c.get "scale").string} ({showFloat lo},{showFloat hi}) clip={clip} v={showFloat vals[i]!}"
        (rgbaEq got want[i]!) fun _ => s!"got {showRGBA got}, want {showRGBA want[i]!}"
  for c in (j.get "resample").arrD do
    let n := (c.get "name").string
    let k := (c.get "k").nat
    let want := (c.get "rgba").arrD.map rgbaOf
    let got := ((Colormap.named n).resample k).colors
    check s!"resample {n} {k}" (got.size == want.size && (Array.range got.size).all fun i => rgbaEq got[i]! want[i]!)
      fun _ => s!"sizes {got.size}/{want.size}"

/-- The fast `mapToRGBA8` equals the per-value reference byte for byte over
colormaps (built-in `Float32` tables, a user map with arbitrary `Float64`
entries, a one-entry map), scales, clip colours, nearest lookup, NaN/±∞ data
and degenerate or reversed limits. -/
def mapFast : TestM Unit := do
  -- deterministic pseudo-random values in [-0.5, 1.5] plus specials
  let rand (k : Nat) : Float := ((k * 2654435761 + 12345) % 1000003).toFloat / 500001.5 - 0.5
  let specials : Array Float := #[nan, inf, -inf, 0, 1, -0.0, 0.5, 1e-300, -1e300]
  let vs : FloatArray := ⟨(Array.range 3000).map rand ++ specials⟩
  let user := Colormap.ofColors #[⟨0.1, 0.2, 0.3, 1⟩, ⟨0.3333333333333, 0.9, 0.05, 0.5⟩, ⟨1, 1, 1, 0.25⟩] "user"
  let one := Colormap.ofColors #[⟨0.2, 0.4, 0.6, 1⟩] "one"
  let maps := #[Colormap.viridis, Colormap.named "RdBu", Colormap.named "turbo", user, one]
  let optss : Array Colormap.MapOptions :=
    #[{}, { interpolate := false }, { lowclip := some RGBA.black }, { highclip := some RGBA.white },
      { lowclip := some ⟨1, 0, 0, 1⟩, highclip := some ⟨0, 0, 1, 0.5⟩, nanColor := ⟨0.5, 0.5, 0.5, 1⟩ },
      { scale := .log10 }, { scale := .sqrt, interpolate := false, lowclip := some RGBA.white }]
  let lims : Array (Float × Float) := #[(0, 1), (0.2, 0.8), (1, 0), (0.5, 0.5), (1e-3, 1)]
  for cm in maps do
    for opts in optss do
      for (lo, hi) in lims do
        let fast := cm.mapToRGBA8 lo hi opts vs
        let ref := cm.mapToRGBA8Ref lo hi opts vs
        check s!"mapToRGBA8 {cm.name} ({showFloat lo}, {showFloat hi})" (fast == ref) fun _ =>
          let i := (List.range fast.size).find? (fun i => fast.get! i != ref.get! i) |>.getD fast.size
          s!"sizes {fast.size}/{ref.size}, first differing byte {i}"

/-- The colour suite. -/
def suite : TestM Unit := do
  colors
  colormaps
  mapFast

end LeanPlotTest.Core.ColorTest
