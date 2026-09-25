import LeanPlot.Font.Face
import LeanPlot.Font.Data.HerosRegular
import LeanPlot.Font.Data.HerosBold
import LeanPlot.Font.Data.DejaVuSans

/-!
# The embedded faces

The three embedded faces, decoded once (at module initialisation in compiled code, on first
use in the interpreter), and the two `Font`s built from them. Makie's default text font is
TeX Gyre Heros Makie; for characters it lacks Makie falls back to DejaVu Sans
(`Makie.alternativefonts()`), and so do we. Provenance and licences: `licenses/FONTS.md`.
-/

namespace LeanPlot.Font

/-- Decode an embedded face; a corrupt blob (caught by the tests) yields an empty face. -/
def Face.ofBlob (blob : String) : Face :=
  match Face.decode blob with
  | .ok f => f
  | .error _ => default

/-- TeX Gyre Heros Makie Regular (subset): Makie's default text face. -/
def herosRegular : Face := Face.ofBlob Data.herosRegularBlob

/-- TeX Gyre Heros Makie Bold (subset): Makie's default bold face (titles). -/
def herosBold : Face := Face.ofBlob Data.herosBoldBlob

/-- DejaVu Sans (subset): fallback for characters Heros lacks (arrows, most math operators,
sub/superscripts, double-struck letters …). -/
def dejaVuSans : Face := Face.ofBlob Data.dejaVuSansBlob

/-- A font for layout: a primary face and the fallback face consulted for characters the
primary face does not map. -/
structure Font where
  /-- Face tried first. -/
  primary : Face
  /-- Face used for characters the primary face lacks. -/
  fallback : Face
  deriving Inhabited

namespace Font

/-- Heros Regular with DejaVu Sans fallback (Makie's default `:regular`). -/
def regular : Font := ⟨herosRegular, dejaVuSans⟩

/-- Heros Bold with DejaVu Sans fallback (Makie's default `:bold`). -/
def bold : Font := ⟨herosBold, dejaVuSans⟩

/-- The font selected by a `TextStyle` (`bold` flag). -/
@[inline] def ofStyle (s : TextStyle) : Font := if s.bold then bold else regular

end Font

end LeanPlot.Font
