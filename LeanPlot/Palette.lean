/-! # Colour palette helpers

Provides a small default colour palette (taken from the classic
Matplotlib/Jupyter qualitative set) and convenience helpers for mapping a
list of series names to distinct stroke colours.  This avoids boiler-plate at
call-sites where users would otherwise have to supply an explicit colour for
each series.

The helpers are intentionally lightweight; future versions may allow
configuration or pluggable palette providers.
-/

namespace LeanPlot.Palette
open Lean

private abbrev Color := String

def darkPurple : Color := "#440154"
def indigo : Color := "#482878"
def bluePurple : Color := "#3e4a89"
def blue : Color := "#31688e"
def turquoise : Color := "#26828e"
def greenTurquoise : Color := "#1f9e89"
def green : Color := "#35b779"
def lime : Color := "#6ece58"
def yellowGreen : Color := "#b5de2b"
def yellow : Color := "#fde725"




/-- A qualitative 10-colour palette that looks good on both dark and light
backgrounds and is colour-blind-friendly.  Source: Matplotlib _tab10_. -/
@[inline] def defaultPalette : Array Color := #[
  yellowGreen,
  greenTurquoise,
  bluePurple,
  blue,
  turquoise,
  indigo,
  green,
  lime,
  darkPurple,
  yellow,
]

/-- Return a colour from `defaultPalette`, cycling if the index exceeds the
palette length.  This ensures we never run out of colours for long series
lists (with the caveat that colours will start repeating). -/
@[inline] def colourFor (i : Nat) : String :=
  defaultPalette[(i % defaultPalette.size)]!

/-- Given an `Array` of series names, assign each a colour from
`defaultPalette` (cycling when necessary).  The result is suitable for the
`seriesStrokes` argument expected by `LeanPlot.Components.mkLineChart` and
friends. -/
@[inline] def autoColours (names : Array String) : Array (String × String) :=
  names.zipIdx.map (fun (n, i) => (n, colourFor i))

end LeanPlot.Palette
