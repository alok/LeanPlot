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

/-- A qualitative 10-colour palette that looks good on both dark and light
backgrounds and is colour-blind-friendly.  Source: Matplotlib _tab10_. -/
@[inline] def defaultPalette : Array String := #[
  "#440154", -- dark purple
  "#482878", -- indigo
  "#3e4a89", -- blue-purple
  "#31688e", -- blue
  "#26828e", -- turquoise
  "#1f9e89", -- green-turquoise
  "#35b779", -- green
  "#6ece58", -- lime
  "#b5de2b", -- yellow-green
  "#fde725"  -- yellow
]

/-- Return a colour from `defaultPalette`, cycling if the index exceeds the
palette length.  This ensures we never run out of colours for long series
lists (with the caveat that colours will start repeating). -/
@[inline] def colourFor (i : Nat) : String :=
  defaultPalette.get! (i % defaultPalette.size)

/-- Given an `Array` of series names, assign each a colour from
`defaultPalette` (cycling when necessary).  The result is suitable for the
`seriesStrokes` argument expected by `LeanPlot.Components.mkLineChart` and
friends. -/
@[inline] def autoColours (names : Array String) : Array (String × String) :=
  (names.toList.enum.map (fun (i, n) => (n, colourFor i))).toArray

end LeanPlot.Palette
