/-! # Color palette helpers

Provides a small default color palette (taken from the classic
Matplotlib/Jupyter qualitative set) and convenience helpers for mapping a
list of series names to distinct stroke colors.  This avoids boiler-plate at
call-sites where users would otherwise have to supply an explicit color for
each series.

The helpers are intentionally lightweight; future versions may allow
configuration or pluggable palette providers.
-/

namespace LeanPlot.Palette
open Lean

/-! A color palette for plots. -/

/-- An alias for hex strings -/
private abbrev Color := String

/-- Dark purple ({lit}`#440154`). -/
def darkPurple : Color := "#440154"

/-- Indigo ({lit}`#482878`). -/
def indigo : Color := "#482878"

/-- Blue-purple ({lit}`#3e4a89`). -/
def bluePurple : Color := "#3e4a89"

/-- Medium blue ({lit}`#31688e`). -/
def blue : Color := "#31688e"

/-- Turquoise (#26828e). -/
def turquoise : Color := "#26828e"

/-- Green-turquoise (#1f9e89). -/
def greenTurquoise : Color := "#1f9e89"

/-- Green ({lit}`#35b779`). -/
def green : Color := "#35b779"

/-- Lime ({lit}`#6ece58`). -/
def lime : Color := "#6ece58"

/-- Yellow-green ({lit}`#b5de2b`). -/
def yellowGreen : Color := "#b5de2b"

/-- Yellow ({lit}`#fde725`). -/
def yellow : Color := "#fde725"

/-- The default color palette. -/
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
  yellow
]

/-- A function that maps a natural number to a color from the default palette.
The colors repeat if {lean}`n` is larger than the number of colors in the palette. -/
@[inline] def colorFromNat (n : Nat) : Color :=
  defaultPalette[n % defaultPalette.size]!

/-- Given an {lean}`Array` of series names, assign each a color from
{name}`defaultPalette`, cycling if necessary.  The result is suitable for the
{lit}`seriesStrokes` argument expected by {lit}`LeanPlot.Components.mkLineChart` and
friends. -/
-- TODO this should be any collection, not just an array
@[inline] def autoColors (names : Array String) : Array (String × String) :=
  names.zipIdx.map (fun (n, i) => (n, colorFromNat i))

/-- Generate a list of {lean}`n` distinct colors.
If {lean}`n` is larger than the number of colors in the palette, the colors will repeat. -/
def f (n : Nat) : List Color :=
  let palette := defaultPalette
  List.range n |>.map fun i => palette[i % palette.size]!
end LeanPlot.Palette

/-- List comprehension syntax. -/
syntax "[" term "|" term " in " term (", " term)? "]" : term

/--
List comprehension syntax diagram:

```
   ┌───── yield ───────────┐ ┌ x ┐       ┌ xs ┐         optional filter
      [  e     |   x   in   xs   ,  p  ]
```
-/
macro_rules
  | `([ $e | $x in $xs, $p ]) =>
      `(List.map (fun $x => $e) (List.filter (fun $x => $p) $xs))
  | `([ $e | $x in $xs ]) =>
      `(List.map (fun $x => $e) $xs)

/-!
  A test suite for the {lit}`defaultPalette`.
-/

/-- A list of squares from 0 to 5. -/
def squares  : List Nat := [ x ^ 2 | x in List.range 6 ]
/-- A list of squares of even numbers from 0 to 5. -/
def evensSq  : List Nat := [ x ^ 2 | x in List.range 6, x % 2 == 0 ]

#eval squares = [0,1,4,9,16,25]
#eval evensSq = [0,4,16]
