# Embedded font data: provenance and licences

LeanPlot renders text as glyph outlines taken from the fonts Makie uses by default, so plots
match Makie's typography and both backends (SVG, raster) draw identical text. The outlines
live in `LeanPlot/Font/Data/*.lean` as base64 blobs of a small binary format (documented in
`scripts/font/gen.py` and `LeanPlot/Font/Face.lean`). They are **subsets of outline and metric
data, not font files**: nothing here can be installed or used as a font.

## Sources

| Data module | Source file | SHA-256 | Licence |
|---|---|---|---|
| `LeanPlot/Font/Data/HerosRegular.lean` | `TeXGyreHerosMakie-Regular.otf` ("TeX Gyre Heros Makie Regular") | `50a5ca8991bbcfe5bbfad31457ed678c021cdb380d31888d8096dc2f9b5c0cd0` | GUST Font License (`GUST-FONT-LICENSE.txt`), i.e. LPPL 1.3c or later (`LPPL-1.3c.txt`) |
| `LeanPlot/Font/Data/HerosBold.lean` | `TeXGyreHerosMakie-Bold.otf` ("TeX Gyre Heros Makie Bold") | `0004879bc0033fc16fd829e66b8ff5f4e0c77cf8427efbca21564bc8bdd3f3ce` | same |
| `LeanPlot/Font/Data/DejaVuSans.lean` | `DejaVuSans.ttf` (DejaVu Sans 2.37) | `7da195a74c55bef988d0d48f9508bd5d849425c1770dba5d7bfc6ce9ed848954` | Bitstream Vera licence + Arev licence; DejaVu changes in the public domain (`DejaVu-LICENSE.txt`) |

The files are the ones shipped in Makie.jl's `assets/fonts` (identical in Makie 0.21.5's
package directory and in the Makie 0.24.15 font artifact used by the oracle environment).
"TeX Gyre Heros Makie" is Makie's build of TeX Gyre Heros (Copyright 2006, 2009 for TeX Gyre
extensions by B. Jackowski and J.M. Nowacki, on behalf of TeX users groups). DejaVu Sans is
Copyright (c) 2003 by Bitstream, Inc. (Bitstream Vera) and Copyright (c) 2006 by Tavmjong Bah
(Arev glyphs).

## What is embedded

`scripts/font/gen.py` (run with `uv run --with fonttools python scripts/font/gen.py`) keeps, for
a fixed character set (printable ASCII, Latin-1, a few Latin Extended-A letters, Greek,
spaces and punctuation, Unicode sub/superscripts and modifier letters, letterlike symbols,
arrows, the Mathematical Operators block, miscellaneous technical and math symbols, a few
geometric shapes and double-struck letters):

* per face: units per em, `hhea` ascender/descender/line gap, cap and x height;
* per glyph: advance width, control box, and the outline (moves, lines, quadratic and cubic
  Béziers, closes) in font units, with composite glyphs decomposed;
* kerning pairs (`kern` table, else GPOS pair adjustments) among the kept glyphs; there are
  none for this character set (Heros Makie has no kerning tables).

Characters present in Heros come from Heros (Regular or Bold); the rest come from DejaVu Sans,
which is Makie's first fallback font. One codepoint that neither face has is aliased to a
visually identical glyph: U+27C2 ⟂ → U+22A5 ⊥.

## Licence notes

* **GUST Font License / LPPL 1.3c.** The GUST licence *requests* (it does not require) that
  derived works be distributed under new font names. This data is a derived work that is not
  a font and has no font name; the face `name` recorded in each blob is the source font's full
  name, kept only for provenance. The unmodified licence texts are in this directory.
* **Bitstream Vera.** Modified versions may not be distributed under names containing
  "Bitstream" or "Vera"; this data is distributed under neither (the DejaVu name is recorded
  for provenance only).
* Documents produced with LeanPlot (SVG and PNG figures) are not covered by these licences.
