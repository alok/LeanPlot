#!/usr/bin/env python3
"""Generate LeanPlot's embedded font data from Makie's bundled fonts.

Run from the repository root:

    uv run --with fonttools python scripts/font/gen.py [--fonts DIR]

Inputs (the exact files Makie ships and uses by default):

* ``TeXGyreHerosMakie-Regular.otf`` and ``TeXGyreHerosMakie-Bold.otf``
  (Makie's default text faces; GUST Font License, see ``licenses/GUST-FONT-LICENSE.txt``);
* ``DejaVuSans.ttf`` (Makie's first fallback face after Heros; Bitstream Vera
  licence + public-domain DejaVu changes, see ``licenses/DejaVu-LICENSE.txt``).

Outputs:

* ``LeanPlot/Font/Data/HerosRegular.lean``, ``HerosBold.lean``, ``DejaVuSans.lean``:
  one base64 ``String`` literal per face holding a compact binary blob (format below);
* ``LeanPlotTest/Font/data/metrics.json``: the fontTools view of every embedded
  codepoint (face, advance, control box, contour count) and the kerning pairs,
  used by the Lean tests as the reference.

Blob format (all integers little endian)::

    "LPF1" u8 version=1  u8 coordDiv
    u16 unitsPerEm  i16 ascender  i16 descender  i16 lineGap  i16 capHeight  i16 xHeight
    u16 nGlyphs  u16 nCmap  u16 nKern  u8 nameLen  name[nameLen] (UTF-8)
    cmap   : nCmap   × (u32 codepoint, u16 glyph)          sorted by codepoint
    glyphs : nGlyphs × (u16 advance, i16 xMin, i16 yMin, i16 xMax, i16 yMax,
                        u16 nVerbs, u16 nCoords)            glyph 0 is .notdef
    verbs  : Σ nVerbs bytes, LeanPlot `Verb` encoding (0 move, 1 line, 2 quad, 3 cubic, 4 close)
    coords : Σ nCoords zigzag LEB128 varints: per-axis deltas (x against the previous x,
             y against the previous y, both starting at 0 for every glyph) in units of
             1/coordDiv font unit
    kern   : nKern × (u16 left, u16 right, i16 value)       sorted by (left, right)

Outlines are in font units, y up. The control box (xMin..yMax) is the box of all
outline points including off-curve controls, which is what FreeType (and so Makie)
reports as a glyph's ink bounding box. Heros stores straight edges as degenerate
cubics; those are emitted as ``lineTo`` (the traced point set is identical).
"""
from __future__ import annotations

import argparse
import base64
import json
import os
import struct
import sys
import unicodedata
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable

from fontTools.pens.basePen import BasePen
from fontTools.pens.boundsPen import BoundsPen, ControlBoundsPen
from fontTools.ttLib import TTFont

REPO = Path(__file__).resolve().parents[2]
DEFAULT_FONTS = Path.home() / ".julia/packages/Makie/rEu75/assets/fonts"

# ---------------------------------------------------------------------------
# Character set
# ---------------------------------------------------------------------------


def R(a: int, b: int) -> list[int]:
    """Inclusive codepoint range."""
    return list(range(a, b + 1))


def S(s: str) -> list[int]:
    """Codepoints of a string."""
    return [ord(c) for c in s]


CHARSET_GROUPS: list[tuple[str, list[int]]] = [
    ("ascii", R(0x20, 0x7E)),
    ("latin1", R(0xA0, 0xFF)),  # incl. ¹ ² ³ ° ± · × ÷ µ ¬
    ("latinExtA", S("ıħŁłŒœŠšŽžŸ")),
    ("greek", R(0x391, 0x3A1) + R(0x3A3, 0x3A9) + R(0x3B1, 0x3C9) + S("ϐϑϒϕϖϝϰϱϲϴϵ϶")),
    ("spaces", R(0x2000, 0x200B) + [0x202F, 0x205F, 0x2060]),
    ("punctuation", S("‐‑‒–—―‖‗‘’‚‛“”„‟†‡•‣․‥…‧‰‱′″‴‵‶‷‹›※‼‽‾⁀⁄⁅⁆⁎⁏⁒⁓⁗")),
    ("supsub", R(0x2070, 0x209C)),  # ⁰ ⁱ ⁴…⁹ ⁺ ⁻ ⁼ ⁽ ⁾ ⁿ ₀…₉ ₊ ₋ ₌ ₍ ₎ ₐ ₑ ₒ ₓ ₔ ₕ ₖ ₗ ₘ ₙ ₚ ₛ ₜ
    # superscript lowercase / uppercase letters and subscript letters (Unicode modifier forms)
    ("modifierLetters", S("ᵃᵇᶜᵈᵉᶠᵍʰʲᵏˡᵐᵒᵖʳˢᵗᵘᵛʷˣʸᶻ" "ᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁⱽᵂ"
                            "ᵢᵣᵤᵥⱼ" "ᵅᵝᵞᵟᵋᶿᶥᶲᵠᵡ" "ᵦᵧᵨᵩᵪ" "ᐟ")),
    ("currency", S("€")),
    ("letterlike", S("ℂℇℋℌℍℎℏℐℑℒℓℕ№℘ℙℚℛℜℝ™ℤΩ℧ℨKÅℬℭℯℰℱℳℴℵℶℷℸ℃℉ⅅⅆⅇⅈⅉ")),
    ("arrows", R(0x2190, 0x21FF)),
    ("mathOperators", R(0x2200, 0x22FF)),
    ("miscTechnical", S("⌀⌈⌉⌊⌋⌐⌠⌡") + [0x2329, 0x232A]),  # 〈 〉 (deprecated angle brackets)
    ("miscMathA", S("⟅⟆⟦⟧⟨⟩⟪⟫")),
    ("supplementalArrowsA", R(0x27F0, 0x27FF)),
    ("miscMathB", S("⦃⦄⧺⧻")),
    ("supplementalMath", S("⨀⨁⨂⨌⨍⨎⨏⨯⩽⩾")),
    ("geometric", S("■□▪▫▬▭▮▯▲△▴▵▶▷▸▹▼▽▾▿◀◁◂◃◆◇◈◊○◌◎●◦◯◻◼")),
    ("symbols", S("★☆☉☾✓✗✕")),
    ("doubleStruck", R(0x1D538, 0x1D56B) + R(0x1D7D8, 0x1D7E1)),  # 𝔸…𝕫, 𝟘…𝟡 (where present)
]

# Codepoints that neither face has, drawn with a visually identical glyph instead.
ALIASES: dict[int, int] = {
    0x27C2: 0x22A5,  # ⟂ PERPENDICULAR -> ⊥ UP TACK
}

# ---------------------------------------------------------------------------
# Outline extraction
# ---------------------------------------------------------------------------

MOVE, LINE, QUAD, CUBIC, CLOSE = 0, 1, 2, 3, 4
ARITY = {MOVE: 2, LINE: 2, QUAD: 4, CUBIC: 6, CLOSE: 0}


class OutlinePen(BasePen):
    """Records a decomposed outline as (verb, coords) with implied TrueType points resolved."""

    def __init__(self, glyphSet) -> None:  # type: ignore[no-untyped-def]
        super().__init__(glyphSet)
        self.ops: list[tuple[int, list[float]]] = []
        self._start: tuple[float, float] = (0.0, 0.0)

    def _moveTo(self, p):  # type: ignore[no-untyped-def]
        self._start = p
        self.ops.append((MOVE, [p[0], p[1]]))

    def _lineTo(self, p):  # type: ignore[no-untyped-def]
        self.ops.append((LINE, [p[0], p[1]]))

    def _qCurveToOne(self, a, b):  # type: ignore[no-untyped-def]
        self.ops.append((QUAD, [a[0], a[1], b[0], b[1]]))

    def _curveToOne(self, a, b, c):  # type: ignore[no-untyped-def]
        p0 = self._getCurrentPoint()
        if _is_straight(p0, a, b, c):
            self.ops.append((LINE, [c[0], c[1]]))
        else:
            self.ops.append((CUBIC, [a[0], a[1], b[0], b[1], c[0], c[1]]))

    def _closePath(self):  # type: ignore[no-untyped-def]
        # A final lineTo back to the contour start is implied by `close`.
        if self.ops and self.ops[-1][0] == LINE and tuple(self.ops[-1][1]) == tuple(self._start):
            if len(self.ops) >= 2 and self.ops[-2][0] != MOVE:
                self.ops.pop()
        self.ops.append((CLOSE, []))

    def _endPath(self):  # type: ignore[no-untyped-def]
        raise ValueError("open contour in font outline")


def _is_straight(p0, c1, c2, p3) -> bool:  # type: ignore[no-untyped-def]
    """True when the cubic traces exactly the segment p0→p3 (collinear, monotone controls)."""
    dx, dy = p3[0] - p0[0], p3[1] - p0[1]
    for c in (c1, c2):
        if (c[0] - p0[0]) * dy - (c[1] - p0[1]) * dx != 0:
            return False
    L = dx * dx + dy * dy
    if L == 0:
        return c1 == p0 and c2 == p0
    t1 = ((c1[0] - p0[0]) * dx + (c1[1] - p0[1]) * dy) / L
    t2 = ((c2[0] - p0[0]) * dx + (c2[1] - p0[1]) * dy) / L
    return 0 <= t1 <= t2 <= 1


@dataclass
class Glyph:
    name: str
    advance: int
    cbox: tuple[int, int, int, int]
    tight: tuple[float, float, float, float]
    ops: list[tuple[int, list[float]]]

    @property
    def contours(self) -> int:
        return sum(1 for v, _ in self.ops if v == MOVE)


@dataclass
class Face:
    key: str
    lean_name: str
    path: Path
    font: TTFont
    coord_div: int
    glyph_names: list[str] = field(default_factory=list)
    glyphs: list[Glyph] = field(default_factory=list)
    cmap: dict[int, int] = field(default_factory=dict)  # codepoint -> subset glyph id
    kern: dict[tuple[int, int], int] = field(default_factory=dict)

    @property
    def upem(self) -> int:
        return int(self.font["head"].unitsPerEm)

    def gid_for(self, name: str) -> int:
        if name in self.glyph_names:
            return self.glyph_names.index(name)
        self.glyph_names.append(name)
        self.glyphs.append(extract(self.font, name))
        return len(self.glyph_names) - 1


def extract(font: TTFont, name: str) -> Glyph:
    gs = font.getGlyphSet()
    pen = OutlinePen(gs)
    gs[name].draw(pen)
    cb = ControlBoundsPen(gs)
    gs[name].draw(cb)
    tb = BoundsPen(gs)
    gs[name].draw(tb)
    adv = int(font["hmtx"][name][0])
    if cb.bounds is None:
        cbox = (0, 0, 0, 0)
        tight = (0.0, 0.0, 0.0, 0.0)
    else:
        if any(v != int(v) for v in cb.bounds):
            raise ValueError(f"non-integral control box for {name}: {cb.bounds}")
        cbox = tuple(int(v) for v in cb.bounds)  # type: ignore[assignment]
        tight = tuple(float(v) for v in tb.bounds)  # type: ignore[arg-type,assignment]
    return Glyph(name, adv, cbox, tight, pen.ops)  # type: ignore[arg-type]


# ---------------------------------------------------------------------------
# Kerning (legacy `kern` table first, as FreeType's FT_Get_Kerning; else GPOS pairs)
# ---------------------------------------------------------------------------


def kerning_pairs(font: TTFont) -> dict[tuple[str, str], int]:
    pairs: dict[tuple[str, str], int] = {}
    if "kern" in font:
        for t in font["kern"].kernTables:
            if getattr(t, "format", 0) == 0:
                for (l, r), v in t.kernTable.items():
                    pairs.setdefault((l, r), int(v))
        return pairs
    if "GPOS" not in font:
        return pairs
    gpos = font["GPOS"].table
    kern_lookups: set[int] = set()
    for fr in gpos.FeatureList.FeatureRecord:
        if fr.FeatureTag == "kern":
            kern_lookups.update(fr.Feature.LookupListIndex)
    for li in sorted(kern_lookups):
        lookup = gpos.LookupList.Lookup[li]
        subtables = lookup.SubTable
        if lookup.LookupType == 9:
            subtables = [st.ExtSubTable for st in subtables]
        for st in subtables:
            if getattr(st, "LookupType", 2) != 2 and lookup.LookupType != 9:
                continue
            cov = st.Coverage.glyphs
            if st.Format == 1:
                for i, first in enumerate(cov):
                    for pvr in st.PairSet[i].PairValueRecord:
                        v = getattr(pvr.Value1, "XAdvance", 0) if pvr.Value1 else 0
                        if v:
                            pairs.setdefault((first, pvr.SecondGlyph), int(v))
            elif st.Format == 2:
                c1 = st.ClassDef1.classDefs
                c2 = st.ClassDef2.classDefs
                seconds: dict[int, list[str]] = {}
                for g, c in c2.items():
                    seconds.setdefault(c, []).append(g)
                for first in cov:
                    rec = st.Class1Record[c1.get(first, 0)]
                    for k, c2r in enumerate(rec.Class2Record):
                        v = getattr(c2r.Value1, "XAdvance", 0) if c2r.Value1 else 0
                        if v and k in seconds:
                            for second in seconds[k]:
                                pairs.setdefault((first, second), int(v))
    return pairs


# ---------------------------------------------------------------------------
# Binary encoding
# ---------------------------------------------------------------------------


def zigzag(n: int) -> int:
    return (n << 1) ^ (n >> 63) if n >= 0 else ((-n) << 1) - 1


def varint(n: int, out: bytearray) -> None:
    while True:
        b = n & 0x7F
        n >>= 7
        if n:
            out.append(b | 0x80)
        else:
            out.append(b)
            return


def encode_face(face: Face) -> bytes:
    f = face.font
    hhea = f["hhea"]
    os2 = f["OS/2"]
    cap = int(getattr(os2, "sCapHeight", 0) or 0)
    xh = int(getattr(os2, "sxHeight", 0) or 0)
    if cap == 0:
        cap = _glyph_top(f, "H")
    if xh == 0:
        xh = _glyph_top(f, "x")
    name = f["name"].getDebugName(4) or face.key
    nb = name.encode("utf-8")[:255]
    out = bytearray(b"LPF1")
    out += struct.pack(
        "<BBHhhhhhHHHB",
        1,
        face.coord_div,
        face.upem,
        int(hhea.ascent),
        int(hhea.descent),
        int(hhea.lineGap),
        cap,
        xh,
        len(face.glyphs),
        len(face.cmap),
        len(face.kern),
        len(nb),
    )
    out += nb
    for cp in sorted(face.cmap):
        out += struct.pack("<IH", cp, face.cmap[cp])
    for g in face.glyphs:
        nverbs = len(g.ops)
        ncoords = sum(len(c) for _, c in g.ops)
        out += struct.pack("<HhhhhHH", g.advance, *g.cbox, nverbs, ncoords)
    for g in face.glyphs:
        out += bytes(v for v, _ in g.ops)
    coords = bytearray()
    for g in face.glyphs:
        px = py = 0
        for _, cs in g.ops:
            for i in range(0, len(cs), 2):
                x, y = cs[i] * face.coord_div, cs[i + 1] * face.coord_div
                if x != int(x) or y != int(y):
                    raise ValueError(f"{face.key}/{g.name}: coordinate not a multiple of 1/{face.coord_div}")
                x, y = int(x), int(y)
                varint(zigzag(x - px), coords)
                varint(zigzag(y - py), coords)
                px, py = x, y
    out += coords
    for (l, r) in sorted(face.kern):
        out += struct.pack("<HHh", l, r, face.kern[(l, r)])
    return bytes(out)


def _glyph_top(f: TTFont, ch: str) -> int:
    cmap = f.getBestCmap()
    if ord(ch) not in cmap:
        return 0
    gs = f.getGlyphSet()
    cb = ControlBoundsPen(gs)
    gs[cmap[ord(ch)]].draw(cb)
    return int(cb.bounds[3]) if cb.bounds else 0


def lean_module(face: Face, blob: bytes, src_name: str, provenance: str, licence: str) -> str:
    b64 = base64.b64encode(blob).decode("ascii")
    width = 100
    lines = [b64[i : i + width] for i in range(0, len(b64), width)]
    body = "\n".join(lines)
    return f'''/-
Generated by `scripts/font/gen.py`; do not edit by hand.

Source font: `{src_name}` ({provenance}).
{licence}
Subset: {len(face.cmap)} codepoints, {len(face.glyphs)} glyphs, {len(face.kern)} kerning pairs,
{len(blob)} bytes (blob format documented in `scripts/font/gen.py` and `LeanPlot/Font/Face.lean`).
-/

namespace LeanPlot.Font.Data

/-- Base64 blob of the `{face.key}` face subset (see `LeanPlot.Font.Face.decode`). Whitespace
inside the literal is ignored by the decoder. -/
def {face.lean_name} : String := "
{body}
"

end LeanPlot.Font.Data
'''


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

HEROS_LICENCE = (
    "Licence: GUST Font License (LPPL 1.3c or later), see `licenses/GUST-FONT-LICENSE.txt`.\n"
    "Copyright 2006, 2009 for TeX Gyre extensions by B. Jackowski and J.M. Nowacki\n"
    "(on behalf of TeX users groups). \"TeX Gyre Heros Makie\" is the Makie.jl build of\n"
    "TeX Gyre Heros; this subset is a derived work and is not a font file."
)
DEJAVU_LICENCE = (
    "Licence: Bitstream Vera Fonts licence + DejaVu changes in the public domain (+ Arev\n"
    "licence for Arev glyphs), see `licenses/DejaVu-LICENSE.txt`. Copyright (c) 2003 by\n"
    "Bitstream, Inc.; Copyright (c) 2006 by Tavmjong Bah. This subset is a derived work and\n"
    "is not a font file; it is not distributed under the Bitstream Vera / DejaVu names."
)


def main(argv: Iterable[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--fonts", type=Path, default=Path(os.environ.get("MAKIE_FONTS", DEFAULT_FONTS)))
    ap.add_argument("--out", type=Path, default=REPO)
    args = ap.parse_args(list(argv) if argv is not None else None)

    fonts: Path = args.fonts
    out: Path = args.out
    reg = Face("HerosRegular", "herosRegularBlob", fonts / "TeXGyreHerosMakie-Regular.otf",
               TTFont(fonts / "TeXGyreHerosMakie-Regular.otf"), 1)
    bold = Face("HerosBold", "herosBoldBlob", fonts / "TeXGyreHerosMakie-Bold.otf",
                TTFont(fonts / "TeXGyreHerosMakie-Bold.otf"), 1)
    dv = Face("DejaVuSans", "dejaVuSansBlob", fonts / "DejaVuSans.ttf", TTFont(fonts / "DejaVuSans.ttf"), 2)

    charset: list[int] = []
    seen: set[int] = set()
    for _, cps in CHARSET_GROUPS:
        for cp in cps:
            # skip unassigned codepoints inside requested ranges (e.g. U+2072, holes in 𝔸…𝕫)
            if unicodedata.category(chr(cp)) == "Cn" and cp not in ALIASES:
                continue
            if cp not in seen:
                seen.add(cp)
                charset.append(cp)

    hcm = reg.font.getBestCmap()
    bcm = bold.font.getBestCmap()
    dcm = dv.font.getBestCmap()
    for face in (reg, bold, dv):
        face.gid_for(face.font.getGlyphOrder()[0])  # .notdef first

    missing: list[int] = []
    placement: dict[int, str] = {}
    for cp in charset:
        if cp in hcm or cp in bcm:
            # Makie: Heros Bold -> Heros Regular -> DejaVu; the two Heros cmaps are identical.
            if cp in hcm and cp in bcm:
                reg.cmap[cp] = reg.gid_for(hcm[cp])
                bold.cmap[cp] = bold.gid_for(bcm[cp])
                placement[cp] = "primary"
                continue
            raise SystemExit(f"U+{cp:04X} is in only one Heros face; fallback order would differ")
        if cp in dcm:
            dv.cmap[cp] = dv.gid_for(dcm[cp])
            placement[cp] = "fallback"
        else:
            missing.append(cp)
    for src, dst in ALIASES.items():
        if src in hcm or src in dcm:
            continue
        if dst in reg.cmap:
            reg.cmap[src] = reg.cmap[dst]
            bold.cmap[src] = bold.cmap[dst]
            placement[src] = "primary"
        elif dst in dv.cmap:
            dv.cmap[src] = dv.cmap[dst]
            placement[src] = "fallback"
        else:
            raise SystemExit(f"alias target U+{dst:04X} not embedded")

    for face in (reg, bold, dv):
        pairs = kerning_pairs(face.font)
        idx = {n: i for i, n in enumerate(face.glyph_names)}
        for (l, r), v in pairs.items():
            if l in idx and r in idx and v != 0:
                face.kern[(idx[l], idx[r])] = v

    data_dir = out / "LeanPlot/Font/Data"
    data_dir.mkdir(parents=True, exist_ok=True)
    sizes = {}
    for face, fname, prov, lic in (
        (reg, "TeXGyreHerosMakie-Regular.otf", "Makie.jl assets/fonts, TeX Gyre Heros Makie Regular", HEROS_LICENCE),
        (bold, "TeXGyreHerosMakie-Bold.otf", "Makie.jl assets/fonts, TeX Gyre Heros Makie Bold", HEROS_LICENCE),
        (dv, "DejaVuSans.ttf", "Makie.jl assets/fonts, DejaVu Sans 2.37", DEJAVU_LICENCE),
    ):
        blob = encode_face(face)
        sizes[face.key] = len(blob)
        (data_dir / f"{face.key}.lean").write_text(lean_module(face, blob, fname, prov, lic), encoding="utf-8")

    # Reference data for the Lean tests.
    def face_json(face: Face) -> dict:
        hhea = face.font["hhea"]
        glyphs = {}
        for cp, gid in sorted(face.cmap.items()):
            g = face.glyphs[gid]
            glyphs[str(cp)] = {
                "gid": gid,
                "name": g.name,
                "advance": g.advance,
                "cbox": list(g.cbox),
                "bounds": [round(v, 6) for v in g.tight],
                "contours": g.contours,
                "verbs": len(g.ops),
            }
        notdef = face.glyphs[0]
        return {
            "name": face.font["name"].getDebugName(4),
            "unitsPerEm": face.upem,
            "ascender": int(hhea.ascent),
            "descender": int(hhea.descent),
            "lineGap": int(hhea.lineGap),
            "nGlyphs": len(face.glyphs),
            "notdef": {"advance": notdef.advance, "cbox": list(notdef.cbox), "contours": notdef.contours},
            "glyphs": glyphs,
            "kern": [[l, r, v] for (l, r), v in sorted(face.kern.items())],
        }

    ref = {
        "generator": "scripts/font/gen.py",
        "faces": {"HerosRegular": face_json(reg), "HerosBold": face_json(bold), "DejaVuSans": face_json(dv)},
        "placement": {str(cp): p for cp, p in sorted(placement.items())},
        "missing": missing,
        "aliases": {str(k): v for k, v in ALIASES.items()},
    }
    test_dir = out / "LeanPlotTest/Font/data"
    test_dir.mkdir(parents=True, exist_ok=True)
    (test_dir / "metrics.json").write_text(json.dumps(ref, ensure_ascii=False, indent=None, separators=(",", ":")) + "\n",
                                           encoding="utf-8")

    print(f"charset: {len(charset)} codepoints requested (+{len(ALIASES)} aliased), {len(placement)} embedded "
          f"({sum(1 for p in placement.values() if p == 'primary')} Heros, "
          f"{sum(1 for p in placement.values() if p == 'fallback')} DejaVu), {len(missing)} not in either face")
    if missing:
        print("  not embedded:", "".join(chr(c) for c in missing))
    for face in (reg, bold, dv):
        print(f"  {face.key}: {len(face.glyphs)} glyphs, {len(face.cmap)} cmap, {len(face.kern)} kern, "
              f"{sizes[face.key]} bytes")
    return 0


if __name__ == "__main__":
    sys.exit(main())
