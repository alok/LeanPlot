# Makie text-layout oracle for LeanPlot.Font.
#
#   julia --startup-file=no --project=<env with CairoMakie> scripts/font/oracle.jl [out.json]
#
# Dumps, for the fonts Makie actually uses (default `TeX Gyre Heros Makie` + its fallbacks):
#   * FreeType face metrics (ascender, descender, height, units_per_EM) per face;
#   * FreeTypeAbstraction extents (hadvance, ink bounding box) for sample characters;
#   * `Makie.glyph_collection` output (per-character origins, face, hadvance) and
#     `Makie.unchecked_boundingbox` for sample strings under every halign × valign,
#     multi-line text and a non-default lineheight/justification.
# Everything is in pixels at the given font size (no rotation; rotation is tested in Lean).
using CairoMakie
using JSON
const M = CairoMakie.Makie
const FTA = M.FreeTypeAbstraction

out = length(ARGS) >= 1 ? ARGS[1] : joinpath(@__DIR__, "..", "..", "LeanPlotTest", "Font", "data", "makie_layout.json")

regular = M.to_font("TeX Gyre Heros Makie")
bold = M.to_font("TeX Gyre Heros Makie Bold")
dejavu = M.alternativefonts()[2]

facekey(f) = f.family_name == "DejaVu Sans" ? "DejaVuSans" :
             (f.style_name == "Bold" ? "HerosBold" : "HerosRegular")

facejson(f) = Dict(
    "family" => f.family_name, "style" => f.style_name,
    "ascender" => f.ascender, "descender" => f.descender,
    "height" => f.height, "unitsPerEm" => f.units_per_EM,
)

faces = Dict("HerosRegular" => facejson(regular), "HerosBold" => facejson(bold), "DejaVuSans" => facejson(dejavu))

sample_chars = collect("AVWxgjÅé0123456789−+.,-()[]αβπθωΣΔ∂∞≤≥≈√∑∫∇∧∨⋅→⟨⟩₀₁₂⁰¹²ᵢⱼ ")
extents = []
for c in sample_chars
    f = M.find_font_for_char(c, regular)
    e = M.GlyphExtent(f, c)
    bb = e.ink_bounding_box
    push!(extents, Dict(
        "char" => string(c), "cp" => Int(c), "face" => facekey(f),
        "hadvance" => e.hadvance, "ascender" => e.ascender, "descender" => e.descender,
        "ink" => [bb.origin[1], bb.origin[2], bb.origin[1] + bb.widths[1], bb.origin[2] + bb.widths[2]],
    ))
end

cases = []
function addcase(str, font, fontsize, halign, valign; lineheight = 1.0f0, justification = M.automatic)
    rot = M.Quaternionf(0, 0, 0, 1)
    gc = M.glyph_collection(str, font, Float32(fontsize), halign, valign, Float32(lineheight),
                            justification, -1, rot)
    chars = collect(str)
    glyphs = []
    for (i, c) in enumerate(chars)
        o = gc.char_origins[i]
        push!(glyphs, Dict("char" => string(c), "cp" => Int(c), "face" => facekey(gc.font_per_char[i]),
                           "glyph" => Int(gc.glyphindices[i]), "x" => o[1], "y" => o[2],
                           "hadvance" => gc.glyph_extents[i].hadvance))
    end
    bb = isempty(chars) ? nothing :
        M.unchecked_boundingbox(gc.glyphindices, gc.char_origins, Float32(fontsize), gc.glyph_extents, rot)
    push!(cases, Dict(
        "text" => str, "bold" => font === bold, "size" => fontsize,
        "halign" => string(halign), "valign" => string(valign),
        "lineheight" => lineheight,
        "justification" => justification === M.automatic ? nothing : justification,
        "glyphs" => glyphs,
        "bbox" => bb === nothing ? nothing :
            [bb.origin[1], bb.origin[2], bb.origin[1] + bb.widths[1], bb.origin[2] + bb.widths[2]],
    ))
end

strings = ["x", "Hello, World", "−1.5", "10⁻³", "v₁₂ ∧ w₃", "∂ₖ ϵ¹ ∇ ⟨a, b⟩", "AVAWAY",
           "θ ∈ [0, 2π)", "∫ f dx ≈ ∑ fᵢ Δx", "Å gjpqy"]
for s in strings, ha in (:left, :center, :right), va in (:baseline, :bottom, :center, :top)
    addcase(s, regular, 14, ha, va)
end
for s in ["Bold title", "x∫y"]
    addcase(s, bold, 16, :center, :top)
end
for s in ["line one\nline two", "a\n∫\nccc", "first\n", "\nsecond", "a\n\nb"],
    ha in (:left, :center, :right), va in (:baseline, :bottom, :center, :top)
    addcase(s, regular, 12, ha, va)
end
addcase("wide line\nx", regular, 10, :left, :top; lineheight = 1.5f0)
addcase("wide line\nx", regular, 10, :left, :top; justification = 1.0f0)
addcase("wide line\nx", regular, 10, :right, :bottom; justification = 0.5f0)

open(out, "w") do io
    JSON.print(io, Dict("makie" => string(pkgversion(M)), "faces" => faces,
                        "extents" => extents, "cases" => cases))
end
println("wrote ", out, " (", length(cases), " cases)")
