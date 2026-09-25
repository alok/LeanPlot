# Julia/Makie oracle for LeanPlot.Recipes.Algo.Voxels (Makie `voxels` data as
# CairoMakie draws it).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/voxels_oracle.jl
# Writes LeanPlotTest/oracle/recipes/voxels.json: per case the voxel ids
# (chunk_u8), the id colormap, and CairoMakie's cube positions, size and colours.
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const OUT = @__DIR__
rng = MersenneTwister(2718)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]
cols(ps) = [jfv(getindex.(ps, k)) for k in 1:length(first(ps))]
rgba(cs) = [jfv([c.r, c.g, c.b, c.alpha]) for c in cs]

cases = Any[]
push!(cases, ("random", rand(rng, 4, 3, 5), nothing, (;)))
ch = rand(rng, 5, 4, 3); ch[2, 2, 2] = NaN; ch[1, 3, 1] = NaN
push!(cases, ("clips", ch, nothing, (; colorrange = (0.2, 0.8), lowclip = :red, highclip = :orange)))
push!(cases, ("log", Float64.(reshape(1:27, 3, 3, 3)), (0 .. 3, -1 .. 1, 2 .. 4), (; colorscale = log10, gap = 0.1)))
xs = range(-1, 1, length = 6)
push!(cases, ("holes", [x^2 + y^2 + z^2 for x in xs, y in xs, z in xs], nothing, (; is_air = x -> !(0.9 <= x <= 1.7))))
push!(cases, ("alpha", rand(rng, 3, 3, 3), nothing, (; colormap = :magma, alpha = 0.5)))

out = Any[]
for (name, chunk, ext, kw) in cases
    fig = Figure(); ax = LScene(fig[1, 1])
    p = ext === nothing ? voxels!(ax, chunk; kw...) : voxels!(ax, ext..., chunk; kw...)
    Mk.update_state_before_display!(fig)
    pos = Mk.voxel_positions(p); sz = Mk.voxel_size(p); cs = Mk.voxel_colors(p)
    d = Dict{String,Any}("name" => name, "dims" => collect(size(chunk)), "values" => jfv(vec(chunk)),
        "extent" => ext === nothing ? nothing : [jfv([Mk.leftendpoint(e), Mk.rightendpoint(e)]) for e in ext],
        "ids" => Int.(vec(p.chunk_u8[])), "colormap" => rgba(p.voxel_colormap[]),
        "pos" => isempty(pos) ? [[], [], []] : cols(pos), "size" => jfv(sz), "colors" => rgba(cs))
    push!(out, d)
    println(name, ": ", length(pos), " cubes")
end
open(joinpath(OUT, "voxels.json"), "w") do io; JSON.print(io, Dict("cases" => out)); end
