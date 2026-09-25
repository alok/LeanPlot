# Julia/Makie oracle for LeanPlot.Recipes.Algo.Isoband (isoband C++ library via
# Isoband.jl, and Makie's `contourf` recipe).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/isoband_oracle.jl
# Writes LeanPlotTest/oracle/recipes/isoband.json. For every case:
#  * "raw": Isoband.isobands on Makie's inputs (Float64 x/y, Float32-rounded z,
#    Float32 band edges) per band: x, y, id;
#  * "makie": the recipe's computed_levels and polygons (exterior + interiors,
#    Point2f) with their colour values.
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const IB = Mk.Isoband
const OUT = @__DIR__
rng = MersenneTwister(777)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]

cases = Any[]
x = collect(range(-2, 2, length = 7)); y = collect(range(-1, 3, length = 9))
push!(cases, ("paraboloid", x, y, [a^2 + b^2 for a in x, b in y], 10, false, false))
x = collect(range(0, 2pi, length = 40)); y = collect(range(0, 2pi, length = 30))
push!(cases, ("sincos", x, y, [sin(a) * cos(b) for a in x, b in y], 7, false, false))
x = collect(range(-1, 1, length = 9)); y = collect(range(-1, 1, length = 9))
push!(cases, ("saddle", x, y, [a * b for a in x, b in y], [-0.5, 0.0, 0.5], true, true))
x = sort(randn(rng, 11)); y = sort(randn(rng, 8))
push!(cases, ("irregular", x, y, randn(rng, 11, 8), 6, false, false))
x = collect(range(-5, 5, length = 40)); y = collect(range(-5, 5, length = 40))
push!(cases, ("himmelblau", x, y, [(a^2 + b - 11)^2 + (a + b^2 - 7)^2 for a in x, b in y], [0.0, 10.0, 50.0, 100.0, 200.0], false, true))
x = collect(1.0:6.0); y = collect(1.0:5.0)
push!(cases, ("checker", x, y, [(-1.0)^(i + j) for i in 1:6, j in 1:5], [-0.5, 0.5], true, false))
x = collect(range(-3, 3, length = 25)); y = collect(range(-3, 3, length = 21))
push!(cases, ("rings", x, y, [cos(a^2 + b^2) for a in x, b in y], 5, false, false))
x = collect(range(0, 1, length = 12)); y = collect(range(0, 1, length = 10))
zz = [sin(7a) * cos(5b) for a in x, b in y]; zz[4, 5] = NaN; zz[8, 2] = NaN
push!(cases, ("nans", x, y, zz, [-0.6, -0.1, 0.3, 0.8], false, false))
x = collect(range(-2, 2, length = 21)); y = collect(range(-2, 2, length = 21))
push!(cases, ("annulus", x, y, [exp(-(a^2 + b^2)) - 0.5 * exp(-4 * ((a - 0.3)^2 + b^2)) for a in x, b in y], 8, false, false))

out = Any[]
for (name, x, y, z, levels, elow, ehigh) in cases
    fig = Figure(); ax = Axis(fig[1, 1])
    cf = contourf!(ax, x, y, z; levels = levels, extendlow = elow ? :auto : nothing, extendhigh = ehigh ? :auto : nothing)
    lv = collect(cf.computed_levels[])
    edges = Float32.(copy(lv))
    elow && pushfirst!(edges, -Inf32)
    ehigh && push!(edges, Inf32)
    lows = edges[1:end-1]; highs = edges[2:end]
    z32 = Float32.(z)
    raw = IB.isobands(x, y, z32', lows, highs)
    rawj = [Dict("x" => jfv(r.x), "y" => jfv(r.y), "id" => collect(r.id)) for r in raw]
    polys = cf.polys[]
    pj = [Dict("outer" => [jfv(first.(p.exterior)), jfv(last.(p.exterior))],
               "holes" => [[jfv(first.(h)), jfv(last.(h))] for h in p.interiors]) for p in polys]
    Mk.update_state_before_display!(fig)
    rgba = Mk.numbers_to_colors(cf.computed_colors[], Mk.to_colormap(cf.computed_colormap[]), identity,
        Mk.Vec2f(cf.computed_colorrange[]...), cf.computed_lowcolor[], cf.computed_highcolor[], Mk.RGBAf(0, 0, 0, 0), true)
    d = Dict{String,Any}("name" => name, "nx" => size(z, 1), "ny" => size(z, 2), "x" => jfv(x), "y" => jfv(y),
        "z" => jfv(vec(z)), "spec" => levels isa Int ? levels : jfv(levels), "extendlow" => elow, "extendhigh" => ehigh,
        "levels" => jfv(lv), "lows" => jfv(lows), "highs" => jfv(highs), "raw" => rawj,
        "polys" => pj, "colors" => jfv(cf.computed_colors[]),
        "rgba" => [jfv([c.r, c.g, c.b, c.alpha]) for c in rgba])
    push!(out, d)
    println(name, ": ", length(lv), " levels, ", sum(r -> length(unique(r.id)), raw), " rings, ", length(polys), " polygons")
end
open(joinpath(OUT, "isoband.json"), "w") do io; JSON.print(io, Dict("cases" => out)); end
