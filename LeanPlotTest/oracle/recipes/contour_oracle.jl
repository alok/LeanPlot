# Julia/Makie oracle for LeanPlot.Recipes.Algo.Contour (Contour.jl marching
# squares as used by Makie's `contour` recipe).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/contour_oracle.jl
# Writes LeanPlotTest/oracle/recipes/contour.json. For every case:
#  * "makie": the recipe's zlevels, contour_points (Point2f, NaN separated) and
#    elements_per_segment (level index, count);
#  * "raw64": Contour.contours on the Float64 data at the Float64-widened levels,
#    every line in Makie's canonical_line_order.
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const Ct = Mk.Contours
const OUT = @__DIR__
rng = MersenneTwister(4242)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]

cases = Any[]
x = collect(range(-2, 2, length = 7)); y = collect(range(-1, 3, length = 9))
push!(cases, ("paraboloid", x, y, [a^2 + b^2 for a in x, b in y], 5))
x = collect(range(0, 2pi, length = 64)); y = collect(range(0, 2pi, length = 64))
push!(cases, ("sincos", x, y, [sin(a) * cos(b) for a in x, b in y], 7))
x = collect(range(-1, 1, length = 9)); y = collect(range(-1, 1, length = 9))
push!(cases, ("saddle", x, y, [a * b for a in x, b in y], [-0.5, 0.0, 0.5]))
x = sort(randn(rng, 11)); y = sort(randn(rng, 8))
push!(cases, ("irregular", x, y, randn(rng, 11, 8), 6))
x = collect(range(-5, 5, length = 40)); y = collect(range(-5, 5, length = 40))
push!(cases, ("himmelblau", x, y, [(a^2 + b - 11)^2 + (a + b^2 - 7)^2 for a in x, b in y], 10))
x = collect(1.0:6.0); y = collect(1.0:5.0)
push!(cases, ("checker", x, y, [(-1.0)^(i + j) for i in 1:6, j in 1:5], [0.0, 0.1, -0.1]))
x = collect(range(-3, 3, length = 25)); y = collect(range(-3, 3, length = 21))
push!(cases, ("rings", x, y, [cos(a^2 + b^2) for a in x, b in y], [0.25, -0.5, 0.9]))
# curvilinear grid (matrix x, y)
r = range(0.5, 2.0, length = 12); th = range(0, 3pi / 2, length = 17)
xm = [a * cos(t) for a in r, t in th]; ym = [a * sin(t) for a in r, t in th]
push!(cases, ("curvilinear", xm, ym, [a^2 + 0.3 * sin(3t) for a in r, t in th], 5))

out = Any[]
for (name, x, y, z, levels) in cases
    fig = Figure(); ax = Axis(fig[1, 1])
    c = contour!(ax, x, y, z; levels = levels)
    zl = collect(c.zlevels[])
    pts = c.contour_points[]
    eps = c.elements_per_segment[]
    cr = c.computed_colorrange[]
    lc = c.level_colors[]
    raw = Ct.contours(x, y, z, Float64.(zl))
    rawlines = Any[]
    for (k, lvl) in enumerate(Ct.levels(raw)), l in Ct.lines(lvl)
        v = Mk.canonical_line_order(l.vertices)
        push!(rawlines, Dict("level" => k - 1, "x" => jfv(first.(v)), "y" => jfv(last.(v))))
    end
    d = Dict{String,Any}("name" => name, "curvilinear" => x isa Matrix, "nx" => size(z, 1), "ny" => size(z, 2),
        "x" => jfv(vec(x)), "y" => jfv(vec(y)), "z" => jfv(vec(z)), "spec" => levels isa Int ? levels : jfv(levels),
        "zlevels" => jfv(zl), "px" => jfv(first.(pts)), "py" => jfv(last.(pts)),
        "segs" => [[Int(first(e)) - 1, Int(last(e))] for e in eps], "raw64" => rawlines,
        "colorrange" => jfv(collect(cr)), "level_colors" => [jfv([q.r, q.g, q.b, q.alpha]) for q in lc])
    push!(out, d)
    println(name, ": ", length(zl), " levels, ", length(eps), " lines, ", length(pts), " points")
end
open(joinpath(OUT, "contour.json"), "w") do io; JSON.print(io, Dict("cases" => out)); end
