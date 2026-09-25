# Julia/Makie oracle for LeanPlot.Recipes.Algo.Stream (Makie `streamplot_impl`).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/streamplot_oracle.jl
# Writes LeanPlotTest/oracle/recipes/streamplot.json. The field functions are
# mirrored exactly (same operation order) in LeanPlotTest/Recipes/StreamTest.lean.
using CairoMakie, JSON, LinearAlgebra
const Mk = CairoMakie.Makie
const OUT = @__DIR__

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]

rotation(p) = Mk.Point2d(-p[2], p[1])
saddle(p) = Mk.Point2d(p[1], -p[2])
source(p) = Mk.Point2d(p[1], p[2])
function dipole(p)
    ax = p[1] + 0.5; bx = p[1] - 0.5; y = p[2]
    ra = ax * ax + y * y; rb = bx * bx + y * y
    ka = ra * sqrt(ra); kb = rb * sqrt(rb)
    return Mk.Point2d(ax / ka - bx / kb, y / ka - y / kb)
end
f32field(p) = Mk.Point2f(-p[2] + 0.3 * p[1], p[1])
vortex3(p) = Mk.Point3d(-p[2], p[1], 0.3)
tangent3(p) = Mk.Point3d(1.0 - 2.0 * p[2], p[1] * p[2] + 0.2, 0.0)
spiral(p) = Mk.Point2d(-p[2] - 0.2 * p[1], p[1] - 0.2 * p[2])

cases = [
    ("rotation", rotation, Mk.Rect(-2, -2, 4, 4), (8, 8), 0.01, 500, 1.0),
    ("saddle", saddle, Mk.Rect2d(-1.5, -1.0, 3.0, 2.0), (16, 12), 0.02, 200, 0.5),
    ("source", source, Mk.Rect(-1, -1, 2, 2), (8, 8), 0.01, 500, 2.0),
    ("dipole", dipole, Mk.Rect2f(-2, -1.5, 4, 3), (12, 12), 0.05, 100, 1.0),
    ("f32field", f32field, Mk.Rect(-2, -2, 4, 4), (10, 10), 0.01, 500, 1.0),
    ("spiral32", spiral, Mk.Rect2d(-2.0, -2.0, 4.0, 4.0), (32, 32), 0.01, 500, 1.0),
    ("vortex3", vortex3, Mk.Rect3d(Mk.Vec3d(-1, -1, -1), Mk.Vec3d(2, 2, 2)), (5, 5, 5), 0.01, 60, 1.0),
    ("tangent3", tangent3, Mk.Rect3d(Mk.Vec3d(0, 0, -1e-15), Mk.Vec3d(1, 1, 2e-15)), (8, 8, 1), 0.01, 500, 1.0),
    ("vortex3pad", vortex3, Mk.Rect3f(Mk.Vec3f(-1, -1, -1), Mk.Vec3f(2, 2, 2)), (4, 3), 0.03, 80, 0.7),
]

out = Dict{String,Any}()
out["acoeff2"] = jfv(1.324717957244746 .^ (-(1:2)))
out["acoeff3"] = jfv(1.2207440846057596 .^ (-(1:3)))
res = Any[]
for (name, f, lim, gs, st, ms, dens) in cases
    ap, ad, lp, ac, lc = Mk.streamplot_impl(Mk.Point, f, lim, gs, st, ms, dens, norm)
    N = length(first(ap))
    d = Dict{String,Any}("name" => name, "dim" => N, "origin" => jfv(Mk.origin(lim)), "widths" => jfv(Mk.widths(lim)),
        "f32limits" => eltype(Mk.origin(lim)) == Float32, "gridsize" => collect(gs), "stepsize" => st, "maxsteps" => ms,
        "density" => dens, "narrows" => length(ap), "npoints" => length(lp),
        "arrow_pos" => [jfv(getindex.(ap, k)) for k in 1:N], "arrow_dir" => [jfv(getindex.(ad, k)) for k in 1:N],
        "line_points" => [jfv(getindex.(lp, k)) for k in 1:N], "arrow_colors" => jfv(ac), "line_colors" => jfv(lc))
    push!(res, d)
    println(name, ": ", length(ap), " arrows, ", length(lp), " points")
end
out["cases"] = res

# The recipe itself must produce the same arrays as the direct call.
fig = Figure(); ax = Axis(fig[1, 1])
sp = streamplot!(ax, rotation, -2..2, -2..2; gridsize = (8, 8))
ap, ad, lp, ac, lc = Mk.streamplot_impl(Mk.Point, rotation, Mk.Rect(-2, -2, 4, 4), (8, 8), 0.01, 500, 1.0, norm)
out["recipe_matches_impl"] = isequal(sp.line_points[], lp) && isequal(sp.arrow_positions[], ap) && isequal(sp.line_colors[], lc)

open(joinpath(OUT, "streamplot.json"), "w") do io; JSON.print(io, out); end
println("recipe matches impl: ", out["recipe_matches_impl"])
