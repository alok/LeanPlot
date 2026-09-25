# Julia/Makie oracle for LeanPlot.Recipes.Algo.Heatmap (Makie heatmap cell edges,
# CairoMakie regular-grid detection).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/heatmap_oracle.jl
# Writes LeanPlotTest/oracle/recipes/heatmap.json.
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const CM = CairoMakie
const OUT = @__DIR__
rng = MersenneTwister(12)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]

out = Dict{String,Any}()
vs = Any[[0.0], [1.0, 2.0], collect(range(0, 1, length = 7)), [0.0, 0.1, 0.5, 2.0, 2.2], sort(randn(rng, 9)),
         collect(range(-3, 5, length = 33)), [5.0, 4.0, 1.0], collect(0.1:0.1:0.9)]
out["edges"] = [Dict("v" => jfv(v), "e" => jfv(Mk.edges(v))) for v in vs]

plots = Any[]
z = rand(rng, 7, 5)
for (name, x, y) in (("centers", collect(range(0, 1, length = 7)), collect(range(-1, 1, length = 5))),
                     ("edges", collect(range(0, 1, length = 8)), collect(range(-1, 1, length = 6))),
                     ("irregular", sort(rand(rng, 7)), sort(rand(rng, 5))),
                     ("tuple", (0.0, 1.0), (-2.0, 3.0)),
                     ("interval", 0.0 .. 3.0, 1.0 .. 2.0),
                     ("range", range(0, 1, length = 7), range(-1, 1, length = 5)))
    fig = Figure(); ax = Axis(fig[1, 1])
    h = heatmap!(ax, x, y, z)
    hx = h.x[]; hy = h.y[]
    enc(v) = v isa Mk.EndPoints ? Dict("endpoints" => jfv([v[1], v[2]])) : Dict("values" => jfv(collect(v)))
    inp(v) = v isa Tuple ? jfv(collect(v)) : (v isa Mk.ClosedInterval ? jfv([Mk.leftendpoint(v), Mk.rightendpoint(v)]) : jfv(collect(v)))
    push!(plots, Dict("name" => name, "kind" => string(typeof(x).name.name), "x" => inp(x), "y" => inp(y),
        "nx" => size(z, 1), "ny" => size(z, 2), "hx" => enc(hx), "hy" => enc(hy)))
end
out["plots"] = plots

reg = Any[]
for arr in (Float32.(collect(range(0, 1, length = 8))), Float32[0, 0.1, 0.5, 2.0], Float32.(Mk.edges(collect(range(-2, 3, length = 11)))),
            Float32.(collect(0.1:0.1:1.0)), Float32[3, 2, 1, 0], Float32.(sort(rand(rng, 6))))
    r = CM.regularly_spaced_array_to_range(arr)
    push!(reg, Dict("arr" => jfv(arr), "regular" => r isa AbstractRange,
        "start" => r isa AbstractRange ? jf(first(r)) : nothing, "step" => r isa AbstractRange ? jf(step(r)) : nothing))
end
out["regular"] = reg

open(joinpath(OUT, "heatmap.json"), "w") do io; JSON.print(io, out); end
println("edges ", length(vs), ", plots ", length(plots), ", regular ", length(reg))
