# Julia/Makie oracle for LeanPlot.Scale.
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/core/scale_oracle.jl
# Writes LeanPlotTest/oracle/core/scales.json: forward/inverse transforms on
# sample points, default limits and defined intervals for Makie's scales.
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const OUT = @__DIR__
rng = MersenneTwister(777)
jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
safe(f, x) = try jf(f(x)) catch; "NaN" end

scales = Any[
    ("identity", identity, Dict()),
    ("log10", log10, Dict()), ("log2", log2, Dict()), ("ln", log, Dict()),
    ("sqrt", sqrt, Dict()),
    ("pseudolog10", Mk.pseudolog10, Dict()),
    ("symlog10", Mk.Symlog10(-2.0, 3.0), Dict("lower" => -2.0, "upper" => 3.0, "linscale" => 1.0)),
    ("symlog10", Mk.Symlog10(-1.0, 1.0; linscale = 0.5), Dict("lower" => -1.0, "upper" => 1.0, "linscale" => 0.5)),
    ("logit", Mk.logit, Dict()),
]
xs = vcat([0.0, 1.0, -1.0, 0.5, 2.0, 10.0, 1e-3, 1e5, -1e5, 0.999, 0.001, 3.0, -2.0], randn(rng, 20) .* 10, rand(rng, 20))
ys = vcat([0.0, 1.0, -1.0, 0.5, 2.0, 3.0, -3.0, 0.25, 7.5], randn(rng, 20) .* 3)
out = Any[]
for (name, s, params) in scales
    inv = Mk.inverse_transform(s)
    iv = Mk.defined_interval(s)
    lims = Mk.defaultlimits(s)
    push!(out, merge(Dict("name" => name, "x" => [jf(x) for x in xs], "forward" => [safe(s, x) for x in xs],
                          "y" => [jf(y) for y in ys], "inverse" => [safe(inv, y) for y in ys],
                          "defaultlimits" => [Float64(lims[1]), Float64(lims[2])],
                          "interval" => [jf(Float64(Mk.IntervalSets.leftendpoint(iv))), jf(Float64(Mk.IntervalSets.rightendpoint(iv))),
                                         Mk.IntervalSets.isleftclosed(iv), Mk.IntervalSets.isrightclosed(iv)]), params))
end
open(joinpath(OUT, "scales.json"), "w") do io; JSON.print(io, out); end
println("wrote scales.json: ", length(out), " scales")
