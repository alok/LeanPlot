# Julia/Makie oracle for LeanPlot.Recipes.Algo.{F32,Levels}.
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/levels_oracle.jl
# Writes LeanPlotTest/oracle/recipes/levels.json (deterministic, seeded RNG).
# Float32 values are written widened to Float64 (exact); non-finite values as
# the strings "NaN", "Inf", "-Inf".
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const OUT = @__DIR__
rng = MersenneTwister(20260924)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]

randf32() = Float32(randn(rng) * 10.0^rand(rng, -3:3))

out = Dict{String,Any}()

# Float32 range(a, b; length = n)
r1 = Any[]
pairs = Any[(0f0, 1f0), (-1f0, 1f0), (0.1f0, 0.9f0), (1f0, 1f0), (-2.5f0, 7.25f0), (0f0, 13.000001f0),
            (1f-3, 5f-3), (-3f0, -2f0), (1f6, 2f6), (0.3f0, 0.7f0), (-1f0, nextfloat(1f0)), (5f0, -5f0)]
for _ in 1:150
    push!(pairs, (randf32(), randf32()))
end
for _ in 1:40
    push!(pairs, (Float32(rand(rng, -50:50) / rand(rng, 1:16)), Float32(rand(rng, -50:50) / rand(rng, 1:16))))
end
for (a, b) in pairs, n in (0, 1, 2, 3, 6, 11, 21)
    (n == 1 && a != b) && continue
    push!(r1, Dict("a" => jf(a), "b" => jf(b), "n" => n, "v" => jfv(collect(range(a, b; length = n)))))
end
out["range32"] = r1

# Float32 range(a; step = s, length = n)
r2 = Any[]
spairs = Any[(0f0, 0.1f0), (1f0, 0.5f0), (2.1666667f0, 2.1666667f0), (-1f0, 0.25f0), (0.1f0, 0.2f0)]
for _ in 1:150
    push!(spairs, (randf32(), abs(randf32()) + 1f-6))
end
for _ in 1:40
    push!(spairs, (Float32(rand(rng, -50:50) / rand(rng, 1:16)), Float32(rand(rng, 1:30) / rand(rng, 1:16))))
end
for (a, s) in spairs, n in (0, 1, 2, 5, 10)
    push!(r2, Dict("a" => jf(a), "s" => jf(s), "n" => n, "v" => jfv(collect(range(a; step = s, length = n)))))
end
out["rangestep32"] = r2

# Makie to_levels(n, (zmin, zmax)) on Float32 ranges, and _get_isoband_levels
lv = Any[]
zr = Any[(0f0, 13f0), (-1f0, 1f0), (0f0, 1f0), (2f0, 10f0), (-0.99f0, 0.99f0), (1f0, 1.0001f0)]
for _ in 1:120
    a = randf32()
    push!(zr, (a, a + abs(randf32()) + 1f-3))
end
for (lo, hi) in zr, n in (1, 2, 3, 5, 7, 10, 20)
    push!(lv, Dict("lo" => jf(lo), "hi" => jf(hi), "n" => n,
                   "levels" => jfv(collect(Mk.to_levels(n, (lo, hi)))),
                   "isoband" => jfv(Mk._get_isoband_levels(n, lo, hi))))
end
out["levels"] = lv

# Full recipe nodes: contour zlevels and contourf computed_levels from real plots
fields = Any[]
xs = collect(range(-2, 2, length = 7)); ys = collect(range(-1, 3, length = 9))
push!(fields, ("paraboloid", [x^2 + y^2 for x in xs, y in ys]))
push!(fields, ("constant", fill(2.5, 7, 9)))
push!(fields, ("nearconst", [1.0 + 1e-6 * x for x in xs, y in ys]))
push!(fields, ("saddle", [x * y for x in xs, y in ys]))
push!(fields, ("random", randn(rng, 7, 9)))
nodes = Any[]
for (name, z) in fields
    for spec in (5, 10, 3, [-1.0, 0.0, 0.5, 2.0])
        fig = Figure(); ax = Axis(fig[1, 1])
        c = contour!(ax, xs, ys, z; levels = spec)
        cf = contourf!(ax, xs, ys, z; levels = spec)
        d = Dict{String,Any}("name" => name, "z" => jfv(vec(Float32.(z))), "spec" => spec isa Int ? spec : jfv(spec),
                 "zlevels" => jfv(collect(c.zlevels[])), "cflevels" => jfv(collect(cf.computed_levels[])))
        if spec isa Vector
            cf2 = contourf!(ax, xs, ys, z; levels = [0.1, 0.5, 0.9], mode = :relative)
            d["relative"] = jfv(collect(cf2.computed_levels[]))
        end
        push!(nodes, d)
    end
end
out["nodes"] = nodes

open(joinpath(OUT, "levels.json"), "w") do io; JSON.print(io, out); end
println("levels.json: ", length(r1), " range32, ", length(r2), " rangestep32, ", length(lv), " level cases, ", length(nodes), " nodes")
