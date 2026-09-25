# Julia/Makie oracle for LeanPlot.Core numerics and tick locators.
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/core/num_ticks_oracle.jl
# Writes LeanPlotTest/oracle/core/{num,ticks}.json. Deterministic (seeded RNG).
# Non-finite floats are written as the strings "NaN", "Inf", "-Inf".
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const OUT = @__DIR__

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]

function label_json(l)
    if l isa AbstractString
        return Dict("plain" => String(l))
    end
    # Makie.RichText(:span, [pieces..., RichText(:sup, [exp])])
    base = ""
    sup = nothing
    for c in l.children
        if c isa AbstractString
            base *= c
        elseif c.type == :sup
            sup = join(String.(c.children))
        else
            base *= join(String.(c.children))
        end
    end
    return sup === nothing ? Dict("plain" => base) : Dict("base" => base, "sup" => sup)
end

rng = MersenneTwister(20260924)

# ---------------------------------------------------------------- num.json
num = Dict{String,Any}()
num["pow10"] = [Dict("z" => z, "v" => jf(10.0^z)) for z in -330:330]

rcases = Any[]
push!(rcases, (0.0, 1.0, 11), (-pi, pi, 201), (0.1, 0.7, 7), (1e-300, 3e-300, 4), (-1.0, 1.0, 5),
      (0.0, 2pi, 100), (-2.0, 2.0, 1), (3.0, 3.0, 4), (1.0, 0.0, 6), (-1e308, 1e308, 3), (1e308, -1e308, 2))
for _ in 1:160
    a = randn(rng) * 10.0^rand(rng, -3:3)
    b = a + rand(rng) * 10.0^rand(rng, -3:4) * (rand(rng) < 0.2 ? -1 : 1)
    n = rand(rng, [2, 3, 5, 10, 11, 17, 50, 100, 101, 201])
    if rand(rng) < 0.3
        a = round(a, digits = rand(rng, 0:2)); b = round(b, digits = rand(rng, 0:2))
    end
    push!(rcases, (a, b, n))
end
tryrange(a, b, n) = try jfv(collect(range(a, b, length = n))) catch; nothing end
num["range"] = [Dict("a" => a, "b" => b, "n" => n, "v" => tryrange(a, b, n)) for (a, b, n) in rcases if tryrange(a, b, n) !== nothing]
num["linrange"] = [Dict("a" => a, "b" => b, "n" => n, "v" => jfv(collect(LinRange(a, b, n)))) for (a, b, n) in rcases if n >= 2 && isfinite(b - a)]
ccases = Any[(0.1, 0.1, 1.0), (1.0, -0.25, -0.3), (0.0, 0.1, 0.3), (-1.0, 0.2, 1.0), (0.5, -0.1, -0.5), (1.0, 0.3, 0.9)]
for _ in 1:150
    a = round(randn(rng) * 3, digits = rand(rng, 0:3))
    s = round(rand(rng) * 0.9 + 0.01, digits = rand(rng, 1:3)) * (rand(rng) < 0.3 ? -1 : 1)
    b = a + s * rand(rng, 1:40) + (rand(rng) < 0.5 ? 0.0 : s * rand(rng))
    push!(ccases, (a, s, b))
end
for _ in 1:50
    a = randn(rng); s = rand(rng) * 0.3 + 1e-3
    push!(ccases, (a, s, a + s * rand(rng, 1:30) * 1.0000001))
end
filter!(c -> c[2] != 0, ccases)
num["colon"] = [Dict("a" => a, "s" => s, "b" => b, "v" => jfv(collect(a:s:b))) for (a, s, b) in ccases]
scases = [(0.1, 0.2, 5), (0.0, 0.1, 11), (-1.0, 0.3, 7), (1.0, -0.25, 9)]
for _ in 1:50
    push!(scases, (round(randn(rng), digits = 2), round(rand(rng) + 0.01, digits = 2), rand(rng, 1:30)))
end
num["rangestep"] = [Dict("a" => a, "s" => s, "n" => n, "v" => jfv(collect(range(a, step = s, length = n)))) for (a, s, n) in scases]

floats = Float64[0.0, -0.0, 1.0, 0.1, 0.2, 0.3, 1e-5, 1e23, 5e-324, floatmax(Float64), 2.0^60, 9007199254740993.0,
                 0.125, 0.375, 2.5, 3.5, -1e-7, 123456.0, 1.25, 1.35, pi, -exp(1.0)]
for _ in 1:200
    push!(floats, randn(rng) * 10.0^rand(rng, -30:30))
end
for _ in 1:40
    push!(floats, rand(rng, -1000:1000) / 8.0)
end
num["ryu"] = [Dict("x" => x,
                   "s64" => collect(Int128.(Base.Ryu.reduce_shortest(abs(x))) .|> x -> string(x)),
                   "s32" => (abs(Float32(x)) == 0 || !isfinite(Float32(x))) ? nothing : string.(Base.Ryu.reduce_shortest(abs(Float32(x)))),
                   "fixed" => [Base.Ryu.writefixed(x, p) for p in 0:6],
                   "exp" => [Base.Ryu.writeexp(x, p) for p in 0:6],
                   "repr" => repr(x),
                   "sig" => [jf(round(x, sigdigits = s)) for s in 1:17])
              for x in floats if x != 0.0 || true]

xs = Float64[0.0, -0.0, 1.0, -1.0, 0.5, 22.0, 3.0, -3.0, 0.3, 1e-10, -1e-300, 700.0, 709.7, 709.8, -745.0, -746.0, 1024.0, -1075.0,
             308.2, 308.3, -323.6, -330.0, 307.7, -307.7, NaN, Inf, -Inf]
for _ in 1:300; push!(xs, (rand(rng) - 0.5) * 60); end
for _ in 1:100; push!(xs, (rand(rng) - 0.5) * 1500); end
for k in -40:40; push!(xs, k / 2); end
num["exp"] = [Dict("x" => jf(x), "exp" => jf(exp(x)), "exp2" => jf(exp2(x)), "exp10" => jf(exp10(x))) for x in xs]

open(joinpath(OUT, "num.json"), "w") do io; JSON.print(io, num); end

# ---------------------------------------------------------------- ticks.json
ticks = Dict{String,Any}()
wcases = Tuple{Float64,Float64}[(0.0,1.0), (-3.14159,3.14159), (0.0,7.3), (13.0,97.0), (-0.001,0.002), (1.0,1000.0),
    (-1.0,1.0), (0.0,100.0), (0.0, 0.0), (1.0, 1.0), (5.0, 5.0 + 1e-14), (-1e300, 1e300), (1e-12, 2e-12),
    (0.0, 1e-300), (-Inf, Inf), (NaN, 1.0), (1e6, 1e6 + 1), (-5.0, -4.9999), (0.1, 0.2), (0.01, 0.99),
    (-7.5, 7.5), (2020.0, 2026.0), (0.0, 2pi), (-1.0e-9, 1.0e-9), (123456.0, 123789.0), (0.3, 0.30000000000001)]
for _ in 1:400
    a = randn(rng) * 10.0^rand(rng, -6:6)
    span = 10.0^(rand(rng) * 16 - 8) * (1 + rand(rng))
    push!(wcases, (a, a + span))
end
for _ in 1:100
    a = Float64(rand(rng, -100:100)); b = a + rand(rng, 1:200)
    push!(wcases, (a, b))
end
for _ in 1:60
    a = rand(rng) * 10; push!(wcases, (-a, a))
end
wil = Any[]
for (a, b) in wcases
    t = Mk.get_tickvalues(Mk.WilkinsonTicks(5, k_min = 3), a, b)
    labs = (length(t) > 0 && all(isfinite, t)) ? [label_json(l) for l in Mk.get_ticklabels(Mk.automatic, t)] : nothing
    push!(wil, Dict("vmin" => jf(a), "vmax" => jf(b), "ticks" => jfv(t), "labels" => labs))
end
ticks["wilkinson"] = wil

# optimize_ticks with non-default options (extend_ticks / non-strict)
opt = Any[]
for (a, b) in wcases[1:120]
    (isfinite(a) && isfinite(b) && a < b) || continue
    for (ext, strict) in ((false, false), (true, true), (true, false))
        t, lo, hi = Mk.PlotUtils.optimize_ticks(a, b; extend_ticks = ext, strict_span = strict, k_min = 2, k_max = 10, k_ideal = 5)
        push!(opt, Dict("vmin" => a, "vmax" => b, "extend" => ext, "strict" => strict, "kmin" => 2,
                        "ticks" => jfv(t), "lo" => jf(lo), "hi" => jf(hi)))
    end
end
ticks["optimize"] = opt

# log axes
logc = Any[]
for (scale, name) in ((log10, "log10"), (log2, "log2"), (log, "ln"))
    for _ in 1:60
        e1 = rand(rng) * 20 - 10
        e2 = e1 + rand(rng) * 10 + 0.05
        lo, hi = 10.0^e1, 10.0^e2
        t, l = Mk.get_ticks(Mk.automatic, scale, Mk.automatic, lo, hi)
        push!(logc, Dict("scale" => name, "vmin" => lo, "vmax" => hi, "ticks" => jfv(t), "labels" => [label_json(x) for x in l]))
    end
    for (lo, hi) in ((1.0, 1000.0), (0.1, 10.0), (1.0, 1e5), (2.0, 3.0), (1e-3, 1e3))
        t, l = Mk.get_ticks(Mk.automatic, scale, Mk.automatic, lo, hi)
        push!(logc, Dict("scale" => name, "vmin" => lo, "vmax" => hi, "ticks" => jfv(t), "labels" => [label_json(x) for x in l]))
    end
end
ticks["log"] = logc

# pseudolog10 / Symlog10 (decade picker, Wilkinson fallback)
pl = Any[]
for (lo, hi) in ((-1000.0, 1000.0), (0.0, 1e6), (-50.0, 5e4), (1.0, 1e5), (-1e5, -10.0), (-3.0, 3.0), (0.5, 2.0), (-1e8, 1e3))
    t, l = Mk.get_ticks(Mk.automatic, Mk.pseudolog10, Mk.automatic, lo, hi)
    push!(pl, Dict("scale" => "pseudolog10", "vmin" => lo, "vmax" => hi, "ticks" => jfv(t), "labels" => [label_json(x) for x in l]))
end
for (lo, hi, L, U) in ((-1000.0, 1000.0, -1.0, 1.0), (-0.5, 0.5, -1.0, 1.0), (-1e5, 1e2, -10.0, 10.0), (1.0, 1e6, -2.0, 3.0))
    sc = Mk.Symlog10(L, U)
    t, l = Mk.get_ticks(Mk.automatic, sc, Mk.automatic, lo, hi)
    push!(pl, Dict("scale" => "symlog10", "lower" => L, "upper" => U, "vmin" => lo, "vmax" => hi, "ticks" => jfv(t), "labels" => [label_json(x) for x in l]))
end
for _ in 1:40
    lo = -10.0^(rand(rng) * 6) * (rand(rng) < 0.3 ? -1 : 1)
    hi = lo + 10.0^(rand(rng) * 7)
    t, l = Mk.get_ticks(Mk.automatic, Mk.pseudolog10, Mk.automatic, lo, hi)
    push!(pl, Dict("scale" => "pseudolog10", "vmin" => lo, "vmax" => hi, "ticks" => jfv(t), "labels" => [label_json(x) for x in l]))
end
ticks["decade"] = pl

# minor ticks
mn = Any[]
for (a, b) in wcases[1:80]
    (isfinite(a) && isfinite(b) && a < b) || continue
    t = Mk.get_tickvalues(Mk.WilkinsonTicks(5, k_min = 3), a, b)
    for (n, mirror) in ((2, true), (5, true), (4, false))
        m = Mk.get_minor_tickvalues(Mk.IntervalsBetween(n, mirror), identity, t, a, b)
        push!(mn, Dict("scale" => "identity", "vmin" => a, "vmax" => b, "n" => n, "mirror" => mirror, "ticks" => jfv(t), "minor" => jfv(m)))
    end
end
for c in logc[1:30]
    lo, hi = c["vmin"], c["vmax"]
    sc = c["scale"] == "log10" ? log10 : (c["scale"] == "log2" ? log2 : log)
    t = Float64[x isa String ? parse(Float64, x) : x for x in c["ticks"]]
    length(t) >= 2 || continue
    m = Mk.get_minor_tickvalues(Mk.IntervalsBetween(9, true), sc, t, lo, hi)
    push!(mn, Dict("scale" => c["scale"], "vmin" => lo, "vmax" => hi, "n" => 9, "mirror" => true, "ticks" => jfv(t), "minor" => jfv(m)))
end
ticks["minor"] = mn

# plain label formatting of arbitrary sets
fl = Any[]
for t in ([0.0,0.2,0.4,0.6,0.8,1.0], [-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0], [0.0,0.0005,0.001,0.0015,0.002], [0.0,250.0,500.0,750.0,1000.0],
          [1e-5, 2e-5, 3e-5], [0.0, 1.5e-5, 3e-5], [-2e10, 0.0, 2e10], [1.0e6, 1.5e6, 2.0e6], [0.1, 0.25, 1e7], [-0.0, 0.5], [NaN, 1.0, 2.0])
    push!(fl, Dict("ticks" => jfv(t), "labels" => [label_json(x) for x in Mk.get_ticklabels(Mk.automatic, t)],
                   "plain" => Mk.format_ticks_plain(t)))
end
ticks["format"] = fl

open(joinpath(OUT, "ticks.json"), "w") do io; JSON.print(io, ticks); end
println("wrote num.json, ticks.json: ", length(wil), " wilkinson cases, ", length(logc), " log cases")
