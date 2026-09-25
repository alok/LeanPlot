# Julia/Makie oracle for LeanPlot.Recipes.Algo.Surface (surface2mesh, mesh normals,
# wireframe segments, CairoMakie per-vertex shading).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/surface_oracle.jl
# Writes LeanPlotTest/oracle/recipes/surface.json.
using CairoMakie, JSON, Random, LinearAlgebra
const Mk = CairoMakie.Makie
const GB = Mk.GeometryBasics
const CM = CairoMakie
const OUT = @__DIR__
rng = MersenneTwister(5)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]
cols(ps) = [jfv(getindex.(ps, k)) for k in 1:length(first(ps))]
tris(fs) = [[Int(GB.value(i)) - 1 for i in f] for f in fs]

out = Dict{String,Any}()

grids = Any[]
x = collect(range(0, 1, length = 5)); y = collect(range(-1, 1, length = 4))
z = [sin(3a) + b^2 for a in x, b in y]; z[3, 2] = NaN
push!(grids, ("nan", x, y, z))
push!(grids, ("two", [0.0, 1.0], [0.0, 2.0], [0.0 1.0; 2.0 3.0]))
x = collect(range(-2, 2, length = 12)); y = collect(range(-1.5, 1.5, length = 9))
push!(grids, ("smooth", x, y, [exp(-(a^2 + b^2)) * cos(2a) for a in x, b in y]))
x = sort(rand(rng, 6)); y = sort(rand(rng, 7))
push!(grids, ("irregular", x, y, rand(rng, 6, 7)))
sg = Any[]
for (name, x, y, z) in grids
    fig = Figure(); ax = Axis3(fig[1, 1])
    s = surface!(ax, x, y, z)
    Mk.update_state_before_display!(fig)
    m = Mk.surface2mesh(s.x[], s.y[], s.z[])
    wf = wireframe!(ax, x, y, z)
    lp = wf.plots[1].converted[][1]
    push!(sg, Dict("name" => name, "nx" => size(z, 1), "ny" => size(z, 2), "x" => jfv(x), "y" => jfv(y), "z" => jfv(vec(z)),
        "pos" => cols(GB.coordinates(m)), "faces" => tris(GB.decompose(GB.GLTriangleFace, m)), "normals" => cols(GB.normals(m)),
        "wire" => cols(lp)))
end
out["surfaces"] = sg

# curvilinear surface (matrix x, y) and its wireframe
rr = range(0.5, 1.5, length = 6); th = range(0, pi, length = 7)
xm = [a * cos(t) for a in rr, t in th]; ym = [a * sin(t) for a in rr, t in th]; zm = [a^2 - cos(2t) for a in rr, t in th]
fig = Figure(); ax = Axis3(fig[1, 1])
s = surface!(ax, xm, ym, zm)
m = Mk.surface2mesh(s.x[], s.y[], s.z[])
wf = wireframe!(ax, xm, ym, zm)
out["curvilinear"] = Dict("nx" => 6, "ny" => 7, "x" => jfv(vec(xm)), "y" => jfv(vec(ym)), "z" => jfv(vec(zm)),
    "pos" => cols(GB.coordinates(m)), "faces" => tris(GB.decompose(GB.GLTriangleFace, m)), "normals" => cols(GB.normals(m)),
    "wire" => cols(wf.plots[1].converted[][1]))

# triangle mesh normals (Makie mesh(vertices, faces) conversion)
ms = Any[]
for T in (Float64, Float32)
    vs = [Mk.Point3{T}(randn(rng, 3)...) for _ in 1:12]
    fs = [GB.GLTriangleFace(rand(rng, 1:12, 3)...) for _ in 1:18]
    fs = filter(f -> length(unique(Int.(GB.value.(f)))) == 3, fs)
    mesh = Mk.convert_arguments(Mk.Mesh, vs, fs)[1]
    push!(ms, Dict("f32" => T == Float32, "pos" => cols(vs), "faces" => tris(fs), "normals" => cols(GB.normals(mesh))))
end
out["meshes"] = ms

# CairoMakie shading with the default Axis3 scene light
fig = Figure(); ax = Axis3(fig[1, 1])
s = surface!(ax, x, y, rand(rng, 6, 7))
Mk.update_state_before_display!(fig)
sc = ax.scene
amb = CM.to_vec(sc.compute[:ambient_color][]); lc = CM.to_vec(sc.compute[:dirlight_color][])
L = sc.compute[:dirlight_final_direction][]; L0 = sc.compute[:dirlight_direction][]
view = sc.camera.view[]
light = Dict("ambient" => jfv(amb), "color" => jfv(lc), "direction" => jfv(L), "rawdirection" => jfv(L0),
    "view" => jfv(vec(Matrix(view))), "diffuse" => jfv(s.diffuse[]), "specular" => jfv(s.specular[]), "shininess" => Float64(s.shininess[]))
samples = Any[]
for _ in 1:40
    N = normalize(Mk.Vec3d(randn(rng, 3)...)); v = normalize(Mk.Vec3f(randn(rng, 3)...))
    c = Mk.RGBAf(rand(rng), rand(rng), rand(rng), rand(rng))
    r = CM._calculate_shaded_vertexcolors(N, v, c, L, lc, amb, s.diffuse[], s.specular[], s.shininess[])
    push!(samples, Dict("n" => jfv(N), "v" => jfv(v), "c" => jfv([c.r, c.g, c.b, c.alpha]), "out" => jfv([r.r, r.g, r.b, r.alpha])))
end
light["samples"] = samples
# corner normals and normal-matrix transform
cn = Any[]
for _ in 1:20
    ns = [normalize(Mk.Vec3f(randn(rng, 3)...)) for _ in 1:3]
    mean_normal = sum(i -> ns[i], 1:3) / 3
    res = [normalize(ns[k] + 1.0e-20 * mean_normal) for k in 1:3]
    M = Mk.Mat3f(randn(rng, 3, 3)...)
    tn = [CM.zero_normalize(M * n) for n in ns]
    push!(cn, Dict("ns" => [jfv(n) for n in ns], "out" => [jfv(r) for r in res], "m" => jfv(vec(Matrix(M))), "tn" => [jfv(t) for t in tn]))
end
light["corners"] = cn
out["lighting"] = light

open(joinpath(OUT, "surface.json"), "w") do io; JSON.print(io, out); end
println("surfaces ", length(sg), ", meshes ", length(ms), ", light dir ", L)
