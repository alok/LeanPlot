# Julia/Makie oracle for LeanPlot.Recipes.Algo.Arrows (Makie 0.24 arrows2d /
# arrows3d geometry and Cartan's arrow scaling).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/arrows_oracle.jl
# Writes LeanPlotTest/oracle/recipes/arrows.json:
#  * "process": Makie._process_arrow_arguments for 2D/3D inputs over align,
#    lengthscale, normalize and argmode;
#  * "arrows2d": real arrows2d plots: pixel start points/directions, metrics and
#    the pixel-space component meshes;
#  * "arrows3d": real arrows3d plots: world start points/directions, arrowscale,
#    metrics, marker placements and rotations; the Cylinder/Cone marker meshes;
#  * "spacing": Cartan-style spacing of sampled curves.
using CairoMakie, JSON, Random, LinearAlgebra
import Cartan, Grassmann
const Mk = CairoMakie.Makie
const GB = Mk.GeometryBasics
const OUT = @__DIR__
rng = MersenneTwister(99)

jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
jfv(v) = [jf(x) for x in v]
cols(ps) = [jfv(getindex.(ps, k)) for k in 1:length(first(ps))]

out = Dict{String,Any}()

proc = Any[]
for N in (2, 3)
    P = N == 2 ? Mk.Point2d : Mk.Point3d
    pos = [P(randn(rng, N)...) for _ in 1:7]
    dir = [P((randn(rng, N) .* 3)...) for _ in 1:7]
    for align in (:tail, :center, :tip, 0.3), ls in (0.5, 1.0f0, 2.0), nrm in (false, true), mode in (:direction, :endpoint)
        s, e = Mk._process_arrow_arguments(pos, dir, align, ls, nrm, mode)
        push!(proc, Dict("dim" => N, "pos" => cols(pos), "dir" => cols(dir), "align" => string(align),
            "lengthscale" => Float64(ls), "normalize" => nrm, "argmode" => string(mode), "starts" => cols(s), "ends" => cols(e)))
    end
end
out["process"] = proc

a2 = Any[]
pts = [Mk.Point2d(1, 1), Mk.Point2d(5, 5), Mk.Point2d(2, 8), Mk.Point2d(8, 2), Mk.Point2d(6, 6.5)]
dirs = [Mk.Vec2d(3, 1), Mk.Vec2d(-1, 2), Mk.Vec2d(0.2, 0.1), Mk.Vec2d(0, -3), Mk.Vec2d(-2.5, -0.4)]
for (name, kw) in (("default", (;)), ("tail", (; taillength = 6, tailwidth = 10)),
                   ("fixedshaft", (; shaftlength = 40, shaftwidth = 5, tiplength = 12, tipwidth = 16)),
                   ("center", (; align = :center, lengthscale = 0.7)), ("normalized", (; normalize = true, lengthscale = 1.5)),
                   ("nomin", (; minshaftlength = 0, strokemask = 0)))
    fig = Figure(size = (400, 300)); ax = Axis(fig[1, 1], limits = (0, 10, 0, 10))
    a = arrows2d!(ax, pts, dirs; kw...)
    Mk.update_state_before_display!(fig)
    meshes = a.meshes[]
    push!(a2, Dict("name" => name, "kw" => Dict(string(k) => (v isa Symbol ? string(v) : Float64(v)) for (k, v) in pairs(kw)),
        "start" => cols(a.pixel_startpoints[]), "dir" => cols(a.pixel_directions[]),
        "metrics" => [jfv(collect(m)) for m in a.arrow_metrics[]],
        "meshes" => [cols(GB.coordinates(m)) for m in meshes],
        "faces" => [[[Int(GB.value(i)) - 1 for i in f] for f in GB.faces(m)] for m in meshes]))
end
out["arrows2d"] = a2

a3 = Any[]
p3 = [Mk.Point3d(0, 0, 0), Mk.Point3d(1, 2, 0.5), Mk.Point3d(-1, 0.5, 1), Mk.Point3d(0.3, -1, -0.5)]
d3 = [Mk.Vec3d(1, 0, 0), Mk.Vec3d(0, 0.3, 1), Mk.Vec3d(-0.5, 0.5, 0.2), Mk.Vec3d(0, 0, -1)]
for (name, kw) in (("default", (;)), ("tail", (; taillength = 0.2, tailradius = 0.1, markerscale = 0.8)),
                   ("scaled", (; lengthscale = 0.5, tiplength = 0.3, tipradius = 0.2)))
    fig = Figure(size = (400, 400)); ax = LScene(fig[1, 1])
    a = arrows3d!(ax, p3, d3; kw...)
    Mk.update_state_before_display!(fig)
    rots = a.rot[]
    push!(a3, Dict("name" => name, "kw" => Dict(string(k) => Float64(v) for (k, v) in pairs(kw)),
        "starts" => cols(a.startpoints[]), "ends" => cols(a.endpoints[]),
        "world_type" => string(eltype(a.world_startpoints[])),
        "world_start" => cols(a.world_startpoints[]), "world_end" => cols(a.world_endpoints[]),
        "world_dir" => cols(a.world_directions[]),
        "arrowscale" => Float64(a.arrowscale[]), "metrics" => [jfv(collect(m)) for m in a.arrow_metrics[]],
        "shaft_pos" => cols(a.shaft_pos[]), "tip_pos" => cols(a.tip_pos[]), "rot" => cols(rots),
        "tail_scale" => cols(a.tail_scale[]), "shaft_scale" => cols(a.shaft_scale[]), "tip_scale" => cols(a.tip_scale[]),
        "quats" => [jfv(collect(Mk.to_rotation(r).data)) for r in rots]))
end
out["arrows3d"] = a3

markers = Dict{String,Any}()
for (name, prim) in (("cylinder", GB.Cylinder(Mk.Point3f(0, 0, 0), Mk.Point3f(0, 0, 1), 0.5f0)),
                     ("cone", GB.Cone(Mk.Point3f(0, 0, 0), Mk.Point3f(0, 0, 1), 0.5f0)))
    m = Mk.to_mesh(prim, 32)
    nv = GB.normals(m)
    nvals = nv isa GB.FaceView ? GB.values(nv) : nv
    nfaces = nv isa GB.FaceView ? GB.faces(nv) : GB.faces(m)
    markers[name] = Dict("pos" => cols(GB.coordinates(m)), "normals" => cols(nvals),
        "faces" => [[Int(GB.value(i)) - 1 for i in f] for f in GB.faces(m)],
        "nfaces" => [[Int(GB.value(i)) - 1 for i in f] for f in nfaces])
end
out["markers"] = markers

# Cartan.spacing on TensorFields (curves and a 2D grid)
sp = Any[]
for n in (5, 26, 50)
    t = LinRange(0, 2pi, n)
    curve = [Mk.Point3d(cos(s), sin(s), 0.1s) for s in t]
    tf = Cartan.TensorField(t, [Grassmann.Chain(cos(s), sin(s), 0.1s) for s in t])
    push!(sp, Dict("pts" => cols(curve), "spacing" => Cartan.spacing(tf)))
end
out["spacing"] = sp
gx = LinRange(0, 1, 5); gy = LinRange(0, 2, 4)
gf = Cartan.TensorField(Cartan.ProductSpace(gx, gy), [Grassmann.Chain(x + 0.1y*y, y + x*x, 0.3x*y) for x in gx, y in gy])
out["spacing_grid"] = Dict("n1" => 5, "n2" => 4,
    "pts" => cols(vec([Mk.Point3d(x + 0.1y*y, y + x*x, 0.3x*y) for x in gx, y in gy])), "spacing" => Cartan.spacing(gf))

open(joinpath(OUT, "arrows.json"), "w") do io; JSON.print(io, out); end
println("process ", length(proc), ", arrows2d ", length(a2), ", arrows3d ", length(a3), "; world type ", a3[1]["world_type"])
