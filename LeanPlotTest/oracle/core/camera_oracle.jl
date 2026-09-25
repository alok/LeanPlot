# Julia/Makie oracle for the Axis3 camera (LeanPlot.Camera3).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/core/camera_oracle.jl
# Writes LeanPlotTest/oracle/core/camera.json:
#  * "matrices": Makie.calculate_matrices for many view settings (model, view,
#    projection as column-major 16-vectors) and projected sample points
#    (viewport-local device pixels, y down, plus NDC depth), all in Float64;
#  * "figures": real Axis3 figures: scene viewport (converted to device pixels,
#    y down) and the pixel positions of sample points under the scene's camera
#    (projectionview * model, as CairoMakie projects them).
using CairoMakie, JSON, Random
const Mk = CairoMakie.Makie
const OUT = @__DIR__
rng = MersenneTwister(31337)

colmajor(M) = [Float64(M[i, j]) for j in 1:4 for i in 1:4]

function project_px(proj, view, model, w, h, p)
    c = proj * view * model * Mk.Vec4d(p[1], p[2], p[3], 1.0)
    return [(c[1] / c[4] + 1) / 2 * w, (1 - c[2] / c[4]) / 2 * h, c[3] / c[4]]
end

aspect_json(a) = a isa Symbol ? string(a) : collect(Float64.(a))

cases = Any[]
base_limits = [((0.0, 0.0, 0.0), (1.0, 1.0, 1.0)), ((-2.0, -1.0, 0.5), (4.0, 2.0, 3.0)), ((10.0, -5.0, -1.0), (0.5, 20.0, 2.0))]
configs = Any[]
for lim in base_limits, vp in ((600, 450), (300, 500), (401, 401))
    push!(configs, (lim, vp, 1.275pi, pi / 8, 0.0, (1.0, 1.0, 2 / 3), :fitzoom, (30.0, 30.0, 30.0, 30.0), (false, false, false), (0.0, 0.0)))
end
for aspect in (:data, :equal, (1.0, 2.0, 3.0)), mode in (:fit, :fitzoom, :stretch, :free), persp in (0.0, 0.3, 1.0)
    push!(configs, (base_limits[2], (500, 400), 0.3pi, 0.2pi, persp, aspect, mode, (10.0, 20.0, 30.0, 40.0), (false, true, false), (0.1, -0.2)))
end
for _ in 1:40
    o = (randn(rng) * 5, randn(rng) * 5, randn(rng) * 5)
    w = (rand(rng) * 10 + 0.1, rand(rng) * 10 + 0.1, rand(rng) * 10 + 0.1)
    vp = (rand(rng, 100:900), rand(rng, 100:900))
    push!(configs, ((o, w), vp, rand(rng) * 2pi, (rand(rng) - 0.5) * pi * 0.98, rand(rng), rand(rng, [:data, :equal, (1.0, 1.0, 2 / 3), (2.0, 1.0, 0.5)]),
                    rand(rng, [:fit, :fitzoom, :stretch]), (rand(rng) * 50, rand(rng) * 50, rand(rng) * 50, rand(rng) * 50),
                    (rand(rng) < 0.2, rand(rng) < 0.2, rand(rng) < 0.2), (0.0, 0.0)))
end
for (lim, vp, azim, elev, persp, aspect, mode, prot, rev, off) in configs
    limits = Mk.Rect3d(Mk.Vec3d(lim[1]...), Mk.Vec3d(lim[2]...))
    viewport = Mk.Rect2i(0, 0, vp[1], vp[2])
    model, view, proj, lookat, eyepos = Mk.calculate_matrices(limits, viewport, prot, elev, azim, persp, aspect, mode,
        rev[1], rev[2], rev[3], 1.0, Mk.Vec2d(off...), 1.0e-3)
    pts = [[lim[1][k] + rand(rng) * lim[2][k] for k in 1:3] for _ in 1:12]
    append!(pts, [[lim[1][1], lim[1][2], lim[1][3]], [lim[1][1] + lim[2][1], lim[1][2] + lim[2][2], lim[1][3] + lim[2][3]]])
    push!(cases, Dict("origin" => collect(lim[1]), "widths" => collect(lim[2]), "viewport" => collect(vp),
                      "azimuth" => azim, "elevation" => elev, "perspectiveness" => persp, "aspect" => aspect_json(aspect),
                      "viewmode" => string(mode), "protrusions" => collect(prot), "reversed" => collect(rev), "offset" => collect(off),
                      "model" => colmajor(model), "view" => colmajor(view), "proj" => colmajor(proj),
                      "lookat" => collect(Float64.(lookat)), "eyepos" => collect(Float64.(eyepos)),
                      "points" => pts, "pixels" => [project_px(proj, view, model, vp[1], vp[2], p) for p in pts]))
end

figs = Any[]
for (sz, lims, kw) in (((600, 450), (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0), ()),
                       ((500, 500), (0.0, 10.0, -2.0, 2.0, 0.0, 1.0), (azimuth = 0.3pi, elevation = 0.1pi)),
                       ((800, 400), (0.0, 1.0, 0.0, 1.0, 0.0, 1.0), (perspectiveness = 0.5,)),
                       ((400, 600), (-3.0, 3.0, -3.0, 3.0, -1.0, 4.0), (aspect = :data,)))
    fig = Mk.Figure(size = sz)
    ax = Mk.Axis3(fig[1, 1]; limits = lims, kw...)
    Mk.scatter!(ax, [lims[1], lims[2]], [lims[3], lims[4]], [lims[5], lims[6]])
    Mk.colorbuffer(fig)  # run layout
    vpr = ax.scene.viewport[]
    fl = ax.finallimits[]
    o = Mk.origin(vpr); w = Mk.widths(vpr)
    dev_vp = [Float64(o[1]), Float64(sz[2] - (o[2] + w[2])), Float64(w[1]), Float64(w[2])]
    pts = [[lims[1] + rand(rng) * (lims[2] - lims[1]), lims[3] + rand(rng) * (lims[4] - lims[3]), lims[5] + rand(rng) * (lims[6] - lims[5])] for _ in 1:10]
    # NB: Makie.project(scene, :data, :pixel, p) ignores the model matrix, so
    # project with the scene's camera and model explicitly (as CairoMakie does)
    M = ax.scene.transformation.model[]
    pv = ax.scene.camera.projectionview[]
    px = map(pts) do p
        c = pv * M * Mk.Vec4d(p[1], p[2], p[3], 1.0)
        qx = (c[1] / c[4] + 1) / 2 * w[1]
        qy = (c[2] / c[4] + 1) / 2 * w[2]
        [Float64(o[1] + qx), Float64(sz[2] - (o[2] + qy))]
    end
    push!(figs, Dict("size" => collect(sz), "finallimits_origin" => collect(Float64.(Mk.origin(fl))), "finallimits_widths" => collect(Float64.(Mk.widths(fl))),
                     "viewport" => dev_vp, "settings" => Dict(string(k) => (v isa Symbol ? string(v) : v) for (k, v) in pairs(kw)),
                     "points" => pts, "pixels" => px))
end

open(joinpath(OUT, "camera.json"), "w") do io; JSON.print(io, Dict("matrices" => cases, "figures" => figs)); end
println("wrote camera.json: ", length(cases), " matrix cases, ", length(figs), " figures")
