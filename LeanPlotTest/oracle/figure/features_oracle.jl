# Julia/Makie oracle for the figure features added after v0.2: contour labels, reversible
# (custom) axis and colour scales with a colorbar, the volume mark.
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/figure/features_oracle.jl
# Writes LeanPlotTest/oracle/figure/features.json and the CairoMakie reference renders
# LeanPlotTest/oracle/figure/ref/<name>.png (px_per_unit = 1). Every figure is rebuilt with the
# same data in LeanPlotTest/Figure/FeatureTest.lean.
using CairoMakie, JSON
const Mk = CairoMakie.Makie
const OUT = @__DIR__
const REF = joinpath(OUT, "ref")
mkpath(REF)
CairoMakie.activate!(px_per_unit = 1)

f64(x) = Float64(x)
jf(x::Real) = isfinite(x) ? Float64(x) : string(Float64(x))
out = Dict{String,Any}()

# ---- 1. contour labels: 6 automatic levels, then orange bold labels of explicit levels -------
xs = LinRange(-3, 3, 60)
ys = LinRange(-2, 2, 50)
zs = [exp(-(x^2 + y^2) / 2) * cos(2x) + 0.1y for x in xs, y in ys]
fig = Figure()
ax = Axis(fig[1, 1])
c1 = contour!(ax, xs, ys, zs; levels = 6, labels = true)
c2 = contour!(ax, xs, ys, zs; levels = [-0.1, 0.3], labels = true, color = :orange, labelfont = :bold, labelsize = 12)
Mk.update_state_before_display!(fig)
function labels_json(ctr)
    txt = first(p for p in ctr.plots if p isa Mk.Text)
    lns = first(p for p in ctr.plots if p isa Mk.Lines)
    pts = lns[1][]
    Dict("text" => collect(String.(txt.text[])),
         "pos" => [jf.(f64.(collect(p))[1:2]) for p in ctr.text_positions[]],
         "rot" => jf.(f64.(ctr.text_rotation[])),
         "n" => length(pts), "nnan" => count(p -> isnan(p[1]), pts),
         "nseg" => count(i -> !isnan(pts[i][1]) && !isnan(pts[i+1][1]), 1:length(pts)-1),
         "nnan_unmasked" => count(p -> isnan(p[1]), ctr.contour_points[]))
end
out["contour_labels"] = Dict("plots" => [labels_json(c1), labels_json(c2)],
    "limits" => f64.([ax.finallimits[].origin..., ax.finallimits[].widths...]))
save(joinpath(REF, "contour_labels.png"), fig; px_per_unit = 1)

# ---- 2. reversible scales: an AsinhScale x axis, a PowerScale colour scale, a colorbar ---------
hx = 10.0 .^ range(0, 3; length = 25)
hy = 1.0:1.0:12.0
hz = [x * y for x in hx, y in hy]
sx = Mk.AsinhScale(1.0)
sc = Mk.PowerScale(0.5)
fig = Figure()
ax = Axis(fig[1, 1]; xscale = sx)
hm = heatmap!(ax, hx, hy, hz; colorscale = sc)
cb = Colorbar(fig[1, 2], hm)
Mk.update_state_before_display!(fig)
lax(la) = Dict("values" => jf.(f64.(la.tickvalues[])), "labels" => string.(la.ticklabels[]),
               "positions" => [jf.(f64.(collect(p))) for p in la.tickpositions[]])
out["heatmap_scales"] = Dict("limits" => f64.([ax.finallimits[].origin..., ax.finallimits[].widths...]),
    "xticks" => lax(ax.xaxis), "yticks" => lax(ax.yaxis), "cbticks" => lax(cb.axis))
save(joinpath(REF, "heatmap_scales.png"), fig; px_per_unit = 1)

open(joinpath(OUT, "features.json"), "w") do io; JSON.print(io, out); end
println("wrote features.json and ", length(out), " reference renders")
