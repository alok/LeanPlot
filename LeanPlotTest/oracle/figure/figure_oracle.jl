# Julia/Makie oracle for the figure/layout layer (LeanPlot.Figure).
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/figure/figure_oracle.jl [refdir]
# Writes LeanPlotTest/oracle/figure/figure.json and, when `refdir` is given,
# the CairoMakie reference renders `<refdir>/<name>.png` (px_per_unit = 1).
#
# Every figure here is rebuilt with the same data in
# `LeanPlotTest/Figure/Specs.lean`; the JSON records Makie's solved layout in
# figure pixels (origin bottom-left, y up, as Makie reports it):
#  * axes: scene viewport, final limits, protrusions, tick values / positions /
#    labels, title and axis-label anchors;
#  * colorbars: frame box, tick values / positions / labels;
#  * legends: computed box, patch boxes and label anchors;
#  * Axis3: scene viewport, final limits, tick positions (pixels) and labels.
using CairoMakie, JSON
const Mk = CairoMakie.Makie
const OUT = @__DIR__
const REF = length(ARGS) >= 1 ? ARGS[1] : nothing

f64(x) = Float64(x)
rect(r) = [f64(r.origin[1]), f64(r.origin[2]), f64(r.widths[1]), f64(r.widths[2])]
sides(p) = [f64(p.left), f64(p.right), f64(p.bottom), f64(p.top)]
pt(p) = [f64(p[1]), f64(p[2])]

# tick labels: plain strings, rich superscripts as "base^exp"
function labelstr(l)
    l isa AbstractString && return String(l)
    if l isa Mk.RichText
        if l.type === :sup
            return "^" * join(labelstr.(l.children))
        else
            return join(labelstr.(l.children))
        end
    end
    return string(l)
end

function lineaxis_json(la)
    Dict(
        "values" => f64.(la.tickvalues[]),
        "positions" => [pt(p) for p in la.tickpositions[]],
        "labels" => [labelstr(s) for s in la.ticklabels[]],
        "protrusion" => f64(la.protrusion[]),
    )
end

function textpos(t)
    ps = t[1][]
    ps isa AbstractVector ? pt(ps[1]) : pt(ps)
end

function axis_json(ax::Axis)
    Dict(
        "viewport" => rect(ax.scene.viewport[]),
        "bbox" => rect(ax.layoutobservables.computedbbox[]),
        "limits" => [f64(ax.finallimits[].origin[1]), f64(ax.finallimits[].origin[1] + ax.finallimits[].widths[1]),
                     f64(ax.finallimits[].origin[2]), f64(ax.finallimits[].origin[2] + ax.finallimits[].widths[2])],
        "protrusions" => sides(ax.layoutobservables.protrusions[]),
        "xticks" => lineaxis_json(ax.xaxis),
        "yticks" => lineaxis_json(ax.yaxis),
        "title" => textpos(ax.elements[:title]),
        "xlabel" => textpos(ax.xaxis.elements[:labeltext]),
        "ylabel" => textpos(ax.yaxis.elements[:labeltext]),
    )
end

function colorbar_json(cb::Colorbar)
    Dict(
        "bbox" => rect(cb.layoutobservables.computedbbox[]),
        "protrusions" => sides(cb.layoutobservables.protrusions[]),
        "ticks" => lineaxis_json(cb.axis),
    )
end

function legend_json(leg::Legend)
    boxes = Any[]
    labels = Any[]
    function walk(g)
        for c in g.content
            x = c.content
            if x isa GridLayout
                walk(x)
            elseif x isa Box && c.span.cols.start == c.span.cols.stop
                push!(boxes, rect(x.layoutobservables.computedbbox[]))
            elseif x isa Label
                push!(labels, Dict("text" => string(x.text[]), "bbox" => rect(x.layoutobservables.computedbbox[])))
            end
        end
    end
    walk(leg.grid)
    Dict(
        "bbox" => rect(leg.layoutobservables.computedbbox[]),
        "autosize" => [f64(leg.layoutobservables.autosize[][1]), f64(leg.layoutobservables.autosize[][2])],
        "patches" => boxes,
        "labels" => labels,
    )
end

function axis3_json(ax::Axis3)
    lims = ax.finallimits[]
    lo = Mk.minimum(lims); hi = Mk.maximum(lims)
    ticks = Any[]; ticklabels = Any[]; labels = Any[]
    for p in ax.blockscene.plots
        sp = haskey(p, :space) ? p.space[] : :data
        sp === :pixel || continue
        z = Mk.zvalue2d(p)
        if p isa Mk.LineSegments
            push!(ticks, [pt(q) for q in p[1][]])
        elseif p isa Mk.Text && z > 100
            push!(ticklabels, Dict("positions" => [pt(q) for q in p[1][]], "align" => [string(p.align[][1]), string(p.align[][2])]))
        elseif p isa Mk.Text
            q = p.rotation[]
            ang = 2 * atan(Float64(q[3]), Float64(q[4]))
            push!(labels, Dict("position" => pt(p[1][][1]), "align" => [string(p.align[][1]), string(p.align[][2])], "rotation" => ang))
        end
    end
    tv = [f64.(Mk.get_ticks(t, identity, Mk.automatic, f64(lo[k]), f64(hi[k]))[1])
          for (k, t) in enumerate((ax.xticks[], ax.yticks[], ax.zticks[]))]
    Dict(
        "viewport" => rect(ax.scene.viewport[]),
        "bbox" => rect(ax.layoutobservables.computedbbox[]),
        "limits" => [f64(lo[1]), f64(hi[1]), f64(lo[2]), f64(hi[2]), f64(lo[3]), f64(hi[3])],
        "tickvalues" => tv,
        "ticks" => ticks,
        "ticklabels" => ticklabels,
        "labels" => labels,
    )
end

function figure_json(name, f)
    Mk.update_state_before_display!(f)
    blocks = f.content
    d = Dict{String, Any}(
        "name" => name,
        "size" => [f64(f.scene.viewport[].widths[1]), f64(f.scene.viewport[].widths[2])],
        "axes" => [axis_json(b) for b in blocks if b isa Axis],
        "colorbars" => [colorbar_json(b) for b in blocks if b isa Colorbar],
        "legends" => [legend_json(b) for b in blocks if b isa Legend],
        "axes3" => [axis3_json(b) for b in blocks if b isa Axis3],
    )
    if REF !== nothing
        save(joinpath(REF, name * ".png"), f; px_per_unit = 1)
    end
    return d
end

xs = collect(range(0, 10, length = 101))

figs = Any[]

# 1. lines + scatter with title and labels
let f = Figure()
    ax = Axis(f[1, 1], title = "Title", xlabel = "x label", ylabel = "y label")
    lines!(ax, xs, sin.(xs))
    scatter!(ax, [1.0, 2.0, 3.0], [0.5, -0.5, 0.2])
    push!(figs, figure_json("basic", f))
end

# 2. an empty axis (default limits)
let f = Figure()
    Axis(f[1, 1])
    push!(figs, figure_json("empty", f))
end

# 3. two axes side by side, large y values (scientific labels)
let f = Figure(size = (800, 400))
    ax1 = Axis(f[1, 1], xlabel = "t")
    lines!(ax1, xs, cos.(xs))
    ax2 = Axis(f[1, 2], title = "growth", ylabel = "count")
    lines!(ax2, xs, 2000 .* xs .^ 2)
    push!(figs, figure_json("twoaxes", f))
end

# 4. a 2x2 grid of axes
let f = Figure(size = (700, 500))
    for i in 1:2, j in 1:2
        ax = Axis(f[i, j], title = i == 1 ? "panel $i$j" : "", xlabel = i == 2 ? "x" : "", ylabel = j == 1 ? "y" : "")
        lines!(ax, xs, (i + j) .* sin.(xs .* j) .+ 10 * i)
    end
    push!(figs, figure_json("grid22", f))
end

# 5. explicit limits
let f = Figure()
    ax = Axis(f[1, 1], limits = (0, 5, -2, 2))
    lines!(ax, xs, sin.(xs))
    push!(figs, figure_json("limits", f))
end

# 6. heatmap with a colorbar
let f = Figure()
    ax = Axis(f[1, 1])
    zs = [sin(0.5 * i) * cos(0.3 * j) for i in 1:20, j in 1:15]
    hm = heatmap!(ax, 1:20, 1:15, zs)
    Colorbar(f[1, 2], hm)
    push!(figs, figure_json("heatmap_colorbar", f))
end

# 7. a legend in its own column
let f = Figure()
    ax = Axis(f[1, 1])
    lines!(ax, xs, sin.(xs), label = "sin")
    lines!(ax, xs, cos.(xs), label = "cos")
    scatter!(ax, xs[1:10:end], sin.(xs[1:10:end]), label = "samples")
    Legend(f[1, 2], ax)
    push!(figs, figure_json("legend", f))
end

# 8. axislegend
let f = Figure()
    ax = Axis(f[1, 1])
    lines!(ax, xs, sin.(xs), label = "sin")
    lines!(ax, xs, cos.(xs), label = "cos")
    axislegend(ax)
    push!(figs, figure_json("axislegend", f))
end

# 9. a log10 y axis
let f = Figure()
    ax = Axis(f[1, 1], yscale = log10)
    lines!(ax, xs[2:end], exp.(xs[2:end]))
    push!(figs, figure_json("logy", f))
end

# 10. DataAspect
let f = Figure()
    ax = Axis(f[1, 1], aspect = DataAspect())
    ts = collect(range(0, 2pi, length = 101))
    lines!(ax, cos.(ts), sin.(ts))
    push!(figs, figure_json("dataaspect", f))
end

# 11. band, poly and text
let f = Figure()
    ax = Axis(f[1, 1])
    band!(ax, xs, sin.(xs) .- 0.5, sin.(xs) .+ 0.5)
    poly!(ax, Point2f[(2, 2), (4, 2), (3, 3)])
    text!(ax, 5.0, 2.5, text = "note")
    push!(figs, figure_json("bandpoly", f))
end

# 12. a default Axis3 with a helix
let f = Figure()
    ax = Axis3(f[1, 1])
    ts = collect(range(0, 4pi, length = 101))
    lines!(ax, cos.(ts), sin.(ts), ts ./ 4pi)
    push!(figs, figure_json("axis3", f))
end

# 13. Axis3 with a surface, another view and a title
let f = Figure(size = (500, 400))
    ax = Axis3(f[1, 1], title = "surface", azimuth = 0.3pi, elevation = 0.2pi)
    xs3 = collect(range(-2, 2, length = 21)); ys3 = collect(range(-1, 1, length = 11))
    surface!(ax, xs3, ys3, [exp(-(x^2 + y^2)) for x in xs3, y in ys3])
    push!(figs, figure_json("surface3", f))
end

open(joinpath(OUT, "figure.json"), "w") do io
    JSON.print(io, Dict("figures" => figs), 1)
end
println("wrote ", length(figs), " figures")
