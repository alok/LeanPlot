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
    titles = Any[]
    for c in leg.grid.content
        x = c.content
        if x isa Label
            push!(titles, Dict("text" => string(x.text[]), "bbox" => rect(x.layoutobservables.computedbbox[])))
        elseif x isa GridLayout
            for cc in x.content
                y = cc.content
                if y isa Box && cc.span.cols.start == cc.span.cols.stop
                    push!(boxes, rect(y.layoutobservables.computedbbox[]))
                elseif y isa Label
                    push!(labels, Dict("text" => string(y.text[]), "bbox" => rect(y.layoutobservables.computedbbox[])))
                end
            end
        end
    end
    Dict(
        "bbox" => rect(leg.layoutobservables.computedbbox[]),
        "autosize" => [f64(leg.layoutobservables.autosize[][1]), f64(leg.layoutobservables.autosize[][2])],
        "patches" => boxes,
        "labels" => labels,
        "titles" => titles,
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
        "labels" => [Dict("bbox" => rect(b.layoutobservables.computedbbox[])) for b in blocks if b isa Label],
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

# 14. marker shapes, stroked markers and line styles
let f = Figure()
    ax = Axis(f[1, 1], title = "markers")
    for (i, m) in enumerate([:circle, :rect, :diamond, :utriangle, :dtriangle, :cross, :xcross, :star5])
        scatter!(ax, [Float64(i)], [1.0], marker = m, markersize = 15)
    end
    scatter!(ax, collect(1.0:8.0), fill(2.0, 8), markersize = 12, color = :white, strokecolor = :black, strokewidth = 1)
    lines!(ax, [0.5, 8.5], [3.0, 3.0], linestyle = :dash, color = :black)
    lines!(ax, [0.5, 8.5], [3.5, 3.5], linestyle = :dot, color = :black)
    lines!(ax, [0.5, 8.5], [4.0, 4.0], linestyle = :dashdot, color = :black, linewidth = 2)
    push!(figs, figure_json("markers", f))
end

# 15. colour-mapped lines, arrows and an explicit colorbar with a label
let f = Figure()
    ax = Axis(f[1, 1])
    ts = collect(range(0, 2pi, length = 200))
    lines!(ax, cos.(ts), sin.(ts), color = ts, linewidth = 4)
    g = collect(-1.0:0.5:1.0)
    arrows2d!(ax, vec([Point2f(x, y) for x in g, y in g]), vec([Vec2f(-y, x) * 0.3 for x in g, y in g]))
    Colorbar(f[1, 2], limits = (0, 2pi), colormap = :viridis, label = "angle")
    push!(figs, figure_json("colormapped", f))
end

# 16. an irregular heatmap and an image
let f = Figure(size = (700, 350))
    ax1 = Axis(f[1, 1], title = "irregular")
    heatmap!(ax1, [0.0, 1.0, 3.0, 6.0, 10.0], [0.0, 2.0, 3.0, 5.0], [Float64(i + j) for i in 1:4, j in 1:3], colormap = :inferno)
    ax2 = Axis(f[1, 2], title = "image", aspect = DataAspect())
    image!(ax2, 0 .. 4, 0 .. 3, [RGBf(i / 4, j / 3, 0.5) for i in 1:4, j in 1:3])
    push!(figs, figure_json("heatmap_image", f))
end

# 17. a titled horizontal legend below the axis
let f = Figure()
    ax = Axis(f[1, 1])
    lines!(ax, xs, sin.(xs), label = "sin")
    scatter!(ax, xs[1:10:end], cos.(xs[1:10:end]), marker = :rect, label = "cos")
    band!(ax, xs, sin.(xs) .- 0.2, sin.(xs) .+ 0.2, label = "band")
    Legend(f[2, 1], ax, "Functions", orientation = :horizontal)
    push!(figs, figure_json("legend_horizontal", f))
end

# 18. a spanning title label, log x with minor ticks, reversed x, a horizontal colorbar
let f = Figure(size = (700, 450))
    Label(f[1, 1:2], "Super title", fontsize = 20, font = :bold)
    ax1 = Axis(f[2, 1], xscale = log10, xminorticksvisible = true, xminorgridvisible = true)
    lines!(ax1, collect(1.0:100.0), sqrt.(collect(1.0:100.0)))
    ax2 = Axis(f[2, 2], xreversed = true, ylabel = "y")
    hm = heatmap!(ax2, collect(1.0:10.0), collect(1.0:8.0), [sin(i / 3) + cos(j / 2) for i in 1:10, j in 1:8])
    Colorbar(f[3, 2], hm, vertical = false, label = "value")
    push!(figs, figure_json("label_log_reversed", f))
end

# 19. Axis3 with perspective, a wireframe and scatter
let f = Figure()
    ax = Axis3(f[1, 1], perspectiveness = 0.5)
    g3 = collect(range(-1, 1, length = 9))
    wireframe!(ax, g3, g3, [x * y for x in g3, y in g3])
    scatter!(ax, [0.5, -0.5, 0.0], [0.5, 0.5, -0.5], [0.8, 0.2, 0.5], markersize = 15)
    push!(figs, figure_json("axis3_wire", f))
end

# 20. hlines/vlines and autolimitaspect
let f = Figure()
    ax = Axis(f[1, 1], autolimitaspect = 1)
    ts = collect(range(0, 2pi, length = 101))
    lines!(ax, cos.(ts), 0.5 .* sin.(ts))
    hlines!(ax, [0.25, -0.25], color = :gray)
    vlines!(ax, [0.0], color = :red, linestyle = :dash)
    push!(figs, figure_json("hvlines_aspect", f))
end

# 22. rotated tick labels, clipped colour range with lowclip/highclip triangles
let f = Figure()
    ax = Axis(f[1, 1], xticklabelrotation = pi / 4, yticklabelrotation = pi / 2, xlabel = "x")
    hm = heatmap!(ax, collect(1.0:12.0), collect(1.0:9.0), [sin(i / 2) * cos(j / 3) for i in 1:12, j in 1:9],
                  colorrange = (-0.5, 0.5), lowclip = :red, highclip = :blue)
    Colorbar(f[1, 2], hm)
    push!(figs, figure_json("rotated_clip", f))
end

# 23. a coloured 2D mesh, a stroked polygon, rotated text, colour-mapped scatter, per-segment colours
let f = Figure()
    ax = Axis(f[1, 1])
    mesh!(ax, [Point2f(0, 0), Point2f(1, 0), Point2f(0.5, 1), Point2f(1.5, 1)],
          [Mk.GLTriangleFace(1, 2, 3), Mk.GLTriangleFace(2, 4, 3)], color = [1.0, 2.0, 3.0, 4.0])
    poly!(ax, Point2f[(2, 0), (3, 0), (3, 1), (2, 1)], color = :orange, strokecolor = :black, strokewidth = 2)
    text!(ax, 2.5, 1.2, text = "rotated", rotation = pi / 6, align = (:center, :bottom), fontsize = 18)
    scatter!(ax, [0.5, 1.5, 2.5], [1.5, 1.5, 1.5], color = [0.0, 0.5, 1.0], colormap = :plasma, markersize = 20)
    linesegments!(ax, [Point2f(0, -0.5), Point2f(1, -0.5), Point2f(2, -0.5), Point2f(3, -0.5)], color = [:red, :blue], linewidth = 3)
    push!(figs, figure_json("mesh_poly_text", f))
end

# 24. fixed axis sizes, custom ticks, grid gaps
let f = Figure(size = (640, 400))
    ax1 = Axis(f[1, 1], width = 250, height = 200, xticks = [0, 2.5, 7], yticks = ([-1, 0, 1], ["low", "mid", "high"]))
    lines!(ax1, xs, sin.(xs))
    ax2 = Axis(f[1, 2], title = "gap")
    scatter!(ax2, xs[1:5:end], xs[1:5:end] .^ 2)
    colgap!(f.layout, 40)
    push!(figs, figure_json("fixed_ticks", f))
end

# 25. x axis on top, y axis on the right
let f = Figure()
    ax = Axis(f[1, 1], xaxisposition = :top, yaxisposition = :right, title = "flipped", xlabel = "x", ylabel = "y")
    lines!(ax, xs, cos.(xs))
    push!(figs, figure_json("flipped_axes", f))
end

# 26. a two-bank legend with a title, and dashed/dotted legend lines
let f = Figure()
    ax = Axis(f[1, 1])
    for (k, st) in enumerate([:solid, :dash, :dot, :dashdot])
        lines!(ax, xs, sin.(xs .+ k), linestyle = st, label = "phase $k")
    end
    Legend(f[1, 2], ax, "Phases", nbanks = 2)
    push!(figs, figure_json("legend_banks", f))
end

# 21. a streamplot (its computed lines and arrowheads are dumped to streamplot.json so the
#     Lean test draws exactly the same data)
let f = Figure()
    ax = Axis(f[1, 1])
    sp = streamplot!(ax, p -> Point2f(-p[2], p[1]), -1 .. 1, -1 .. 1, gridsize = (12, 12))
    Mk.update_state_before_display!(f)
    # Float32 values in their shortest form (read back with Float32 rounding)
    arr(v) = "[" * join((isfinite(x) ? string(Float32(x)) : "\"NaN\"" for x in v), ",") * "]"
    open(joinpath(OUT, "streamplot.json"), "w") do io
        print(io, "{\"line_x\":", arr(first.(sp.line_points[])), ",\"line_y\":", arr(last.(sp.line_points[])),
              ",\"line_c\":", arr(sp.line_colors[]),
              ",\"arrow_x\":", arr(first.(sp.arrow_positions[])), ",\"arrow_y\":", arr(last.(sp.arrow_positions[])),
              ",\"arrow_u\":", arr(first.(sp.arrow_directions[])), ",\"arrow_v\":", arr(last.(sp.arrow_directions[])),
              ",\"arrow_c\":", arr(sp.arrow_colors[]), "}")
    end
    push!(figs, figure_json("streamplot", f))
end

open(joinpath(OUT, "figure.json"), "w") do io
    JSON.print(io, Dict("figures" => figs), 1)
end
println("wrote ", length(figs), " figures")
