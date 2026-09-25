# Cairo reference renders for LeanPlotTest/Raster/Cairo.lean.
#
# Regenerate (uses the Cairo.jl that CairoMakie loads, i.e. the same
# rasteriser as the Makie oracle):
#   cd LeanPlotTest/Raster/cairo
#   julia --startup-file=no --project=<env with CairoMakie> cairo_ref.jl
#
# Each scene is drawn on a white 200×150 ARGB32 surface and written with
# Cairo's own PNG writer (libpng), so the Lean test also exercises our PNG
# decoder on third-party output. Keep the geometry in sync with `scenes` in
# Cairo.lean.
using CairoMakie
const C = CairoMakie.Cairo

function render(fname, draw; w = 200, h = 150)
    surf = C.CairoARGBSurface(w, h)
    cr = C.CairoContext(surf)
    C.set_source_rgb(cr, 1, 1, 1); C.paint(cr)
    draw(cr)
    C.write_to_png(surf, fname)
end

set_miter_limit(cr, v) = ccall((:cairo_set_miter_limit, C.libcairo), Nothing, (Ptr{Nothing}, Float64), cr.ptr, v)

render("circle.png", cr -> begin
    C.set_source_rgb(cr, 0, 0, 0); C.arc(cr, 100.3, 75.7, 40.0, 0, 2pi); C.fill(cr) end)
render("miter.png", cr -> begin
    C.set_source_rgb(cr, 0, 0, 0); C.set_line_width(cr, 6.0); C.set_line_join(cr, C.CAIRO_LINE_JOIN_MITER)
    set_miter_limit(cr, 4.0)
    C.move_to(cr, 20.3, 120.1); C.line_to(cr, 60.7, 30.2); C.line_to(cr, 110.2, 110.9); C.line_to(cr, 180.5, 40.4)
    C.stroke(cr) end)
render("round.png", cr -> begin
    C.set_source_rgb(cr, 0.8, 0.1, 0.1); C.set_line_width(cr, 11.0)
    C.set_line_cap(cr, C.CAIRO_LINE_CAP_ROUND); C.set_line_join(cr, C.CAIRO_LINE_JOIN_ROUND)
    C.move_to(cr, 30.0, 40.0); C.line_to(cr, 170.0, 60.0); C.line_to(cr, 60.0, 120.0)
    C.stroke(cr) end)
render("dash.png", cr -> begin
    C.set_source_rgb(cr, 0, 0, 0); C.set_line_width(cr, 2.0); C.set_dash(cr, [10.0, 5.0], 0.0)
    C.move_to(cr, 10.5, 75.25); C.line_to(cr, 190.5, 75.25); C.stroke(cr) end)
render("thin.png", cr -> begin
    C.set_source_rgb(cr, 0, 0, 0)
    for (k, lw) in enumerate([0.5, 1.0, 1.5, 2.0])
        C.set_line_width(cr, lw); C.move_to(cr, 10 + 40k, 10); C.line_to(cr, 40 + 40k, 140); C.stroke(cr)
    end end)
render("evenodd.png", cr -> begin
    C.set_source_rgba(cr, 0.1, 0.3, 0.9, 0.6); C.set_fill_type(cr, C.CAIRO_FILL_RULE_EVEN_ODD)
    for i in 0:4
        t = -pi/2 + 4pi*i/5
        x = 100 + 60cos(t); y = 78 + 60sin(t)
        i == 0 ? C.move_to(cr, x, y) : C.line_to(cr, x, y)
    end
    C.close_path(cr); C.fill(cr) end)
render("bezier.png", cr -> begin
    C.set_source_rgb(cr, 0.2, 0.6, 0.2)
    C.move_to(cr, 20, 130); C.curve_to(cr, 40, 10, 160, 10, 180, 130); C.close_path(cr); C.fill(cr)
    C.set_source_rgb(cr, 0, 0, 0); C.set_line_width(cr, 3.0)
    C.move_to(cr, 20, 20); C.curve_to(cr, 60, 140, 140, -40, 180, 80); C.stroke(cr) end)
println("wrote reference PNGs")
