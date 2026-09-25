# Oracle for LeanPlot.Recipes.Algo.Volume: a Julia transcription of GLMakie's ray marcher
# (GLMakie `assets/shader/volume.frag`, the source of Makie's `volume` algorithms; CairoMakie
# draws no volumes: "Volume … is not supported by cairo right now"), run in Float64 on a small
# volume for a set of rays. Makie 0.24's attribute semantics: `absorption` also scales
# `:indexedabsorption`; `samples` steps with `step_size = 1.3/samples`.
#
# Run from the repository root:
#   julia --startup-file=no --project=<env with CairoMakie + JSON> \
#         LeanPlotTest/oracle/recipes/volume_oracle.jl
# Writes LeanPlotTest/oracle/recipes/volume.json.
using CairoMakie, JSON, LinearAlgebra
const Mk = CairoMakie.Makie
const OUT = @__DIR__

# ---- the volume and the colormaps ---------------------------------------------------------
const NX, NY, NZ = 6, 5, 4
vol = [sin(0.7i) + 0.3j * k - 0.1 * (i - j)^2 for i in 0:NX-1, j in 0:NY-1, k in 0:NZ-1]
lo, hi = extrema(vol)
viridis = Mk.to_colormap(:viridis)
cmap = [Float64.((c.r, c.g, c.b, c.alpha)) for c in viridis]
idxcolors = [(1.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.5), (0.0, 0.0, 1.0, 1.0)]

# ---- GL texture semantics ------------------------------------------------------------------
clampi(i, n) = clamp(i, 0, n - 1)
texel(i, j, k) = vol[clampi(i, NX) + 1, clampi(j, NY) + 1, clampi(k, NZ) + 1]
function sample(u, v, w, interp)
    if interp
        x = u * NX - 0.5; y = v * NY - 0.5; z = w * NZ - 0.5
        i = floor(Int, x); j = floor(Int, y); k = floor(Int, z)
        fx = x - floor(x); fy = y - floor(y); fz = z - floor(z)
        l(a, b, t) = a + (b - a) * t
        c00 = l(texel(i, j, k), texel(i + 1, j, k), fx)
        c10 = l(texel(i, j + 1, k), texel(i + 1, j + 1, k), fx)
        c01 = l(texel(i, j, k + 1), texel(i + 1, j, k + 1), fx)
        c11 = l(texel(i, j + 1, k + 1), texel(i + 1, j + 1, k + 1), fx)
        return l(l(c00, c10, fy), l(c01, c11, fy), fz)
    else
        return texel(floor(Int, u * NX), floor(Int, v * NY), floor(Int, w * NZ))
    end
end
# sampler1D with GL_LINEAR, texel centres at (i + 1/2)/n, clamped
function lut(t)
    n = length(cmap)
    x = t * n - 0.5
    i0 = floor(Int, x); f = x - floor(x)
    a = cmap[clampi(i0, n) + 1]; b = cmap[clampi(i0 + 1, n) + 1]
    return a .+ (b .- a) .* f
end
fetch(i) = (i < 0 || i >= length(idxcolors)) ? (0.0, 0.0, 0.0, 0.0) : idxcolors[i + 1]
nrm(v) = (v - lo) / (hi - lo)

# ---- lighting (lighting.frag, FAST_SHADING) ------------------------------------------------
const LIGHT = normalize([-0.3, -0.5, -0.8])
function smooth_zero_max(x)
    c = 0.00390625; xswap = 0.6406707120152759; yswap = 0.20508383900190955
    return x < yswap ? c * (x + 1 + xswap - yswap)^8 : x
end
function coefficients(n, cam)
    diff = smooth_zero_max(dot(LIGHT, -n))
    h = normalize(LIGHT + cam)
    spec = max(dot(h, -n), 0.0)^32
    return diff, (diff <= 0 || isnan(spec)) ? 0.0 : spec
end
shade(diff, spec, col) = 0.45 * col + 0.5 * (diff * col + 0.2 * spec)
function gennormal(u, v, w, d, interp)
    u + d >= 1 && return [1.0, 0, 0]; v + d >= 1 && return [0, 1.0, 0]; w + d >= 1 && return [0, 0, 1.0]
    u - d <= 0 && return [-1.0, 0, 0]; v - d <= 0 && return [0, -1.0, 0]; w - d <= 0 && return [0, 0, -1.0]
    ox, oy, oz = 0.5 / NX, 0.5 / NY, 0.5 / NZ
    s(a, b, c) = sample(a, b, c, interp)
    g = [s(u - ox, v, w) - s(u + ox, v, w), s(u, v - oy, w) - s(u, v + oy, w), s(u, v, w - oz) - s(u, v, w + oz)]
    n = norm(g)
    return n < 1e-12 ? g : g ./ n
end

# ---- the algorithms ------------------------------------------------------------------------
function march(alg, front, back; interp = true, absorption = 1.0, isovalue = 0.5, isorange = 0.05, samples = 200)
    dir = (back .- front) ./ samples
    st = 1.3 / samples
    pos = copy(front)
    if alg == "mip"
        m = 0.0
        for _ in 1:samples
            s = sample(pos..., interp); m < s && (m = s); pos = pos .+ dir
        end
        return collect(lut(nrm(m)))
    elseif alg == "absorption"
        T = 1.0; L = [0.0, 0.0, 0.0]
        for _ in 1:samples
            d = lut(nrm(sample(pos..., interp)))
            op = st * d[4] * absorption
            T *= 1 - op
            T <= 0.01 && break
            L .+= (T * op) .* collect(d[1:3])
            pos = pos .+ dir
        end
        return [clamp.(L, 0, 1)..., clamp(1 - T, 0, 1)]
    elseif alg == "indexedabsorption"
        T = 1.0; L = [0.0, 0.0, 0.0]
        for _ in 1:samples
            s = sample(pos..., interp)
            idx = trunc(Int, s) - 1
            d = fetch(idx)
            op = st * d[4] * absorption
            L .+= (T * op) .* collect(d[1:3])
            T *= 1 - op
            T <= 0.01 && break
            pos = pos .+ dir
        end
        return [clamp.(L, 0, 1)..., clamp(1 - T, 0, 1)]
    elseif alg == "iso"
        for _ in 1:samples
            s = sample(pos..., interp)
            if abs(s - isovalue) < isorange
                c = lut(nrm(isovalue))
                n = gennormal(pos..., st, interp)
                diff, spec = coefficients(n, normalize(dir))
                return [clamp(shade(diff, spec, c[1]), 0, 1), clamp(shade(diff, spec, c[2]), 0, 1),
                        clamp(shade(diff, spec, c[3]), 0, 1), c[4]]
            end
            pos = pos .+ dir
        end
        return [0.0, 0.0, 0.0, 0.0]
    end
end

# ---- rays -------------------------------------------------------------------------------------
rays = Any[]
for a in 0:5, b in 0:2
    front = [0.05 + 0.15a, 0.1 + 0.3b, 0.0]
    back = [0.9 - 0.12a, 0.95 - 0.25b, 1.0]
    push!(rays, (front, back))
end
push!(rays, ([0.0, 0.5, 0.5], [1.0, 0.5, 0.5]))
push!(rays, ([0.5, 0.0, 0.2], [0.5, 1.0, 0.8]))
cases = Any[]
for (f, b) in rays, interp in (true, false)
    push!(cases, Dict("front" => f, "back" => b, "interp" => interp,
        "mip" => march("mip", f, b; interp),
        "absorption" => march("absorption", f, b; interp, absorption = 1.5),
        "indexed" => march("indexedabsorption", f, b; interp, absorption = 2.0),
        "iso" => march("iso", f, b; interp, isovalue = 0.8, isorange = 0.1, samples = 150)))
end
open(joinpath(OUT, "volume.json"), "w") do io
    JSON.print(io, Dict("nx" => NX, "ny" => NY, "nz" => NZ, "values" => vec(vol), "light" => LIGHT,
        "lo" => lo, "hi" => hi, "cases" => cases))
end
println("wrote volume.json: ", length(cases), " rays")
