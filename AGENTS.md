# AGENTS.md: LeanPlot

Read `docs/DESIGN.md` (architecture, rules) and `docs/AUDIT.md` (detailed plan, Makie algorithms,
measured perf rules). The root package must stay dependency-free. `lake build` must be warning-free
and `lake test` green. `LeanPlot/Scene/DrawOp.lean` is the frozen backend contract. Julia/Makie oracle:
`julia --startup-file=no --project=<env>` with CairoMakie.
