# LeanPlot Test-Case Gallery

> **Note:** The long-term plan is to _mirror most of the official [Recharts examples](https://recharts.org/en-US/examples)_ using equivalent Lean code.  Each bullet below can therefore be cross-checked with a counterpart in the upstream gallery — once a Lean demo reproduces the visual & interactive behaviour, tick it off.

A living checklist of demo/test cases we want LeanPlot to support.  Tick items off as they are implemented.

## Line charts
- [ ] 1-variable linear  `y = x`
- [ ] Quadratic  `y = x²`
- [ ] Cubic  `y = x³`
- [ ] Trig periodic  `y = sin(2πx)`
- [ ] Trig overlay  `y = sin(2πx)`, `y = cos(2πx)`
- [ ] Damped sine  `y = e^(−3x) · sin(8πx)`
- [ ] Exponential growth  `y = e^x`
- [ ] Piecewise "step"  `y = ⌊5x⌋ / 5`
- [ ] Rational with asymptote  `y = 1 / (x − 0.5)`
- [ ] Parameter sweep (slider)  `y = sin(ωx)` for `ω ∈ {1 … 10}`

## Scatter / point clouds
- [ ] IID Gaussian  `(x,y) ~ 𝒩(0,1)²`
- [ ] Uniform ring in ℝ²
- [ ] Cluster mixture  (two Gaussian blobs)

## Area & bar
- [ ] Cumulative distribution (area chart)
- [ ] Histogram of Gaussian samples
- [ ] Stacked bar  (counts of {A,B,C} across 4 groups)

## Animated / time-series
- [ ] Bouncing sine wave  `y(t,x)=sin(2π(x−t))`
- [ ] Real-time random walk  `Sₙ = Sₙ₋₁ + εₙ`

## Utility / regression
- [ ] Very large N (10 000 pts)  `y = sin(x)`
- [ ] Zero-length data (empty chart)
- [ ] Negative-only domain  `y = −x²`
- [ ] Custom colour palette mapping
- [ ] Axis units / formatting demo 