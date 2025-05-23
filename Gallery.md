# LeanPlot Gallery

A living checklist of demo / test cases LeanPlot supports. Tick items off as they are implemented. Each example mirrors an official [Recharts example](https://recharts.org/en-US/examples).

## Line charts
- [x] 1-variable linear  `y = x`
- [x] Quadratic  `y = x²`
- [x] Cubic  `y = x³`
- [x] Trig periodic  `y = sin(2πx)`
- [x] Trig overlay  `y = sin(2πx)`, `y = cos(2πx)`
- [x] Damped sine  `y = e^(−3x) · sin(8πx)`
- [x] Exponential growth  `y = e^x`
- [x] Piecewise "step"  `y = ⌊5x⌋ / 5`
- [x] Rational with asymptote  `y = 1 / (x − 0.5)`
- [ ] Parameter sweep (slider)  `y = sin(ωx)` for `ω ∈ {1 … 10}`

## Scatter / point clouds
- [x] Quadratic points demo (simple scatter)
- [ ] IID Gaussian  `(x,y) ~ 𝒩(0,1)²`
- [ ] Uniform ring in ℝ²
- [ ] Cluster mixture  (two Gaussian blobs)

## Area & bar
- [x] Simple bar chart (five values)
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
- [ ] Custom color palette mapping
- [ ] Axis units / formatting demo

## Data transformations
- [x] Log scale transforms
- [x] Square root scale
- [x] Symlog scale (handles negative values)
- [x] Data normalization [0,1]
- [x] Moving average smoothing