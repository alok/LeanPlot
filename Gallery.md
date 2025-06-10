# LeanPlot Gallery

A living checklist of demo / test cases LeanPlot supports. Tick items off as they are implemented. Each example mirrors an official [Recharts example](https://recharts.org/en-US/examples).

## Line charts

- [x] 1-variable linear  `y = x` [`LinearDemo.lean`](LeanPlot/Demos/LinearDemo.lean)
- [x] Quadratic  `y = x²` [`QuadraticDemo.lean`](LeanPlot/Demos/QuadraticDemo.lean)
- [x] Cubic  `y = x³` [`CubicDemo.lean`](LeanPlot/Demos/CubicDemo.lean)
- [x] Trig periodic  `y = sin(2πx)` [`TrigDemo.lean`](LeanPlot/Demos/TrigDemo.lean)
- [x] Trig overlay  `y = sin(2πx)`, `y = cos(2πx)` [`TrigDemo.lean`](LeanPlot/Demos/TrigDemo.lean)
- [x] Damped sine  `y = e^(−3x) · sin(8πx)` [`TrigDemo.lean`](LeanPlot/Demos/TrigDemo.lean)
- [x] Exponential growth  `y = e^x` [`TrigDemo.lean`](LeanPlot/Demos/TrigDemo.lean)
- [x] Piecewise "step"  `y = ⌊5x⌋ / 5` [`TrigDemo.lean`](LeanPlot/Demos/TrigDemo.lean)
- [x] Rational with asymptote  `y = 1 / (x − 0.5)` [`TrigDemo.lean`](LeanPlot/Demos/TrigDemo.lean)
- [ ] Parameter sweep (slider)  `y = sin(ωx)` for `ω ∈ {1 … 10}`

## Scatter / point clouds

- [x] Quadratic points demo (simple scatter) [`ScatterDemo.lean`](LeanPlot/Demos/ScatterDemo.lean)
- [ ] IID Gaussian  `(x,y) ~ 𝒩(0,1)²`
- [ ] Uniform ring in ℝ²
- [ ] Cluster mixture  (two Gaussian blobs)

## Area & bar

- [x] Simple bar chart (five values) [`BarDemo.lean`](LeanPlot/Demos/BarDemo.lean)
- [x] Area chart [`GrammarDemo.lean`](LeanPlot/Demos/GrammarDemo.lean)
- [ ] Histogram of Gaussian samples
- [ ] Stacked bar  (counts of {A,B,C} across 4 groups)

## Mixed charts

- [x] Bar + Line overlay [`MixedChartDemo.lean`](LeanPlot/Demos/MixedChartDemo.lean)
- [x] Multiple chart types [`GrammarDemo.lean`](LeanPlot/Demos/GrammarDemo.lean)

## Animated / time-series

- [ ] Bouncing sine wave  `y(t,x)=sin(2π(x−t))`
- [ ] Real-time random walk  `Sₙ = Sₙ₋₁ + εₙ`

## Scale transformations

- [x] Log scale transforms [`LogScaleDemo.lean`](LeanPlot/Demos/LogScaleDemo.lean)
- [x] Square root scale [`TransformDemo.lean`](LeanPlot/Demos/TransformDemo.lean)
- [x] Symlog scale (handles negative values) [`TransformDemo.lean`](LeanPlot/Demos/TransformDemo.lean)
- [x] Data normalization [0,1] [`TransformDemo.lean`](LeanPlot/Demos/TransformDemo.lean)
- [x] Moving average smoothing [`TransformDemo.lean`](LeanPlot/Demos/TransformDemo.lean)

## Advanced features

- [x] Grammar of Graphics DSL [`GrammarDemo.lean`](LeanPlot/Demos/GrammarDemo.lean)
- [x] Plot composition & overlays [`StackDemo.lean`](LeanPlot/Demos/StackDemo.lean), [`OverlayDemo.lean`](LeanPlot/Demos/OverlayDemo.lean)
- [x] Faceting (small multiples) [`FacetDemo.lean`](LeanPlot/Demos/FacetDemo.lean)
- [x] Type-safe series specification [`SeriesKindDemo.lean`](LeanPlot/Demos/SeriesKindDemo.lean)
- [x] Auto axis labels from parameter names [`AutoAxisLabelsDemo.lean`](LeanPlot/Demos/AutoAxisLabelsDemo.lean)

## Edge cases & robustness

- [x] Invalid data handling (NaN/Infinity) [`InvalidDataDemo.lean`](LeanPlot/Demos/InvalidDataDemo.lean)
- [x] Plot specification validation [`SpecificationTest.lean`](LeanPlot/Demos/SpecificationTest.lean)
- [ ] Very large N (10 000 pts)  `y = sin(x)`
- [ ] Zero-length data (empty chart)
- [ ] Negative-only domain  `y = −x²`

## Utility / regression

- [ ] Custom color palette mapping
- [ ] Axis units / formatting demo
