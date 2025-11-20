# Quantiles and Percentiles

Understanding and working with quantiles, percentiles, and other distribution divisions.

## What are Quantiles?

Quantiles divide a distribution into equal-probability intervals. They tell you the values at specific positions in the sorted data.

For example, **quartiles** divide data into 4 parts:
- Q0: Minimum (0th percentile)
- Q1: First quartile (25th percentile)
- Q2: Median (50th percentile)
- Q3: Third quartile (75th percentile)
- Q4: Maximum (100th percentile)

## Computing Quantiles

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.quantiles.*

val data = Uncertain.normal(100, 15)

// Compute quartiles
val quartiles = Quantiles.quartiles(data, sampleCount = 50_000)

println(s"Min (Q0): ${quartiles.q0}")
println(s"Q1 (25th percentile): ${quartiles.q1}")
println(s"Median (Q2): ${quartiles.q2}")
println(s"Q3 (75th percentile): ${quartiles.q3}")
println(s"Max (Q4): ${quartiles.q4}")
```

## Available Quantile Types

### Tertiles (3-way split)

Divides distribution into 3 parts with 4 boundary points.

```scala mdoc
val tertiles = Quantiles.tertiles(data, sampleCount = 50_000)

println(s"Low (33rd %ile): ${tertiles(1)}")
println(s"Medium (67th %ile): ${tertiles(2)}")
```

### Quartiles (4-way split)

Most common - includes median, Q1, Q3.

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.quantiles.*

val data = Uncertain.normal(100, 15)
val quartiles = Quantiles.quartiles(data, sampleCount = 50_000)

// Access by position
println(s"Median: ${quartiles(2)}")

// Or by name
println(s"Q1: ${quartiles.q1}")
println(s"Q2: ${quartiles.q2}")
println(s"Q3: ${quartiles.q3}")
```

### Quintiles (5-way split)

Divides into 5 parts with 6 boundary points.

```scala mdoc
val quintiles = Quantiles.quintiles(data, sampleCount = 50_000)

println(s"20th %ile: ${quintiles(1)}")
println(s"40th %ile: ${quintiles(2)}")
println(s"60th %ile: ${quintiles(3)}")
println(s"80th %ile: ${quintiles(4)}")
```

### Deciles (10-way split)

Divides into 10 parts with 11 boundary points.

```scala mdoc
val deciles = Quantiles.deciles(data, sampleCount = 50_000)

println(s"10th %ile: ${deciles(1)}")
println(s"90th %ile: ${deciles(9)}")
```

### Percentiles (100-way split)

Most granular - 101 boundary points (0% to 100%).

```scala mdoc
val percentiles = Quantiles.percentiles(data, sampleCount = 100_000)

println(s"1st %ile: ${percentiles(1)}")
println(s"5th %ile: ${percentiles(5)}")
println(s"50th %ile: ${percentiles(50)}")
println(s"95th %ile: ${percentiles(95)}")
println(s"99th %ile: ${percentiles(99)}")
```

### Custom Quantiles

Create quantiles with any number of divisions:

```scala mdoc
// 20-way split
val custom = Quantiles.ofSize(
  quantileIntervals = 20,
  uncertain = data,
  sampleCount = 50_000
)

println(s"5th boundary: ${custom(5)}")
```

## Extension Methods

For convenience, use extension methods on `Uncertain[T]`:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val data = Uncertain.normal(100, 15)

// Extension methods (note the . before percentiles)
val pct = data.percentiles(sampleCount = 100_000)
val qrt = data.quartiles(sampleCount = 50_000)
val dec = data.deciles(sampleCount = 50_000)
```

## Working with Quantile Objects

### Accessing Values

```scala mdoc
val percentiles = data.percentiles(sampleCount = 100_000)

// By index
val median = percentiles(50)
val p95 = percentiles(95)

// Min and max
println(s"Min: ${percentiles.min}")
println(s"Max: ${percentiles.max}")
```

### Converting to Collections

```scala mdoc
// All values as a list
val allPercentiles = percentiles.toList()

// Without extremes
val interior = percentiles.toList(includeMin = false, includeMax = false)

// As a map
val percentileMap = percentiles.toMap()
```

### Converting Between Types

```scala mdoc
// Percentiles to quartiles
val quartilesFromPercentiles = percentiles.toQuartiles
println(s"Q1: ${quartilesFromPercentiles.q1}")

// Percentiles to quintiles
val quintilesFromPercentiles = percentiles.toQuintiles

// Percentiles to deciles
val decilesFromPercentiles = percentiles.toDeciles
```

## Reconstructing Distributions

You can reconstruct an `Uncertain[T]` from quantiles in three ways:

### Discrete Reconstruction

Samples uniformly from quantile boundary values only.

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.quantiles.*

val original = Uncertain.normal(100, 15)
val quintiles = Quantiles.quintiles(original, sampleCount = 50_000)

// Reconstruct as discrete distribution
val discrete = quintiles.reconstructDiscrete

// Will only sample the 6 boundary values
println(discrete.sample())
```

**When to use:** 
- When you want to preserve exact boundary values
- For discrete/categorical outcomes
- Simplest reconstruction method

### Fast Reconstruction (Linear)

Uses linear interpolation between quantile boundaries.

```scala mdoc
// Reconstruct with linear interpolation
val linearReconstructed = quintiles.reconstructFast

// Samples continuously between boundaries
println(linearReconstructed.sample())
```

**When to use:**
- Default choice for most cases
- Fast and predictable
- Good for few quantiles (tertiles, quartiles)

### Smooth Reconstruction (Cubic Spline)

Uses monotonic cubic spline interpolation for smooth curves.

```scala mdoc
val percentiles = Quantiles.percentiles(original, sampleCount = 100_000)

// Reconstruct with cubic spline
val smoothReconstructed = percentiles.reconstructSmooth

// Produces smooth, continuous samples
println(smoothReconstructed.sample())
```

**When to use:**
- Many quantiles (deciles, percentiles)
- When underlying distribution is smooth
- For visualization and plotting
- Need C1 continuity (smooth derivatives)

**Not recommended for:**
- Few quantiles (tertiles, quartiles) - use linear instead
- Discrete or discontinuous distributions

## Practical Applications

### Project Planning

Use percentiles to communicate realistic timelines:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val projectDuration = Uncertain.triangular(10, 15, 30)
val percentiles = projectDuration.percentiles(sampleCount = 100_000)

println("Project Duration Estimates:")
println(s"  Optimistic (10th %ile): ${percentiles(10)} days")
println(s"  Likely (50th %ile): ${percentiles(50)} days")
println(s"  Conservative (90th %ile): ${percentiles(90)} days")
println(s"  Pessimistic (95th %ile): ${percentiles(95)} days")
```

### Risk Metrics

Use quantiles for risk assessment:

```scala mdoc
val portfolioReturn = Uncertain.normal(0.08, 0.15)
val pct = portfolioReturn.percentiles(sampleCount = 100_000)

// Value at Risk (VaR) - 5th percentile loss
val var5 = pct(5)
println(s"VaR (5%): ${var5 * 100}%")

// Expected return at different confidence levels
println(s"50% confidence: ${pct(50) * 100}%")
println(s"95% confidence: ${pct(95) * 100}%")
```

### Quality Control

Monitor process capability:

```scala mdoc
val measurement = Uncertain.normal(10.0, 0.3)
val quartiles = measurement.quartiles(sampleCount = 50_000)

// Interquartile range (IQR) - spread of middle 50%
val iqr = quartiles.q3 - quartiles.q1
println(s"IQR: $iqr")

// Check for outliers (values beyond 1.5 * IQR from quartiles)
val lowerFence = quartiles.q1 - 1.5 * iqr
val upperFence = quartiles.q3 + 1.5 * iqr
println(s"Outlier fences: [$lowerFence, $upperFence]")
```

### Serialization

Quantiles provide a compact way to serialize/store distributions:

```scala mdoc
val original = Uncertain.normal(100, 15)

// Capture as percentiles
val stored = Quantiles.percentiles(original, sampleCount = 100_000)

// Later, reconstruct
val reconstructed = stored.reconstructSmooth

// Similar statistical properties
println(s"Original mean: ${original.mean(sampleCount = 10_000)}")
println(s"Reconstructed mean: ${reconstructed.mean(sampleCount = 10_000)}")
```

## Sample Count Guidelines

| Quantile Type | Recommended Sample Count |
|:--------------|:------------------------|
| Tertiles      | 10,000 - 50,000 |
| Quartiles     | 25,000 - 50,000 |
| Quintiles     | 25,000 - 50,000 |
| Deciles       | 50,000 - 100,000 |
| Percentiles   | 100,000 - 500,000 |
| Custom (small n) | 10,000 * n |
| Custom (large n) | 100,000+ |

Higher sample counts give more stable quantile estimates but take longer to compute.

## Quantile Properties

### Stability

Quantiles are stable under resampling:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.quantiles.*

val dist = Uncertain.normal(100, 15)

val q1 = Quantiles.quartiles(dist, sampleCount = 50_000)
val q2 = Quantiles.quartiles(dist, sampleCount = 50_000)

// Should be similar (within sampling error)
println(s"Q2 from sample 1: ${q1.q2}")
println(s"Q2 from sample 2: ${q2.q2}")
```

### Round-Trip Property

Reconstructing preserves approximate quantile structure:

```scala mdoc
val original = Uncertain.normal(100, 15)
val q1 = Quantiles.quartiles(original, sampleCount = 50_000)
val reconstructed = q1.reconstructFast
val q2 = Quantiles.quartiles(reconstructed, sampleCount = 50_000)

// q1 ≈ q2 (within sampling error)
println(s"Original Q2: ${q1.q2}")
println(s"Reconstructed Q2: ${q2.q2}")
```

## Next Steps

- See [Statistical Operations](statistics.md) for more analysis tools
- Check [Use Cases](use-cases.md) for practical examples
- Review the [API Reference](api.md)
