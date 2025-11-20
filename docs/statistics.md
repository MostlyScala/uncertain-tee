# Statistical Operations

Comprehensive guide to statistical analysis with `Uncertain[T]`.

## Basic Statistics

### Mean (Expected Value)

The average value of the distribution.

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val revenue = Uncertain.normal(10000, 1500)
val meanRevenue = revenue.mean(sampleCount = 10_000)
println(s"Expected revenue: $$$meanRevenue")

// expectedValue is an alias for mean
val expectedRevenue = revenue.expectedValue(sampleCount = 10_000)
```

**When to use:** Understanding the "typical" or "average" outcome.

### Standard Deviation

Measures the spread or variability of the distribution.

```scala mdoc
val measurements = Uncertain.normal(100, 15)

// Sample standard deviation (uses Bessel's correction)
val sampleStdDev = measurements.standardDeviation(sampleCount = 10_000)

// Population standard deviation
val popStdDev = measurements.populationStandardDeviation(sampleCount = 10_000)

println(s"Standard deviation: $sampleStdDev")
```

**When to use:** Understanding variability, risk assessment, quality control.

### Mode

The most frequently occurring value.

```scala mdoc
val diceRoll = Uncertain.uniform(1, 7)
val mostCommon = diceRoll.mode(sampleCount = 10_000)
println(s"Most common roll: $mostCommon")
```

**Best for:** Discrete distributions. Less meaningful for continuous distributions.

### Histogram

Frequency distribution of sampled values.

```scala mdoc
val categorical = Uncertain.categorical(Map(
  "A" -> 0.5,
  "B" -> 0.3,
  "C" -> 0.2
))

val frequencies = categorical.histogram(sampleCount = 10_000)
println(frequencies)
```

**When to use:** Visualizing distributions, understanding discrete outcomes.

## Confidence Intervals

Estimate a range that likely contains the true value.

```scala mdoc
val speed = Uncertain.normal(65, 5)

// 95% confidence interval
val (low95, high95) = speed.confidenceInterval(0.95, sampleCount = 10_000)
println(s"95% CI: [$low95, $high95]")

// 99% confidence interval (wider)
val (low99, high99) = speed.confidenceInterval(0.99, sampleCount = 10_000)
println(s"99% CI: [$low99, $high99]")
```

**Interpretation:** We are 95% confident the true value lies within this interval.

**Common confidence levels:**
- 90% (1.645 standard deviations)
- 95% (1.96 standard deviations)  ← Most common
- 99% (2.576 standard deviations)

## Cumulative Distribution Function (CDF)

Probability that a value is less than or equal to a threshold.

```scala mdoc
val temperature = Uncertain.normal(72, 3)

// What's the probability temperature is below 70?
val probBelow70 = temperature.cdf(70, sampleCount = 10_000)
println(s"P(T ≤ 70) = ${probBelow70}")

// What's the probability temperature is above 75?
val probAbove75 = 1.0 - temperature.cdf(75, sampleCount = 10_000)
println(s"P(T > 75) = ${probAbove75}")
```

**When to use:** Threshold analysis, probability queries, risk assessment.

## Entropy

Measures the "randomness" or "information content" of a distribution.

```scala mdoc
// Fair coin has maximum entropy (1 bit)
val fairCoin = Uncertain.bernoulli(0.5)
val fairCoinEntropy = fairCoin.entropy(sampleCount = 10_000)
println(s"Fair coin entropy: $fairCoinEntropy bits")

// Biased coin has less entropy
val biasedCoin = Uncertain.bernoulli(0.9)
val biasedCoinEntropy = biasedCoin.entropy(sampleCount = 10_000)
println(s"Biased coin entropy: $biasedCoinEntropy bits")

// Uniform distribution over 8 outcomes has 3 bits of entropy
val uniform8 = Uncertain.uniform(1, 9)
val uniform8Entropy = uniform8.entropy(sampleCount = 10_000)
println(s"Uniform(1-8) entropy: $uniform8Entropy bits")
```

**Interpretation:** Higher entropy = more uncertainty/randomness.

**When to use:** Information theory, understanding uncertainty levels, comparing distributions.

## Probability Estimation

### For Boolean Uncertain Values

```scala mdoc
val isDefective = Uncertain.bernoulli(0.02) // 2% defect rate
val defectProbability = isDefective.probability(sampleCount = 10_000)
println(s"Defect probability: ${defectProbability * 100}%")
```

### For Comparisons

```scala mdoc
val timeEstimate = Uncertain.triangular(5, 10, 20)
val onTimeProb = (timeEstimate < 15).probability(sampleCount = 10_000)
println(s"Probability of finishing on time: ${onTimeProb * 100}%")
```

## Hypothesis Testing

### Simple Probability Check

```scala mdoc
val conversionRate = Uncertain.normal(0.12, 0.02)

// Is conversion probably above 10%?
if ((conversionRate > 0.10).isProbable(sampleCount = 10_000)) {
  println("Conversion rate is probably above 10%")
}
```

### Confidence-Based Testing

```scala mdoc
// Are we 95% confident conversion is above 10%?
val isConfident = (conversionRate > 0.10).probabilityExceeds(
  exceeds = 0.95,
  sampleCount = 10_000
)

if (isConfident) {
  println("High confidence that conversion > 10%")
}
```

### Sequential Probability Ratio Test (SPRT)

The library uses SPRT for efficient hypothesis testing. This means:

- Tests can conclude early if evidence is strong
- Fewer samples needed on average
- Bounded error rates (Type I and Type II errors)

```scala mdoc
val metric = Uncertain.normal(0.55, 0.05)

// Test if metric > 0.5 with controlled error rates
val result = (metric > 0.5).evaluateHypothesis(
  threshold = 0.5,
  alpha = 0.05,      // Type I error rate (false positive)
  beta = 0.05,       // Type II error rate (false negative)
  delta = 0.05,      // Effect size
  sampleCount = 10_000
)

println(s"Decision: ${result.decision}")
println(s"Estimated probability: ${result.estimatedProbability}")
println(s"Samples used: ${result.samplesUsed}")
```

## Statistical Comparisons

### Comparing Two Distributions

```scala mdoc
val methodA = Uncertain.normal(100, 10)
val methodB = Uncertain.normal(105, 12)

// Is B significantly better than A?
val improvement = methodB - methodA
val isBetter = (improvement > 0).probabilityExceeds(
  exceeds = 0.95,
  sampleCount = 10_000
)

if (isBetter) {
  val expectedImprovement = improvement.mean(sampleCount = 10_000)
  println(s"Method B is significantly better by $expectedImprovement")
}
```

### Effect Size

```scala mdoc
val control = Uncertain.normal(50, 8)
val treatment = Uncertain.normal(55, 8)

val difference = treatment - control
val meanDiff = difference.mean(sampleCount = 10_000)
val stdDiff = difference.standardDeviation(sampleCount = 10_000)

// Cohen's d (effect size)
val cohensD = meanDiff / stdDiff
println(s"Effect size (Cohen's d): $cohensD")
```

## Working with Option Types

When using `.filter`, you get `Uncertain[Option[T]]`.

```scala mdoc
val measurement = Uncertain.normal(10, 2)
val validMeasurement = measurement.filter(x => x > 5 && x < 15)

// Probability of success (getting a Some)
val successRate = validMeasurement.probabilityOfSuccess(sampleCount = 10_000)
println(s"Success rate: ${successRate * 100}%")

// Probability of failure (getting a None)
val failureRate = validMeasurement.probabilityOfFailure(sampleCount = 10_000)
println(s"Failure rate: ${failureRate * 100}%")
```

## Sampling Strategies

### Single Sample

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val dist = Uncertain.normal(100, 15)
val sample = dist.sample()
```

### Multiple Samples

```scala mdoc
// Get exactly N samples
val samples = dist.take(1000)

// Infinite iterator
val iterator = dist.iterator
val first100 = iterator.take(100).toList
```

### Reproducible Sampling

```scala mdoc
import scala.util.Random

// Use a fixed seed for reproducibility
given Random = new Random(42)

val reproducible = Uncertain.normal(0, 1)
val sample1 = reproducible.sample()
val sample2 = reproducible.sample()
// Results will be consistent across runs
```

## Performance Considerations

### Sample Count Guidelines

| Operation | Recommended Sample Count |
|:----------|:------------------------|
| Quick estimate | 1,000 - 5,000 |
| Reliable estimate | 10,000 - 50,000 |
| High precision | 100,000+ |
| Hypothesis testing | Adaptive (SPRT) |
| Percentiles | 50,000 - 100,000 |

### Trade-offs

- **More samples**: More accurate, slower
- **Fewer samples**: Faster, more variation
- **SPRT**: Often faster, controlled error rates

### Caching Results

If you need the same statistic multiple times, compute once:

```scala mdoc
val data = Uncertain.normal(50, 10)

// Compute once, use many times
val meanValue = data.mean(sampleCount = 10_000)
val stdDevValue = data.standardDeviation(sampleCount = 10_000)

// Use these cached values
println(s"Mean: $meanValue")
println(s"Std Dev: $stdDevValue")
println(s"CV: ${stdDevValue / meanValue}")
```

## Next Steps

- Learn about [Quantiles and Percentiles](quantiles.md)
- See [Real-World Use Cases](use-cases.md)
- Check the [API Reference](api.md)
