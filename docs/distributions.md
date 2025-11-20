# Distributions

The `Uncertain[T]` library provides a rich set of probability distributions for modeling different types of uncertainty.

## Common Distributions

### Normal (Gaussian) Distribution

The bell curve - perfect for modeling measurement errors, natural variations, and many real-world phenomena.

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

// Temperature with measurement error
val temperature = Uncertain.normal(mean = 72.0, standardDeviation = 3.0)

// Speed with variation
val speed = Uncertain.normal(mean = 65.0, standardDeviation = 5.0)
```

**When to use:**
- Measurement errors
- Natural variations (height, weight, etc.)
- Aggregated effects of many small random factors
- Default choice when you know mean and standard deviation

### Uniform Distribution

Equal probability for all values in a range.

```scala mdoc
// Integer uniform (dice roll)
val diceRoll = Uncertain.uniform(1, 7)  // 1-6 inclusive

// Continuous uniform
val randomValue = Uncertain.uniform(0.0, 1.0)
```

**When to use:**
- True randomness with no preferred outcome
- When you only know min and max bounds
- Dice rolls, random selection

### Bernoulli Distribution

Binary outcomes - true/false, success/failure.

```scala mdoc
// Coin flip
val fairCoin = Uncertain.bernoulli(0.5)

// Biased event
val userClicks = Uncertain.bernoulli(0.15)  // 15% click rate
```

**When to use:**
- Yes/no decisions
- Success/failure outcomes
- Any binary event

### Triangular Distribution

Three parameters: min, mode (most likely), and max. Great for expert estimates.

```scala mdoc
// Project estimate: optimistic 5 days, most likely 10, pessimistic 20
val projectDuration = Uncertain.triangular(
  min = 5, 
  peak = 10, 
  max = 20
)
```

**When to use:**
- Project estimates (PERT-like)
- Expert judgment with min/mode/max
- When you have rough estimates of best/typical/worst case

## Advanced Distributions

### Empirical Distribution

Sample from observed data.

```scala mdoc
// Historical user ratings
val ratings = List(4, 5, 3, 5, 4, 5, 3, 4, 5)
val nextRating = Uncertain.empirical(ratings)

// Sample customer purchase amounts
val purchases = List(29.99, 14.50, 45.00, 32.10)
val nextPurchase = Uncertain.empirical(purchases)
```

**When to use:**
- You have historical data
- Distribution shape is unknown or complex
- Bootstrap-style sampling

### Categorical Distribution

Discrete outcomes with specified probabilities.

```scala mdoc
// Weather forecast
val weather = Uncertain.categorical(Map(
  "Sunny" -> 0.6,
  "Cloudy" -> 0.3,
  "Rainy" -> 0.1
))

// User action
val userAction = Uncertain.categorical(Map(
  "Click" -> 0.15,
  "Scroll" -> 0.60,
  "Exit" -> 0.25
))
```

**When to use:**
- Multiple discrete outcomes
- Known probabilities for each outcome
- Surveys, user behavior, categorical predictions

### Exponential Distribution

Time between events in a Poisson process.

```scala mdoc
// Time until next customer arrival (mean = 5 minutes)
val arrivalTime = Uncertain.exponential(rate = 0.2)  // rate = 1/mean
```

**When to use:**
- Time between independent events
- Reliability analysis (time to failure)
- Queue modeling

### Poisson Distribution

Count of events in a fixed interval.

```scala mdoc
// Number of customer arrivals per hour (average = 10)
val arrivals = Uncertain.poisson(lambda = 10.0)
```

**When to use:**
- Counting events in time/space
- Rare events
- Website hits, customer arrivals, defects

### Binomial Distribution

Number of successes in n trials.

```scala mdoc
// 100 coin flips, probability of heads = 0.5
val headsCount = Uncertain.binomial(n = 100, p = 0.5)

// 50 customers, 15% conversion rate
val conversions = Uncertain.binomial(n = 50, p = 0.15)
```

**When to use:**
- Fixed number of independent trials
- Each trial has same success probability
- A/B testing, quality control

### Beta Distribution

Probability of probabilities - models uncertainty about a probability.

```scala mdoc
// Uncertain conversion rate (optimistic about success)
val conversionRatePrior = Uncertain.beta(alpha = 8, beta = 2)
```

**When to use:**
- Bayesian inference for probabilities
- Modeling uncertainty about rates/proportions
- Prior distributions

### Rayleigh Distribution

Magnitude of 2D vector with normally distributed components.

```scala mdoc
// Wind speed (when components are normally distributed)
val windSpeed = Uncertain.rayleigh(sigma = 5.0)
```

**When to use:**
- Wind speed modeling
- Signal processing
- Random walk distances

### Kumaraswamy Distribution

Similar to Beta but with simpler form.

```scala mdoc
// Alternative to Beta distribution
val proportion = Uncertain.kumaraswamy(a = 2.0, b = 5.0)
```

**When to use:**
- Alternative to Beta distribution
- Modeling proportions and percentages
- When Beta computation is too complex

## Mixture Distributions

Combine multiple distributions with weights.

```scala mdoc
// Server load: peak hours 30% of time, off-hours 70%
val peakHours = Uncertain.normal(50, 5)
val offHours = Uncertain.normal(15, 3)

val serverLoad = Uncertain.mixture(Map(
  peakHours -> 0.3,
  offHours -> 0.7
))
```

**When to use:**
- Multiple distinct scenarios
- Bimodal or multimodal distributions
- Different conditions with known frequencies

## Creating Custom Distributions

You can create distributions from any sampling function:

```scala mdoc
import scala.util.Random
given Random = new Random()

// Custom distribution: absolute value of normal
val customDist = Uncertain { () =>
  math.abs(Random.nextGaussian() * 5 + 10)
}

// From inverse CDF
val fromInverseCdf = Uncertain.fromInverseCdf { probability =>
  // Your inverse CDF function here
  probability * 100  // Simple linear example
}
```

## Distribution Properties

All distributions support standard operations:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val dist = Uncertain.normal(100, 15)

// Sample values
val sample = dist.sample()
val samples = dist.take(100)

// Statistical properties
val mean = dist.mean(sampleCount = 10_000)
val stdDev = dist.standardDeviation(sampleCount = 10_000)
val (low, high) = dist.confidenceInterval(0.95, sampleCount = 10_000)

// Probability queries
val prob = dist.cdf(110, sampleCount = 10_000)  // P(X <= 110)
val histogram = dist.histogram(sampleCount = 10_000)
```

## Choosing a Distribution

| Scenario | Recommended Distribution |
|:---------|:------------------------|
| Measurement error | Normal |
| Time estimates | Triangular |
| True randomness | Uniform |
| Binary outcome | Bernoulli |
| Count of events | Poisson |
| Multiple outcomes | Categorical |
| Historical data available | Empirical |
| Time between events | Exponential |
| Fixed trials | Binomial |
| Probability uncertainty | Beta |
| Multiple scenarios | Mixture |

## Next Steps

- See [Real-World Use Cases](use-cases.md)
- Learn about [Statistical Operations](statistics.md)
- Check the [API Reference](api.md)
