# API Reference

Complete reference for `Uncertain[T]` operations and syntax.

## Imports

All functionality is available through these imports:

```scala
import mostly.uncertaintee.Uncertain  // Core type
import mostly.uncertaintee.syntax.*   // All operations (recommended)
```

### Selective Imports

If you prefer to pick and choose:

```scala
import mostly.uncertaintee.syntax.arithmetic.*    // +, -, *, /
import mostly.uncertaintee.syntax.boolean.*       // &&, ||, !
import mostly.uncertaintee.syntax.comparison.*    // >, <, >=, <=, ===
import mostly.uncertaintee.syntax.statistical.*   // mean, stdDev, etc.
import mostly.uncertaintee.syntax.functional.*    // zipWith, product
import mostly.uncertaintee.syntax.option.*        // orElse, getOrElse
import mostly.uncertaintee.syntax.quantile.*      // percentiles, quartiles
import mostly.uncertaintee.syntax.distribution.*  // normal, uniform, etc.
```

## Core Uncertain[T] Methods

These methods are available on any `Uncertain[T]` instance.

### Basic Operations

| Method | Description | Example |
|:-------|:------------|:--------|
| `sample()` | Get a single random sample | `val x = dist.sample()` |
| `take(n)` | Get n samples as a List | `val samples = dist.take(100)` |
| `iterator` | Infinite iterator of samples | `val it = dist.iterator` |
| `map(f)` | Transform values | `val doubled = dist.map(_ * 2)` |
| `flatMap(f)` | Chain dependent computations | `val result = dist.flatMap(x => ...)` |
| `filter(p)` | Filter values, returns `Uncertain[Option[T]]` | `val valid = dist.filter(_ > 0)` |

### Examples

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val dist = Uncertain.normal(10, 2)

// Basic sampling
val oneSample = dist.sample()
val manySamples = dist.take(100)
val firstTen = dist.iterator.take(10).toList

// Transformation
val scaled = dist.map(_ * 10)
val squared = dist.map(x => x * x)

// Chaining
val conditional = dist.flatMap { value =>
  if (value > 10) Uncertain.always(value * 2)
  else Uncertain.always(value / 2)
}

// Filtering
val positive = dist.filter(_ > 0)
```

## Constructing Uncertain Values

### Point Values

| Method | Description |
|:-------|:------------|
| `Uncertain.always(value)` | Constant (no uncertainty) |

```scala mdoc
val constant = Uncertain.always(42)
```

### Common Distributions

| Method | Parameters | Description |
|:-------|:-----------|:------------|
| `Uncertain.normal(mean, stdDev)` | mean: Double, stdDev: Double | Normal/Gaussian distribution |
| `Uncertain.uniform(min, max)` | min: T, max: T | Uniform distribution |
| `Uncertain.triangular(min, peak, max)` | min: Double, peak: Double, max: Double | Triangular distribution |
| `Uncertain.bernoulli(p)` | p: Double | True with probability p |
| `Uncertain.empirical(data)` | data: Seq[T] | Sample from observed data |
| `Uncertain.categorical(outcomes)` | outcomes: Map[T, Double] | Discrete with probabilities |

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val normal = Uncertain.normal(100, 15)
val uniform = Uncertain.uniform(1, 7)
val triangular = Uncertain.triangular(5, 10, 20)
val bernoulli = Uncertain.bernoulli(0.75)
val empirical = Uncertain.empirical(List(1, 2, 3, 2, 1, 3))
val categorical = Uncertain.categorical(Map(
  "A" -> 0.5,
  "B" -> 0.3,
  "C" -> 0.2
))
```

### Advanced Distributions

| Method | Parameters | Description |
|:-------|:-----------|:------------|
| `Uncertain.binomial(n, p)` | n: Int, p: Double | Binomial distribution |
| `Uncertain.poisson(lambda)` | lambda: Double | Poisson distribution |
| `Uncertain.exponential(rate)` | rate: Double | Exponential distribution |
| `Uncertain.beta(alpha, beta)` | alpha: Double, beta: Double | Beta distribution |
| `Uncertain.rayleigh(sigma)` | sigma: Double | Rayleigh distribution |
| `Uncertain.kumaraswamy(a, b)` | a: Double, b: Double | Kumaraswamy distribution |

```scala mdoc
val binomial = Uncertain.binomial(n = 100, p = 0.5)
val poisson = Uncertain.poisson(lambda = 10.0)
val exponential = Uncertain.exponential(rate = 0.1)
val beta = Uncertain.beta(alpha = 2, beta = 5)
```

### Composite Distributions

| Method | Description |
|:-------|:------------|
| `Uncertain.mixture(components)` | Weighted mixture of distributions |
| `Uncertain.sequence(list)` | Convert `List[Uncertain[T]]` to `Uncertain[List[T]]` |

```scala mdoc
val peak = Uncertain.normal(100, 5)
val off = Uncertain.normal(30, 3)
val mixture = Uncertain.mixture(Map(peak -> 0.3, off -> 0.7))

val list = List(
  Uncertain.normal(10, 1),
  Uncertain.normal(20, 2),
  Uncertain.normal(30, 3)
)
val sequenced: Uncertain[List[Double]] = Uncertain.sequence(list)
```

### Custom Distributions

| Method | Description |
|:-------|:------------|
| `Uncertain(sampler)` | From custom sampling function |
| `Uncertain.fromInverseCdf(f)` | From inverse CDF function |
| `Uncertain.fromRange(range)` | Uniform over a range |

```scala mdoc
import scala.util.Random
given Random = new Random()

// Custom sampler
val custom = Uncertain(() => math.random() * 100)

// From inverse CDF
val fromCdf = Uncertain.fromInverseCdf(p => p * 100)

// From range
val fromRange = Uncertain.fromRange(1 to 10)
```

## Arithmetic Operations

Requires `import mostly.uncertaintee.syntax.arithmetic.*` (or `.syntax.*`)

| Operator | Description | Types |
|:---------|:------------|:------|
| `+` | Addition | Numeric types |
| `-` | Subtraction | Numeric types |
| `*` | Multiplication | Numeric types |
| `/` | Division | Numeric types |

Works with `Uncertain[T]` and constants:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val a = Uncertain.normal(10, 2)
val b = Uncertain.normal(5, 1)

val sum = a + b
val diff = a - b
val product = a * b
val quotient = a / b

// With constants
val doubled = a * 2
val offset = a + 5
```

## Comparison Operations

Requires `import mostly.uncertaintee.syntax.comparison.*`

Returns `Uncertain[Boolean]` for all comparisons.

| Method/Operator | Description |
|:----------------|:------------|
| `>`, `gt(value)` | Greater than |
| `<`, `lt(value)` | Less than |
| `>=`, `gte(value)` | Greater than or equal |
| `<=`, `lte(value)` | Less than or equal |
| `===` | Equals (sample-by-sample) |
| `!==` | Not equals |
| `between(a, b, ...)` | Check if between bounds |

```scala mdoc
val speed = Uncertain.normal(65, 5)

// Comparisons with constants
val isSpeeding = speed > 60
val isSlowEnough = speed <= 70
val inRange = speed.between(60, 70, minInclusive = true, maxInclusive = true)

// Comparisons between Uncertain values
val speedA = Uncertain.normal(65, 5)
val speedB = Uncertain.normal(70, 5)
val aFaster = speedA > speedB
```

## Boolean Operations

Requires `import mostly.uncertaintee.syntax.boolean.*`

| Operator/Method | Description | Parameters |
|:----------------|:------------|:-----------|
| `!`, `unary_!` | Logical NOT | - |
| `&&` | Logical AND | - |
| `\|\|` | Logical OR | - |
| `probability(sampleCount)` | Estimate P(true) | sampleCount: Int |
| `probabilityExceeds(...)` | Test if P(true) > threshold | exceeds: Double, alpha: Double, beta: Double, sampleCount: Int |
| `isProbable(sampleCount)` | Test if P(true) > 0.5 | alpha: Double, beta: Double, sampleCount: Int |

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val isDefective = Uncertain.bernoulli(0.05)
val isExpensive = Uncertain.bernoulli(0.20)

// Logical operations
val notDefective = !isDefective
val bothBad = isDefective && isExpensive
val eitherBad = isDefective || isExpensive

// Probability estimation
val defectRate = isDefective.probability(sampleCount = 10_000)

// Hypothesis testing
val isProbablyDefective = isDefective.isProbable(sampleCount = 10_000)
val isConfidentlyDefective = isDefective.probabilityExceeds(
  exceeds = 0.95,
  sampleCount = 10_000
)
```

## Statistical Operations

Requires `import mostly.uncertaintee.syntax.statistical.*`

All require `sampleCount: Int` parameter.

### Descriptive Statistics

| Method | Returns | Description |
|:-------|:--------|:------------|
| `mean(sampleCount)` | Double | Average value |
| `expectedValue(sampleCount)` | Double | Alias for mean |
| `standardDeviation(sampleCount)` | Double | Sample standard deviation |
| `populationStandardDeviation(sampleCount)` | Double | Population standard deviation |
| `mode(sampleCount)` | T | Most frequent value |
| `histogram(sampleCount)` | Map[T, Int] | Frequency distribution |
| `entropy(sampleCount)` | Double | Information entropy (bits) |

```scala mdoc
val data = Uncertain.normal(100, 15)

val avg = data.mean(sampleCount = 10_000)
val expected = data.expectedValue(sampleCount = 10_000)
val stdDev = data.standardDeviation(sampleCount = 10_000)
val mostCommon = data.mode(sampleCount = 10_000)
val hist = data.histogram(sampleCount = 10_000)
val randomness = data.entropy(sampleCount = 10_000)
```

### Confidence and Probability

| Method | Returns | Description |
|:-------|:--------|:------------|
| `confidenceInterval(confidence, sampleCount)` | (T, T) | Confidence interval |
| `cdf(value, sampleCount)` | Double | P(X ≤ value) |

```scala mdoc
val (low, high) = data.confidenceInterval(0.95, sampleCount = 10_000)
val probBelow90 = data.cdf(90, sampleCount = 10_000)
```

### Option Operations

For `Uncertain[Option[T]]`:

| Method | Returns | Description |
|:-------|:--------|:------------|
| `probabilityOfSuccess(sampleCount)` | Double | P(Some) |
| `probabilityOfFailure(sampleCount)` | Double | P(None) |
| `orElse(fallback)` | Uncertain[T] | Use fallback for None |
| `getOrElse(default)` | Uncertain[T] | Use default for None |

```scala mdoc
val measurement = Uncertain.normal(10, 2)
val filtered = measurement.filter(x => x > 5 && x < 15)

val successRate = filtered.probabilityOfSuccess(sampleCount = 10_000)
val failureRate = filtered.probabilityOfFailure(sampleCount = 10_000)

val withFallback = filtered.orElse(Uncertain.always(10.0))
val withDefault = filtered.getOrElse(10.0)
```

## Functional Operations

Requires `import mostly.uncertaintee.syntax.functional.*`

| Method | Description |
|:-------|:------------|
| `product(other)` | Combine as tuple: `Uncertain[(T, B)]` |
| `zipWith(other)(f)` | Combine with function |
| `collect(pf)` | Filter and map with partial function |
| `flatten` | Flatten `Uncertain[Uncertain[T]]` to `Uncertain[T]` |

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val height = Uncertain.normal(5.8, 0.2)
val width = Uncertain.normal(3.2, 0.1)

// Create tuple
val pair = height.product(width)

// Combine with function
val area = height.zipWith(width)(_ * _)

// Collect with partial function
val positive = Uncertain.normal(0, 1).collect {
  case x if x > 0 => x * 2
}

// Flatten nested
val nested: Uncertain[Uncertain[Double]] = 
  Uncertain.bernoulli(0.5).map { b =>
    if (b) Uncertain.normal(10, 1)
    else Uncertain.normal(20, 2)
  }
val flattened = nested.flatten
```

## Quantile Operations

Requires `import mostly.uncertaintee.syntax.quantile.*`

Available as extension methods on `Uncertain[T]`:

| Method | Returns | Description |
|:-------|:--------|:------------|
| `tertiles(sampleCount)` | Tertiles[T] | 3-way split |
| `quartiles(sampleCount)` | Quartiles[T] | 4-way split |
| `quintiles(sampleCount)` | Quintiles[T] | 5-way split |
| `deciles(sampleCount)` | Deciles[T] | 10-way split |
| `percentiles(sampleCount)` | Percentiles[T] | 100-way split |

```scala mdoc
val data = Uncertain.normal(100, 15)

val tertiles = data.tertiles(sampleCount = 50_000)
val quartiles = data.quartiles(sampleCount = 50_000)
val quintiles = data.quintiles(sampleCount = 50_000)
val deciles = data.deciles(sampleCount = 50_000)
val percentiles = data.percentiles(sampleCount = 100_000)

// Access values
val median = quartiles.q2
val p95 = percentiles(95)
```

Also available as static methods:

```scala mdoc
import mostly.uncertaintee.quantiles.Quantiles

val q = Quantiles.percentiles(data, sampleCount = 100_000)
val custom = Quantiles.ofSize(
  quantileIntervals = 20,
  uncertain = data,
  sampleCount = 50_000
)
```

## Next Steps

- See [Quick Start](quickstart.md) for getting started
- Check [Use Cases](use-cases.md) for practical examples
- Learn about [Core Concepts](concepts.md)
