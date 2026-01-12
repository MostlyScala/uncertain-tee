<div align="center">
  <img 
    width="25%" 
    alt="Uncertain-tee logo - a red T rises out of a histogram bracketed by square brackets" 
    src="https://raw.githubusercontent.com/MostlyScala/uncertain-tee/refs/heads/main/img/uncertain-tee-logo-sharp.svg"
  />
  <h1>Uncertain[T]</h1>
  <h5>(uncertain-tee)</h2>
</div>

`uncertain-tee` is a library for probabilistic computing. It is a **correlation preserving** library for modelling and working with uncertainty around data, using the concept of an **Uncertain[T]**, without having to hand-roll complex statistics-code.

An `Uncertain[T]` helps you work with data that isn't exact, like measurements with error, user behavior predictions, or
any value that has uncertainty. Instead of just working with single values, you work with *distributions* of possible
values.

```scala
val frontend = Uncertain.triangular(5, 10, 20) // days
val backend = Uncertain.triangular(8, 15, 30)
val testing = Uncertain.triangular(3, 5, 10)

val total = frontend + backend + testing

val onTimeProb = (total < 30).probability(sampleCount = 100_000)
val percentiles = total.percentiles(sampleCount = 100_000)

println(s"50% chance: ${percentiles(50)} days")
println(s"90% chance: ${percentiles(90)} days")
println(s"Probability of finishing in 30 days: $onTimeProb")

```

When coding with uncertainty, you don't say "the user will click the button," instead we say "there's a 75% chance the
user
will click the button" - and write code that handles that uncertainty, without needing to hand-roll
a big block of statistics-calculating-code.

The primary guarantee of this library is **correlation preserving** operations that make combining, calculating and
composing `Uncertain[T]` instances safe and correct. The core idea revolves around the monadic `Uncertain[T]` (it
provides a constructor and a `.map` and a `.flatMap`) that uses a memoized computation graph internally to preserve
correlation. Getting a value from an `Uncertain[T]` - sampling - is done via **monte carlo simulation**.

It is very flexible and intuitive; it allows composition via for-comprehensions, leading to very legible
code with guaranteed correctness, despite a complex statistical domain. It allows non-statisticians and statisticians alike
to work with Uncertainty in a deterministic manner.

## Installation

🦺🚧🏗️- library is still under construction/testing, not yet released to the wild.

| Build Tool    | Instruction                                                              |
|:--------------|:-------------------------------------------------------------------------|
| **sbt**       | `libraryDependencies += "mostly" %% "uncertain-tee" % NOT_YET_RELEASED"` |
| **mill**      | `ivy"mostly::uncertain-tee:NOT_YET_RELEASED"`                            |
| **scala-cli** | `//> using dep "mostly::uncertain-tee:NOT_YET_RELEASED"`                 |

### Modules

| Module                       | Description                                                                                              |
|:-----------------------------|:---------------------------------------------------------------------------------------------------------|
| **uncertain-tee**            | Core logic and operations; dependency-free.                                                              |
| **uncertain-tee-cats**       | Functional Programming typeclass instances for [Cats](https://github.com/Typelevel/cats)                 |
| **uncertain-tee-circe**      | JSON serialization syntax and Encoders/Decoders for [Circe](https://github.com/circe/circe)              |
| **uncertain-tee-scalacheck** | Gen/Arbitrary instances and syntax for property driven testing via [ScalaCheck](https://scalacheck.org/) |
| **uncertain-tee-squants**    | Uncertain logic and operations with SI-unit library [Squants](https://github.com/typelevel/squants)      |
| **uncertain-tee-zio**        | Functional Programming typeclass instances for [Zio (Prelude)](https://github.com/zio/zio-prelude)       |

## Quick Start

**Quick start example**

```scala 3
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

// Create an uncertain speed with some measurement error
val speed = Uncertain.normal(65.0, 5.0) // mean=65 mph, std dev=5 mph

// Check if we're probably speeding (speed limit is 60)
if (speed.gt(60).isProbable(sampleCount = 10_000)) {
  println("You're probably speeding")
}

// Get a confidence interval
val (low, high) = speed.confidenceInterval(0.95, sampleCount = 10_000)
println(s"95% confident speed is between $low and $high mph")
```

### Creating Uncertain Values

The library provides several ways to create uncertain values:

```scala 3
// From common distributions
val temperature = Uncertain.normal(72.0, 3.0) // Normal distribution
val diceRoll = Uncertain.uniform(1, 7) // Uniform between 1-6 
val coinFlip = Uncertain.bernoulli(0.5) // True/false with 50% chance

// From observed data
val userRatings = List(4, 5, 3, 5, 4, 5)
val nextRating = Uncertain.empirical(userRatings)

// From explicit probabilities
val weather = Uncertain.categorical(Map(
  "Sunny" -> 0.7,
  "Cloudy" -> 0.2,
  "Rain" -> 0.1
))
```

### Working with Uncertain Values

You can perform operations on uncertain values just like regular values:

```scala 3
val height = Uncertain.normal(5.8, 0.2) // feet
val width = Uncertain.normal(3.2, 0.1) // feet

// Calculate area (uncertainty propagates automatically)
val area = height * width

// Compare values
val isLargeRoom = area.gt(15.0)
```

### Making Decisions with Uncertainty

The library provides statistical tests to help make decisions:

```scala 3
val conversionRate = Uncertain.normal(0.12, 0.02)

// Is conversion rate probably above 10%?
if (conversionRate.gt(0.10).isProbable(sampleCount = 10_000)) {
  println("Conversion rate looks good")
}

// Are we 90% confident it's above 10%?
if (conversionRate.gt(0.10).probability(sampleCount = 10_000) > 0.9) {
  println("High confidence in good conversion rate")
}
```

### Transforming and Chaining with .map and .flatMap

Besides using standard math operators, you can use `.map` and `.flatMap`
for more complex operations, which allows you to build probabilistic workflows in a very
'scala native' way.

#### Transforming values with .map

Use `.map` to apply a simple function to the value inside an `Uncertain` container.
It transforms the value, but keeps the underlying uncertainty.

```scala 3
// Let's say we have speed in miles per hour
val speedMph = Uncertain.uniform(50, 70)

// We can convert it to kilometers per hour
val speedKph = speedMph.map(_ * 1.60934)

println(speedKph.sample()) // A random value between 80.467 and 112.6538
```

You can `.map` to any type.
For instance, you can classify a numeric value into a String:

```scala 3
val temperatureF = Uncertain.normal(75, 10) // 75°F ± 10°

val comfortLevel = temperatureF.map { temp =>
  if (temp > 85) "Hot"
  else if (temp > 65) "Comfortable"
  else "Cold"
}

// See the distribution of comfort levels
println(comfortLevel.histogram(sampleCount = 1000))
// Might output: Map(Comfortable -> 831, Hot -> 148, Cold -> 21)
```

or `.map` multiple uncertain values into a case class,
using a for-comprehension

```scala 3
final case class Rectangle(width: Double, height: Double) {
  def area: Double = width * height
}

val uncertainWidth = Uncertain.normal(10, 1)
val uncertainHeight = Uncertain.normal(5, 0.5)

// Create an uncertain Rectangle
val uncertainRectangle: Uncertain[Rectangle] =
  for {
    width <- uncertainWidth
    height <- uncertainHeight
  } yield Rectangle(width, height)

val uncertainArea = uncertainRectangle.map(_.area)
println(s"Expected area: ${uncertainArea.mean(sampleCount = 10_000)}")
```

#### Chaining operations with `.flatMap`

This is useful for creating conditional logic where the outcome of one probabilistic event determines the next one.

For example, let's model event attendance, which depends on the weather:

```scala 3
// First, model the weather as a probabilistic event
val weatherIsGood = Uncertain.bernoulli(0.7) // 70% chance of good weather

// Then, model attendance based on the weather
val attendance = weatherIsGood.flatMap { isGood =>
  if (isGood) {
    Uncertain.normal(100, 10) // If weather is good, expect 100 ± 10 people
  } else {
    Uncertain.normal(60, 15) // If bad, expect 60 ± 15 people
  }
}
println(s"Expected attendance: ${attendance.mean(sampleCount = 10_000)}")
```

Here, `.flatMap` chains the two uncertain events (`weatherIsGood` and a normal distribution ("bell curve" of attendance)
together.

### Measurement with Error

```scala 3
// Sensor reading with known error
val sensorReading = Uncertain.normal(actualValue = 23.5, error = 0.8)

// Check if it's within acceptable range
val isAcceptable = sensorReading > 20.0 && sensorReading < 25.0
if (isAcceptable.isProbable(sampleCount = 10_000)) {
  println("Sensor reading is probably acceptable")
}
```

### A/B Testing

```scala 3
val controlConversion = Uncertain.normal(0.08, 0.01) // 8% ± 1%
val testConversion = Uncertain.normal(0.11, 0.015) // 11% ± 1.5%

val improvement = testConversion - controlConversion
if ((improvement > 0.0).probabilityExceeds(exceeds = 0.95, sampleCount = 10_000)) {
  println("Test variant is significantly better")
}
```

### Risk Assessment

```scala 3
val serverLoad = Uncertain.normal(0.65, 0.15) // 65% ± 15%
val criticalThreshold = 0.9

val riskOfOverload = serverLoad > criticalThreshold
println(s"Risk of overload: ${riskOfOverload.probability(sampleCount = 10_000) * 100}%")
```

## Understanding Correlation

One of the library's key features is preserving correlation. This means that `x - x` always equals zero:

```scala 3
val x = Uncertain.normal(10, 2)

// These behave differently:
val uncorrelated = Uncertain.normal(10, 2) - Uncertain.normal(10, 2) // Not always zero, as each normal distribution is radomly sampled
val correlated = x - x // Always exactly zero since x is memoized

println(uncorrelated.sample()) // Might be 1.5, -0.8, 2.1, etc.
println(correlated.sample()) // Always 0.0
```

This matters when you use the same uncertain value multiple times in a calculation.

### Mixture Models

```scala 3
// Combine multiple distributions
val peakHours = Uncertain.normal(50, 5) // 50 users ± 5
val offHours = Uncertain.normal(15, 3) // 15 users ± 3

val userCount = Uncertain.mixture(Map(
  peakHours -> 0.3, // 30% of time it's peak hours
  offHours -> 0.7 // 70% of time it's off hours
))
```

## Performance Notes

The library uses smart sampling techniques:

- **Lazy evaluation**: Computations happen only when you request samples
- **Automatic sample sizing**: Statistical tests use only as many samples as needed
- **Efficient hypothesis testing**: Uses Sequential Probability Ratio Test (SPRT) instead of fixed large sample sizes

### Continuous vs Discrete Distributions

```scala 3
val continuous = Uncertain.normal(5, 1)
val discrete = Uncertain.categorical(Map(1 -> 0.3, 2 -> 0.7))

// Mode works well for discrete:
println(discrete.mode()) // Meaningful result

// Mode is less useful for continuous:
println(continuous.mode()) // Probably not very meaningful
```

## Testing

```scala 3
// Use fixed seeds for reproducible tests
val testValue = Uncertain.normal(10, 1)(Random(42))
// Results will be consistent across test runs
```

## API Reference

All functionality here is available via following two imports

```scala 3
import mostly.uncertaintee.Uncertain // brings in the core data type
import mostly.uncertaintee.syntax.* // brings in all syntax below
```

### **`Uncertain[T]` Methods**

These are the fundamental methods available on any `Uncertain[T]` instance.

| Method                                                                                                                           | Description                                                             | Example Use                                             |
|:---------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------|:--------------------------------------------------------|
| [`u.sample()`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)   | Retrieves a single random sample from the distribution.                 | `val singleRoll = diceRoll.sample()`                    |
| [`u.map(f)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)     | Transforms the value within the `Uncertain` container using a function. | `val speedKph = speedMph.map(_ * 1.609)`                |
| [`u.flatMap(f)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala) | Chains dependent uncertain computations together.                       | `val attendance = weatherIsGood.flatMap(isGood => ...)` |
| [`u.filter(p)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)  | Filters the uncertain value, returning an `Uncertain[Option[T]]`.       | `val validSpeed = speed.filter(s => s > 0 && s < 130)`  |
| [`u.iterator`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)   | Returns an infinite iterator of samples from the distribution.          | `val firstTenSamples = speed.iterator.take(10).toList`  |
| [`u.take(n)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)    | Collects a specified number of samples into a list.                     | `val samples = speed.take(1000)`                        |

---

### **Constructing an `Uncertain[T]` instance**

| Method                                                                                                                                               | Description                                                                    | Example Use                                                                 |
|:-----------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------|:----------------------------------------------------------------------------|
| [`Uncertain[T](sampler)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)            | Creates an `Uncertain` from a custom sampling function.                        | `val custom = Uncertain(() => math.random() * 10)`                          |
| [`Uncertain.always(value)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)          | Creates an `Uncertain` value that is always the same constant value.           | `val fixedValue = Uncertain.point(42)`                                      |
| [`Uncertain.normal(mean, stdDev)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)   | Creates a normal (Gaussian) distribution.                                      | `val temp = Uncertain.normal(72, 3)`                                        |
| [`Uncertain.uniform(min, max)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)      | Creates a uniform distribution.                                                | `val diceRoll = Uncertain.uniform(1, 7)`                                    |
| [`Uncertain.bernoulli(probability)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala) | Creates a true/false distribution with a given probability of `true`.          | `val coinFlip = Uncertain.bernoulli(0.5)`                                   |
| [`Uncertain.empirical(data)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)        | Creates a distribution by sampling from a collection of observed data.         | `val nextRating = Uncertain.empirical(List(4, 5, 3, 5))`                    |
| [`Uncertain.categorical(outcomes)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)  | Creates a distribution from a map of outcomes to their probabilities.          | `val weather = Uncertain.categorical(Map("Sunny" -> 0.7, "Rainy" -> 0.3))`  |
| [`Uncertain.mixture(components)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)    | Creates a mixture of different uncertain distributions with specified weights. | `val userCount = Uncertain.mixture(Map(peakHours -> 0.3, offHours -> 0.7))` |
| [`Uncertain.sequence(uncertains)`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/Uncertain.scala)   | Converts a `List[Uncertain[T]]` into a single `Uncertain[List[T]]`.            | `val allRolls = Uncertain.sequence(List(die1, die2))`                       |

---

### **Operations on Uncertain**

#### **Arithmetic Operations**

They are brought in specifically by
`import mostly.uncertaintee.syntax.arithmetic.*`.

| Method                                                                                                                        | Description                                                           | Example Use                                                    |
|:------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------|:---------------------------------------------------------------|
| [`+`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ArithmeticOps.scala) | Adds two uncertain values or an uncertain value and a constant.       | `val total = Uncertain.normal(10, 1) + Uncertain.normal(5, 2)` |
| [`-`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ArithmeticOps.scala) | Subtracts two uncertain values or a constant from an uncertain value. | `val difference = Uncertain.normal(10, 1) - 5.0`               |
| [`*`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ArithmeticOps.scala) | Multiplies two uncertain values or an uncertain value by a constant.  | `val area = uncertainWidth * uncertainHeight`                  |
| [`/`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ArithmeticOps.scala) | Divides an uncertain value by another or by a constant.               | `val ratio = Uncertain.normal(100, 5) / 10.0`                  |

---

#### **Boolean and Logical Operations**

These methods enable logical operations and statistical hypothesis testing on `Uncertain[Boolean]` values. They are
brought in specifically by `import mostly.uncertaintee.syntax.boolean.*`.

| Method                                                                                                                                      | Description                                                                | Example Use                                                                                |
|:--------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------|
| [`unary_!`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/BooleanOps.scala)            | Performs a logical NOT on an uncertain boolean.                            | `val isFailure = !isSuccess`                                                               |
| [`&&`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/BooleanOps.scala)                 | Performs a logical AND between two uncertain booleans.                     | `val bothTrue = Uncertain.bernoulli(0.8) && Uncertain.bernoulli(0.5)`                      |
| [\|\|](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/BooleanOps.scala)                 | Performs a logical OR between two uncertain booleans.                      | `val atLeastOneTrue = a \|\| b`                                                            |
| [`probability`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/BooleanOps.scala)        | Estimates the probability that the uncertain boolean is true.              | `val prob = (speed > 60).probability(sampleCount = 10_000)`                                |
| [`probabilityExceeds`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/BooleanOps.scala) | Tests if the probability of being true exceeds a given threshold.          | `val isConfident = (rate > 0.10).probabilityExceeds(exceeds = 0.95, sampleCount = 10_000)` |
| [`isProbable`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/BooleanOps.scala)         | A shorthand to check if the probability of being true is greater than 50%. | `if (isProfitable.isProbable(sampleCount = 10_000)) { ... }`                               |

---

#### **Comparison Operations**

These methods provide comparison operators for `Uncertain` values. They are brought in specifically by
`import mostly.uncertaintee.syntax.comparison.*`.

| Method                                                                                                                          | Description                                                             | Example Use                                |
|:--------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------|:-------------------------------------------|
| [`===`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ComparisonOps.scala) | Compares two uncertain values for equality on a sample-by-sample basis. | `val areSame = dieRoll1 === dieRoll2`      |
| [`!==`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ComparisonOps.scala) | Compares two uncertain values for inequality.                           | `val areDifferent = dieRoll1 !== dieRoll2` |
| [`>`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ComparisonOps.scala)   | Performs a greater-than comparison.                                     | `val isSpeeding = speed > 60.0`            |
| [`<`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ComparisonOps.scala)   | Performs a less-than comparison.                                        | `val isBelowFreezing = temp < 0.0`         |
| [`>=`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ComparisonOps.scala)  | Performs a greater-than-or-equal-to comparison.                         | `val hasEnough = stock >= orderSize`       |
| [`<=`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/ComparisonOps.scala)  | Performs a less-than-or-equal-to comparison.                            | `val withinLimit = weight <= 100.0`        |

---

#### **Functional Operations**

These methods provide powerful, functional programming-style operators. They are brought in specifically by
`import mostly.uncertaintee.syntax.functional.*`.

| Method                                                                                                                      | Description                                                                                     | Example Use                                                         |
|:----------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------|
| [`product`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/FpOps.scala) | Combines two uncertain values into an uncertain pair `Uncertain[(T, B)]`.                       | `val stats = height.product(weight)`                                |
| [`zipWith`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/FpOps.scala) | Combines two uncertain values using a provided function.                                        | `val bmi = height.zipWith(weight)(calculateBmi)`                    |
| [`collect`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/FpOps.scala) | Filters and maps an uncertain value using a partial function, returning `Uncertain[Option[B]]`. | `val sqrtOfPositives = dist.collect { case x if x > 0 => sqrt(x) }` |
| [`flatten`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/FpOps.scala) | Flattens a nested `Uncertain[Uncertain[T]]` into a single `Uncertain[T]`.                       | `val finalDist = chosenModel.flatten`                               |
| [`mapN`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/FpOps.scala)    | Applies a function to the results of multiple uncertain values.                                 | `val rect = (width, height).mapN(Rectangle.apply)`                  |

---

#### **Option Operations**

These methods help manage `Uncertain[Option[T]]` values. They are brought in specifically by
`import mostly.uncertaintee.syntax.option.*`.

| Method                                                                                                                            | Description                                                           | Example Use                                         |
|:----------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------|:----------------------------------------------------|
| [`orElse`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/OptionOps.scala)    | Provides a fallback `Uncertain` value to use when a sample is `None`. | `val finalSpeed = validSpeed.orElse(fallbackModel)` |
| [`getOrElse`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/OptionOps.scala) | Provides a constant default value to use when a sample is `None`.     | `val finalTemp = plausibleTemp.getOrElse(15.0)`     |

---

#### **Statistical Operations**

These methods are for performing statistical analysis on `Uncertain` values. They are brought in specifically by
`import mostly.uncertaintee.syntax.statistical.*`.

| Method                                                                                                                                                   | Description                                                                                        | Example Use                                                                      |
|:---------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------|
| [`mode`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)                        | Finds the most frequently occurring value in a set of samples.                                     | `val mostLikelyOutcome = diceRoll.mode(sampleCount = 10_000)`                    |
| [`histogram`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)                   | Creates a map showing the frequency of each sampled value.                                         | `val frequencies = diceRoll.histogram(sampleCount = 10_000)`                     |
| [`entropy`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)                     | Estimates the information entropy (randomness) of the distribution in bits.                        | `val randomness = fairCoin.entropy(sampleCount = 10_000)`                        |
| [`expectedValue` / `mean`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)      | Estimates the average value of the distribution by sampling.                                       | `val avgSpeed = speed.mean(sampleCount = 10_000)`                                |
| [`populationStandardDeviation`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala) | Estimates the standard deviation of the population.                                                | `val popStdDev = distribution.populationStandardDeviation(sampleCount = 10_000)` |
| [`standardDeviation`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)           | Estimates the sample standard deviation using Bessel's correction.                                 | `val sampleStdDev = distribution.standardDeviation(sampleCount = 10_000)`        |
| [`confidenceInterval`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)          | Estimates an interval that contains the true value with a given confidence.                        | `val (low, high) = speed.confidenceInterval(0.95, sampleCount = 10_000)`         |
| [`cdf`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)                         | Estimates the Cumulative Distribution Function (the probability that a sample is ≤ a given value). | `val prob = speed.cdf(60.0, sampleCount = 10_000)`                               |
| [`probabilityOfSuccess`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)        | For `Uncertain[Option[T]]`, calculates the probability of it being a `Some`.                       | `val successRate = filteredValue.probabilityOfSuccess(sampleCount = 10_000)`     |
| [`probabilityOfFailure`](https://github.com/MostlyScala/uncertain-tee/tree/main/core/src/main/scala/mostly/uncertaintee/ops/StatisticalOps.scala)        | For `Uncertain[Option[T]]`, calculates the probability of it being a `None`.                       | `val failureRate = filteredValue.probabilityOfFailure(sampleCount = 10_000)`     |

## Learn more

* It originates from the research paper `Uncertain<T>: A First-Order Type for Uncertain
  Data.` (https://www.microsoft.com/en-us/research/publication/uncertaint-a-first-order-type-for-uncertain-data-2/)
* You can learn more from this excellent Blog Post about the pattern: https://nshipster.com/uncertainty
    * Author of which, @Mattt, has a swift implementation of the Uncertain[T] pattern
      here: https://github.com/mattt/Uncertain
* Feel like contributing? Read
  the [Code of Conduct](https://github.com/MostlyScala/uncertain-tee/blob/main/CODE_OF_CONDUCT.md]) first 🙏

## Cats Support

The cats-support library exposes the following typeclasses for `Uncertain[T]`; `Functor`, `Applicative`, `Monad`,
`Monoid`.

Use these instances by including the support module, and the following import:

```scala
import mostly.uncertaintee.cats.instances.given
```

| Build Tool    | Instruction                                                              |
|:--------------|:-------------------------------------------------------------------------|
| **sbt**       | `libraryDependencies += "mostly" %% "uncertain-tee" % NOT_YET_RELEASED"` |
| **mill**      | `ivy"mostly::uncertain-tee:NOT_YET_RELEASED"`                            |
| **scala-cli** | `//> using dep "mostly::uncertain-tee:NOT_YET_RELEASED"`                 |
