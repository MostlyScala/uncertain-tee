# Uncertain[T] - Working with Probabilistic Data

## What is this library?

`Uncertain[T]` helps you work with data that isn't exact, like measurements with error, user behavior predictions, or
any value that has uncertainty. Instead of just working with single values, you work with *distributions* of possible
values.

Instead of saying "the user will click the button," with Uncertain[T]  you say "there's a 75% chance the user
will click the button" and write code that handles that uncertainty without needing to hand-roll
a big block of statistics-calculating-code.

Amongst other features of the library, it provides `.map` and `.flatMap` (and therefor for-comprehensions) for
probabilistic computing, all centered around the core data type of `Uncertain[T]` - which ends up being quite legible in
application code compared to writing out "raw" statistical math by hand.

## Quick Start

```scala 3
// Create an uncertain speed with some measurement error
val speed = Uncertain.normal(65.0, 5.0) // mean=65 mph, std dev=5 mph

// Check if we're probably speeding (speed limit is 60)
if (speed.gt(60).isProbable()) {
  println("You're probably speeding")
}

// Get a confidence interval
val (low, high) = speed.confidenceInterval(0.95)
println(s"95% confident speed is between $low and $high mph")
```

## Core Concepts

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
if (conversionRate.gt(0.10).isProbable()) {
  println("Conversion rate looks good")
}

// Are we 90% confident it's above 10%?
if (conversionRate.gt(0.10).probability(exceeds = 0.9)) {
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
println(comfortLevel.histogram(1000))
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
println(s"Expected area: ${uncertainArea.expectedValue()}")
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
println(s"Expected attendance: ${attendance.expectedValue()}")
```

Here, `.flatMap` chains the two uncertain events (`weatherIsGood` and `attendance`) together.

### Measurement with Error

```scala 3
// Sensor reading with known error
val sensorReading = Uncertain.normal(actualValue = 23.5, error = 0.8)

// Check if it's within acceptable range
val isAcceptable = sensorReading > 20.0 && sensorReading < 25.0
if (isAcceptable.isProbable()) {
  println("Sensor reading is probably acceptable")
}
```

### A/B Testing

```scala 3
val controlConversion = Uncertain.normal(0.08, 0.01) // 8% ± 1%
val testConversion = Uncertain.normal(0.11, 0.015) // 11% ± 1.5%

val improvement = testConversion - controlConversion
if ((improvement > 0.0).probability(exceeds = 0.95)) {
  println("Test variant is significantly better")
}
```

### Risk Assessment

```scala 3
val serverLoad = Uncertain.normal(0.65, 0.15) // 65% ± 15%
val criticalThreshold = 0.9

val riskOfOverload = serverLoad > criticalThreshold
println(s"Risk of overload: ${riskOfOverload.expectedValue() * 100}%")
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

## Learn more

* It originates from the research paper `Uncertain<T>: A First-Order Type for Uncertain
  Data.` (https://www.microsoft.com/en-us/research/publication/uncertaint-a-first-order-type-for-uncertain-data-2/)

* You can learn more from this excellent Blog Post about the pattern: https://nshipster.com/uncertainty
    * Author of which, @Mattt, has a swift implementation of the Uncertain[T] pattern
      here: https://github.com/mattt/Uncertain

* Released under the [MIT license](https://github.com/MostlyScala/uncertain-tee/blob/main/LICENSE])
* Feel like contributing? Read
  the [Code of Conduct](https://github.com/MostlyScala/uncertain-tee/blob/main/CODE_OF_CONDUCT.md]) first 🙏
