# Uncertain[T] - Working with Probabilistic Data

## What is this library?

`Uncertain[T]` helps you work with data that isn't exact, like measurements with error, user behavior predictions, or
any value that has uncertainty. Instead of just working with single values, you work with *distributions* of possible
values.

Think of it like this: instead of saying "the user will click the button," you can say "there's a 75% chance the user
will click the button" and write code that handles that uncertainty properly.

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

### Measurement with Error

```scala 3
// Sensor reading with known error
val sensorReading = Uncertain.normal(actualValue = 23.5, error = 0.8)

// Check if it's within acceptable range
val isAcceptable = sensorReading.gt(20.0) && sensorReading.lt(25.0)
if (isAcceptable.isProbable()) {
  println("Sensor reading is probably acceptable")
}
```

### A/B Testing

```scala 3
val controlConversion = Uncertain.normal(0.08, 0.01) // 8% ± 1%
val testConversion = Uncertain.normal(0.11, 0.015) // 11% ± 1.5%

val improvement = testConversion - controlConversion
if (improvement.gt(0.0).probability(exceeds = 0.95)) {
  println("Test variant is significantly better")
}
```

### Risk Assessment

```scala 3
val serverLoad = Uncertain.normal(0.65, 0.15) // 65% ± 15%
val criticalThreshold = 0.9

val riskOfOverload = serverLoad.gt(criticalThreshold)
println(s"Risk of overload: ${riskOfOverload.expectedValue() * 100}%")
```

## Understanding Correlation

One of the library's key features is preserving correlation. This means that `x - x` always equals zero:

```scala 3
val x = Uncertain.normal(10, 2)

// These behave differently:
val uncorrelated = Uncertain.normal(10, 2) - Uncertain.normal(10, 2) // Not always zero!
val correlated = x - x // Always exactly zero

println(uncorrelated.sample()) // Might be 1.5, -0.8, 2.1, etc.
println(correlated.sample()) // Always 0.0
```

This matters when you use the same uncertain value multiple times in a calculation.

### Conditional Logic

```scala 3
val weatherIsGood = Uncertain.bernoulli(0.7)
val attendance = weatherIsGood.flatMap { good =>
  if (good) Uncertain.normal(100, 10) // Good weather
  else Uncertain.normal(60, 15) // Bad weather
}
```

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

## 'Gotchas' that are worth understanding

### 1. Boolean Arithmetic Quirks

```scala 3
val flag = Uncertain.bernoulli(0.5)

// This works for statistics:
println(flag.expectedValue()) // 0.5 (50% chance of true)

// But Boolean + Boolean gives OR, not addition:
val combined = flag + flag // This is flag || flag, not 2*flag
```

### 2. Continuous vs Discrete Distributions

```scala 3
val continuous = Uncertain.normal(5, 1)
val discrete = Uncertain.categorical(Map(1 -> 0.3, 2 -> 0.7))

// Mode works well for discrete:
println(discrete.mode()) // Meaningful result

// Mode is less useful for continuous:
println(continuous.mode()) // Probably not very meaningful
```

## Integration Tips

### Testing

```scala 3
// Use fixed seeds for reproducible tests
val testValue = Uncertain.normal(10, 1)(Random(42))
// Results will be consistent across test runs
```

## Learn more

* It originates from the research paper `Uncertain<T>: A First-Order Type for Uncertain
  Data.`, available
  at: https://www.microsoft.com/en-us/research/publication/uncertaint-a-first-order-type-for-uncertain-data-2/

* You can learn more from this excellent Blog Post about the pattern: https://nshipster.com/uncertainty
    * Author of which, @Mattt, has a swift implementation of the Uncertain[T] pattern
      here: https://github.com/mattt/Uncertain
