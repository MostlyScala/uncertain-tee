# Quick Start

Get started with `Uncertain[T]` in just a few minutes.

## Installation

Add the following to your `build.sbt`:

```scala
libraryDependencies += "mostly" %% "uncertain-tee" % "@VERSION@"
```

Or with other build tools:

| Build Tool    | Instruction                                                |
|:--------------|:-----------------------------------------------------------|
| **mill**      | `ivy"mostly::uncertain-tee:@VERSION@"`                     |
| **scala-cli** | `//> using dep "mostly::uncertain-tee:@VERSION@"`          |

> **Note:** The library is still under active development and not yet released to Maven Central.

## Your First Example

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

// Create an uncertain speed with measurement error
val speed = Uncertain.normal(65.0, 5.0) // mean=65 mph, std dev=5 mph

// Check if we're probably speeding (speed limit is 60)
if (speed.gt(60).isProbable(sampleCount = 10_000)) {
  println("You're probably speeding")
}

// Get a confidence interval
val (low, high) = speed.confidenceInterval(0.95, sampleCount = 10_000)
println(s"95% confident speed is between $low and $high mph")
```

## Core Concepts

### What is Uncertain[T]?

`Uncertain[T]` represents a value that isn't exact - it has randomness or measurement error. Instead of working with single values, you work with *distributions* of possible values.

### Creating Uncertain Values

The library provides several ways to create uncertain values:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

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

### Performing Operations

You can perform operations on uncertain values just like regular values:

```scala mdoc
val height = Uncertain.normal(5.8, 0.2) // feet
val width = Uncertain.normal(3.2, 0.1) // feet

// Calculate area (uncertainty propagates automatically)
val area = height * width

// Compare values
val isLargeRoom = area > 15.0
```

## Next Steps

- Learn about [Core Concepts](concepts.md)
- Explore [Distributions](distributions.md)
- See [Use Cases](use-cases.md)
- Check the [API Reference](api.md)
