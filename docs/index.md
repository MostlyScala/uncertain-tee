# Uncertain[T] - Working with Probabilistic Data

Uncertain-tee is a **monte carlo simulation based** and **correlation preserving** library for working with uncertain data in Scala.

## Overview

`Uncertain[T]` helps you work with data that isn't exact - like measurements with error, user behavior predictions, or any value that has uncertainty. Instead of working with single values, you work with *distributions* of possible values.

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

// Project estimation with uncertainty
val frontend = Uncertain.triangular(5, 10, 20)  // days
val backend = Uncertain.triangular(8, 15, 30)
val testing = Uncertain.triangular(3, 5, 10)

val total = frontend + backend + testing

// Get realistic estimates
val percentiles = total.percentiles(sampleCount = 100_000)
println(s"50% chance: ${percentiles(50)} days")
println(s"90% chance: ${percentiles(90)} days")

// Probability of meeting deadline
val onTimeProb = (total < 30).probability(sampleCount = 100_000)
println(f"On-time probability: ${onTimeProb * 100}%.1f%%")
```

## Key Features

### 🔗 Correlation Preservation

The library automatically preserves correlations between uncertain values, ensuring mathematically correct operations:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val x = Uncertain.normal(10, 2)
val difference = x - x  // Always 0.0, not a distribution!

println(s"x - x = ${difference.sample()}")  // Always 0.0
```

### 🎯 Type Safe

Fully type-safe operations that work with any type:

```scala mdoc
val uncertainInt: Uncertain[Int] = Uncertain.uniform(1, 7)
val uncertainString: Uncertain[String] = Uncertain.categorical(Map(
  "Success" -> 0.8,
  "Failure" -> 0.2
))
```

### 🧮 Rich Statistical Operations

Built-in support for common statistical operations:

```scala mdoc
val data = Uncertain.normal(100, 15)

val mean = data.mean(sampleCount = 10_000)
val stdDev = data.standardDeviation(sampleCount = 10_000)
val (low, high) = data.confidenceInterval(0.95, sampleCount = 10_000)
```

### 🎲 Many Distributions

Support for common probability distributions:

- Normal, Uniform, Triangular
- Bernoulli, Binomial, Poisson
- Exponential, Beta, Rayleigh
- Empirical, Categorical, Mixture
- And more...

### 🔄 Monadic Composition

Use `.map`, `.flatMap`, and for-comprehensions for elegant probabilistic programming:

```scala mdoc
val weatherIsGood = Uncertain.bernoulli(0.7)

val attendance = weatherIsGood.flatMap { isGood =>
  if (isGood) Uncertain.normal(100, 10)
  else Uncertain.normal(60, 15)
}

println(s"Expected: ${attendance.mean(sampleCount = 10_000)} people")
```

## Installation

This library is available for Scala 3.3+

Add to your `build.sbt`:

```scala
libraryDependencies += "mostly" %% "uncertain-tee" % "@VERSION@"
```

| Build Tool    | Instruction                                                |
|:--------------|:-----------------------------------------------------------|
| **sbt**       | `libraryDependencies += "mostly" %% "uncertain-tee" % "@VERSION@"` |
| **mill**      | `ivy"mostly::uncertain-tee:@VERSION@"`                     |
| **scala-cli** | `//> using dep "mostly::uncertain-tee:@VERSION@"`          |

> **Note:** The library is still under active development and not yet released to Maven Central.

## Quick Example

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

// A/B test comparison
val controlConversion = Uncertain.normal(0.08, 0.01)
val testConversion = Uncertain.normal(0.11, 0.015)

val improvement = testConversion - controlConversion

if ((improvement > 0.0).probabilityExceeds(exceeds = 0.95, sampleCount = 10_000)) {
  println("Test variant is significantly better!")
  println(f"Expected improvement: ${improvement.mean(sampleCount = 10_000) * 100}%.2f%%")
}
```

## Documentation

- **[Quick Start](quickstart.md)** - Get started in minutes
- **[Core Concepts](concepts.md)** - Understand the fundamentals
- **[Distributions](distributions.md)** - Available probability distributions
- **[Use Cases](use-cases.md)** - Real-world examples
- **[Statistical Operations](statistics.md)** - Statistical analysis guide
- **[Quantiles](quantiles.md)** - Working with percentiles and quartiles
- **[API Reference](api.md)** - Complete API documentation

## Cats Support

Typeclass instances for Cats are available in a separate module:

```scala
libraryDependencies += "mostly" %% "uncertain-tee-cats" % "@VERSION@"
```

```scala
import mostly.uncertaintee.cats.instances.given
// Provides: Functor, Applicative, Monad, Monoid instances
```

## Learn More

- Originated from the research paper [Uncertain<T>: A First-Order Type for Uncertain Data](https://www.microsoft.com/en-us/research/publication/uncertaint-a-first-order-type-for-uncertain-data-2/)
- Excellent blog post about the pattern: [Uncertainty - NSHipster](https://nshipster.com/uncertainty)
- Swift implementation by @mattt: [Uncertain](https://github.com/mattt/Uncertain)

## Contributing

Contributions are welcome! Please read the [Code of Conduct](https://github.com/MostlyScala/uncertain-tee/blob/main/CODE_OF_CONDUCT.md) first.

## License

Licensed under the Apache License, Version 2.0.

