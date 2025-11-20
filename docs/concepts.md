# Core Concepts

Understanding the fundamental concepts behind `Uncertain[T]`.

## Probabilistic Programming

When coding with uncertainty, you don't say "the user will click the button," instead we say "there's a 75% chance the user will click the button" - and write code that handles that uncertainty.

Traditional approach:
```scala
val clickRate = 0.75  // Single value
val revenue = clickRate * 100  // Deterministic calculation
```

With `Uncertain[T]`:
```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val clickRate = Uncertain.bernoulli(0.75)  // Distribution
val revenue = clickRate.map(clicked => if (clicked) 100 else 0)

// Get probability-based insights
val expectedRevenue = revenue.mean(sampleCount = 10_000)
```

## Correlation Preservation

One of the library's key features is **preserving correlation**. This means that when you use the same uncertain value multiple times in a calculation, it maintains its relationship.

### Why This Matters

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val x = Uncertain.normal(10, 2)

// These behave differently:
val uncorrelated = Uncertain.normal(10, 2) - Uncertain.normal(10, 2) 
// Not always zero - each normal distribution is independently sampled

val correlated = x - x 
// Always exactly zero since x is memoized
```

Let's verify:

```scala mdoc
println(s"Uncorrelated sample: ${uncorrelated.sample()}")
println(s"Correlated sample: ${correlated.sample()}")  // Always 0.0
```

### How It Works

Internally, `Uncertain[T]` uses a **computation graph** (also called a computation tree) that tracks dependencies between uncertain values. When you sample from a derived uncertain value, the graph ensures that:

1. Each base uncertain value is sampled once per evaluation
2. All derived values use the same base samples
3. Correlations are preserved throughout the computation

This is implemented through lazy evaluation and memoization, making it both efficient and correct.

## Monadic Structure

`Uncertain[T]` is a monad, providing `.map` and `.flatMap` operations that let you compose uncertain computations safely.

### Transforming with .map

Use `.map` to apply a function to uncertain values:

```scala mdoc:reset
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

val speedMph = Uncertain.uniform(50, 70)
val speedKph = speedMph.map(_ * 1.60934)

println(s"Sample speed: ${speedKph.sample()} kph")
```

You can map to any type:

```scala mdoc
val temperatureF = Uncertain.normal(75, 10)

val comfortLevel = temperatureF.map { temp =>
  if (temp > 85) "Hot"
  else if (temp > 65) "Comfortable"
  else "Cold"
}

println(comfortLevel.histogram(sampleCount = 1000))
```

### Chaining with .flatMap

Use `.flatMap` for conditional logic where one uncertain event affects another:

```scala mdoc
// Weather determines attendance
val weatherIsGood = Uncertain.bernoulli(0.7)

val attendance = weatherIsGood.flatMap { isGood =>
  if (isGood) {
    Uncertain.normal(100, 10) // Good weather: 100 ± 10 people
  } else {
    Uncertain.normal(60, 15)  // Bad weather: 60 ± 15 people
  }
}

println(s"Expected attendance: ${attendance.mean(sampleCount = 10_000)}")
```

### For-Comprehensions

The monadic structure enables for-comprehensions:

```scala mdoc
case class Rectangle(width: Double, height: Double) {
  def area: Double = width * height
}

val uncertainWidth = Uncertain.normal(10, 1)
val uncertainHeight = Uncertain.normal(5, 0.5)

val uncertainRectangle: Uncertain[Rectangle] =
  for {
    width <- uncertainWidth
    height <- uncertainHeight
  } yield Rectangle(width, height)

val uncertainArea = uncertainRectangle.map(_.area)
println(s"Expected area: ${uncertainArea.mean(sampleCount = 10_000)}")
```

## Monte Carlo Simulation

Under the hood, `Uncertain[T]` uses Monte Carlo simulation - it generates many random samples to estimate statistical properties. This makes it:

- **Flexible**: Works with any distribution
- **Intuitive**: No complex mathematical formulas needed
- **Practical**: Handles real-world complexity

### Sampling Strategy

The library uses smart sampling:

- **Lazy evaluation**: Computations happen only when you request samples
- **Automatic sample sizing**: Statistical tests use only as many samples as needed
- **Efficient hypothesis testing**: Uses Sequential Probability Ratio Test (SPRT) instead of fixed large sample sizes

## Type Safety

`Uncertain[T]` is fully type-safe. You can create uncertain values of any type:

```scala mdoc
val uncertainInt: Uncertain[Int] = Uncertain.uniform(1, 7)
val uncertainString: Uncertain[String] = Uncertain.categorical(Map(
  "Success" -> 0.8,
  "Failure" -> 0.2
))
val uncertainOption: Uncertain[Option[Double]] = 
  Uncertain.normal(10, 2).filter(_ > 5)
```

Operations preserve types appropriately:

```scala mdoc
val sum: Uncertain[Int] = uncertainInt + uncertainInt
val comparison: Uncertain[Boolean] = uncertainInt > 3
```

## Next Steps

- Learn about [Available Distributions](distributions.md)
- See [Real-World Use Cases](use-cases.md)
- Explore [Statistical Operations](statistics.md)
