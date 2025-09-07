# 🚧🦺🏗️ UNDER CONSTRUCTION 🏗🦺🚧️

# Uncertain[T] - Uncertainty aware programming in Scala

`uncertain-tee` is a library (currently under development) that implements the Uncertain[T] pattern for probabilistic
programming in Scala.

* It originates from this research paper "Uncertain<T>: A First-Order Type for Uncertain
  Data." (https://www.microsoft.com/en-us/research/publication/uncertaint-a-first-order-type-for-uncertain-data-2/)
* You can learn more from this excellent Blog Post about the pattern: https://nshipster.com/uncertainty

`uncertain-tee` provides core constructs for uncertainty-aware programming, for decisions-making within with noisy,
error-prone, or incomplete data. Memoized computation graphs ensure that the same sample is used across operations (You
don't have to know this to use the library, but it is the key to the library's guarantees and it's also sort of
interesting!)

## Quick Start

```scala

import mostly.uncertaintee.*

object UncertainExample extends App {
  // Randomness can be controlled by providing an implicit instance; if not provided, code will default to `new Random()`
  given Random = new Random()

  // Basic distributions
  println("\n--- Basic Example ---")
  val speed = Uncertain.normal(5.0, 2.0)
  val distance = Uncertain.uniform(100.0, 200.0)

  // Arithmetic operations are composed into a computation graph.
  val time = distance / speed

  // Conditional probability using hypothesis testing with symbolic operators.
  if ((speed > 4.0).probability(exceeds = 0.9)) {
    println("90% confident you're going fast")
  }

  // Implicit conditional: a shorthand for checking if P(condition) > 0.5.
  if ((speed > 4.0).isProbable()) {
    println("More likely than not you're going fast")
  }

  // Statistics
  println(f"Expected time: ${time.expectedValue(5000)}%.2f")
  println(f"Speed std dev: ${speed.standardDeviation()}%.2f")
  speed.confidenceInterval().foreach { case (low, high) =>
    println(f"Speed 95%% Confidence Interval: ($low%.2f, $high%.2f)")
  }

  // Map
  println("--- `.map` preserves correlation ---")
  val x: Uncertain[Double] = Uncertain.normal(10, 2)
  val y: Uncertain[Double] = x.map(_ * 2)
  val z: Uncertain[Double] = y - y + 20.0
  println(s"10 samples of (x*2 - x*2 + 20): ${z.take(10)}")

  // --- Demonstrating controllable randomness ---
  println("\n--- Demonstrating Controllable Randomness ---")
  println("By providing a 'given Random' with a fixed seed, we get reproducible results.")

  def generateSeededSamples(): List[Double] = {
    // This `given` is lexically scoped to this method.
    // All `Uncertain` factory methods called within this scope will use this specific `Random` instance.
    given seededRandom: Random = new Random(12345L)

    val dist = Uncertain.normal(10, 2)
    dist.take(5)
  }

  val samples1 = generateSeededSamples()
  val samples2 = generateSeededSamples()

  println(s"First run with seed 12345:  ${samples1.map(v => f"$v%.3f")}")
  println(s"Second run with seed 12345: ${samples2.map(v => f"$v%.3f")}")
  println(s"The results are identical: ${samples1 == samples2}")
```

### Technical details

* Wherever possible, the library attempts to express the mathematical concepts in the paper in a Scala-idiomatic way,
  using `map` and `flatMap` (allowing for-comprehensions over the `Uncertain[T]` type)
* Scala 3 only (for now)
* **It is still under development** so breaking changes and runtime exceptions and faulty math are still likely.
    * Test suite is not yet complete. Probably don't use this in production just yet.
* Eventual scope:
    * "core" model (`mostly.uncertaintee.Uncertain[T]`) with no extra dependencies
    * "cats/cats-effect" module (`mostly.uncertaintee.cats.Uncertain[F[_], T]`)
    * Other effects framework bindings (?)

### Acknowledgements and Influences

Influenced and inspired by the [Blog post by @Mattt](https://nshipster.com/uncertainty) and
their [Implementation of Uncertain<T> for Swift](https://github.com/mattt/Uncertain)