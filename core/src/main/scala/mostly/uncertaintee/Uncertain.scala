package mostly.uncertaintee

import java.util.UUID
import scala.collection.mutable
import scala.math.*
import scala.util.Random

/** A type for working with uncertain data - values that have some randomness or measurement error.
  *
  * Instead of working with single values like `speed = 65.0`, you can work with uncertain values like
  * `speed = "somewhere between 60-70 mph"` and let the library handle the math correctly.
  *
  * This is useful for:
  *   - Sensor readings with measurement error
  *   - User behavior predictions
  *   - A/B test analysis
  *   - Risk assessment
  *   - Any calculation where inputs aren't perfectly known
  *
  * @example
  *   Basic usage:
  *   {{{
  * // Create uncertain speed with measurement error
  * val speed = Uncertain.normal(65.0, 5.0)  // 65 mph ± 5 mph
  *
  * // Check if we're probably speeding (limit is 60 mph)
  * if (speed.gt(60).isProbable()) {
  *   println("You're probably speeding")
  * }
  *
  * // Math works naturally - uncertainty propagates
  * val distance = Uncertain.normal(120, 10)  // miles ± 10
  * val time = distance / speed  // Automatically handles uncertainty in both values
  *   }}}
  *
  * @example
  *   A/B testing:
  *   {{{
  * val controlRate = Uncertain.normal(0.08, 0.01)  // 8% conversion ± 1%
  * val testRate = Uncertain.normal(0.11, 0.015)     // 11% conversion ± 1.5%
  *
  * val improvement = testRate - controlRate
  * if (improvement.gt(0.0).probability(exceeds = 0.95)) {
  *   println("Test variant is significantly better!")
  * }
  *   }}}
  *
  * The library preserves correlations automatically. This means `x - x` always equals zero, even when `x` is uncertain.
  * This is crucial for correct uncertainty propagation.
  *
  * @tparam T
  *   The type of the uncertain value (Double, Int, Boolean, String, etc.)
  */
sealed abstract class Uncertain[T] {

  /** The function that generates a single random sample from the distribution. */
  val sampler: () => T

  /** Internal computation graph node - handles lazy evaluation and correlation preservation. */
  val node: ComputationNode[T]

  /** Gets one sample from this uncertain value.
    *
    * Each call returns a different random sample from the distribution. If this uncertain value is built from other
    * uncertain values, their samples are coordinated to preserve correlations.
    *
    * @example
    *   Correlation preservation:
    *   {{{
    * val x = Uncertain.normal(10, 2)
    * val difference = x - x
    * println(difference.sample())  // Always 0.0, never something like 1.3
    *   }}}
    *
    * @return
    *   A single sample of type T
    */
  def sample(): T = {
    val context = new SampleContext()
    node.evaluate(context)
  }

  /** Transforms each sample using a function.
    *
    * This is like calling `list.map(f)` but for probability distributions. The uncertainty structure is preserved.
    *
    * @example
    *   Unit conversion:
    *   {{{
    * val speedMph = Uncertain.uniform(50, 70)
    * val speedKph = speedMph.map(_ * 1.60934)  // Convert each sample to km/h
    *   }}}
    *
    * @param f
    *   Function to apply to each sample
    * @return
    *   New uncertain value with the function applied
    */
  def map[U](f: T => U): Uncertain[U] = {
    val newNode = ComputationMap(this.node, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Chains uncertain computations together.
    *
    * Use this when the next uncertain value depends on the current sample. Think "if the weather is good, expect 100
    * customers, otherwise expect 60".
    *
    * @example
    *   Weather-dependent attendance:
    *   {{{
    * val isGoodWeather = Uncertain.bernoulli(0.7)  // 70% chance of good weather
    * val attendance = isGoodWeather.flatMap { good =>
    *   if (good) Uncertain.normal(100, 10)  // Good weather: 100 ± 10 people
    *   else Uncertain.normal(60, 15)        // Bad weather: 60 ± 15 people
    * }
    *   }}}
    *
    * @param f
    *   Function that takes a sample and returns a new uncertain value
    * @return
    *   New uncertain value representing the chained computation
    */
  def flatMap[U](f: T => Uncertain[U]): Uncertain[U] = {
    val newNode = ComputationFlatMap(this.node, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Returns an endless stream of samples.
    *
    * Useful for functional-style processing or when you need many samples.
    *
    * @example
    *   Getting multiple samples:
    *   {{{
    * val speed = Uncertain.normal(65, 5)
    * val samples = speed.iterator.take(1000).toList
    * val average = samples.sum / samples.length
    *   }}}
    */
  def iterator: Iterator[T] = Iterator.continually(sample())

  /** Collects multiple samples into a list.
    *
    * @param n
    *   Number of samples to collect (must be non-negative)
    * @return
    *   List containing n random samples
    *
    * @example
    *   Collecting samples for analysis:
    *   {{{
    * val temperature = Uncertain.normal(72, 3)
    * val readings = temperature.take(100)  // Get 100 temperature readings
    *   }}}
    */
  def take(n: Int): List[T] = {
    require(n >= 0, "Number of samples must be non-negative.")
    iterator.take(n).toList
  }
}

// Precise doubles for common values used throughout the library
private[this] val Zero: Double     = Numeric[Double].fromInt(0)
private[this] val One: Double      = Numeric[Double].fromInt(1)
private[this] val Two: Double      = Numeric[Double].fromInt(2)
private[this] val MinusTwo: Double = Numeric[Double].fromInt(-2)

// =================================================================================================
// Computation Graph Nodes
// =================================================================================================

/** Internal: A node in the computation graph that represents operations or sources of uncertainty.
  *
  * This is the foundation for lazy evaluation and correlation preservation. Don't work with these directly - they're
  * created automatically when you use operations like `map`, `flatMap`, or arithmetic operators.
  */
sealed private[uncertaintee] trait ComputationNode[+T] {

  /** Evaluates this node within a sampling context.
    *
    * The SampleContext ensures that shared uncertain values are sampled only once per evaluation, which preserves
    * correlation.
    *
    * @param context
    *   The sampling context for correlation preservation
    * @return
    *   The sampled value
    */
  def evaluate(context: SampleContext = new SampleContext): T = this match {
    case Computation(id, sampler)          =>
      context
        .getValue[T](id)
        .getOrElse {
          val value = sampler()
          context.setValue(id, value)
          value
        }
    case ComputationMap(source, operation) =>
      operation(source.evaluate(context))
    case ComputationFlatMap(source, f)     =>
      val sourceValue    = source.evaluate(context)
      val innerUncertain = f(sourceValue)
      innerUncertain.node.evaluate(context)
  }
}

/** Internal: A source of uncertainty in the computation graph.
  *
  * This represents original uncertain values (like `Uncertain.normal(10, 2)`). It holds a unique ID and sampling
  * function, and uses memoization to ensure the same value is used consistently within a single evaluation.
  *
  * @param id
  *   Unique identifier for this uncertainty source
  * @param sampler
  *   Function that generates random samples
  */
final private[uncertaintee] case class Computation[T](
  id: UUID,
  sampler: () => T
) extends ComputationNode[T]

/** Internal: Represents applying a function to an uncertain value (used by `map`).
  *
  * @param source
  *   The uncertain value to transform
  * @param operation
  *   The function to apply
  */
final private[uncertaintee] case class ComputationMap[A, B](
  source: ComputationNode[A],
  operation: A => B
) extends ComputationNode[B]

/** Internal: Represents chaining uncertain computations (used by `flatMap`).
  *
  * @param source
  *   The first uncertain value
  * @param f
  *   Function that creates the next uncertain value based on the first sample
  */
final private[uncertaintee] case class ComputationFlatMap[A, B](
  source: ComputationNode[A],
  f: A => Uncertain[B]
) extends ComputationNode[B]

// =================================================================================================
// Factory Object
// =================================================================================================

/** Factory for creating uncertain values from various distributions and data sources. */
object Uncertain {

  /** Creates an uncertain value from any sampling function.
    *
    * This is the most flexible way to create uncertain values.
    *
    * @example
    *   Custom distribution:
    *   {{{
    * // 30% chance of 1, 70% chance of 2
    * val custom = Uncertain { () =>
    *   if (Random.nextDouble() < 0.3) 1 else 2
    * }
    *   }}}
    *
    * @param sampler
    *   Function that produces a new sample each time it's called
    * @param random
    *   Source of randomness (usually omit this to use default)
    * @return
    *   New uncertain value
    */
  def apply[T](sampler: () => T)(using random: Random = new Random()): Uncertain[T] = {
    val id = UUID.nameUUIDFromBytes(random.nextBytes(16))
    val s  = sampler
    new Uncertain[T] {
      override val sampler: () => T         = s
      override val node: ComputationNode[T] = Computation(id = id, sampler = s)
    }
  }

  /** Internal constructor with pre-defined computation node. */
  private[uncertaintee] def apply[T](sampler: () => T, computationNode: ComputationNode[T]): Uncertain[T] = {
    val s = sampler
    new Uncertain[T] {
      override val sampler: () => T         = s
      override val node: ComputationNode[T] = computationNode
    }
  }

  /** Creates an uncertain value that's always the same (no uncertainty).
    *
    * Useful when you need to mix certain and uncertain values in calculations.
    *
    * @example
    *   Mixing certain and uncertain values:
    *   {{{
    * val uncertainSpeed = Uncertain.normal(65, 5)
    * val certainTime = Uncertain.point(2.0)  // Exactly 2 hours
    * val distance = uncertainSpeed * certainTime
    *   }}}
    *
    * @param value
    *   The constant value to always return
    * @return
    *   Uncertain value that always samples to the given value
    */
  def point[T](value: T): Uncertain[T] = Uncertain(() => value)

  /** Creates a mixture of different uncertain distributions.
    *
    * This is like saying "sometimes use distribution A, sometimes distribution B". Useful for modeling scenarios with
    * multiple modes or populations.
    *
    * @example
    *   Peak vs off-peak hours:
    *   {{{
    * val peakTraffic = Uncertain.normal(100, 15)    // Busy times
    * val offPeakTraffic = Uncertain.normal(30, 8)   // Quiet times
    *
    * val serverLoad = Uncertain.mixture(Map(
    *   peakTraffic -> 0.3,     // 30% of time it's peak hours
    *   offPeakTraffic -> 0.7   // 70% of time it's off-peak
    * ))
    *   }}}
    *
    * @param components
    *   Map of uncertain values to their weights/probabilities
    * @param random
    *   Source of randomness (usually omit this)
    * @return
    *   New uncertain value representing the mixture
    */
  def mixture[T](components: Map[Uncertain[T], Double])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "Need at least one component for a mixture.")
    require(components.values.forall(_ >= 0), "Weights cannot be negative.")
    val totalWeight = components.values.sum
    require(totalWeight > 0, "Weights must sum to something positive.")

    if (components.size == 1) return components.head._1
    categorical(components)(using random).flatMap(identity)
  }

  /** Creates a mixture where all components have equal weight.
    *
    * @example
    *   Equally likely scenarios:
    *   {{{
    * val scenario1 = Uncertain.normal(100, 10)
    * val scenario2 = Uncertain.normal(150, 20)
    * val scenario3 = Uncertain.normal(80, 5)
    *
    * val outcome = Uncertain.equalMixture(List(scenario1, scenario2, scenario3))
    *   }}}
    *
    * @param components
    *   List of uncertain values to mix equally
    * @return
    *   New uncertain value with equal probability of each component
    */
  def equalMixture[T](components: List[Uncertain[T]])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "Need at least one component for equal mixture.")
    mixture(components.map((x: Uncertain[T]) => x -> One).toMap)(using random)
  }

  /** Creates a distribution by sampling from observed data.
    *
    * This is perfect when you have historical data and want to model future values as "probably similar to what we've
    * seen before".
    *
    * @example
    *   User rating prediction:
    *   {{{
    * val pastRatings = List(4, 5, 3, 5, 4, 5, 2, 4, 5, 3)
    * val nextRating = Uncertain.empirical(pastRatings)
    * // Each sample picks randomly from the historical ratings
    *   }}}
    *
    * @param data
    *   Historical data points to sample from (must not be empty)
    * @return
    *   Uncertain value that samples uniformly from the data
    */
  def empirical[T](data: List[T])(using random: Random = new Random()): Uncertain[T] = {
    require(data.nonEmpty, "Need at least one data point for empirical distribution.")
    Uncertain(() => data(random.nextInt(data.length)))
  }

  /** Creates a distribution that chooses from specific outcomes with given probabilities.
    *
    * Perfect for modeling discrete choices like user behavior, weather, or game outcomes.
    *
    * @example
    *   User behavior model:
    *   {{{
    * val userAction = Uncertain.categorical(Map(
    *   "click_button" -> 0.6,
    *   "scroll_down" -> 0.3,
    *   "leave_page" -> 0.1
    * ))
    *   }}}
    *
    * @param outcomes
    *   Map where keys are possible outcomes and values are their probabilities (probabilities will be normalized to sum
    *   to 1.0)
    * @return
    *   Uncertain value that selects outcomes according to the probabilities
    */
  def categorical[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] = {
    require(outcomes.nonEmpty, "Need at least one outcome for categorical distribution")
    require(outcomes.forall(_._2 >= 0), "Probabilities cannot be negative")
    val totalWeight = outcomes.values.sum
    require(totalWeight > 0, "Probabilities must sum to something positive")

    val normalizedOutcomes = outcomes.map((outcome, weight) => (outcome, weight / totalWeight))
    val cumulativeProbs    = normalizedOutcomes.values.scanLeft(Zero)(_ + _).tail
    val paired             = normalizedOutcomes.keys.zip(cumulativeProbs)

    val sampler: () => T = () => {
      val u = random.nextDouble()
      paired.find { case (_, cumProb) => u <= cumProb }.get._1
    }

    Uncertain(sampler)
  }

  /** Creates a normal (bell curve) distribution.
    *
    * This is the most common distribution - useful for measurement errors, natural variations, and many real-world
    * phenomena.
    *
    * @example
    *   Sensor reading with error:
    *   {{{
    * val temperature = Uncertain.normal(72.5, 1.2)  // 72.5°F ± 1.2°F
    * val pressure = Uncertain.normal(1013.25, 5.0)  // 1013.25 mbar ± 5 mbar
    *   }}}
    *
    * @param mean
    *   The center value (average)
    * @param standardDeviation
    *   How spread out the values are (must be ≥ 0)
    * @return
    *   Uncertain value following a normal distribution
    */
  def normal(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(standardDeviation >= 0, "Standard deviation cannot be negative.")
    Uncertain { () =>
      if (standardDeviation == 0) mean
      else {
        // Box-Muller transform for generating normal samples
        var u1 = Zero
        while (u1 == Zero) u1 = random.nextDouble() // Avoid log(0)
        val u2 = random.nextDouble()
        val z0 = sqrt(MinusTwo * log(u1)) * cos(Two * Pi * u2)
        mean + standardDeviation * z0
      }
    }
  }

  /** Creates a uniform distribution (all values equally likely within a range).
    *
    * Useful when you know the bounds but have no reason to expect any particular value within those bounds.
    *
    * @example
    *   Random response time:
    *   {{{
    * val responseTime = Uncertain.uniform(100, 500)  // Between 100-500ms
    * val diceRoll = Uncertain.uniform(1.0, 7.0)      // Between 1-6 (exclusive of 7)
    *   }}}
    *
    * @param min
    *   Lower bound (inclusive)
    * @param max
    *   Upper bound (exclusive, must be ≥ min)
    * @return
    *   Uncertain value uniformly distributed in [min, max)
    */
  def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(max >= min, s"max ($max) must be ≥ min ($min).")
    Uncertain(() => min + random.nextDouble() * (max - min))
  }

  /** Creates an exponential distribution (models time between random events).
    *
    * Common for modeling waiting times, failure rates, or time between arrivals.
    *
    * @example
    *   Time until next customer:
    *   {{{
    * val timeBetweenCustomers = Uncertain.exponential(0.1)  // Average 10 minutes
    * val serverUptime = Uncertain.exponential(0.001)        // Average 1000 hours
    *   }}}
    *
    * @param rate
    *   How frequently events occur (λ, must be > 0) Higher rate = shorter average time between events
    * @return
    *   Uncertain value following exponential distribution
    */
  def exponential(rate: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(rate > 0, "Rate parameter must be positive.")
    Uncertain { () =>
      var u = Zero
      while (u == Zero) u = random.nextDouble() // Avoid log(0)
      -log(u) / rate
    }
  }

  /** Creates a true/false distribution with a given probability of true.
    *
    * Perfect for modeling binary events like success/failure, yes/no decisions, or any two-outcome scenario.
    *
    * @example
    *   User conversion:
    *   {{{
    * val willConvert = Uncertain.bernoulli(0.08)      // 8% conversion rate
    * val coinFlip = Uncertain.bernoulli(0.5)          // Fair coin
    * val biasedCoin = Uncertain.bernoulli(0.6)        // 60% heads
    *   }}}
    *
    * @param probability
    *   Chance of getting true (must be between 0.0 and 1.0)
    * @return
    *   Uncertain Boolean value
    */
  def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] = {
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => random.nextDouble() < probability)
  }

  /** Creates a Kumaraswamy distribution (similar to Beta, but simpler to sample).
    *
    * Useful for modeling percentages, proportions, or any value constrained to [0,1]. Often used in Bayesian statistics
    * and machine learning.
    *
    * @param a
    *   First shape parameter (> 0)
    * @param b
    *   Second shape parameter (> 0)
    * @return
    *   Uncertain value in [0,1] following Kumaraswamy distribution
    */
  def kumaraswamy(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(a > 0 && b > 0, "Kumaraswamy parameters must be positive")
    val reciprocalA = One / a
    val reciprocalB = One / b
    Uncertain { () =>
      val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
      pow(One - pow(One - u, reciprocalB), reciprocalA)
    }
  }

  /** Creates a Rayleigh distribution (models magnitude of 2D random vector).
    *
    * Common in physics (wind speed, wave heights) and signal processing.
    *
    * @param scale
    *   Scale parameter (σ > 0) - affects the spread
    * @return
    *   Uncertain positive value following Rayleigh distribution
    */
  def rayleigh(scale: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(scale > 0, "Rayleigh scale parameter must be positive")
    Uncertain { () =>
      val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
      scale * sqrt(-Two * log(One - u))
    }
  }

  /** Creates a binomial distribution (number of successes in n trials).
    *
    * Perfect for counting successful outcomes when you repeat something multiple times, like the number of users who
    * convert out of 1000 visitors.
    *
    * @example
    *   User conversions:
    *   {{{
    * val conversions = Uncertain.binomial(1000, 0.08)  // Out of 1000 visitors, ~8% convert
    * val coinFlips = Uncertain.binomial(10, 0.5)       // Heads in 10 coin flips
    *   }}}
    *
    * @param trials
    *   Number of attempts (n, must be ≥ 0)
    * @param probability
    *   Chance of success on each trial (must be between 0.0 and 1.0)
    * @return
    *   Uncertain integer representing number of successes
    */
  def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
    require(trials >= 0, "Number of trials cannot be negative.")
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
  }

  /** Creates a Poisson distribution (number of events in a fixed time period).
    *
    * Models count data like number of customers per hour, bugs per release, or emails per day.
    *
    * @example
    *   Server requests:
    *   {{{
    * val requestsPerMinute = Uncertain.poisson(12.5)  // Average 12.5 requests/minute
    * val bugsPerSprint = Uncertain.poisson(3.2)       // Average 3.2 bugs per sprint
    *   }}}
    *
    * @param lambda
    *   Average number of events (λ, must be ≥ 0)
    * @return
    *   Uncertain integer representing event count
    */
  def poisson(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
    require(lambda >= 0, "Lambda (average rate) cannot be negative.")
    if (lambda == Zero) {
      Uncertain.point(0)
    } else {
      Uncertain { () =>
        val L = exp(-lambda)
        var k = 0
        var p = One
        while (p > L) {
          k += 1
          p *= random.nextDouble()
        }
        k - 1
      }
    }
  }
}

// =================================================================================================
// Extensions
// =================================================================================================

/** Equality comparisons for uncertain values. */
extension [T](uncertain: Uncertain[T]) {

  /** Compares two uncertain values sample-by-sample.
    *
    * Returns an uncertain Boolean - sometimes true, sometimes false, depending on the samples drawn from both
    * distributions.
    *
    * @example
    *   Comparing uncertain values:
    *   {{{
    * val speed1 = Uncertain.normal(65, 5)
    * val speed2 = Uncertain.normal(70, 3)
    * val areSame = speed1 === speed2
    * // areSame.sample() is usually false, but occasionally true when samples match
    *   }}}
    */
  def ===(other: Uncertain[T]): Uncertain[Boolean] = for {
    a <- uncertain
    b <- other
  } yield a == b

  /** Sample-wise inequality comparison (opposite of ===). */
  def !==(other: Uncertain[T]): Uncertain[Boolean] = for {
    a <- uncertain
    b <- other
  } yield a != b
}

/** Comparison operations for uncertain values with ordered types. */
extension [T](lhs: Uncertain[T])(using ord: Ordering[T]) {

  // Comparisons with fixed values

  /** True when sample is greater than the fixed value.
    *
    * @example
    *   Speed limit checking:
    *   {{{
    * val speed = Uncertain.normal(65, 5)
    * val isSpeeding = speed.gt(60)  // Sometimes true, sometimes false
    *   }}}
    */
  def gt(value: T): Uncertain[Boolean] = lhs.map(a => ord.gt(a, value))

  /** True when sample is less than the fixed value. */
  def lt(value: T): Uncertain[Boolean] = lhs.map(a => ord.lt(a, value))

  /** True when sample is greater than or equal to the fixed value. */
  def gte(value: T): Uncertain[Boolean] = lhs.map(a => ord.gteq(a, value))

  /** True when sample is less than or equal to the fixed value. */
  def lte(value: T): Uncertain[Boolean] = lhs.map(a => ord.lteq(a, value))

  // Symbolic operators (same as above)
  def >(value: T): Uncertain[Boolean]  = gt(value)
  def <(value: T): Uncertain[Boolean]  = lt(value)
  def >=(value: T): Uncertain[Boolean] = gte(value)
  def <=(value: T): Uncertain[Boolean] = lte(value)

  // Comparisons between two uncertain values

  /** Compares two uncertain values sample-by-sample.
    *
    * @example
    *   Comparing uncertain measurements:
    *   {{{
    * val measurement1 = Uncertain.normal(10.2, 0.5)
    * val measurement2 = Uncertain.normal(10.1, 0.3)
    * val firstIsLarger = measurement1.gt(measurement2)
    *   }}}
    */
  def gt(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.gt(lhsSample, rhsSample)

  def lt(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.lt(lhsSample, rhsSample)

  def gte(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.gteq(lhsSample, rhsSample)

  def lte(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.lteq(lhsSample, rhsSample)

  // Symbolic operators for uncertain-uncertain comparisons
  def >(other: Uncertain[T]): Uncertain[Boolean]  = gt(other)
  def <(other: Uncertain[T]): Uncertain[Boolean]  = lt(other)
  def >=(other: Uncertain[T]): Uncertain[Boolean] = gte(other)
  def <=(other: Uncertain[T]): Uncertain[Boolean] = lte(other)
}

/** Division operations for uncertain values with fractional types. */
extension [T](lhs: Uncertain[T])(using frac: Fractional[T]) {

  /** Divides uncertain value by a fixed value.
    *
    * @example
    *   Converting units:
    *   {{{
    * val meters = Uncertain.normal(1000, 50)
    * val kilometers = meters / 1000.0  // Convert to km
    *   }}}
    */
  def /(rhs: T): Uncertain[T] = lhs.map(a => frac.div(a, rhs))

  /** Divides two uncertain values sample-by-sample.
    *
    * @example
    *   Calculating rate:
    *   {{{
    * val distance = Uncertain.normal(120, 10)  // miles
    * val time = Uncertain.normal(2.0, 0.2)     // hours
    * val speed = distance / time               // mph with propagated uncertainty
    *   }}}
    */
  def /(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield frac.div(lhsSample, rhsSample)
}

/** Boolean operations and statistical testing for uncertain Boolean values. */
extension (lhs: Uncertain[Boolean]) {

  /** Logical NOT operation. */
  def unary_! : Uncertain[Boolean] = lhs.map(!_)

  /** Logical AND between two uncertain booleans. */
  def &&(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield lhsSample && rhsSample

  /** Logical OR between two uncertain booleans. */
  def ||(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield lhsSample || rhsSample

  /** Tests if the probability of this being true exceeds a threshold.
    *
    * This uses smart statistical testing (Sequential Probability Ratio Test) to determine if we can be confident that
    * P(true) > threshold. It automatically uses the right number of samples - no need to guess.
    *
    * @example
    *   A/B test analysis:
    *   {{{
    * val testIsBetter = testConversion.gt(controlConversion)
    * if (testIsBetter.probability(exceeds = 0.95)) {
    *   println("We're 95%+ confident the test is better")
    * }
    *   }}}
    *
    * @example
    *   Risk assessment:
    *   {{{
    * val serverLoad = Uncertain.normal(0.7, 0.15)
    * val overloaded = serverLoad.gt(0.9)
    * if (overloaded.probability(exceeds = 0.1)) {
    *   println("More than 10% chance of overload - add capacity")
    * }
    *   }}}
    *
    * @param exceeds
    *   Probability threshold to test against (0.0 to 1.0)
    * @param alpha
    *   Type I error rate - chance of false positive (default 5%)
    * @param beta
    *   Type II error rate - chance of false negative (default 5%)
    * @param delta
    *   Effect size - how much above threshold to detect (auto-calculated if not specified)
    * @param maxSamples
    *   Safety limit on sampling (default 10,000)
    * @return
    *   true if we're confident P(true) > threshold, false otherwise
    */
  def probability(
    exceeds: Double,
    alpha: Double = 0.05,
    beta: Double = 0.05,
    delta: Option[Double] = None,
    maxSamples: Int = 10_000
  ): Boolean = {
    require(exceeds >= 0 && exceeds <= 1, s"Threshold ($exceeds) must be between 0 and 1.")
    require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1.")
    require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1.")
    require(maxSamples > 0, "Max samples must be positive.")

    val effectSize = delta.getOrElse(math.max(0.01, 0.1 * (1.0 - exceeds)))
    require(exceeds + effectSize <= 1.0, s"Threshold + effect size too large: ${exceeds + effectSize}")

    val result = evaluateHypothesis(exceeds, alpha, beta, effectSize, maxSamples)
    result.decision
  }

  /** Shorthand for testing if something is "more likely than not" (> 50% chance).
    *
    * @example
    *   Quick decision making:
    *   {{{
    * val willSucceed = Uncertain.bernoulli(0.7)
    * if (willSucceed.isProbable()) {
    *   println("Go for it!")  // 70% > 50%, so this will print
    * }
    *   }}}
    */
  def isProbable(alpha: Double = 0.05, beta: Double = 0.05): Boolean =
    probability(0.5, alpha, beta)

  /** Internal: Performs Sequential Probability Ratio Test for hypothesis testing.
    *
    * Tests H₁: p > threshold vs H₀: p ≤ threshold using statistical sampling. Uses smart stopping rules to minimize
    * required samples while maintaining specified error rates.
    */
  def evaluateHypothesis(
    threshold: Double,
    alpha: Double,
    beta: Double,
    delta: Double,
    maxSamples: Int
  ): HypothesisResult = {
    require(threshold >= 0 && threshold <= 1, s"Threshold ($threshold) must be between 0 and 1.")
    require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1.")
    require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1.")
    require(delta > 0, s"Effect size delta ($delta) must be positive.")
    require(threshold + delta <= 1.0, s"Threshold + delta (${threshold + delta}) must be ≤ 1.0")
    require(maxSamples > 0, "Maximum samples must be positive.")

    val p0 = threshold
    val p1 = threshold + delta

    // SPRT decision boundaries
    val A = log(beta / (1.0 - alpha))
    val B = log((1.0 - beta) / alpha)

    var successes = 0
    var samples   = 0

    while (samples < maxSamples) {
      val sample = lhs.sample()
      if (sample) successes += 1
      samples += 1

      val x = successes
      val n = samples

      val llr = if (p0 > 0 && p0 < 1 && p1 > 0 && p1 < 1) {
        x * log(p1 / p0) + (n - x) * log((1.0 - p1) / (1.0 - p0))
      } else {
        // Handle boundary cases
        if (p0 == 0.0) {
          if (x > 0) Double.PositiveInfinity else (n - x) * log(1.0 - p1)
        } else if (p0 == 1.0) {
          if (x < n) Double.NegativeInfinity else x * log(p1)
        } else if (p1 == 0.0) {
          if (x > 0) Double.NegativeInfinity else (n - x) * log(1.0 - p0)
        } else {
          if (x < n) Double.PositiveInfinity else x * log(1.0 / p0)
        }
      }

      if (llr <= A) {
        val estimatedP = successes.toDouble / samples
        return HypothesisResult(false, estimatedP, 1.0 - alpha, samples)
      } else if (llr >= B) {
        val estimatedP = successes.toDouble / samples
        return HypothesisResult(true, estimatedP, 1.0 - alpha, samples)
      }
    }

    // Fallback: normal approximation test
    val pHat = successes.toDouble / samples
    val se   = sqrt(pHat * (1.0 - pHat) / samples)

    if (se > 0) {
      val z         = (pHat - threshold) / se
      val criticalZ = approximateNormalQuantile(1.0 - alpha)
      val decision  = z > criticalZ
      HypothesisResult(decision, pHat, 1.0 - alpha, samples)
    } else {
      HypothesisResult(pHat > threshold, pHat, 1.0 - alpha, samples)
    }
  }

  /** Internal: Approximates standard normal quantiles using Beasley-Springer-Moro algorithm. */
  private def approximateNormalQuantile(p: Double): Double = {
    require(p > 0 && p < 1, "Probability must be between 0 and 1")

    if (p < 0.5) {
      -approximateNormalQuantile(1.0 - p)
    } else {
      val t  = sqrt(-2.0 * log(1.0 - p))
      val c0 = 2.515517
      val c1 = 0.802853
      val c2 = 0.010328
      val d1 = 1.432788
      val d2 = 0.189269
      val d3 = 0.001308

      t - (c0 + c1 * t + c2 * t * t) / (1.0 + d1 * t + d2 * t * t + d3 * t * t * t)
    }
  }
}

/** Summary statistics and analysis methods for uncertain values. */
extension [T](uncertain: Uncertain[T]) {

  /** Finds the most common sample value (best for discrete distributions).
    *
    * For continuous distributions like normal or uniform, this probably won't be useful since each sample is likely to
    * be unique.
    *
    * @example
    *   Finding most common user action:
    *   {{{
    * val userAction = Uncertain.categorical(Map("click" -> 0.6, "scroll" -> 0.4))
    * println(userAction.mode())  // Probably "click"
    *   }}}
    *
    * @param sampleCount
    *   Number of samples to analyze (default 1000)
    * @return
    *   The most frequently sampled value
    */
  def mode(sampleCount: Int = 1000): T = {
    require(sampleCount > 0, "Sample count must be positive.")
    uncertain
      .take(sampleCount)
      .groupBy(identity)
      .view
      .maxBy((_, elems) => elems.length)
      ._1
  }

  /** Creates a frequency count of all sample values.
    *
    * @example
    *   Understanding distribution shape:
    *   {{{
    * val rating = Uncertain.categorical(Map(1 -> 0.1, 2 -> 0.2, 3 -> 0.4, 4 -> 0.2, 5 -> 0.1))
    * val frequencies = rating.histogram(1000)
    * // frequencies might be: Map(1 -> 98, 2 -> 203, 3 -> 401, 4 -> 194, 5 -> 104)
    *   }}}
    *
    * @param sampleCount
    *   Number of samples to analyze
    * @return
    *   Map from sample values to their frequencies
    */
  def histogram(sampleCount: Int = 1000): Map[T, Int] = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples = uncertain.take(sampleCount)
    samples.groupBy(identity).view.mapValues(_.length).toMap
  }

  /** Estimates the information entropy (randomness) of the distribution.
    *
    * Higher entropy means more unpredictable. A coin flip has high entropy, while a heavily biased coin has low
    * entropy.
    *
    * @param sampleCount
    *   Number of samples to use for estimation
    * @return
    *   Estimated entropy in bits
    */
  def entropy(sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples = uncertain.take(sampleCount)
    val counts  = samples.groupBy(identity).values.map(_.length)
    val total   = samples.length.toDouble
    counts.foldLeft(Zero) { (acc, count) =>
      val p = count / total
      acc - (if (p > 0) p * log(p) / log(2) else 0)
    }
  }
}

/** Internal: Enables Boolean values to be used in numeric operations.
  *
  * Maps true → 1, false → 0 for statistical calculations like mean(). Note: This breaks normal arithmetic laws - use
  * only for statistics!
  */
private[this] given whiteLieBooleanNumeric: Numeric[Boolean] = new Numeric[Boolean] {
  override def compare(x: Boolean, y: Boolean): Int      = x.compareTo(y)
  override def fromInt(x: Int): Boolean                  = x != 0
  override def minus(x: Boolean, y: Boolean): Boolean    = x ^ y  // XOR
  override def negate(x: Boolean): Boolean               = !x
  override def plus(x: Boolean, y: Boolean): Boolean     = x || y // OR
  override def times(x: Boolean, y: Boolean): Boolean    = x && y // AND
  override def toDouble(x: Boolean): Double              = if (x) One else Zero
  override def toFloat(x: Boolean): Float                = if (x) 1.0f else 0.0f
  override def toInt(x: Boolean): Int                    = if (x) 1 else 0
  override def toLong(x: Boolean): Long                  = if (x) 1L else 0L
  override def parseString(str: String): Option[Boolean] = str.toBooleanOption
}

/** Arithmetic operations and statistics for uncertain numeric values. */
extension [T](lhs: Uncertain[T])(using num: Numeric[T]) {

  /** Estimates the average (expected) value by sampling.
    *
    * @example
    *   Expected server load:
    *   {{{
    * val load = Uncertain.normal(0.65, 0.1)
    * val avgLoad = load.expectedValue()  // Should be close to 0.65
    *   }}}
    *
    * @param sampleCount
    *   Number of samples to average (default 1000)
    * @return
    *   Estimated expected value as a Double
    */
  def expectedValue(sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples = lhs.take(sampleCount).map(num.toDouble)
    samples.sum / samples.length.toDouble
  }

  /** Alias for expectedValue - same calculation. */
  def mean(sampleCount: Int = 1000): Double = expectedValue(sampleCount)

  /** Estimates population standard deviation (how spread out the values are).
    *
    * Use this when your samples represent the entire population you care about. For estimating the spread of a larger
    * population from a sample, use standardDeviation().
    *
    * @param sampleCount
    *   Number of samples to analyze
    * @return
    *   Estimated population standard deviation
    */
  def populationStandardDeviation(sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples  = lhs.take(sampleCount).map(num.toDouble)
    val meanVal  = samples.sum / samples.length
    val variance = samples.foldLeft(Zero) { (acc, sample) =>
      val diff = sample - meanVal
      acc + diff * diff
    } / samples.length

    sqrt(variance)
  }

  /** Estimates sample standard deviation with Bessel's correction.
    *
    * Use this when estimating the spread of a larger population from your samples. This applies a correction (n-1
    * instead of n) that removes bias.
    *
    * @param sampleCount
    *   Number of samples to analyze (must be ≥ 2)
    * @return
    *   Estimated sample standard deviation
    */
  def standardDeviation(sampleCount: Int = 1000): Double = {
    require(sampleCount >= 2, "Need at least 2 samples for sample standard deviation.")
    val samples  = lhs.take(sampleCount).map(num.toDouble)
    val meanVal  = samples.sum / samples.length
    val variance = samples.foldLeft(Zero) { (acc, sample) =>
      val diff = sample - meanVal
      acc + diff * diff
    } / (samples.length - 1)

    sqrt(variance)
  }

  // Arithmetic operations between uncertain values

  /** Adds two uncertain values sample-by-sample.
    *
    * @example
    *   Total response time:
    *   {{{
    * val networkDelay = Uncertain.normal(50, 10)   // ms
    * val processingTime = Uncertain.normal(200, 30) // ms
    * val totalTime = networkDelay + processingTime  // Combined uncertainty
    *   }}}
    */
  def +(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield num.plus(lhsSample, rhsSample)

  /** Subtracts two uncertain values sample-by-sample.
    *
    * @example
    *   Performance improvement:
    *   {{{
    * val oldResponseTime = Uncertain.normal(300, 50)
    * val newResponseTime = Uncertain.normal(250, 40)
    * val improvement = oldResponseTime - newResponseTime
    *   }}}
    */
  def -(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield num.minus(lhsSample, rhsSample)

  /** Multiplies two uncertain values sample-by-sample.
    *
    * @example
    *   Area calculation:
    *   {{{
    * val length = Uncertain.normal(10.0, 0.5)
    * val width = Uncertain.normal(8.0, 0.3)
    * val area = length * width  // Uncertainty propagates automatically
    *   }}}
    */
  def *(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield num.times(lhsSample, rhsSample)

  // Arithmetic operations with fixed values

  /** Adds a constant to an uncertain value. */
  def +(rhs: T): Uncertain[T] = lhs.map(l => num.plus(l, rhs))

  /** Subtracts a constant from an uncertain value. */
  def -(rhs: T): Uncertain[T] = lhs.map(l => num.minus(l, rhs))

  /** Multiplies an uncertain value by a constant. */
  def *(rhs: T): Uncertain[T] = lhs.map(l => num.times(l, rhs))
}

/** Order-based statistics for uncertain values with comparable types. */
extension [T](uncertain: Uncertain[T])(using ord: Ordering[T]) {

  /** Estimates a confidence interval using sample percentiles.
    *
    * @example
    *   Understanding the range:
    *   {{{
    * val responseTime = Uncertain.normal(200, 50)
    * val (low, high) = responseTime.confidenceInterval(0.95)
    * println(s"95% of response times are between $low and $high ms")
    *   }}}
    *
    * @param confidence
    *   Confidence level (e.g., 0.95 for 95% CI)
    * @param sampleCount
    *   Number of samples to use for estimation
    * @return
    *   Tuple of (lower bound, upper bound)
    */
  def confidenceInterval(confidence: Double = 0.95, sampleCount: Int = 1000): (T, T) = {
    require(confidence > 0 && confidence < 1, "Confidence must be between 0 and 1.")
    require(sampleCount > 0, "Sample count must be positive.")
    val samples    = uncertain.take(sampleCount).sorted
    val alpha      = One - confidence
    val lowerIndex = ((alpha / Two) * samples.length).toInt
    val upperIndex = ((One - alpha / Two) * samples.length).toInt - 1
    val safeLower  = math.max(0, math.min(lowerIndex, samples.length - 1))
    val safeUpper  = math.max(0, math.min(upperIndex, samples.length - 1))
    (samples(safeLower), samples(safeUpper))
  }

  /** Estimates the Cumulative Distribution Function - P(X ≤ value).
    *
    * This tells you what fraction of samples fall at or below a given value.
    *
    * @example
    *   Response time SLA:
    *   {{{
    * val responseTime = Uncertain.normal(200, 50)
    * val withinSLA = responseTime.cdf(300)  // Fraction of requests under 300ms
    * println(s"${withinSLA * 100}% of requests meet SLA")
    *   }}}
    *
    * @param value
    *   The threshold value to evaluate
    * @param sampleCount
    *   Number of samples for estimation
    * @return
    *   Estimated probability P(X ≤ value)
    */
  def cdf(value: T, sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples   = uncertain.take(sampleCount)
    val successes = samples.count(ord.lteq(_, value))
    successes.toDouble / sampleCount
  }
}

// =================================================================================================
// Helper Classes
// =================================================================================================

/** Result from a statistical hypothesis test.
  *
  * @param decision
  *   true if we accept H₁ (effect exists), false if we accept H₀ (no effect)
  * @param probability
  *   Estimated probability based on samples
  * @param confidenceLevel
  *   Confidence level used (e.g., 0.95 for 95%)
  * @param samplesUsed
  *   Number of samples needed to reach decision
  */
final case class HypothesisResult(
  decision: Boolean,
  probability: Double,
  confidenceLevel: Double,
  samplesUsed: Int
)

/** Internal: Context for preserving correlation during sampling.
  *
  * This ensures that when you use the same uncertain value multiple times (like in x - x), it gets the same sample
  * value each time within one evaluation.
  */
final class SampleContext {
  private val memoizedValues: mutable.Map[UUID, Any] = mutable.Map.empty
  def getValue[T](id: UUID): Option[T]               = memoizedValues.get(id).map(_.asInstanceOf[T])
  def setValue[T](id: UUID, value: T): Unit          = memoizedValues(id) = value
}
