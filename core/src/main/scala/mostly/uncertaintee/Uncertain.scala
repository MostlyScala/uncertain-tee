package mostly.uncertaintee

import mostly.uncertaintee.internal.*

import java.util.UUID
import scala.math.*
import scala.util.Random

/** A type for working with uncertain data - values that have some randomness or measurement error.
  *
  * Instead of working with single values like `speed = 65.0`, you can work with uncertain values like
  * `speed = "somewhere between 60-70 mph"` and let the library handle the math correctly.
  *
  * @example
  *   Basic usage:
  *   {{{
  *     import mostly.uncertaintee.Uncertain
  *    import mostly.uncertaintee.syntax.*
  *
  *    // Create uncertain speed with measurement error
  *    val speed    = Uncertain.normal(65.0, 5.0)  // 65 mph ± 5 mph
  *    val speedKph = speed.map(_ * 1.609344)      // kph
  *
  *    // Check if we're _probably_ speeding (limit is 60 kph)
  *    if ((speed > 60).isProbable()) {
  *       println("You're probably speeding")
  *    }
  *    // Check if we're 95% confident that you're speeding
  *    if ((speed > 60).probability(exceeds = 0.95)) {
  *       println("You're probably speeding")
  *    }
  *   }}}
  *
  * @tparam T
  *   The type of the uncertain value (Double, Int, Boolean, String, etc.)
  */
sealed abstract class Uncertain[T] {

  /** The function that generates a single random sample from the distribution. */
  private[uncertaintee] val sampler: () => T

  /** Internal, computation graph (/tree) - handles lazy evaluation and correlation preservation. */
  private[uncertaintee] val computationTree: ComputationTree[T]

  /** Gets one sample from this uncertain value.
    *
    * Each call returns a different random sample from the distribution. If this uncertain value is built from other
    * uncertain values, their samples are coordinated to preserve correlations.
    *
    * '''Performance Note:''' Each call performs fresh sampling from the underlying distribution(s). For repeated
    * sampling, consider using `take(n)` or `iterator` which may be more efficient.
    *
    * @example
    *   Correlation preservation in action:
    *   {{{
    *   val x = Uncertain.normal(0, 1)
    *   val y = x.map(_ * 2)  // y is always exactly 2 * x
    *
    *   // These will always have the relationship y = 2 * x
    *   val xSample = x.sample()
    *   val ySample = y.sample()  // This will be exactly 2 * xSample
    *
    *   // Multiple samples from the same expression maintain correlation
    *   val difference = for {
    *     a <- x
    *     b <- x  // Same uncertain value
    *   } yield a - b
    *
    *   difference.sample()  // Always returns 0.0 due to correlation
    *   }}}
    *
    * @return
    *   A single sample of type T drawn from this uncertain value's distribution
    */
  def sample(): T = computationTree.evaluate()

  /** Transforms each sample using a function.
    *
    * This is like calling `list.map(f)` but for probability distributions. The uncertainty structure is preserved.
    *
    * @param f
    *   Function to apply to each sample
    * @return
    *   New uncertain value with the function applied to each sample
    */
  def map[U](f: T => U): Uncertain[U] = {
    this.flatMap(value => Uncertain(() => f(value)))
  }

  /** Chains uncertain computations together (monadic bind operation).
    *
    * Use this when the next uncertain value depends on the current sample. This is essential for building complex
    * probabilistic models where later distributions depend on earlier samples.
    *
    * '''Performance Note:''' Creates nested computation trees that are evaluated lazily. Deep nesting may impact
    * performance for very complex models.
    *
    * @example
    *   Temperature-dependent clothing choice:
    *   {{{
    *   val temperature = Uncertain.normal(20, 5)  // degrees Celsius
    *
    *   val clothingChoice = temperature.flatMap { temp =>
    *     if (temp < 10) Uncertain.categorical(Map("coat" -> 0.8, "sweater" -> 0.2))
    *     else if (temp < 25) Uncertain.categorical(Map("sweater" -> 0.6, "shirt" -> 0.4))
    *     else Uncertain.categorical(Map("shirt" -> 0.7, "t-shirt" -> 0.3))
    *   }
    *
    *   clothingChoice.sample()  // Choice depends on the temperature sample
    *   }}}
    *
    * @example
    *   Correlated sequential decisions:
    *   {{{
    *   val firstRoll = Uncertain.uniform(1, 7).map(_.toInt)
    *
    *   val gameContinuation = firstRoll.flatMap { roll1 =>
    *     if (roll1 >= 4) {
    *       // Good first roll, take another risk
    *       Uncertain.uniform(1, 7).map(roll2 => roll1 + roll2.toInt)
    *     } else {
    *       // Bad first roll, play it safe
    *       Uncertain.point(roll1)
    *     }
    *   }
    *   }}}
    *
    * @param f
    *   Function that takes a sample and returns a new uncertain value
    * @return
    *   New uncertain value representing the chained computation
    */
  def flatMap[U](f: T => Uncertain[U]): Uncertain[U] = {
    val newNode = ComputationFlatMapping(this.computationTree, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Returns an endless iterator of samples.
    *
    * '''Performance Notes:'''
    *   - Each iteration performs fresh sampling - no caching
    *   - More memory efficient than `take(n)` for large sample sizes
    *   - Suitable for streaming applications or when you don't know sample size in advance
    *
    * @example
    *   Streaming simulation:
    *   {{{
    *   val coinFlip = Uncertain.bernoulli(0.5)
    *
    *   // Count heads in first 100 flips that come up heads
    *   val headsCount = coinFlip.iterator
    *     .take(100)
    *     .count(identity)
    *   }}}
    *
    * @return
    *   An infinite iterator that produces fresh samples on each call to `next()`
    */
  def iterator: Iterator[T] = Iterator.continually(sample())

  /** Collects multiple samples into a list.
    *
    * '''Performance Notes:'''
    *   - Performs `n` fresh samples and stores them in memory
    *   - For large `n`, consider using `iterator` to avoid memory pressure
    *   - For statistical analysis, typically 1000-10000 samples provide good estimates
    *
    * @example
    *   Monte Carlo estimation:
    *   {{{
    *   val estimation = Uncertain.normal(100, 15)
    *   val samples = estimation.take(1000)
    *   val mean = samples.sum / samples.length
    *   val variance = samples.map(x => (x - mean) * (x - mean)).sum / samples.length
    *   }}}
    *
    * @param n
    *   Number of samples to collect (must be non-negative)
    * @return
    *   List containing n random samples from this uncertain value
    */
  def take(n: Int): List[T] = {
    require(n >= 0, "Number of samples must be non-negative.")
    iterator.take(n).toList
  }

  /** Filters the uncertain value by applying a predicate while preserving correlation.
    *
    * <strong>Q:</strong> Why does this return `Uncertain[Option[T]]` instead of `Uncertain[T]`?
    *
    * <strong>A:</strong> This library's core guarantee is correlation preservation: multiple references to the same
    * uncertain value always yield the same sample. Using rejection sampling (resampling until the predicate passes)
    * would break this fundamental property.
    *
    * Consider what would happen with rejection sampling:
    * {{{
    *   val x = Uncertain.normal(0, 1)
    *   val positive = x.rejectionFilter(_ > 0)  // hypothetical broken version
    *
    *   // These would be DIFFERENT values, breaking correlation:
    *   val pair = (positive.sample(), positive.sample())  // e.g. (0.5, 1.8)
    *
    *   // Even worse in computations:
    *   val shouldBeZero = for {
    *     a <- positive
    *     b <- positive
    *   } yield a - b  // Would NOT be zero!
    * }}}
    *
    * Instead, this method transforms each sample deterministically:
    *   - If the sample passes the predicate → `Some(value)`
    *   - If the sample fails the predicate → `None`
    *   - '''Same input always produces the same output'''
    *
    * @example
    *   Correlation is maintained with the Option-based approach:
    *   {{{
    *   val uncertainInt = Uncertain.uniform(-10, 11).map(_.toInt)
    *   val maybePositive: Uncertain[Option[Int]] = uncertainInt.filter(_ > 0)
    *
    *   // Because correlation is preserved, this will always be consistent:
    *   val correlatedResult = for {
    *     a <- maybePositive
    *     b <- maybePositive
    *   } yield (a, b)
    *
    *   // Always returns (Some(x), Some(x)) or (None, None)
    *   // Never returns (Some(5), Some(2)) or (Some(3), None)
    *   correlatedResult.sample()
    *   }}}
    * @example
    *   Handle filtered results explicitly:
    *   {{{
    *   val speed = Uncertain.normal(65, 10)
    *   val validSpeed = speed.filter(s => s > 0 && s < 200)
    *
    *   val processed = validSpeed.flatMap {
    *     case Some(s) => computeEnergyFromSpeed(s)
    *     case None    => Uncertain.point(0.0)  // default for invalid speeds
    *   }
    *   }}}
    * @param keepWhen
    *   The function to test each sample. Returns `true` to keep the value (in a `Some`), `false` to discard it
    *   (resulting in `None`).
    * @return
    *   A new `Uncertain[Option[T]]` that contains `Some(value)` if the predicate passed for that sample, and `None` if
    *   the predicate failed. Correlation with the original uncertain value is preserved.
    */
  def filter(keepWhen: T => Boolean): Uncertain[Option[T]] =
    this.map {
      case t if keepWhen(t) => Some(t)
      case _                => None
    }

}

/** Factory for creating uncertain values from various distributions and data sources. */
object Uncertain {

  /** Creates an uncertain value from any sampling function.
    *
    * @param sampler
    *   Function that produces a new sample each time it's called
    * @param random
    *   Random number generator to use (defaults to a new Random instance)
    * @return
    *   New uncertain value that calls the sampler function for each sample
    */
  def apply[T](sampler: () => T)(using random: Random = new Random()): Uncertain[T] = {
    val id = UUID.nameUUIDFromBytes(random.nextBytes(16))
    val s  = sampler
    new Uncertain[T] {
      override val sampler: () => T                    = s
      override val computationTree: ComputationTree[T] = ComputationLeaf(id = id, sampler = s)
    }
  }

  /** Internal constructor with pre-defined computation. */
  private[uncertaintee] def apply[T](sampler: () => T, underlying: ComputationTree[T]): Uncertain[T] = {
    val s = sampler
    new Uncertain[T] {
      override val sampler: () => T                    = s
      override val computationTree: ComputationTree[T] = underlying
    }
  }

  /** Creates an uncertain value that's always the same (no uncertainty).
    *
    * @param value
    *   The constant value to always return
    * @return
    *   An uncertain value that always samples to the given value
    */
  def point[T](value: T): Uncertain[T] = Uncertain(() => value)

  /** Converts a `List[Uncertain[T]]` (or another iterable) into a single `Uncertain[List[T]]`.
    *
    * This is useful for aggregating a collection of uncertain values into a single uncertain result.
    *
    * @example
    *   Correlation preservation across multiple uncertain values:
    *   {{{
    *   val temp1 = Uncertain.normal(20, 2)
    *   val temp2 = temp1.map(_ + 5)  // Always 5 degrees warmer
    *   val temp3 = temp1.map(_ - 3)  // Always 3 degrees cooler
    *
    *   val allTemps = Uncertain.sequence(List(temp1, temp2, temp3))
    *
    *   // The relationships are preserved in every sample
    *   val sample = allTemps.sample() // e.g., List(18.5, 23.5, 15.5)
    *   // Notice: temp2 = temp1 + 5, temp3 = temp1 - 3
    *   }}}
    *
    * {{{
    * import mostly.uncertaintee.Uncertain
    * import mostly.uncertaintee.syntax.*
    *
    * // Model three independent dice rolls
    * val dieRollD6 = Uncertain.uniform(1, 7).map(_.toInt)
    * val dieRoll12 = Uncertain.uniform(1, 13).map(_.toInt)
    * val dieRollD20 = Uncertain.uniform(1, 21).map(_.toInt)
    *
    * // Sequence them into a single Uncertain List
    * val uncertainSum: Uncertain[Int] = Uncertain.sequence(
    *   List(dieRollD20, dieRoll12, dieRollD6)
    * ).map(_.sum)
    *
    * // Now you can analyze the distribution of the sum of three dice rolls
    * println(s"Expected sum of three dice rolls: ${uncertainSum.mean()}")
    * }}}
    *
    * @param uncertainTs
    *   a sequence of `Uncertain` values.
    * @return
    *   A single `Uncertain` that, when sampled, produces a list of samples — one from each of the input `Uncertain`
    *   instances, preserving all correlations between them.
    */
  def sequence[T](uncertainTs: Iterable[Uncertain[T]]): Uncertain[List[T]] =
    uncertainTs.foldRight(Uncertain.point(List.empty[T])) { (elem, acc) =>
      for {
        h <- elem
        t <- acc
      } yield h :: t
    }

  /** Applies a function to each element in a collection, where the function returns an `Uncertain` value, then
    * sequences the results into a single `Uncertain` collection.
    *
    * @example
    *   Correlation in dependent processing:
    *   {{{
    *   val baseEfficiency = Uncertain.normal(0.8, 0.1)
    *
    *   def processTask(taskComplexity: Double): Uncertain[Double] =
    *     baseEfficiency.map(eff => taskComplexity / eff)  // Same efficiency for all tasks
    *
    *   val complexities = List(1.0, 2.5, 1.8)
    *   val processingTimes = Uncertain.traverse(complexities)(processTask)
    *
    *   // All processing times are correlated through baseEfficiency
    *   val sample = processingTimes.sample()
    *   // If efficiency is low, ALL tasks take longer proportionally
    *   }}}
    *
    * {{{
    * // Assume we have a function that models the uncertain time to process a task
    * def processTask(taskId: Int): Uncertain[Double] =
    * Uncertain.normal(mean = taskId * 1.5, standardDeviation = 0.5)
    *
    * val taskIds = List(1, 2, 3)
    *
    * // Use traverse to get the distribution of total processing time
    * val totalTime: Uncertain[Double] = Uncertain.traverse(taskIds)(processTask).map(_.sum)
    *
    * println(s"Expected total processing time: ${totalTime.mean()}")
    * }}}
    *
    * @param items
    *   The collection of values to map over.
    * @param toUncertain
    *   The function to apply to each element, which returns an `Uncertain` value.
    * @return
    *   An `Uncertain` value containing the collection of results, preserving correlations.
    */
  def traverse[A, T](items: Iterable[A])(toUncertain: A => Uncertain[T]): Uncertain[List[T]] =
    items.foldRight(Uncertain.point(List.empty[T])) { (elem, acc) =>
      for {
        h <- toUncertain(elem)
        t <- acc
      } yield h :: t
    }

  /** Creates a mixture of different uncertain distributions.
    *
    * @param components
    *   Map from uncertain values to their weights in the mixture
    * @param random
    *   Random number generator to use for mixture selection
    * @return
    *   An uncertain value that samples from the mixture distribution
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
    * @param components
    *   List of uncertain values to mix with equal probability
    * @param random
    *   Random number generator to use for mixture selection
    * @return
    *   An uncertain value that samples uniformly from the given components
    */
  def equalMixture[T](components: List[Uncertain[T]])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "Need at least one component for equal mixture.")
    mixture(components.map((x: Uncertain[T]) => x -> One).toMap)(using random)
  }

  /** Creates a distribution by sampling from observed data.
    *
    * @param data
    *   List of observed values to sample from
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain value that samples uniformly from the given data points
    */
  def empirical[T](data: List[T])(using random: Random = new Random()): Uncertain[T] = {
    require(data.nonEmpty, "Need at least one data point for empirical distribution.")
    Uncertain(() => data(random.nextInt(data.length)))
  }

  /** Creates a distribution that chooses from specific outcomes with given probabilities.
    *
    * @param outcomes
    *   Map from outcome values to their relative probabilities/weights
    * @param random
    *   Random number generator to use for outcome selection
    * @return
    *   An uncertain value that samples according to the specified probability distribution
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
    * @param mean
    *   The center/average value of the distribution
    * @param standardDeviation
    *   The spread of the distribution (must be non-negative)
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain value following a normal distribution with the specified parameters
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
    * @param min
    *   The minimum value (inclusive)
    * @param max
    *   The maximum value (exclusive)
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain value uniformly distributed between min and max
    */
  def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(max >= min, s"max ($max) must be ≥ min ($min).")
    Uncertain(() => min + random.nextDouble() * (max - min))
  }

  /** Creates an exponential distribution (models time between random events).
    *
    * @param rate
    *   The rate parameter (λ) - higher values mean events happen more frequently
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain value following an exponential distribution with the specified rate
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
    * @param probability
    *   The probability of sampling `true` (must be between 0 and 1)
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain Boolean value that is `true` with the specified probability
    */
  def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] = {
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => random.nextDouble() < probability)
  }

  /** Creates a Kumaraswamy distribution (similar to Beta, but simpler to sample).
    *
    * @param a
    *   First shape parameter (must be positive)
    * @param b
    *   Second shape parameter (must be positive)
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain value following a Kumaraswamy distribution on the interval [0,1]
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
    * @param scale
    *   The scale parameter (σ) - must be positive
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain value following a Rayleigh distribution with the specified scale
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
    * @param trials
    *   Number of independent trials (must be non-negative)
    * @param probability
    *   Probability of success on each trial (must be between 0 and 1)
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain integer representing the number of successes in the trials
    */
  def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
    require(trials >= 0, "Number of trials cannot be negative.")
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
  }

  /** Creates a Poisson distribution (number of events in a fixed time period).
    *
    * @param lambda
    *   The average rate of events (λ) - must be non-negative
    * @param random
    *   Random number generator to use for sampling
    * @return
    *   An uncertain integer representing the number of events in the time period
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
