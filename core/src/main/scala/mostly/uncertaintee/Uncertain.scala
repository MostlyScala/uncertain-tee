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
  * // To use operators like +, -, >, etc.
  * import mostly.uncertaintee.syntax.all.*
  *
  * // Create uncertain speed with measurement error
  * val speed = Uncertain.normal(65.0, 5.0)  // 65 mph ± 5 mph
  *
  * // Check if we're probably speeding (limit is 60 mph)
  * if ((speed > 60).isProbable()) {
  * println("You're probably speeding")
  * }
  *   }}}
  *
  * @tparam T
  *   The type of the uncertain value (Double, Int, Boolean, String, etc.)
  */
sealed abstract class Uncertain[T] {

  /** The function that generates a single random sample from the distribution. */
  val sampler: () => T

  /** Internal computation tree node - handles lazy evaluation and correlation preservation. */
  val computationTree: ComputationTree[T]

  /** Gets one sample from this uncertain value.
    *
    * Each call returns a different random sample from the distribution. If this uncertain value is built from other
    * uncertain values, their samples are coordinated to preserve correlations.
    *
    * @return
    *   A single sample of type T
    */
  def sample(): T = computationTree.evaluate()

  /** Transforms each sample using a function.
    *
    * This is like calling `list.map(f)` but for probability distributions. The uncertainty structure is preserved.
    *
    * @param f
    *   Function to apply to each sample
    * @return
    *   New uncertain value with the function applied
    */
  def map[U](f: T => U): Uncertain[U] = {
    val newNode = ComputationMap(this.computationTree, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Chains uncertain computations together.
    *
    * Use this when the next uncertain value depends on the current sample.
    *
    * @param f
    *   Function that takes a sample and returns a new uncertain value
    * @return
    *   New uncertain value representing the chained computation
    */
  def flatMap[U](f: T => Uncertain[U]): Uncertain[U] = {
    val newNode = ComputationFlatMap(this.computationTree, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Returns an endless iterator of samples. */
  def iterator: Iterator[T] = Iterator.continually(sample())

  /** Collects multiple samples into a list.
    *
    * @param n
    *   Number of samples to collect (must be non-negative)
    * @return
    *   List containing n random samples
    */
  def take(n: Int): List[T] = {
    require(n >= 0, "Number of samples must be non-negative.")
    iterator.take(n).toList
  }

  /** Filters the uncertain value by applying a predicate while preserving correlation.
    *
    * <strong>Q:</strong> Why does this return Uncertain[Option[T]] instead of Uncertain[T]?
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
    * @return
    *   New uncertain value
    */
  def apply[T](sampler: () => T)(using random: Random = new Random()): Uncertain[T] = {
    val id = UUID.nameUUIDFromBytes(random.nextBytes(16))
    val s  = sampler
    new Uncertain[T] {
      override val sampler: () => T                    = s
      override val computationTree: ComputationTree[T] = ComputationLeaf(id = id, sampler = s)
    }
  }

  /** Internal constructor with pre-defined computation node. */
  private[uncertaintee] def apply[T](sampler: () => T, computationNode: ComputationTree[T]): Uncertain[T] = {
    val s = sampler
    new Uncertain[T] {
      override val sampler: () => T                    = s
      override val computationTree: ComputationTree[T] = computationNode
    }
  }

  /** Creates an uncertain value that's always the same (no uncertainty). */
  def point[T](value: T): Uncertain[T] = Uncertain(() => value)

  /** Creates a mixture of different uncertain distributions. */
  def mixture[T](components: Map[Uncertain[T], Double])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "Need at least one component for a mixture.")
    require(components.values.forall(_ >= 0), "Weights cannot be negative.")
    val totalWeight = components.values.sum
    require(totalWeight > 0, "Weights must sum to something positive.")

    if (components.size == 1) return components.head._1
    categorical(components)(using random).flatMap(identity)
  }

  /** Creates a mixture where all components have equal weight. */
  def equalMixture[T](components: List[Uncertain[T]])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "Need at least one component for equal mixture.")
    mixture(components.map((x: Uncertain[T]) => x -> One).toMap)(using random)
  }

  /** Creates a distribution by sampling from observed data. */
  def empirical[T](data: List[T])(using random: Random = new Random()): Uncertain[T] = {
    require(data.nonEmpty, "Need at least one data point for empirical distribution.")
    Uncertain(() => data(random.nextInt(data.length)))
  }

  /** Creates a distribution that chooses from specific outcomes with given probabilities. */
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

  /** Creates a normal (bell curve) distribution. */
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

  /** Creates a uniform distribution (all values equally likely within a range). */
  def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(max >= min, s"max ($max) must be ≥ min ($min).")
    Uncertain(() => min + random.nextDouble() * (max - min))
  }

  /** Creates an exponential distribution (models time between random events). */
  def exponential(rate: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(rate > 0, "Rate parameter must be positive.")
    Uncertain { () =>
      var u = Zero
      while (u == Zero) u = random.nextDouble() // Avoid log(0)
      -log(u) / rate
    }
  }

  /** Creates a true/false distribution with a given probability of true. */
  def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] = {
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => random.nextDouble() < probability)
  }

  /** Creates a Kumaraswamy distribution (similar to Beta, but simpler to sample). */
  def kumaraswamy(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(a > 0 && b > 0, "Kumaraswamy parameters must be positive")
    val reciprocalA = One / a
    val reciprocalB = One / b
    Uncertain { () =>
      val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
      pow(One - pow(One - u, reciprocalB), reciprocalA)
    }
  }

  /** Creates a Rayleigh distribution (models magnitude of 2D random vector). */
  def rayleigh(scale: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(scale > 0, "Rayleigh scale parameter must be positive")
    Uncertain { () =>
      val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
      scale * sqrt(-Two * log(One - u))
    }
  }

  /** Creates a binomial distribution (number of successes in n trials). */
  def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
    require(trials >= 0, "Number of trials cannot be negative.")
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
  }

  /** Creates a Poisson distribution (number of events in a fixed time period). */
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
