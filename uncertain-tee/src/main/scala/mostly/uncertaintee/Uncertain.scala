/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee

import mostly.uncertaintee.internal.*

import java.util.UUID
import scala.util.Random

/** A type for working with uncertain data - values that have some randomness or measurement error.
  *
  * Instead of working with single values like `speed = 65.0`, you can work with uncertain values like `speed = "somewhere between 60-70 mph"` and let the library handle the math
  * correctly.
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
    * Each call returns a different random sample from the distribution. If this uncertain value is built from other uncertain values, their samples are coordinated to preserve
    * correlations.
    *
    * '''Performance Note:''' Each call performs fresh sampling from the underlying distribution(s). For repeated sampling, consider using `take(n)` or `iterator` which may be more
    * efficient.
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
  def map[U](f: T => U): Uncertain[U] =
    this.flatMap(value => Uncertain(() => f(value)))

  /** Chains uncertain computations together (monadic bind operation).
    *
    * Use this when the next uncertain value depends on the current sample. This is essential for building complex probabilistic models where later distributions depend on earlier
    * samples.
    *
    * '''Performance Note:''' Creates nested computation trees that are evaluated lazily. Deep nesting may impact performance for very complex models.
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
    *       Uncertain.always(roll1)
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
    * <strong>A:</strong> This library's core guarantee is correlation preservation: multiple references to the same uncertain value always yield the same sample. Using rejection
    * sampling (resampling until the predicate passes) would break this fundamental property.
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
    *     case None    => Uncertain.always(0.0)  // default for invalid speeds
    *   }
    *   }}}
    * @param keepWhen
    *   The function to test each sample. Returns `true` to keep the value (in a `Some`), `false` to discard it (resulting in `None`).
    * @return
    *   A new `Uncertain[Option[T]]` that contains `Some(value)` if the predicate passed for that sample, and `None` if the predicate failed. Correlation with the original
    *   uncertain value is preserved.
    */
  def filter(keepWhen: T => Boolean): Uncertain[Option[T]] =
    this.map {
      case t if keepWhen(t) => Some(t)
      case _                => None
    }

}

/** Factory for creating uncertain values from various distributions and data sources. */
object Uncertain {

  /** Creates a deterministic value wrapped as an Uncertain.
    *
    * This is useful when you need to mix certain and uncertain values in the same computation.
    *
    * @param value
    *   The fixed value to wrap
    * @return
    *   An uncertain value that always returns the same value
    * @example
    *   {{{
    *   val constantSpeed = Uncertain.always(60.0)  // Always 60 mph
    *   val uncertainDistance = Uncertain.bellCurve(centeredOn = 100.0, withSpread = 10.0)
    *
    *   val time = constantSpeed.combine(uncertainDistance)(_ / _)
    *   // Time varies because distance varies, but speed is always 60
    *   }}}
    */
  def always[T](value: T)(using random: Random = new Random()): Uncertain[T] =
    Uncertain(() => value)(using random)

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

}
