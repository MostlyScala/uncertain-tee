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
package mostly.uncertaintee.quantiles

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.internal.SmoothSpline
import mostly.uncertaintee.syntax.*

import scala.math.floor
import scala.util.Random

/** Represents N-quantile boundary values.
  *
  * @note
  *   Outside of the statistical use, you can think of a [[Quantiles]] instance as a way to "serialize" an [[Uncertain]]; an approximate [[Uncertain]] can be re-constructed via
  *   [[reconstructLinear]], [[reconstructSmooth]], [[reconstructStep]] or [[reconstructDiscrete]]
  *
  * For common use cases see:
  *
  *   - [[Tertiles]] ( construct from [[Uncertain]] with [[Quantiles.tertiles]] )
  *   - [[Quartiles]] ( construct from [[Uncertain]] with [[Quantiles.quartiles]] )
  *   - [[Quintiles]] ( construct from [[Uncertain]] with [[Quantiles.quintiles]] )
  *   - [[Deciles]] ( construct from [[Uncertain]] with [[Quantiles.deciles]] )
  *   - [[Percentiles]] ( construct from [[Uncertain]] with [[Quantiles.percentiles]] )
  */
trait Quantiles[T] {

  /** Number of quantile intervals this [[Quantiles]] implementation represents. */
  val quantileIntervals: Int

  /** Returns the value at the specified quantile boundary.
    *
    * A quantile boundary is a position that divides the distribution; This method returns the actual data value at that boundary position.
    *
    * {{{
    *  Quantile boundaries (indices): 0     1     2     3     4     5
    *  Percent of data:               0%    20%   40%   60%   80%   100%
    *  Boundary values:               v0    v1    v2    v3    v4    v5
    *  Extremes:                      min                           max
    * }}}
    *
    * @param index
    *   quantile boundary index (0 = minimum, n = maximum)
    * @return
    *   the data value at the specified quantile boundary
    */
  def apply(index: Int): T

  /** Maximum value (Nth quantile boundary value)
    *
    * @return
    *   the maximum value (data value at the nth quantile boundary)
    */
  def max: T = this.apply(quantileIntervals)

  /** Minimum value (0th quantile boundary value)
    *
    * @return
    *   the minimum value (data value at the 0th quantile boundary)
    */
  def min: T = this.apply(0)

  /** Returns a sequence of quantile boundary values.
    *
    * Example for [[quantileIntervals]] == 5 (if includeMin = true, includeMax = true):
    *
    * {{{
    * Quantile boundaries (indices): 0     1     2     3     4     5
    * Percent of data:               0%    20%   40%   60%   80%   100%
    * Boundary values:               v0    v1    v2    v3    v4    v5
    * Extremes:                      min                           max
    * }}}
    *
    * @param includeMin
    *   whether to include the minimum (0th quantile boundary value) in the sequence
    * @param includeMax
    *   whether to include the maximum ([[quantileIntervals]]th quantile boundary value) in the sequence
    * @return
    *   sequence of quantile boundary values based on inclusion parameters
    */
  def toList(includeMin: Boolean = true, includeMax: Boolean = true): List[T] =
    (includeMin, includeMax) match {
      case (true, true)   => (0 to quantileIntervals).map(this.apply).toList
      case (true, false)  => (0 until quantileIntervals).map(this.apply).toList
      case (false, true)  => (1 to quantileIntervals).map(this.apply).toList
      case (false, false) => (1 until quantileIntervals).map(this.apply).toList
    }

  /** Equivalent to [[toList]] with includeMin = true, includeMax = true; provides tent poles. */
  def boundaryValues: List[T] = toList(includeMin = true, includeMax = true)

  /** Returns a Map[Int, T] of quantile boundary values.
    *
    * The map keys are quantile boundary indices (0 to n), and values are the corresponding data values.
    *
    * Example for [[quantileIntervals]] == 5 (if includeMin = true, includeMax = true):
    *
    * {{{
    * Quantile boundaries (indices): 0     1     2     3     4     5
    * Percent of data:               0%    20%   40%   60%   80%   100%
    * Boundary values:               v0    v1    v2    v3    v4    v5
    * Extremes:                      min                           max
    * }}}
    *
    * @param includeMin
    *   whether to include the minimum (0th quantile boundary value) in the map
    * @param includeMax
    *   whether to include the maximum ([[quantileIntervals]]th quantile boundary value) in the map
    * @return
    *   map of quantile boundary indices to their corresponding data values
    */
  def toMap(includeMin: Boolean = true, includeMax: Boolean = true): Map[Int, T] =
    (includeMin, includeMax) match {
      case (true, true)   => (0 to quantileIntervals).map(i => i -> this.apply(i)).toMap
      case (true, false)  => (0 until quantileIntervals).map(i => i -> this.apply(i)).toMap
      case (false, true)  => (1 to quantileIntervals).map(i => i -> this.apply(i)).toMap
      case (false, false) => (1 until quantileIntervals).map(i => i -> this.apply(i)).toMap
    }

  /** Reconstructs quantiles as a discrete uniform distribution over boundary values.
    *
    * Creates a discrete distribution where each quantile boundary value has equal probability of being sampled. This is the simplest form of reconstruction - it preserves the
    * exact boundary values but loses all information about values between boundaries.
    *
    * ==Comparison with Other Methods==
    *
    *   - vs [[reconstructStep]]: Step fills intervals uniformly; Discrete only returns exact boundary values.
    *   - vs [[reconstructLinear]]: Linear interpolates values between boundaries; Discrete is "gappy".
    *   - vs [[reconstructSmooth]]: Smooth creates a C1 continuous curve; Discrete is not continuous.
    *
    * ==When to Use==
    *
    *   - When the underlying data is inherently discrete (e.g., shoe sizes, counts).
    *   - When you want to minimize assumptions about what happens between quantiles.
    *   - When preserving the exact quantile values is more important than continuity.
    *
    * ==When NOT to Use==
    *
    *   - When the underlying phenomenon is continuous (e.g., height, temperature).
    *   - When "gaps" in the output distribution would cause issues for downstream consumers.
    *   - When you need to simulate a full range of values, not just specific markers.
    *
    * ==Example==
    * {{{
    *  // Original samples: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    *  val original: Uncertain[Int] = ...
    *
    *  // Compute quintiles (5 intervals, 6 boundary values)
    *  val quintiles: Quintiles[Int] = Quantiles.quintiles(original, sampleCount = 100_000)
    *  // Boundary values: [0, 2, 4, 6, 8, 10]
    *
    *  // Reconstruct as discrete uniform distribution
    *  val reconstructed = quintiles.toDiscreteUncertain
    *  // Possible samples: only [0, 2, 4, 6, 8, 10]
    *  // (values 1, 3, 5, 7, 9 are lost)
    *
    *  // Re-computing quintiles produces the same boundary values
    *  val quintiles2: Quintiles[Int] = Quantiles.quintiles(reconstructed, sampleCount = 100_000)
    *  // Boundary values: [0, 2, 4, 6, 8, 10] (unchanged)
    * }}}
    *
    * @param random
    *   random number generator (implicit, defaults to new Random())
    * @return
    *   an [[Uncertain]] distribution that samples uniformly from quantile boundary values
    * @see
    *   [[reconstructStep]] for histogram-like reconstruction
    * @see
    *   [[reconstructLinear]] for continuous reconstruction with linear interpolation
    * @see
    *   [[reconstructSmooth]] for continuous reconstruction with cubic spline interpolation
    */
  def reconstructDiscrete(using random: Random = new Random()): Uncertain[T] =
    Uncertain.empirical(this.toList())(using random)

  /** Reconstructs quantiles as a piecewise constant (step function) distribution.
    *
    * Creates a continuous distribution using a step function where each interval between quantile boundaries samples uniformly within that interval. This produces a histogram-like
    * distribution that preserves the quantile structure while allowing values between boundaries.
    *
    * ==Statistical Method==
    *
    * This implements a **piecewise uniform distribution** (histogram approximation) using the inverse transform method. The empirical quantile function is approximated as constant
    * within each quantile interval, creating a step function CDF.
    *
    * ==Comparison with Other Methods==
    *
    *   - vs [[reconstructDiscrete]]: Step allows values between boundaries, discrete only returns exact boundary values.
    *   - vs [[reconstructLinear]]: Step has discontinuities at boundaries, linear interpolates smoothly.
    *   - vs [[reconstructSmooth]]: Step is piecewise constant, smooth uses cubic splines.
    *
    * ==When to Use==
    *
    *   - When you want a histogram-like representation.
    *   - For discrete-looking data that actually has continuous ranges.
    *   - When you believe the original distribution was roughly uniform within quantile intervals.
    *   - For visualization purposes where step functions are clearer than interpolation.
    *
    * ==When NOT to Use==
    *
    *   - When you know the underlying distribution is smooth (use [[reconstructSmooth]] instead).
    *   - When you want exact boundary values only (use [[reconstructDiscrete]] instead).
    *   - For small numbers of quantiles where linear interpolation is more natural.
    *
    * ==Round-Trip Property==
    *
    * The step reconstruction approximately preserves quantiles when resampled:
    * {{{
    * val original = Uncertain.uniform(0, 100)
    * val deciles = Quantiles.deciles(original, sampleCount = 100_000)
    * val stepped = deciles.reconstructStep
    *
    * // Recomputing deciles gives similar (but not identical) boundary values
    * val newDeciles = Quantiles.deciles(stepped, sampleCount = 100_000)
    * // newDeciles ≈ deciles (within sampling error)
    * }}}
    *
    * @param num
    *   numeric evidence for type T
    * @param random
    *   random number generator (implicit, defaults to new Random())
    * @return
    *   [[Uncertain]][Double] that samples from a piecewise constant distribution
    * @note
    *   Returns Uncertain[Double] regardless of input type T. Use `.map(_.toInt)` or similar to convert back to original type if needed.
    * @note
    *   Requires at least 2 quantile boundaries (1 interval). The method creates n intervals from n+1 boundary values.
    * @see
    *   [[reconstructDiscrete]] for sampling only exact boundary values
    * @see
    *   [[reconstructLinear]] for continuous linear interpolation between boundaries
    * @see
    *   [[reconstructSmooth]] for smooth cubic spline interpolation
    */
  def reconstructStep(using num: Numeric[T], random: Random = new Random()): Uncertain[Double] = {
    val values: List[T] = this.boundaryValues
    require(values.length >= 2, "Need at least 2 quantile boundaries for step reconstruction")

    val valuesAsDouble: IndexedSeq[Double] = values.map(num.toDouble).toIndexedSeq
    val n                                  = values.length - 1 // number of intervals

    Uncertain.fromInverseCdf { p =>
      // Determine which interval this probability falls into
      // Each interval gets equal probability mass (1/n)
      val intervalIndex = math.min((p * n).toInt, n - 1)

      // Calculate the local position within the selected interval
      val intervalStart = intervalIndex.toDouble / n
      val intervalEnd   = (intervalIndex + 1).toDouble / n
      val localPosition = (p - intervalStart) / (intervalEnd - intervalStart)

      // Interpolate uniformly within the interval
      val v0 = valuesAsDouble(intervalIndex)
      val v1 = valuesAsDouble(intervalIndex + 1)
      v0 + localPosition * (v1 - v0)
    }
  }

  /** Reconstructs quantiles as a continuous distribution using linear interpolation.
    *
    * Creates a continuous distribution by linearly interpolating between quantile boundary values. The reconstructed distribution will produce similar quantiles to the input when
    * resampled. This is the recommended default method for continuous reconstruction.
    *
    * ==Design Philosophy==
    *
    * This method does not attempt to guess the "true" underlying distribution. Instead it creates a reasonable reconstruction that:
    *   - Passes through all quantile boundary points exactly
    *   - Interpolates linearly between boundaries (simple, predictable)
    *   - Makes no assumptions about distribution type (normal, uniform, etc.)
    *   - Preserves quantile structure when resampled
    *
    * ==Comparison with Other Methods==
    *
    *   - vs [[reconstructDiscrete]]: Linear creates a continuous range; Discrete is "gappy".
    *   - vs [[reconstructStep]]: Linear connects points directly; Step creates a "staircase" histogram effect.
    *   - vs [[reconstructSmooth]]: Linear has sharp "corners" at boundaries; Smooth has C1 continuity (smooth curves).
    *
    * ==When to Use==
    *
    *   - **Default Choice:** This is the most robust and predictable method for general continuous data.
    *   - When you have a small number of quantiles (e.g., Tertiles, Quartiles).
    *   - When you want to avoid potential "overshoot" or oscillations that can sometimes occur with splines.
    *   - When performance is critical (linear is slightly faster than cubic splines).
    *
    * ==When NOT to Use==
    *
    *   - When you need smooth first derivatives (use [[reconstructSmooth]]).
    *   - When the data is inherently discrete (use [[reconstructDiscrete]]).
    *   - When you specifically want a histogram-like model (use [[reconstructStep]]).
    *
    * ==Round-Trip Property==
    *
    * The reconstructed distribution approximately preserves quantiles:
    *
    * {{{
    * val original = Uncertain.normal(50, 10)
    * val quartiles = Quantiles.quartiles(original, sampleCount = 10_000)
    * val reconstructed = quartiles.reconstructLinear
    *
    * // Reconstructed distribution has similar quartiles to original
    * val newQuartiles = Quantiles.quartiles(reconstructed, sampleCount = 10_000)
    * // quartiles ≈ newQuartiles (within sampling error)
    * }}}
    *
    * @param num
    *   numeric evidence for type T
    * @param random
    *   random number generator (implicit, defaults to new Random())
    * @return
    *   [[Uncertain]][Double] that samples using piecewise linear inverse CDF
    * @note
    *   Returns Uncertain[Double] regardless of input type T. Use `.map(_.toInt)` or similar to convert back to original type if needed.
    * @see
    *   [[reconstructDiscrete]] for discrete reconstruction (samples only boundary values)
    * @see
    *   [[reconstructStep]] for histogram-like reconstruction
    * @see
    *   [[reconstructSmooth]] for smoother reconstruction using cubic splines
    */
  def reconstructLinear(using num: Numeric[T], random: Random = new Random()): Uncertain[Double] = {
    val values: List[T] = this.toList()
    require(values.length >= 2, "Need at least 2 quantile boundaries for continuous reconstruction")

    // Quantile positions: 0/(n-1), 1/(n-1), ..., (n-1)/(n-1)
    // These represent cumulative probabilities at each boundary
    val n                                     = values.length
    val quantilePositions: IndexedSeq[Double] = (0 until n).map(i => i.toDouble / (n - 1))
    val valuesAsDouble: IndexedSeq[Double]    = values.map(num.toDouble).toIndexedSeq

    Uncertain.fromInverseCdf { p =>
      // Handle exact boundary cases
      if (p == 0.0) {
        valuesAsDouble.head
      } else {

        // Find the quantile segment containing this probability
        // We want the first position >= p
        val idx = quantilePositions.indexWhere(_ >= p)

        // If we're exactly at the first position
        if (idx == 0) {
          valuesAsDouble.head
        }
        // Linear interpolation between quantile boundaries
        else {
          val p0 = quantilePositions(idx - 1)
          val p1 = quantilePositions(idx)
          val v0 = valuesAsDouble(idx - 1)
          val v1 = valuesAsDouble(idx)

          // Interpolation parameter: where are we between p0 and p1?
          val t = (p - p0) / (p1 - p0)

          // Linear interpolation: v0 + t * (v1 - v0)
          v0 + t * (v1 - v0)
        }
      }
    }
  }

  /** Reconstructs quantiles as a continuous distribution using monotonic cubic spline interpolation.
    *
    * Creates a smoother continuous distribution than [[reconstructLinear]] by using Fritsch-Carlson monotonic cubic Hermite splines. This produces C1-continuous curves (smooth
    * first derivatives) while maintaining monotonicity.
    *
    * ==Interpolation Properties==
    *
    * Uses Fritsch-Carlson monotonic cubic spline which guarantees:
    *   - C1 continuity (smooth first derivatives across boundaries)
    *   - Monotonicity preservation (no spurious oscillations or reversals)
    *   - Passes through all quantile boundary points exactly
    *   - No external dependencies (self-contained implementation)
    *
    * ==Comparison with Other Methods==
    *
    *   - vs [[reconstructLinear]]: Smooth removes "corners" at boundaries; Linear is simpler.
    *   - vs [[reconstructStep]]: Smooth is a continuous curve; Step is a "staircase".
    *   - vs [[reconstructDiscrete]]: Smooth allows any value in the range; Discrete is fixed points.
    *
    * ==When to Use==
    *
    *   - When you have many quantile boundaries (e.g., Deciles, Percentiles).
    *   - For visualization or plotting where smooth CDFs/PDFs are preferred.
    *   - When you believe the underlying distribution is genuinely smooth (e.g., Normal-like).
    *   - When you need C1 continuity (smooth derivatives) for downstream calculations.
    *
    * ==When NOT to Use==
    *
    *   - With few quantiles (e.g., Tertiles, Quartiles) - linear is usually safer.
    *   - When the original distribution has sharp discontinuities or corners.
    *   - When simple, predictable behavior is more important than visual smoothness.
    *
    * ==Performance==
    *
    *   - O(n) setup cost to build spline coefficients
    *   - O(n) per sample to find segment (could optimize to O(log n))
    *   - Slightly slower than linear interpolation but still very fast
    *
    * @param num
    *   numeric evidence for type T
    * @param random
    *   random number generator (implicit, defaults to new Random())
    * @return
    *   [[Uncertain]][Double] using monotonic cubic spline interpolated inverse CDF
    * @note
    *   Requires at least 3 quantile boundaries. For fewer boundaries or when in doubt, use [[reconstructLinear]] instead.
    * @see
    *   [[reconstructDiscrete]] for discrete reconstruction
    * @see
    *   [[reconstructStep]] for histogram-like reconstruction
    * @see
    *   [[reconstructLinear]] for default linear interpolation (recommended for most cases)
    */
  def reconstructSmooth(using num: Numeric[T], random: Random = new Random()): Uncertain[Double] = {
    val values: List[T]      = this.toList()
    require(values.length >= 3, "Need at least 3 quantile boundaries for cubic spline reconstruction")
    val valuesAsDoubles      = values.map(num.toDouble)
    val spline: SmoothSpline = SmoothSpline(valuesAsDoubles)
    Uncertain.fromInverseCdf((probability: Double) => spline(xCoordinate = probability))
  }
}

object Quantiles {

  /** Computes tertiles (3-way splits) from an uncertain value.
    *
    * Tertiles divide the distribution into 3 equal-probability intervals with 4 boundary points (0%, 33%, 67%, 100%).
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw (must be > 3)
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Tertiles]] with 4 boundary values
    */
  def tertiles[T](uncertain: Uncertain[T], sampleCount: Int)(using ord: Ordering[T]): Tertiles[T] = {
    require(sampleCount > 3, s"sampleCount must be > 3, was: $sampleCount")
    Tertiles.fromUncertain[T](uncertain = uncertain, sampleCount = sampleCount)(using ord)
  }

  /** Computes quartiles (4-way splits) from an uncertain value.
    *
    * Quartiles divide the distribution into 4 equal-probability intervals with 5 boundary points (0%, 25%, 50%, 75%, 100%).
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw (must be > 4)
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Quartiles]] with 5 boundary values including median
    */
  def quartiles[T](uncertain: Uncertain[T], sampleCount: Int)(using ord: Ordering[T]): Quartiles[T] = {
    require(sampleCount > 4, s"sampleCount must be > 4, was: $sampleCount")
    Quartiles.fromUncertain[T](uncertain = uncertain, sampleCount = sampleCount)(using ord)
  }

  /** Computes quintiles (5-way splits) from an uncertain value.
    *
    * Quintiles divide the distribution into 5 equal-probability intervals with 6 boundary points (0%, 20%, 40%, 60%, 80%, 100%).
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw (must be > 5)
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Quintiles]] with 6 boundary values
    */
  def quintiles[T](uncertain: Uncertain[T], sampleCount: Int)(using ord: Ordering[T]): Quintiles[T] = {
    require(sampleCount > 5, s"sampleCount must be > 5, was: $sampleCount")
    Quintiles.fromUncertain[T](uncertain = uncertain, sampleCount = sampleCount)(using ord)
  }

  /** Computes deciles (10-way splits) from an uncertain value.
    *
    * Deciles divide the distribution into 10 equal-probability intervals with 11 boundary points (0%, 10%, 20%, ..., 90%, 100%).
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw (must be > 10)
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Deciles]] with 11 boundary values
    */
  def deciles[T](uncertain: Uncertain[T], sampleCount: Int)(using ord: Ordering[T]): Deciles[T] = {
    require(sampleCount > 10, s"sampleCount must be > 10, was: $sampleCount")
    Deciles.fromUncertain[T](uncertain = uncertain, sampleCount = sampleCount)(using ord)
  }

  /** Computes percentiles (100-way splits) from an uncertain value.
    *
    * Percentiles divide the distribution into 100 equal-probability intervals with 101 boundary points (0%, 1%, 2%, ..., 99%, 100%).
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw (must be > 100)
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Percentiles]] with 101 boundary values
    */
  def percentiles[T](uncertain: Uncertain[T], sampleCount: Int)(using ord: Ordering[T]): Percentiles[T] = {
    require(sampleCount > 100, s"sampleCount must be > 100, was: $sampleCount")
    Percentiles.fromUncertain[T](uncertain = uncertain, sampleCount = sampleCount)(using ord)
  }

  /** Computes quantiles of arbitrary size by sampling and sorting the samples.
    *
    * The quantile boundaries are positions that divide the distribution into n equal-probability intervals. This method computes the data values at those boundary positions.
    *
    * ==Algorithm==
    *
    * For n intervals, we compute n+1 boundary values at positions i/n where i = 0, 1, ..., n. Each position corresponds to a percentile: position i/n represents the (100*i/n)th
    * percentile.
    *
    * Example for n = 4 (quartiles):
    * {{{
    *   // Note: Small sample for illustration. Use much larger samples in practice.
    *   Sorted samples (values):  [1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12]
    *   Sorted samples (indices): [0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11]
    *
    *   Using formula: idx = floor(p * (sampleCount - 1)), where p = i/n
    *
    *   Quantile boundary values:
    *   0th boundary (i=0, p=0.00): idx = floor(0.00 * 11) = 0  -> samples(0)  = 1   (min, 0%)
    *   1st boundary (i=1, p=0.25): idx = floor(0.25 * 11) = 2  -> samples(2)  = 3   (Q1, 25%)
    *   2nd boundary (i=2, p=0.50): idx = floor(0.50 * 11) = 5  -> samples(5)  = 6   (median, 50%)
    *   3rd boundary (i=3, p=0.75): idx = floor(0.75 * 11) = 8  -> samples(8)  = 9   (Q3, 75%)
    *   4th boundary (i=4, p=1.00): idx = floor(1.00 * 11) = 11 -> samples(11) = 12  (max, 100%)
    * }}}
    *
    * @param quantileIntervals
    *   number of quantile intervals (must be > 2)
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw (must be > quartileIntervals)
    * @param ord
    *   implicit ordering for type T
    * @return
    *   a [[Quantiles]] instance with boundary values computed from the samples
    */
  def ofSize[T](quantileIntervals: Int, uncertain: Uncertain[T], sampleCount: Int)(using ord: Ordering[T]): Quantiles[T] = {
    require(quantileIntervals > 2, s"quantile intervals must be > 2, was: $quantileIntervals")
    require(sampleCount > quantileIntervals, s"sampleCount must be > n ($quantileIntervals), was: $sampleCount")
    val sortedSamples: List[T]        = uncertain.take(sampleCount).sorted
    val capturedN                     = quantileIntervals
    val quantileValues: IndexedSeq[T] = (0 to quantileIntervals).map { i =>
      val proportion = i.toDouble / quantileIntervals
      val idx        = floor(proportion * (sampleCount - 1)).toInt
      sortedSamples(idx)
    }

    new Quantiles[T] {
      override val quantileIntervals: Int = capturedN
      override def apply(index: Int): T   = {
        require(index >= 0 && index <= quantileIntervals, s"quantile index must be in range 0 to $quantileIntervals, was: $index")
        quantileValues(index)
      }
    }
  }

  private[uncertaintee] def fromBoundaryValues[T](list: List[T]): Quantiles[T] = new Quantiles[T] {
    override val quantileIntervals: Int = list.length - 1
    override def apply(index: Int): T   = list(index)
  }

}
