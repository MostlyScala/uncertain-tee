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

package mostly.uncertaintee.ops

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

import scala.math.*

/** {{{
  *    import mostly.uncertaintee.syntax.statistical.*
  *    // or just import all the syntax (recommended)
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait StatisticalOps {

  /** Summary statistics and analysis methods for uncertain values. */
  extension [T](uncertain: Uncertain[T]) {

    /** Finds the most common sample value (best for discrete distributions). */
    def mode(sampleCount: Int): T = {
      require(sampleCount > 0, "Sample count must be positive.")
      uncertain.take(sampleCount).groupBy(identity).view.maxBy((_, elems) => elems.length)._1
    }

    /** Creates a frequency count of all sample values. */
    def histogram(sampleCount: Int): Map[T, Int] = {
      require(sampleCount > 0, "Sample count must be positive.")
      uncertain.take(sampleCount).groupBy(identity).view.mapValues(_.length).toMap
    }

    /** Estimates the information entropy (randomness) of the distribution. */
    def entropy(sampleCount: Int): Double = {
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

  extension [T](uncertain: Uncertain[T])(using num: Numeric[T]) {

    /** Computes the sample excess kurtosis of this uncertain distribution using a **single-pass, O(1) memory (streaming) algorithm.**
      *
      * Excess kurtosis is estimated from the population central moments:
      * {{{
      * excess kurtosis = μ₄ / μ₂² - 3
      * }}}
      *
      * Where `μk` is the k-th central moment (`E[(X - μ)ᵏ]`).
      *
      * '''Performance Notes:'''
      *   - This algorithm iterates over the sample generator exactly once.
      *   - It uses constant (O(1)) memory, allowing it to handle arbitrarily large `sampleCount` without `OutOfMemoryError`.
      *   - It uses a numerically stable (Welford-Knuth) online algorithm.
      *   - More `sampleCount` is needed than for mean/variance due to the high variance of the kurtosis estimator itself.
      *   - Consider using 10,000+ `sampleCount` for stable estimates.
      *
      * @example
      *   Pattern matching on results:
      *   {{{
      * val normal = Uncertain.normal(0, 1)
      * normal.kurtosis(10000) match {
      * case Kurtosis.Mesokurtic(k) =>
      * println(s"Normal-like with excess kurtosis ≈ $k")
      * case Kurtosis.Leptokurtic(k) =>
      * println(s"Heavy-tailed with excess kurtosis = $k")
      * case Kurtosis.Platykurtic(k) =>
      * println(s"Light-tailed with excess kurtosis = $k")
      * case Kurtosis.Undefined =>
      * println("Constant distribution")
      * }
      *   }}}
      * @param sampleCount
      *   Number of samples to draw for the estimation.
      * @param mesokurticTolerance
      *   Threshold for classifying as mesokurtic (default: 0.5)
      * @return
      *   A Kurtosis instance representing the measurement and classification
      *
      * See: https://en.wikipedia.org/wiki/Online_algorithm#Higher_moments
      */
    def kurtosis(
      sampleCount: Int,
      mesokurticTolerance: Double = 0.5,
      effectivelyZero: Double = 1e-12
    ): Kurtosis = {
      require(sampleCount > 3, "sampleCount must be greater than 3.")
      require(mesokurticTolerance >= 0, "Threshold must be non-negative.")
      // Mutable state for the single-pass Welford-Knuth algorithm
      // These track n, mean, and the central moments M2, M3, M4
      var n: Long      = 0
      var mean: Double = 0.0
      var m2: Double   = 0.0
      var m3: Double   = 0.0
      var m4: Double   = 0.0

      // Single pass over the generator (No intermediate collection is created)
      uncertain.iterator.take(sampleCount).foreach { t =>
        val x  = num.toDouble(t)
        val n1 = n
        n += 1

        val delta      = x - mean
        val delta_n    = delta / n
        val delta_n_sq = delta_n * delta_n
        val term1      = delta * delta_n * n1

        mean += delta_n
        m4 += term1 * delta_n_sq * (n * n - 3 * n + 3) + 6 * delta_n_sq * m2 - 4 * delta_n * m3
        m3 += term1 * delta_n * (n - 2) - 3 * delta_n * m2
        m2 += term1
      }

      val variance = m2 / (n - 1)
      val stdDev   = sqrt(variance)

      if (stdDev < effectivelyZero) {
        Kurtosis.Undefined
      } else {
        val m2_sample = m2 / (n - 1)

        val rawKurtosis = (n.toDouble * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * (m4 / (m2_sample * m2_sample)) -
          3.0 * ((n - 1) * (n - 1) / ((n - 2) * (n - 3)))

        Kurtosis(
          excessValue = rawKurtosis,
          mesokurticTolerance = mesokurticTolerance
        )
      }
    }
  }

  /** Statistical methods for uncertain values that can be represented numerically. */
  extension [T](uncertain: Uncertain[T])(using num: Numeric[T]) {

    /** Estimates the mean by sampling (alias for [[mean]]) */
    def expectedValue(sampleCount: Int): Double = mean(sampleCount)

    /** Estimates the mean by sampling. */
    def mean(sampleCount: Int): Double = {
      require(sampleCount > 0, "Sample count must be positive.")
      val samples = uncertain.take(sampleCount).map(num.toDouble)
      samples.sum / samples.length.toDouble
    }

    /** Estimates population standard deviation. */
    def populationStandardDeviation(sampleCount: Int): Double = {
      require(sampleCount > 0, "Sample count must be positive.")
      val samples  = uncertain.take(sampleCount).map(num.toDouble)
      val meanVal  = samples.sum / samples.length
      val variance = samples.foldLeft(Zero)((acc, sample) => acc + pow(sample - meanVal, 2)) / samples.length
      sqrt(variance)
    }

    /** Estimates sample standard deviation with Bessel's correction. */
    def standardDeviation(sampleCount: Int): Double = {
      require(sampleCount >= 2, "Need at least 2 samples for sample standard deviation.")
      val samples  = uncertain.take(sampleCount).map(num.toDouble)
      val meanVal  = samples.sum / samples.length
      val variance = samples.foldLeft(Zero)((acc, sample) => acc + pow(sample - meanVal, 2)) / (samples.length - 1)
      sqrt(variance)
    }
  }

  extension [T](uncertain: Uncertain[T])(using ord: Ordering[T]) {

    /** Estimates a confidence interval using sample percentiles. */
    def confidenceInterval(confidence: Double = 0.95, sampleCount: Int): (T, T) = {
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

    /** Estimates the Cumulative Distribution Function - P(X ≤ value). */
    def cdf(value: T, sampleCount: Int): Double = {
      require(sampleCount > 0, "Sample count must be positive.")
      val successes = uncertain.iterator.take(sampleCount).count(ord.lteq(_, value))
      successes.toDouble / sampleCount
    }
  }

  extension [T](uncertainOption: Uncertain[Option[T]]) {

    /** Calculates the probability that the Option is a `Some`.
      *
      * This is useful for understanding the success rate of a [[Uncertain.filter]]
      */
    def probabilityOfSuccess(sampleCount: Int): Double =
      uncertainOption
        .map(_.isDefined)
        .probability(sampleCount)

    /** Calculates the probability that the Option is a `None`.
      */
    def probabilityOfFailure(sampleCount: Int): Double =
      uncertainOption
        .map(_.isEmpty)
        .probability(sampleCount)

  }
}
