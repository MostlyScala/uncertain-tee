/*
 * Copyright (c) 2025 Mostly Codes
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package mostly.uncertaintee.ops

import mostly.uncertaintee.*
import mostly.uncertaintee.StatisticallyConvertible.given

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
    def mode(sampleCount: Int = 1000): T = {
      require(sampleCount > 0, "Sample count must be positive.")
      uncertain.take(sampleCount).groupBy(identity).view.maxBy((_, elems) => elems.length)._1
    }

    /** Creates a frequency count of all sample values. */
    def histogram(sampleCount: Int = 1000): Map[T, Int] = {
      require(sampleCount > 0, "Sample count must be positive.")
      uncertain.take(sampleCount).groupBy(identity).view.mapValues(_.length).toMap
    }

    /** Estimates the information entropy (randomness) of the distribution. */
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

  /** Statistical methods for uncertain values that can be represented numerically. */
  extension [T](uncertain: Uncertain[T])(using sc: StatisticallyConvertible[T]) {

    /** Estimates the average (expected) value by sampling. */
    def expectedValue(sampleCount: Int = 1000): Double = {
      require(sampleCount > 0, "Sample count must be positive.")
      val samples = uncertain.take(sampleCount).map(sc.toDouble)
      samples.sum / samples.length.toDouble
    }

    /** Alias for expectedValue. */
    def mean(sampleCount: Int = 1000): Double = expectedValue(sampleCount)

    /** Estimates population standard deviation. */
    def populationStandardDeviation(sampleCount: Int = 1000): Double = {
      require(sampleCount > 0, "Sample count must be positive.")
      val samples  = uncertain.take(sampleCount).map(sc.toDouble)
      val meanVal  = samples.sum / samples.length
      val variance = samples.foldLeft(Zero)((acc, sample) => acc + pow(sample - meanVal, 2)) / samples.length
      sqrt(variance)
    }

    /** Estimates sample standard deviation with Bessel's correction. */
    def standardDeviation(sampleCount: Int = 1000): Double = {
      require(sampleCount >= 2, "Need at least 2 samples for sample standard deviation.")
      val samples  = uncertain.take(sampleCount).map(sc.toDouble)
      val meanVal  = samples.sum / samples.length
      val variance = samples.foldLeft(Zero)((acc, sample) => acc + pow(sample - meanVal, 2)) / (samples.length - 1)
      sqrt(variance)
    }
  }

  /** Order-based statistics for uncertain values with comparable types. */
  extension [T](uncertain: Uncertain[T])(using ord: Ordering[T]) {

    /** Estimates a confidence interval using sample percentiles. */
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

    /** Estimates the Cumulative Distribution Function - P(X ≤ value). */
    def cdf(value: T, sampleCount: Int = 1000): Double = {
      require(sampleCount > 0, "Sample count must be positive.")
      val samples   = uncertain.take(sampleCount)
      val successes = samples.count(ord.lteq(_, value))
      successes.toDouble / sampleCount
    }
  }

  extension [T](uncertainOption: Uncertain[Option[T]]) {

    /** Calculates the probability that the Option is a `Some`.
      *
      * This is useful for understanding the success rate of a [[Uncertain.filter]]
      */
    def probabilityOfSuccess(sampleCount: Int = 1000): Double =
      uncertainOption.map(_.isDefined).mean(sampleCount)

    /** Calculates the probability that the Option is a `None`.
      */
    def probabilityOfFailure(sampleCount: Int = 1000): Double =
      uncertainOption.map(_.isEmpty).mean(sampleCount)

  }
}
