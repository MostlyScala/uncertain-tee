package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.histogram

import scala.math.Ordering.Implicits.*

trait EquivalenceOps {

  extension (u: Uncertain.type)
    /** Calculates the (Komogorov-Smirnov statistic)[https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test], a metric for analysing the total similarity of two distributions.
      *
      * @see
      *   [[KolmogorovSmirnov.test]] for direct comparisons when deciding equivalence.
      *
      * @note
      *   The values `n` and `m` inform the number of samples taken of each distribution. The higher the number the more accurate the representation of the distribution, but with a
      *   larger impact on overall performance. We then compare these samples across `x` values where `x` represents the range of possible values. For example, if both distributions
      *   return an `Int` then in theory we should check all values from `Int.MinValue` to `Int.MaxValue`. For performance reasons this isn't practical so the `xValues` should
      *   represent a realistic range of expected values. The more values included means a more accurate result at the cost of performance.
      *
      * @example
      *   Calculates the statistic for two static distributions, which produce values between 0 and 10.
      *   {{{
      *   KolmogorovSmirnov.statistic(
      *     f1 = Uncertain.always(1),
      *     n = 1,
      *     f2 = Uncertain.always(10),
      *     m = 1,
      *     xValues = 0 until 10
      *   )
      *   }}}
      *
      * @param f1
      *   the first distribution
      * @param n
      *   the number of samples to take from `f1`
      * @param f2
      *   the first distribution
      * @param m
      *   the number of samples to take from `f2`
      * @param xValues
      *   the range of values to compare the distribution against
      * @tparam T
      *   distribution type
      * @return
      *   the largest difference between two distributions
      */
    def ksStatistic[T: Ordering](f1: Uncertain[T], n: Int, f2: Uncertain[T], m: Int, xValues: Seq[T]): Double = {
      require(xValues.nonEmpty, "x-values was empty! while technically valid, probably not what you wanted.")

      val sampleA = f1.histogram(sampleCount = n)
      val sampleB = f2.histogram(sampleCount = m)

      val value =
        xValues.map { x =>
          val f1n = sampleA.view.filterKeys(_ <= x).values.sum.toDouble / xValues.size
          val f2m = sampleB.view.filterKeys(_ <= x).values.sum.toDouble / xValues.size

          math.abs(f1n - f2m)
        }.max

      value
    }

    /** Decides if two distributions return the same results, within a given confidence threshold (set by `alpha`). A higher `alpha` value permits larger differences between
      * distributions.
      *
      * @see
      *   [[KolmogorovSmirnov.statistic]] for performance notes around sample sizes.
      *
      * @param f1
      *   the first distribution
      * @param n
      *   the number of samples to take from `f1`
      * @param f2
      *   the first distribution
      * @param m
      *   the number of samples to take from `f2`
      * @param xValues
      *   the range of values to compare the distribution against
      * @param alpha
      *   value between 0 and 1 indicating allowable differences between distributions
      * @tparam T
      *   distribution type
      * @return
      */
    def ksTest[T: Ordering](f1: Uncertain[T], n: Int, f2: Uncertain[T], m: Int, xValues: Seq[T], alpha: Double): Boolean = {
      val statistic  = Uncertain.ksStatistic(f1, n, f2, m, xValues)
      val confidence = math.sqrt(-math.log(alpha / 2) * 0.5).min(0).max(0)

      // the point where we can't consider the two distributions equal
      val threshold = confidence * math.sqrt((n + m).toDouble / (n * m))
      statistic <= threshold
    }
}
