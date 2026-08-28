package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.histogram

import scala.collection.{SortedMap, SortedSet}
import scala.math.Ordering.Implicits.*

trait EquivalenceOps {

  extension (u: Uncertain.type)
    /** Calculates the [[https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test Kolmogorov-Smirnov statistic]], a metric for analysing the total similarity of two
      * distributions.
      *
      * @see
      *   [[KolmogorovSmirnov.test]] for direct comparisons when deciding equivalence.
      *
      * @note
      *   The values `n` and `m` inform the number of samples taken of each distribution. The higher the number the more accurate the representation of the distribution, but with a
      *   larger impact on overall performance. We then compare these samples across `x` values where `x` represents the range of possible values. Again, a larger range and
      *   granularity of `x` increases the accuracy at the cost of overall performance.
      *
      * @example
      *   Comparing two distributions across the range of 0 to 100.
      *   {{{
      *   Uncertain.ksStatistic(
      *     f1 = Uncertain.normal(mean=50, standardDeviation=10),
      *     n = 100,
      *     f2 = Uncertain.oneHundredSidedDie,
      *     m = 100,
      *     xValues = SortedSet(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0)
      *   )
      *   }}}
      *
      * @example
      *   Comparing two distributions across the range of 0 to 1.
      *   {{{
      *   Uncertain.ksStatistic(
      *     f1 = Uncertain.uniform(0, 1)
      *     n = 100,
      *     f2 = Uncertain.uniform(0, 2)
      *     m = 100,
      *     xValues = SortedSet(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
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
      *   defines the range of X values used in the calculation
      * @return
      *   the largest difference between two distributions
      */
    def ksStatistic(f1: Uncertain[Double], n: Int, f2: Uncertain[Double], m: Int, xValues: SortedSet[Double]): Double = {
      require(n > 0, s"invalid sample size (n=$n), use one or more samples for a meaningful result")
      require(m > 0, s"invalid sample size (m=$m), use one or more samples for a meaningful result")
      require(xValues.nonEmpty, s"no x values provided, use one or more x values for a meaningful result")

      val f1Samples = SortedMap.from(f1.histogram(sampleCount = n))
      val f2Samples = SortedMap.from(f2.histogram(sampleCount = m))

      ksStatisticImpl(f1Samples, n, f2Samples, m, xValues)
    }

    def ksStatistic(f1: Uncertain[Double], n: Int, f2: Uncertain[Double], m: Int): Double = {
      require(n > 0, s"invalid sample size (n=$n), use one or more samples for a meaningful result")
      require(m > 0, s"invalid sample size (m=$m), use one or more samples for a meaningful result")

      val f1Samples = SortedMap.from(f1.histogram(sampleCount = n))
      val f2Samples = SortedMap.from(f2.histogram(sampleCount = m))

      ksStatisticImpl(f1Samples, n, f2Samples, m, xValues = f1Samples.keySet ++ f2Samples.keySet)
    }

    private def ksStatisticImpl(f1Samples: SortedMap[Double, Int], n: Int, f2Samples: SortedMap[Double, Int], m: Int, xValues: SortedSet[Double]) = {
      val maxDistance = xValues.map { x =>
        val sumAToX = f1Samples.rangeTo(x).map(_ * _).sum / n
        val sumBToX = f2Samples.rangeTo(x).map(_ * _).sum / m

        math.abs(sumAToX - sumBToX)
      }.max

      maxDistance
    }

    /** Decides if two distributions return the same results, within a given confidence threshold (set by `alpha`). A higher `alpha` value permits larger differences between
      * distributions.
      *
      * @see
      *   [[ksStatistic]] for performance notes around sample sizes.
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
      *   defines the range of X values used in the calculation
      * @param alpha
      *   value between 0 and 1 indicating allowable differences between distributions
      * @return
      */
    def ksTest(f1: Uncertain[Double], n: Int, f2: Uncertain[Double], m: Int, xValues: SortedSet[Double], alpha: Double): Boolean = {
      val statistic  = Uncertain.ksStatistic(f1, n, f2, m, xValues)
      val confidence = math.sqrt(-math.log(alpha / 2) * 0.5).min(0).max(0)

      // the point where we can't consider the two distributions equal
      val threshold = confidence * math.sqrt((n + m).toDouble / (n * m))

      statistic <= threshold
    }
}
