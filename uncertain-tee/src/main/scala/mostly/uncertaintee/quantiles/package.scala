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

import scala.math.floor

package object quantiles {

  /** Represents N-quantile values.
    *
    * For common use cases see:
    *
    *   - [[Tertiles]] ( construct from [[Uncertain]] with [[Quantiles.tertiles]] )
    *   - [[Quartiles]] ( construct from [[Uncertain]] with [[Quantiles.quartiles]] )
    *   - [[Quintiles]] ( construct from [[Uncertain]] with [[Quantiles.quintiles]]
    *   - [[Deciles]] ( construct from [[Uncertain]] with [[Quantiles.deciles]] )
    *   - [[Percentiles]] ( construct from [[Uncertain]] with [[Quantiles.percentiles]] )
    */
  trait Quantiles[T] {

    /** Represents the amount of quantities this [[Quantile]]-implementation represents */
    def n: Int

    /** Returns the value at the given quantile boundary.
      *
      * @param index
      *   quantile boundary index (0 to [[n]]th quantile)
      * @return
      *   the value at the specified quantile boundary
      */
    def quantile(index: Int): T

    /** Maximum value (Nth quantile boundary)
      *
      * @return
      *   the maximum value (equivalent to the nth quantile)
      */
    def max: T = quantile(n)

    /** Minimum value (0th quantile boundary)
      *
      * @return
      *   the minimum value (equivalent to the 0th quantile)
      */
    def min: T = quantile(0)

    /** As Sequence of quantiles up to [[n]]th quantile (including the zero-quantile) - e.g. for [[n]] == 5, this will return:
      *   - quantile0 == min (0%)
      *   - quantile1 == 20%
      *   - quantile2 == 40%
      *   - quantile3 == 60%
      *   - quantile4 == 80%
      *   - quantile5 == max (100%)
      *
      * @param includeMin
      *   whether to include the minimum (0th quantile) in the sequence
      * @param includeMax
      *   whether to include the maximum ([[n]]th quantile) in the sequence
      * @return
      *   sequence of quantile values based on inclusion parameters
      */
    def toSeq(includeMin: Boolean = true, includeMax: Boolean = true): Seq[T] =
      (includeMin, includeMax) match {
        case (true, true)   => (0 to n).map(quantile)
        case (true, false)  => (0 until n).map(quantile)
        case (false, true)  => (1 to n).map(quantile)
        case (false, false) => (1 until n).map(quantile)
      }
  }

  object Quantiles {

    /** Computes tertiles (3 way splits) from an uncertain value
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   [[Tertiles]] computed from the samples
      */
    def tertiles[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Tertiles[T] =
      Tertiles.fromUncertain[T](
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

    /** Computes quartiles from an uncertain value
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   [[Quartiles]] computed from the samples
      */
    def quartiles[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Quartiles[T] =
      Quartiles.fromUncertain[T](
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

    /** Computes quintiles (5 way splits) from an uncertain value
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   [[Tertiles]] computed from the samples
      */
    def quintiles[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Quintiles[T] =
      Quintiles.fromUncertain[T](
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

    /** Computes deciles from an uncertain value
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   [[Deciles]] computed from the samples
      */
    def deciles[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Deciles[T] =
      Deciles.fromUncertain[T](
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

    /** Computes percentiles from an uncertain value
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   [[Percentiles]] computed from the samples
      */
    def percentiles[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Percentiles[T] =
      Percentiles.fromUncertain[T](
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

    /** Computes quantiles (by sampling and sorting the samples)
      *
      * See also [[quartiles]], [[deciles]] and [[percentiles]] for common quantiles.
      *
      * @param n
      *   the number of quantiles (N).
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   a Quantiles instance with exact quantile values
      */
    def ofSize[T](
      n: Int,
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Quantiles[T] = {
      require(n > 0, s"quantile size must be > 0, was: $n")
      require(sampleCount > 0, s"sampleCount must be positive, was: $sampleCount")
      val sortedSamples: List[T]        = uncertain.take(sampleCount).sorted
      val capturedN                     = n
      val quantileValues: IndexedSeq[T] = (0 to n).map { i =>
        val proportion = i.toDouble / n
        val idx        = floor(proportion * (sampleCount - 1)).toInt
        sortedSamples(idx)
      }

      new Quantiles[T] {
        override val n: Int                  = capturedN
        override def quantile(index: Int): T = {
          require(index >= 0 && index <= n, s"quantile index must be in range 0 to $n, was: $index")
          quantileValues(index)
        }
      }
    }
  }

  /** Represents Tertile values (a 3-way split, 3-quantities)
    *
    * @param t0
    *   0th tertile (minimum value encountered, 0%)
    * @param t1
    *   1st tertile (1/3rd, 33.3% repeating)
    * @param t2
    *   2nd tertile (2/3rd, 66.6% repeating)
    * @param t3
    *   3rd tertile (3/3rd, maximum, 99.9% repeating, 100%)
    */
  final case class Tertiles[T](
    t0: T,
    t1: T,
    t2: T,
    t3: T
  ) extends Quantiles[T] {

    override val n: Int = 3

    def tertile(n: Int): T = n match {
      case 0         => t0
      case 1         => t1
      case 2         => t2
      case 3         => t3
      case otherwise => throw new IllegalArgumentException(s"Tertile has to be in range (0 to 3), was: $otherwise")
    }

    override def quantile(n: Int): T = tertile(n)
  }

  object Tertiles {

    /** Computes Tertiles (3 way split)
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   quartiles computed from the samples
      */
    def fromUncertain[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Tertiles[T] = {

      val quantiles = Quantiles.ofSize[T](
        n = 3,
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

      Tertiles[T](
        t0 = quantiles.quantile(0),
        t1 = quantiles.quantile(1),
        t2 = quantiles.quantile(2),
        t3 = quantiles.quantile(3)
      )
    }
  }

  /** Represents quartile values (4-quantiles)
    *
    * @param q0
    *   0th quartile (minimum, 0%)
    * @param q1
    *   1st quartile (25%)
    * @param q2
    *   2nd quartile (median, 50%)
    * @param q3
    *   3rd quartile (75%)
    * @param q4
    *   4th quartile (maximum, 100%)
    */
  final case class Quartiles[T](
    q0: T,
    q1: T,
    q2: T,
    q3: T,
    q4: T
  ) extends Quantiles[T] {

    override val n: Int = 4

    /** The median value (2nd quartile)
      *
      * @return
      *   the median value (identical to [[q2]] )
      */
    def median: T = q2

    /** Returns the value at the given quartile boundary.
      *
      * Method exists for parity with [[Quantiles.quantile]], [[Deciles.decile]] and [[Percentiles.percentile]]; it may be easier to just pattern match Quartiles itself, depending
      * on your usage.
      *
      * @param n
      *   quartile boundary index (0 to 4)
      * @return
      *   the value at the specified quartile boundary
      */
    def quartile(n: Int): T = n match {
      case 0         => q0
      case 1         => q1
      case 2         => q2
      case 3         => q3
      case 4         => q4
      case otherwise => throw new IllegalArgumentException(s"Quartile must be in range (0 to 4), was: $otherwise")
    }

    override def quantile(n: Int): T = quartile(n)
  }

  object Quartiles {

    /** Computes Quartiles
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   quartiles computed from the samples
      */
    def fromUncertain[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Quartiles[T] = {

      val quantiles = Quantiles.ofSize[T](
        n = 4,
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

      Quartiles[T](
        q0 = quantiles.quantile(0),
        q1 = quantiles.quantile(1),
        q2 = quantiles.quantile(2),
        q3 = quantiles.quantile(3),
        q4 = quantiles.quantile(4)
      )
    }
  }

  /** Represents Quantile values (a 5-way split, 5-quantities)
    *
    * @param q0
    *   0th quintile (min, 0%)
    * @param q1
    *   1st quintile (min, 20%)
    * @param q2
    *   2nd quintile (min, 40%)
    * @param q3
    *   3rd quintile (min, 60%)
    * @param q4
    *   4th quintile (min, 80%)
    * @param q5
    *   5th quintile (max, 100%)
    */
  final case class Quintiles[T](
    q0: T,
    q1: T,
    q2: T,
    q3: T,
    q4: T,
    q5: T
  ) extends Quantiles[T] {
    override val n = 5

    def quintile(n: Int): T = n match {
      case 0         => q0
      case 1         => q1
      case 2         => q2
      case 3         => q3
      case 4         => q4
      case 5         => q5
      case otherwise => throw new IllegalArgumentException(s"Quintile has to be in range (0 to 5), was: $otherwise")
    }

    override def quantile(n: Int): T = quintile(n)

  }

  object Quintiles {

    /** Computes Quintiles (5 way splits)
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   quintiles computed from the samples
      */
    def fromUncertain[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Quintiles[T] = {

      val quantiles = Quantiles.ofSize[T](
        n = 5,
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

      Quintiles[T](
        q0 = quantiles.quantile(0),
        q1 = quantiles.quantile(1),
        q2 = quantiles.quantile(2),
        q3 = quantiles.quantile(3),
        q4 = quantiles.quantile(4),
        q5 = quantiles.quantile(5)
      )
    }
  }

  /** Represents decile values (10-quantiles)
    *
    * @param d0
    *   0th decile (minimum, 0%)
    * @param d1
    *   1st decile (10%)
    * @param d2
    *   2nd decile (20%)
    * @param d3
    *   3rd decile (30%)
    * @param d4
    *   4th decile (40%)
    * @param d5
    *   5th decile (median, 50%)
    * @param d6
    *   6th decile (60%)
    * @param d7
    *   7th decile (70%)
    * @param d8
    *   8th decile (80%)
    * @param d9
    *   9th decile (90%)
    * @param d10
    *   10th decile (maximum, 100%)
    */
  final case class Deciles[T](
    d0: T,
    d1: T,
    d2: T,
    d3: T,
    d4: T,
    d5: T,
    d6: T,
    d7: T,
    d8: T,
    d9: T,
    d10: T
  ) extends Quantiles[T] {

    override val n: Int = 10

    val median: T = d5

    /** Returns the value at the given decile boundary.
      *
      * @param n
      *   decile boundary index (0 to 10)
      * @return
      *   the value at the specified decile boundary
      */
    def decile(n: Int): T = n match {
      case 0         => d0
      case 1         => d1
      case 2         => d2
      case 3         => d3
      case 4         => d4
      case 5         => d5
      case 6         => d6
      case 7         => d7
      case 8         => d8
      case 9         => d9
      case 10        => d10
      case otherwise => throw new IllegalArgumentException(s"Deciles are in range (0 to 10), was $otherwise")
    }

    override def quantile(n: Int): T = decile(n)
  }

  object Deciles {

    /** Computes Deciles
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   deciles computed from the samples
      */
    def fromUncertain[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Deciles[T] = {

      val quantiles = Quantiles.ofSize[T](
        n = 10,
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)

      Deciles[T](
        d0 = quantiles.quantile(0),
        d1 = quantiles.quantile(1),
        d2 = quantiles.quantile(2),
        d3 = quantiles.quantile(3),
        d4 = quantiles.quantile(4),
        d5 = quantiles.quantile(5),
        d6 = quantiles.quantile(6),
        d7 = quantiles.quantile(7),
        d8 = quantiles.quantile(8),
        d9 = quantiles.quantile(9),
        d10 = quantiles.quantile(10)
      )
    }
  }

  /** Represents percentile values (100-quantiles) */
  trait Percentiles[T] extends Quantiles[T] {

    override val n = 100

    /** Returns the value at the given percentile boundary.
      *
      * @param n
      *   percentile boundary index (0 to 100)
      * @return
      *   the value at the specified percentile boundary
      */
    def percentile(n: Int): T = quantile(n)

    /** Represent as quartiles
      *
      * @return
      *   quartiles derived from this percentile distribution
      */
    def toQuartiles: Quartiles[T] = Quartiles[T](
      q0 = percentile(0),
      q1 = percentile(25),
      q2 = percentile(50),
      q3 = percentile(75),
      q4 = percentile(100)
    )

    /** Represent as quintiles
      *
      * @return
      *   quintiles derived from this percentile distribution
      */
    def toQuintiles: Quintiles[T] = Quintiles[T](
      q0 = percentile(0),
      q1 = percentile(20),
      q2 = percentile(40),
      q3 = percentile(60),
      q4 = percentile(80),
      q5 = percentile(100)
    )

    /** Represent as Deciles
      *
      * @return
      *   deciles derived from this percentile distribution
      */
    def toDeciles: Deciles[T] = Deciles[T](
      d0 = percentile(0),
      d1 = percentile(10),
      d2 = percentile(20),
      d3 = percentile(30),
      d4 = percentile(40),
      d5 = percentile(50),
      d6 = percentile(60),
      d7 = percentile(70),
      d8 = percentile(80),
      d9 = percentile(90),
      d10 = percentile(100)
    )
  }

  object Percentiles {

    /** Computes percentiles
      *
      * @param uncertain
      *   the uncertain value to sample from
      * @param sampleCount
      *   number of samples to draw
      * @param ord
      *   implicit ordering for type T
      * @return
      *   percentiles computed from the samples
      */
    def fromUncertain[T](
      uncertain: Uncertain[T],
      sampleCount: Int
    )(using ord: Ordering[T]): Percentiles[T] = {
      val underlying = Quantiles.ofSize[T](
        n = 100,
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)
      (n: Int) => underlying.quantile(n)
    }
  }
}
