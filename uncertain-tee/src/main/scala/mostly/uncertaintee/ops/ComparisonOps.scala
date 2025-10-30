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

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.quantiles.Quantiles
import mostly.uncertaintee.syntax.*

import scala.language.implicitConversions
import scala.math.*
import scala.util.NotGiven

/** {{{
  *    import mostly.uncertaintee.syntax.comparison.*
  *    // or just import all the syntax (recommended)
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ComparisonOps {

  /** Equality comparisons for uncertain values. */
  extension [T](uncertain: Uncertain[T]) {

    /** Compares two uncertain values sample-by-sample. */
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
    def gt(value: T): Uncertain[Boolean]  = lhs.map(a => ord.gt(a, value))
    def lt(value: T): Uncertain[Boolean]  = lhs.map(a => ord.lt(a, value))
    def gte(value: T): Uncertain[Boolean] = lhs.map(a => ord.gteq(a, value))
    def lte(value: T): Uncertain[Boolean] = lhs.map(a => ord.lteq(a, value))

    def >(value: T): Uncertain[Boolean]  = gt(value)
    def <(value: T): Uncertain[Boolean]  = lt(value)
    def >=(value: T): Uncertain[Boolean] = gte(value)
    def <=(value: T): Uncertain[Boolean] = lte(value)

    def gt(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.gt(lhsSample, rhsSample)

    def lt(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.lt(lhsSample, rhsSample)

    def gte(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.gteq(lhsSample, rhsSample)

    def lte(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.lteq(lhsSample, rhsSample)

    def >(other: Uncertain[T]): Uncertain[Boolean]  = gt(other)
    def <(other: Uncertain[T]): Uncertain[Boolean]  = lt(other)
    def >=(other: Uncertain[T]): Uncertain[Boolean] = gte(other)
    def <=(other: Uncertain[T]): Uncertain[Boolean] = lte(other)
  }

  // ============== approximate equality ======================

  /** Type class for approximate equality of `T` via a double-valued distance metric.
    *
    * Provides a way to compare values that may not be exactly equal but are "close enough" according to some numeric measure.
    */
  trait ApproxEq[T] {

    /** Computes a numeric distance between `a` and `b`.
      *
      * The distance is interpreted as "how far apart" the values are. Negative or positive values do not matter; only the magnitude is used.
      *
      * @param a
      *   first value
      * @param b
      *   second value
      * @return
      *   numeric distance between `a` and `b`
      */
    def distanceBetween(a: T, b: T): Double

    def absoluteDistanceBetween(a: T, b: T): Double = abs(distanceBetween(a, b))

    /** Checks approximate equality of `a` and `b`.
      *
      * If a threshold is provided, returns true if the distance between `a` and `b` is less than or equal to the threshold. Otherwise, falls back to exact equality.
      *
      * @param a
      *   first value
      * @param b
      *   second value
      * @param threshold
      *   optional numeric tolerance
      * @return
      *   true if `a` and `b` are approximately equal
      */
    def approxEq(a: T, b: T, threshold: Double = 0.0): Boolean = absoluteDistanceBetween(a, b) <= threshold
  }

  object ApproxEq {

    /** Allows summoning an implicit ApproxEq when one exists in implicit scope without summon[..]
      *
      * {{{
      *   import somewhere.over.the.rainbow.given // let's say this has an implicit ApproxEq[Boolean] defined
      *
      *   def myLogic: Boolean = {
      *       // at compile time, this `ApproxEq[Boolean]` will be replaced with the actual instance. We've 'summoned' the approxEq.
      *      val distance = ApproxEq[Boolean].absoluteDistanceBetween(true,false)
      *      println(s"distance is $distance")
      *      ApproxEq[Boolean].approxEq(true,false)
      *   }
      * }}}
      */
    def apply[T](using approxEq: ApproxEq[T]): ApproxEq[T] = approxEq
  }

  /** Provides approximate equality for non-numeric Quantiles. Uses "closest boundary" matching.
    *
    * If `T` is known to be numeric, the correct implicit to pick is [[quantileApproxEqInstanceNumeric]], since it can linearly interpolate between the values of T (this assumes
    * discrete Ts, picked from the closest quantile boundary).
    *
    * Accuraccy is better if a and b have similar quantile count.
    */
  implicit private def quantileApproxEqDiscrete[T](using ApproxEq[T], NotGiven[Numeric[T]]): ApproxEq[Quantiles[T]] =
    (a, b) => {
      val steps = math.max(a.quantileIntervals, b.quantileIntervals)

      def quantAt(q: Quantiles[T], p: Double): T = {
        val n: Double          = q.quantileIntervals.toDouble
        val idealIndex: Double = p * n
        val index: Int         = math.round(idealIndex).toInt
        q.quantile(math.min(math.max(index, 0), q.quantileIntervals))
      }

      (0 to steps).map { i =>
        val p    = i.toDouble / steps
        val dist = ApproxEq[T].distanceBetween(quantAt(a, p), quantAt(b, p))
        math.abs(dist)
      }.max
    }

  /** Provides high-accuracy approximate equality for numeric Quantiles. Uses linear interpolation by converting T values to Double. */
  implicit private def quantileApproxEqInstanceNumeric[T](using num: Numeric[T]): ApproxEq[Quantiles[T]] =
    (a, b) => {
      val steps = math.max(a.quantileIntervals, b.quantileIntervals)

      /** Linearly interpolates the value at percentile p, returning as Double. */
      def quantAt(q: Quantiles[T], p: Double): Double = {
        val n                  = q.quantileIntervals
        val idealIndex: Double = p * n
        val lowerIndex         = math.floor(idealIndex).toInt
        val upperIndex         = math.ceil(idealIndex).toInt

        if (lowerIndex == upperIndex) {
          // p maps exactly to a boundary
          num.toDouble(q.quantile(lowerIndex))
        } else {
          // Get boundary values as Doubles
          val lowerValue_d = num.toDouble(q.quantile(lowerIndex))
          val upperValue_d = num.toDouble(q.quantile(upperIndex))

          // 'weight' is how far p is between the two boundaries
          val weight = idealIndex - lowerIndex

          // Linearly interpolate in Double-space
          lowerValue_d + (upperValue_d - lowerValue_d) * weight
        }
      }

      (0 to steps).map { i =>
        val p    = i.toDouble / steps
        val dist = quantAt(a, p) - quantAt(b, p)
        math.abs(dist)
      }.max
    }

  extension [T](lhs: Quantiles[T])(using ApproxEq[T]) {

    /** max distance between the quantiles is <= threshold */
    def approxEq(rhs: Quantiles[T], threshold: Double): Boolean = ApproxEq[Quantiles[T]].approxEq(lhs, rhs, threshold)

    /** max distance between the quantiles. Error rate is lowest when quantilecount is close to (if not) equivalent. */
    def maxDistanceTo(rhs: Quantiles[T]): Double = ApproxEq[Quantiles[T]].absoluteDistanceBetween(lhs, rhs)
  }

  extension [T](lhs: Uncertain[T])(using Ordering[T], ApproxEq[T]) {

    /** Samples each distribution `sampleCount` times to construct percentiles, then returns true if max distance between the resulting quantile distributions is within threshold.
      */
    def approxEq(rhs: Uncertain[T], threshold: Double, sampleCount: Int): Boolean =
      lhs.percentiles(sampleCount).approxEq(rhs.percentiles(sampleCount), threshold)

    /** Samples each distribution `sampleCount` times to construct percentiles, then returns max distance between the resulting quantile distributions */
    def maxDistanceTo(rhs: Uncertain[T], sampleCount: Int): Double = {
      val lhsPercentiles = lhs.percentiles(sampleCount)
      val rhsPercentiles = rhs.percentiles(sampleCount)
      lhsPercentiles.maxDistanceTo(rhsPercentiles)
    }
  }

  // ---------------- public library-provided instances ------------------------

  /** Unit is always the same as unit
    *
    * @see
    *   [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]]
    */
  implicit val unitApproxEq: ApproxEq[Unit] = (_, _) => 0

  /**   - (true,true) => 0
    *   - (false,false) => 0
    *   - (true,false) => 1
    *   - (false,true) => 1
    *
    * @see
    *   [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]]
    */
  implicit val boolApproxEq: ApproxEq[Boolean] = (a, b) => if (a == b) 0.0 else 1.0

  /** Numeric approximate equality via `Numeric[T]` conversion to double.
    *
    * @see
    *   [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]]
    */
  implicit def numericApproxEq[T](using num: Numeric[T]): ApproxEq[T] = (a, b) => num.toDouble(num.minus(a, b))

}
