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

package mostly.uncertaintee.squants.ops

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.squants.QuantityStatisticallyConvertible
import mostly.uncertaintee.syntax._
import squants.Quantity

import scala.util.Random

trait QuantityDistributionOps {

  extension (u: Uncertain.type) {

    /** Creates a normal (Gaussian) distribution for a quantity.
      *
      * @param mean
      *   The center/average value
      * @param stdDev
      *   The spread (standard deviation)
      * @param qs
      *   Typeclass instance for quantity statistics
      * @param random
      *   Random number generator
      * @return
      *   An uncertain quantity following a normal distribution
      */
    def normalQuantity[Q <: Quantity[Q]](
      mean: Q,
      stdDev: Q
    )(using qs: QuantityStatisticallyConvertible[Q], random: Random = new Random()): Uncertain[Q] = {
      require(qs.toDouble(stdDev) >= 0, "Standard deviation cannot be negative")

      // Reuse the existing normalDouble implementation
      u.normalDouble(
        mean = qs.toDouble(mean),
        standardDeviation = qs.toDouble(stdDev)
      )(using random)
        .map(qs.fromDouble)
    }

    /** Creates a uniform distribution for a quantity.
      *
      * All values between min and max are equally likely.
      *
      * @param min
      *   Minimum value (inclusive)
      * @param max
      *   Maximum value (inclusive)
      * @param qs
      *   Typeclass instance for quantity statistics
      * @param random
      *   Random number generator
      * @return
      *   An uncertain quantity following a uniform distribution
      */
    def uniformQuantity[Q <: Quantity[Q]](
      min: Q,
      max: Q
    )(using qs: QuantityStatisticallyConvertible[Q], random: Random = new Random()): Uncertain[Q] =
      u.uniformDouble(
        min = qs.toDouble(min),
        max = qs.toDouble(max)
      )(using random)
        .map(qs.fromDouble)

    /** Creates a triangular distribution for a quantity.
      *
      * Useful when you know the min, max, and most likely value. Common in risk analysis and project estimation.
      *
      * @param min
      *   Minimum possible value
      * @param peak
      *   Most likely value (peak of triangle)
      * @param max
      *   Maximum possible value
      * @param qs
      *   Typeclass instance for quantity statistics
      * @param random
      *   Random number generator
      * @return
      *   An uncertain quantity following a triangular distribution
      */
    def triangularQuantity[Q <: Quantity[Q]](
      min: Q,
      peak: Q,
      max: Q
    )(using qs: QuantityStatisticallyConvertible[Q], random: Random = new Random()): Uncertain[Q] =
      u.triangularViaDouble(
        min = qs.toDouble(min),
        peak = qs.toDouble(peak),
        max = qs.toDouble(max)
      )(using random)
        .map(qs.fromDouble)

    /** Creates a measurement with absolute error (normal distribution).
      *
      * This is syntactic sugar for a normal distribution centered at the measured value with the given error as
      * standard deviation.
      *
      * @param value
      *   The measured/nominal value
      * @param absoluteError
      *   The measurement error (standard deviation)
      * @param qs
      *   Typeclass instance for quantity statistics
      * @param random
      *   Random number generator
      * @return
      *   An uncertain quantity representing the measurement
      */
    def quantityMeasurementWithError[Q <: Quantity[Q]](
      value: Q,
      absoluteError: Q
    )(using qs: QuantityStatisticallyConvertible[Q], random: Random = new Random()): Uncertain[Q] =
      normalQuantity(
        mean = value,
        stdDev = absoluteError
      )

    /** Creates a measurement with relative error (percentage-based).
      *
      * Common in engineering where tolerances are specified as percentages (e.g., "100 mm ± 5%").
      *
      * @param value
      *   The measured/nominal value
      * @param percentError
      *   The relative error as a percentage (e.g., 5.0 for ±5%)
      * @param qs
      *   Typeclass instance for quantity statistics
      * @param random
      *   Random number generator
      * @return
      *   An uncertain quantity representing the measurement
      */
    def quantityMeasurementWithRelativeError[Q <: Quantity[Q]](
      value: Q,
      percentError: Double
    )(using qs: QuantityStatisticallyConvertible[Q], random: Random = new Random()): Uncertain[Q] = {
      require(percentError >= 0, "Percent error cannot be negative")
      val valueDouble = qs.toDouble(value)
      val errorDouble = math.abs(valueDouble * percentError / 100.0)
      normalQuantity(
        mean = value,
        stdDev = qs.fromDouble(errorDouble)
      )
    }
  }
}
