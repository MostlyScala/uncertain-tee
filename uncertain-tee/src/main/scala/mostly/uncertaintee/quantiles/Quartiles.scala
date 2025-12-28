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

  override val quantileIntervals: Int = 4

  /** The median value (2nd quartile)
    *
    * @return
    *   the median value (identical to [[q2]] )
    */
  def median: T = q2

  /** Returns the value at the given Quartile boundary.
    *
    * @param n
    *   percentile boundary index (0 to 5)
    * @return
    *   the value at the specified percentile boundary
    */
  override def apply(n: Int): T = n match {
    case 0         => q0
    case 1         => q1
    case 2         => q2
    case 3         => q3
    case 4         => q4
    case otherwise => throw new IllegalArgumentException(s"Quartile must be in range (0 to 4), was: $otherwise")
  }
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
      quantileIntervals = 4,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Quartiles[T](
      q0 = quantiles(0),
      q1 = quantiles(1),
      q2 = quantiles(2),
      q3 = quantiles(3),
      q4 = quantiles(4)
    )
  }
}
