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
  override val quantileIntervals = 5

  /** Returns the value at the given Quintile boundary.
   *
   * @param n
   * percentile boundary index (0 to 5)
   * @return
   * the value at the specified percentile boundary
   */
  override def apply(n: Int): T = n match {
    case 0         => q0
    case 1         => q1
    case 2         => q2
    case 3         => q3
    case 4         => q4
    case 5         => q5
    case otherwise => throw new IllegalArgumentException(s"Quintile has to be in range (0 to 5), was: $otherwise")
  }

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
      quantileIntervals = 5,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Quintiles[T](
      q0 = quantiles(0),
      q1 = quantiles(1),
      q2 = quantiles(2),
      q3 = quantiles(3),
      q4 = quantiles(4),
      q5 = quantiles(5)
    )
  }
}
