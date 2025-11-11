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

  override val quantileIntervals: Int = 3

  /** Returns the value at the given Tertile boundary.
    *
    * @param n
    *   percentile boundary index (0 to 3)
    * @return
    *   the value at the specified percentile boundary
    */
  override def apply(n: Int): T = n match {
    case 0         => t0
    case 1         => t1
    case 2         => t2
    case 3         => t3
    case otherwise => throw new IllegalArgumentException(s"Tertile has to be in range (0 to 3), was: $otherwise")
  }

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
      quantileIntervals = 3,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Tertiles[T](
      t0 = quantiles(0),
      t1 = quantiles(1),
      t2 = quantiles(2),
      t3 = quantiles(3)
    )
  }
}
