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

/** Represents percentile values (100-quantiles).
  */
trait Percentiles[T] extends Quantiles[T] {

  override val quantileIntervals = 100

  /** Returns the value at the given percentile boundary.
    *
    * @param n
    *   percentile boundary index (0 to 100)
    * @return
    *   the value at the specified percentile boundary
    */
  override def apply(n: Int): T

  /** Represent as quartiles
    *
    * @return
    *   quartiles derived from this percentile distribution
    */
  def toQuartiles: Quartiles[T] = Quartiles[T](
    q0 = this.apply(0),
    q1 = this.apply(25),
    q2 = this.apply(50),
    q3 = this.apply(75),
    q4 = this.apply(100)
  )

  /** Represent as quintiles
    *
    * @return
    *   quintiles derived from this percentile distribution
    */
  def toQuintiles: Quintiles[T] = Quintiles[T](
    q0 = this.apply(0),
    q1 = this.apply(20),
    q2 = this.apply(40),
    q3 = this.apply(60),
    q4 = this.apply(80),
    q5 = this.apply(100)
  )

  /** Represent as Deciles
    *
    * @return
    *   deciles derived from this percentile distribution
    */
  def toDeciles: Deciles[T] = Deciles[T](
    d0 = this.apply(0),
    d1 = this.apply(10),
    d2 = this.apply(20),
    d3 = this.apply(30),
    d4 = this.apply(40),
    d5 = this.apply(50),
    d6 = this.apply(60),
    d7 = this.apply(70),
    d8 = this.apply(80),
    d9 = this.apply(90),
    d10 = this.apply(100)
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
  )(using ord: Ordering[T]): Percentiles[T] =
    Quantiles
      .ofSize[T](
        quantileIntervals = 100,
        uncertain = uncertain,
        sampleCount = sampleCount
      )(using ord)(_)
}
