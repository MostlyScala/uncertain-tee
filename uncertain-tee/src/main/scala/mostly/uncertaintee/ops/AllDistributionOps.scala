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

import mostly.uncertaintee.*
import mostly.uncertaintee.ops.distribution.*

import scala.util.Random

/** //TODO extensive scaladocs
  *
  * Constructors ("factory methods") for statistical distributions.
  *
  * {{{
  * import mostly.uncertaintee.syntax.distribution.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait AllDistributionOps
    extends DistributionOpsBigDecimal
    with DistributionOpsBigInt
    with DistributionOpsBoolean
    with DistributionOpsByte
    with DistributionOpsDouble
    with DistributionOpsInt
    with DistributionOpsLong
    with DistributionOpsShort {

  extension (u: Uncertain.type) {

    /** Creates an uncertain value that's always the same (no uncertainty).
      *
      * @param value
      *   The constant value to always return
      * @return
      *   An uncertain value that always samples to the given value
      */
    def point[T](value: T)(using random: Random = new Random()): Uncertain[T] = Uncertain(() => value)(using random)

    def mixture[T](components: Map[Uncertain[T], Double])(using random: Random = new Random()): Uncertain[T] =
      Uncertain.mixtureViaDouble(components)

    /** Creates a mixture where all components have equal weight.
      *
      * @param components
      *   uncertain values to mix with equal probability
      * @param random
      *   Random number generator to use for mixture selection
      * @return
      *   An uncertain value that samples uniformly from the given components
      */
    def equalMixture[T](components: Iterable[Uncertain[T]])(using random: Random = new Random()): Uncertain[T] = {
      require(components.nonEmpty, "Need at least one component for equal mixture.")
      Uncertain.mixture(components.map(x => x -> One).toMap)(using random)
    }

    def empirical[T](data: Iterable[T])(using random: Random = new Random()): Uncertain[T] = {
      require(data.nonEmpty, "Need at least one data point for empirical distribution.")
      val indexedData = data.toVector
      Uncertain(() => indexedData(random.nextInt(indexedData.length)))
    }

    def categorical[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] =
      Uncertain.categoricalViaDouble(outcomes)(using random)

    def normal(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.normalDouble(mean, standardDeviation)(using random)

    def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.uniformDouble(min, max)(using random)

    def triangular(min: Double, peak: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.triangularViaDouble(min, peak, max)(using random)

    def exponential(rate: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.exponentialViaDouble(rate)(using random)

    def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] =
      Uncertain.bernoulliViaDouble(probability)(using random)

    def kumaraswamy(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.kumaraswamyViaDouble(a, b)(using random)

    def rayleigh(scale: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.rayleighViaDouble(scale)(using random)

    def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.binomialViaDouble(trials, probability)(using random)

    def poisson(lambda: Double)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.poissonViaDouble(lambda)(using random)

  }
}
