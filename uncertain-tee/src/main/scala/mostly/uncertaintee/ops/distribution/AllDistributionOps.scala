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

package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.*

import scala.util.Random

/** Constructors ("factory methods") for statistical distributions.
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

    /** Creates a Beta distribution.
      *
      * The Beta distribution is a continuous probability distribution on [0,1] parameterized by two positive shape parameters. It is widely used to model random variables limited
      * to intervals of finite length in Bayesian statistics.
      *
      * @see
      *   https://en.wikipedia.org/wiki/Beta_distribution
      *
      * @param alpha
      *   The first shape parameter (must be positive and finite). Higher values shift the distribution right.
      * @param beta
      *   The second shape parameter (must be positive and finite). Higher values shift the distribution left.
      * @param random
      *   Random number generator to use for sampling.
      * @return
      *   An uncertain value following a Beta distribution on the interval [0,1].
      * @note
      *   Common special cases:
      *   - Beta(1,1) is uniform on [0,1]
      *   - Beta(α,α) is symmetric around 0.5
      *   - Beta(0.5,0.5) is U-shaped (concentrated near 0 and 1)
      *   - Beta(2,5) is left-skewed, Beta(5,2) is right-skewed
      * @note
      *   Implementation uses the Gamma ratio method: if X ~ Gamma(α,1) and Y ~ Gamma(β,1), then X/(X+Y) ~ Beta(α,β). Gamma samples are generated using the Marsaglia-Tsang method.
      * @example
      *   {{{
      * // Symmetric distribution around 0.5
      * val symmetric = Uncertain.betaViaDouble(2.0, 2.0)
      *
      * // Right-skewed distribution
      * val rightSkewed = Uncertain.betaViaDouble(5.0, 2.0)
      *
      * // U-shaped distribution (concentrated at extremes)
      * val uShaped = Uncertain.betaViaDouble(0.5, 0.5)
      *   }}}
      */
    def beta(alpha: Double, beta: Double)(using random: Random = new Random()): Uncertain[Double] = Uncertain.betaViaDouble(alpha, beta)(using random)

    def categorical[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] =
      Uncertain.categoricalViaDouble(outcomes)(using random)

    def normal(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.normalDouble(mean, standardDeviation)(using random)

    def gaussian(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.normalDouble(mean, standardDeviation)(using random)

    def geometric(probability: Double)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.geometricViaDouble(probability)(using random)

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

    def negativeBinomial(
      r: Int,
      probability: Double
    )(using random: Random = new Random()): Uncertain[Int] = Uncertain.negativeBinomialInt(r, probability)

    def poisson(lambda: Double)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.poissonViaDouble(lambda)(using random)

  }
}
