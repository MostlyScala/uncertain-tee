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

import scala.math.*
import scala.util.Random

trait DistributionOpsInt {
  extension (u: Uncertain.type) {

    /** Creates a uniform distribution of Integers. */
    def uniformInt(minInclusive: Int, maxExclusive: Int)(using random: Random = new Random()): Uncertain[Int] = {
      require(maxExclusive >= minInclusive, s"max ($maxExclusive) must be >= min ($minInclusive).")
      if (minInclusive == maxExclusive) Uncertain.always(minInclusive)
      else Uncertain(() => random.between(minInclusive, maxExclusive))
    }

    /** Creates a binomial distribution. */
    def binomialInt(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(trials >= 0, "Number of trials cannot be negative.")
      require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
    }

    /** Creates a Negative Binomial distribution.
      *
      * It counts the number of failures before the r-th success
      *
      * This distribution models the number of failures encountered before achieving a specified number of successes (`r`) in a sequence of independent (bernoulli) trials, each
      * with probability of success `p`.
      *
      * See: https://en.wikipedia.org/wiki/Negative_binomial_distribution
      *
      * @param r
      *   The number of successes required (must be > 0).
      * @param probability
      *   The probability of success on each trial (must be in (0, 1]).
      * @param random
      *   Random number generator to use for sampling.
      * @return
      *   An `Uncertain[Int]` representing the number of failures.
      * @note
      *   Runtime: O(r) due to summing r Geometric samples.
      */
    def negativeBinomialInt(r: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(r > 0, "Number of successes (r) must be positive.")
      require(probability > 0 && probability <= 1, s"Probability of success (p) must be in (0, 1], got $probability.")
      if (probability == 1.0) {
        // If success is guaranteed, there are always 0 failures.
        Uncertain.always(0)
      } else {
        Uncertain { () =>
          var totalFailures = 0

          // Sum r independent geometric random variables (failures before each success)
          var i = 0
          while (i < r) {
            // Inverse transform for geometric distribution
            val u               = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
            val trialsToSuccess = math.ceil(math.log(u) / math.log(1.0 - probability)).toInt
            totalFailures += (trialsToSuccess - 1) // Convert trials to failures
            i += 1
          }

          totalFailures
        }
      }
    }

    /** Creates a Poisson distribution. */
    def poissonInt(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(lambda >= 0, "Lambda (average rate) cannot be negative.")
      if (lambda == Zero) {
        Uncertain.always(0)
      } else {
        Uncertain { () =>
          val L = exp(-lambda)
          var k = 0
          var p = One
          while (p > L) {
            k += 1
            p *= random.nextDouble()
          }
          k - 1
        }
      }
    }
  }
}
