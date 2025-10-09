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

import scala.math.{abs, pow, sqrt}
import mostly.uncertaintee.syntax.*

class KurtosisSpec extends RngSuite {

  private val tolerance = 0.05

  // --- Helper Function ---

  /** Asserts that the excess kurtosis of a distribution matches the theoretical value. This function uses multiple smaller runs and averages the kurtosis to get a stable estimate,
    * avoiding the high variance of a single, large sample run.
    *
    * @param distribution
    *   The Uncertain distribution to test.
    * @param theoreticalKurtosis
    *   The expected theoretical excess kurtosis.
    * @param distName
    *   A string name for the distribution for clear test failure messages.
    * @param iterations
    *   The number of runs to average over.
    * @param samplesPerIteration
    *   The number of samples to take in each run.
    */
  private def assertKurtosis(
    distribution: Uncertain[Double],
    theoreticalKurtosis: Double,
    distName: String,
    iterations: Int = 500,
    samplesPerIteration: Int = 1000
  ): Unit = {
    val kurtosisEstimates = (1 to iterations).map { _ =>
      val samples      = distribution.take(samplesPerIteration)
      val sampleMean   = samples.sum / samplesPerIteration
      val sampleStdDev = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (samplesPerIteration - 1))
      // Check for zero std dev to avoid division by zero in rare cases of low sample count/variance
      if (sampleStdDev > 1e-9) {
        // The "- 3.0" calculates *excess* kurtosis
        (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / samplesPerIteration) - 3.0
      } else {
        0.0 // Kurtosis is undefined or zero if all samples are identical
      }
    }

    val averageKurtosis = kurtosisEstimates.sum / iterations
    val hint            =
      s"Average sample excess kurtosis for $distName ($averageKurtosis) over $iterations runs should be close to $theoreticalKurtosis."

    // With averaging, the tolerance can be reasonable, but it still needs to be somewhat generous for the 4th moment.
    assert(abs(averageKurtosis - theoreticalKurtosis) < tolerance * 15, hint)
  }

  // --- Kurtosis Tests for all Distributions ---

  rngTest("Normal distribution's sample excess kurtosis should be 0") {
    // By definition, the Normal distribution is the baseline for kurtosis (mesokurtic).
    val normal = Uncertain.normal(mean = 10, standardDeviation = 5)
    assertKurtosis(normal, 0.0, "Normal(10, 5)")
  }

  rngTest("Exponential distribution's sample excess kurtosis should be 6") {
    // The exponential distribution is leptokurtic (heavy-tailed).
    val exponential = Uncertain.exponential(rate = 1.0)
    assertKurtosis(exponential, 6.0, "Exponential(1.0)")
  }

  rngTest("Continuous Uniform distribution's sample excess kurtosis should be -1.2") {
    // The uniform distribution is platykurtic (light-tailed, box-shaped).
    val uniform = Uncertain.uniform(0.0, 10.0)
    assertKurtosis(uniform, -1.2, "Uniform(0, 10)")
  }

  rngTest("Poisson distribution's sample excess kurtosis should be 1/λ") {
    // The Poisson distribution's kurtosis depends on its rate parameter λ.
    val lambda  = 4.0
    val poisson = Uncertain.poisson(lambda).map(_.toDouble)
    assertKurtosis(poisson, 1.0 / lambda, s"Poisson($lambda)")
  }

  rngTest("Binomial distribution's sample excess kurtosis should follow its formula") {
    val trials      = 50
    val probability = 0.4
    val binomial    = Uncertain.binomial(trials, probability).map(_.toDouble)

    // Formula: (1 - 6p(1-p)) / (n*p(1-p))
    val theoreticalKurtosis =
      (1.0 - 6.0 * probability * (1.0 - probability)) / (trials * probability * (1.0 - probability))
    assertKurtosis(binomial, theoreticalKurtosis, s"Binomial($trials, $probability)")
  }

  rngTest("Bernoulli distribution's sample excess kurtosis should follow its formula") {
    // Bernoulli is a special case of Binomial with n=1. It is simple enough that it doesn't need the averaging helper.
    val p         = 0.3
    val bernoulli = Uncertain.bernoulli(p).map(if (_) 1.0 else 0.0)
    val samples   = bernoulli.take(100_000)

    // Formula: (1 - 6p(1-p)) / (p(1-p))
    val theoreticalKurtosis = (1.0 - 6.0 * p * (1.0 - p)) / (p * (1.0 - p))

    val mean           = samples.sum / samples.length
    val stdDev         = sqrt(samples.map(x => pow(x - mean, 2)).sum / (samples.length - 1))
    val sampleKurtosis = (samples.map(x => pow((x - mean) / stdDev, 4)).sum / samples.length) - 3.0

    val hint =
      s"Sample excess kurtosis ($sampleKurtosis) should be close to theoretical kurtosis ($theoreticalKurtosis) for Bernoulli(p=$p)."
    assert(
      cond = abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2,
      clue = hint
    )
  }
}
