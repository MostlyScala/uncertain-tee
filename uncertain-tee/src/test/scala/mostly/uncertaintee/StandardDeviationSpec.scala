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

import mostly.uncertaintee.syntax.*
import munit.FunSuite

import scala.math.{abs, pow}

class StandardDeviationSpec extends RngSuite {

  private val sampleCountForTrials = 100 // Smallish sample size to highlight the effect of Bessel's correction.
  private val numTrials            = 10_000

  // --- Bessel's Correction Test ---

  rngTest("standardDeviation should provide a less biased estimate of the true population variance") {
    val populationMean   = 50.0
    val populationStdDev = 10.0
    val populationVar    = pow(populationStdDev, 2)
    val dist             = Uncertain.normal(populationMean, populationStdDev)

    // Bessel's correction uses a denominator of (n-1) instead of n when calculating variance.
    // This corrects for the bias introduced when estimating a population's variance from a sample.
    // The sample variance (using n-1) will, on average, be a better estimate of the true population variance.
    // ... I found this hard to wrap my head around, as a non-statistician, but see: https://en.wikipedia.org/wiki/Bessel%27s_correction

    var totalSampleVariance             = 0.0
    var totalPopulationVarianceOfSample = 0.0

    (1 to numTrials).foreach { _ =>
      val samples       = dist.take(sampleCountForTrials) // Draw a small sample from the true population.
      // We create a new 'Uncertain' from the fixed samples to calculate stats on that specific sample.
      val empiricalDist = Uncertain.empirical(samples)

      val sampleVar      = pow(empiricalDist.standardDeviation(sampleCountForTrials), 2)
      val popVarOfSample = pow(empiricalDist.populationStandardDeviation(sampleCountForTrials), 2)

      totalSampleVariance += sampleVar
      totalPopulationVarianceOfSample += popVarOfSample
    }

    val avgSampleVariance      = totalSampleVariance / numTrials
    val avgPopVarianceOfSample = totalPopulationVarianceOfSample / numTrials

    val sampleError     = abs(avgSampleVariance - populationVar)
    val populationError = abs(avgPopVarianceOfSample - populationVar)

    val hint =
      s"The error from the sample variance estimate ($sampleError) should be smaller than the error from the biased population variance of the sample ($populationError)."
    assert(sampleError < populationError, hint)
  }

  // --- Edge Case Tests ---

  rngTest("standardDeviation for a point distribution should be exactly 0") {
    val pointDist = Uncertain.point(42.0)
    // Even with Bessel's correction, a sample with no variation has a standard deviation of 0.
    val stdDev    = pointDist.standardDeviation(100)
    assertEquals(stdDev, 0.0, "Sample standard deviation of a constant value must be 0.")
  }

  // --- Input Validation Tests ---

  rngTest("standardDeviation should throw IllegalArgumentException for sample count less than 2") {
    val dist = Uncertain.normal(0, 1)
    // Bessel's correction requires n-1, so n must be at least 2 to avoid division by zero.
    intercept[IllegalArgumentException] {
      dist.standardDeviation(1)
    }
    intercept[IllegalArgumentException] {
      dist.standardDeviation(0)
    }
  }
}
