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

import scala.math.{abs, log}

class ConfidenceIntervalSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Standard Distribution Tests ---

  rngTest("confidenceInterval for a standard normal distribution should approximate the theoretical Z-scores") {
    val confidence = 0.95
    val normalDist = Uncertain.normal(0.0, 1.0)

    // For a standard normal distribution (mean=0, stddev=1), the 95% CI is defined by the z-scores
    // for the 2.5th and 97.5th percentiles.
    // The Z-score for the 97.5th percentile is approximately 1.96.
    // See: https://en.wikipedia.org/wiki/1.96
    val theoreticalLower = -1.96
    val theoreticalUpper = 1.96

    val (sampleLower, sampleUpper) = normalDist.confidenceInterval(confidence, sampleCount)

    assert(
      cond = abs(sampleLower - theoreticalLower) < tolerance,
      clue = s"Sample lower bound ($sampleLower) should be close to theoretical bound ($theoreticalLower) for 95% CI of N(0,1)."
    )
    assert(
      cond = abs(sampleUpper - theoreticalUpper) < tolerance,
      clue = s"Sample upper bound ($sampleUpper) should be close to theoretical bound ($theoreticalUpper) for 95% CI of N(0,1)."
    )
  }

  rngTest("confidenceInterval for a continuous uniform distribution should be symmetric around the midpoint") {
    val confidence  = 0.80
    val min         = 0.0
    val max         = 10.0
    val uniformDist = Uncertain.uniform(min, max)

    // For a uniform distribution U(a, b) and confidence C, the interval is [a + (1-C)/2 * (b-a), b - (1-C)/2 * (b-a)].
    val alpha            = 1.0 - confidence                  // alpha = 0.20
    val theoreticalLower = min + (alpha / 2.0) * (max - min) // 0 + 0.1 * 10 = 1.0
    val theoreticalUpper = max - (alpha / 2.0) * (max - min) // 10 - 0.1 * 10 = 9.0

    val (sampleLower, sampleUpper) = uniformDist.confidenceInterval(confidence, sampleCount)

    assert(
      cond = abs(sampleLower - theoreticalLower) < tolerance,
      clue = s"Sample lower bound ($sampleLower) should be close to theoretical bound ($theoreticalLower) for 80% CI of U(0,10)."
    )
    assert(
      cond = abs(sampleUpper - theoreticalUpper) < tolerance,
      clue = s"Sample upper bound ($sampleUpper) should be close to theoretical bound ($theoreticalUpper) for 80% CI of U(0,10)."
    )
  }

  rngTest("confidenceInterval for a skewed (Exponential) distribution should be asymmetric") {
    val confidence      = 0.95
    val rate            = 1.0
    val exponentialDist = Uncertain.exponential(rate)

    // The quantile function (inverse CDF) for an exponential distribution is Q(p) = -ln(1-p) / rate.
    // We need the quantiles for p=0.025 and p=0.975 to find the bounds of the 95% CI.
    // See: https://en.wikipedia.org/wiki/Quantile_function#Exponential_distribution
    val alpha            = 1.0 - confidence
    val theoreticalLower = -log(1.0 - (alpha / 2.0)) / rate // -ln(0.975) / 1.0 ≈ 0.0253
    val theoreticalUpper = -log(alpha / 2.0) / rate         // -ln(0.025) / 1.0 ≈ 3.6889

    val (sampleLower, sampleUpper) = exponentialDist.confidenceInterval(confidence, sampleCount)

    // The upper tail of a skewed distribution is harder to estimate, so we use a slightly larger tolerance.
    assert(
      cond = abs(sampleLower - theoreticalLower) < tolerance,
      clue = s"Sample lower bound ($sampleLower) should be close to theoretical bound (${"%.4f".format(theoreticalLower)}) for 95% CI of Exp(1)."
    )
    assert(
      cond = abs(sampleUpper - theoreticalUpper) < tolerance * 2,
      clue = s"Sample upper bound ($sampleUpper) should be close to theoretical bound (${"%.4f".format(theoreticalUpper)}) for 95% CI of Exp(1)."
    )
  }

  // --- Edge Case Tests ---

  rngTest("confidenceInterval with high confidence (99.9%) should produce a wider interval than a 95% CI") {
    val highConfidence = 0.999
    val normalDist     = Uncertain.normal(0.0, 1.0)

    // Theoretical Z-score for 99.9% confidence is approx ±3.291
    val (highConfLower, highConfUpper) = normalDist.confidenceInterval(highConfidence, sampleCount)
    val (midConfLower, midConfUpper)   = normalDist.confidenceInterval(0.95, sampleCount)

    assert(
      cond = highConfLower < midConfLower,
      clue = s"99.9% CI lower bound ($highConfLower) should be less than 95% CI lower bound ($midConfLower)."
    )
    assert(
      cond = highConfUpper > midConfUpper,
      clue = s"99.9% CI upper bound ($highConfUpper) should be greater than 95% CI upper bound ($midConfUpper)."
    )
  }

  rngTest("confidenceInterval with low confidence (20%) should produce a narrower interval than a 95% CI") {
    val lowConfidence = 0.20
    val normalDist    = Uncertain.normal(0.0, 1.0)

    // Theoretical Z-score for 20% confidence (quantiles at 0.4 and 0.6) is approx ±0.253
    val (lowConfLower, lowConfUpper) = normalDist.confidenceInterval(lowConfidence, sampleCount)
    val (midConfLower, midConfUpper) = normalDist.confidenceInterval(0.95, sampleCount)

    assert(
      cond = lowConfLower > midConfLower,
      clue = s"20% CI lower bound ($lowConfLower) should be greater than 95% CI lower bound ($midConfLower)."
    )
    assert(
      cond = lowConfUpper < midConfUpper,
      clue = s"20% CI upper bound ($lowConfUpper) should be less than 95% CI upper bound ($midConfUpper)."
    )
  }

  rngTest("confidenceInterval for a point distribution should have zero width") {
    val pointDist      = Uncertain.always(42.0)
    val (lower, upper) = pointDist.confidenceInterval(0.95, 1000)

    assertEquals(
      obtained = lower,
      expected = 42.0,
      clue = "Lower bound of a point distribution's CI must be the point itself."
    )
    assertEquals(
      obtained = upper,
      expected = 42.0,
      clue = "Upper bound of a point distribution's CI must be the point itself."
    )
  }

  // --- Input Validation Tests ---

  rngTest("confidenceInterval should throw IllegalArgumentException for confidence <= 0") {
    val dist = Uncertain.normal(0, 1)
    intercept[IllegalArgumentException] {
      dist.confidenceInterval(0.0, sampleCount)
    }
    intercept[IllegalArgumentException] {
      dist.confidenceInterval(-0.5, sampleCount)
    }
  }

  rngTest("confidenceInterval should throw IllegalArgumentException for confidence >= 1") {
    val dist = Uncertain.normal(0, 1)
    intercept[IllegalArgumentException] {
      dist.confidenceInterval(1.0, sampleCount)
    }
    intercept[IllegalArgumentException] {
      dist.confidenceInterval(1.5, sampleCount)
    }
  }

  rngTest("confidenceInterval should throw IllegalArgumentException for non-positive sample count") {
    val dist = Uncertain.normal(0, 1)
    intercept[IllegalArgumentException] {
      dist.confidenceInterval(0.95, 0)
    }
    intercept[IllegalArgumentException] {
      dist.confidenceInterval(0.95, -100)
    }
  }
}
