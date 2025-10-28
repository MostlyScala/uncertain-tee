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

import scala.math.*

class CdfSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Continuous Distribution Tests ---

  rngTest("cdf for a standard normal distribution should approximate its theoretical probability") {
    val normalDist = Uncertain.normal(0.0, 1.0)

    // The CDF of a standard normal distribution at its mean (0) is exactly 0.5.
    val theoreticalAtMean = 0.5
    val sampleAtMean      = normalDist.cdf(0.0, sampleCount)
    assert(
      cond = abs(sampleAtMean - theoreticalAtMean) < tolerance,
      clue = s"CDF at mean ($sampleAtMean) should be close to theoretical value ($theoreticalAtMean) for N(0,1)."
    )

    // The probability of a value being within one standard deviation is ~68%.
    // So, P(x <= 1) is 50% (for x<0) + 34% (for 0<x<1) = ~84%.
    // The exact value is given by the error function: 0.5 * (1 + erf(1/sqrt(2))) ≈ 0.8413.
    // See: https://en.wikipedia.org/wiki/Normal_distribution
    val theoreticalAtOneStdDev = 0.8413
    val sampleAtOneStdDev      = normalDist.cdf(1.0, sampleCount)
    assert(
      cond = abs(sampleAtOneStdDev - theoreticalAtOneStdDev) < tolerance,
      clue = s"CDF at +1 std dev ($sampleAtOneStdDev) should be close to theoretical value ($theoreticalAtOneStdDev) for N(0,1)."
    )
  }

  rngTest("cdf for a continuous uniform distribution should be linear") {
    val min         = 10.0
    val max         = 20.0
    val uniformDist = Uncertain.uniform(min, max)
    val range       = max - min

    // For a uniform distribution, the CDF is a linear function: F(x) = (x - min) / (max - min).
    // See: https://en.wikipedia.org/wiki/Continuous_uniform_distribution
    val point1          = 12.5                   // 25% of the way through the range
    val theoreticalCdf1 = (point1 - min) / range // (12.5 - 10) / 10 = 0.25
    val sampleCdf1      = uniformDist.cdf(point1, sampleCount)
    assert(
      cond = abs(sampleCdf1 - theoreticalCdf1) < tolerance,
      clue = s"Sample CDF ($sampleCdf1) should be close to theoretical ($theoreticalCdf1) for U(10,20) at x=$point1."
    )

    val point2          = 17.5                   // 75% of the way through the range
    val theoreticalCdf2 = (point2 - min) / range // (17.5 - 10) / 10 = 0.75
    val sampleCdf2      = uniformDist.cdf(point2, sampleCount)
    assert(
      cond = abs(sampleCdf2 - theoreticalCdf2) < tolerance,
      clue = s"Sample CDF ($sampleCdf2) should be close to theoretical ($theoreticalCdf2) for U(10,20) at x=$point2."
    )
  }

  rngTest("cdf for an exponential distribution should follow its theoretical curve") {
    val rate            = 0.5
    val exponentialDist = Uncertain.exponential(rate)
    val point           = 2.0

    // The CDF for an exponential distribution is F(x) = 1 - e^(-rate * x).
    // See: https://en.wikipedia.org/wiki/Exponential_distribution
    val theoreticalCdf = 1.0 - exp(-rate * point) // 1 - e^(-0.5 * 2) = 1 - e^(-1) ≈ 0.632
    val sampleCdf      = exponentialDist.cdf(point, sampleCount)
    assert(
      cond = abs(sampleCdf - theoreticalCdf) < tolerance,
      clue = s"Sample CDF ($sampleCdf) should be close to theoretical ($theoreticalCdf) for Exp(0.5) at x=$point."
    )
  }

  // --- Discrete Distribution Tests ---

  rngTest("cdf for a Bernoulli distribution should be a step function") {
    val p         = 0.7 // P(true) = 0.7, P(false) = 0.3
    val bernoulli = Uncertain.bernoulli(p).map(if (_) 1.0 else 0.0)

    // CDF at a point below the first possible value (0) should be 0.
    val cdfBelow = bernoulli.cdf(-0.1, sampleCount)
    assert(abs(cdfBelow - 0.0) < tolerance, s"CDF for x < 0 ($cdfBelow) should be 0 for Bernoulli.")

    // CDF between the two possible values should be the probability of the first value (0).
    // P(X <= 0.5) is the same as P(X=0), which is 1 - p = 0.3.
    val cdfBetween = bernoulli.cdf(0.5, sampleCount)
    assert(
      cond = abs(cdfBetween - (1.0 - p)) < tolerance,
      clue = s"CDF between 0 and 1 ($cdfBetween) should be P(false) (${1.0 - p})."
    )

    // CDF at a point above the second possible value (1) should be 1.
    val cdfAbove = bernoulli.cdf(1.1, sampleCount)
    assert(
      cond = abs(cdfAbove - 1.0) < tolerance,
      clue = s"CDF for x > 1 ($cdfAbove) should be 1 for Bernoulli."
    )
  }

  // --- Edge Case Tests ---

  rngTest("cdf for a value far below the distribution's mass should be 0") {
    val dist   = Uncertain.normal(100, 5) // Mean well away from zero
    val cdfVal = dist.cdf(0.0, sampleCount)
    assert(
      cond = abs(cdfVal - 0.0) < tolerance,
      clue = s"CDF for a value far to the left of the distribution should be close to 0. Got: $cdfVal"
    )
  }

  rngTest("cdf for a value far above the distribution's mass should be 1") {
    val dist   = Uncertain.normal(100, 5)
    val cdfVal = dist.cdf(200.0, sampleCount)
    assert(
      cond = abs(cdfVal - 1.0) < tolerance,
      clue = s"CDF for a value far to the right of the distribution should be close to 1. Got: $cdfVal"
    )
  }

  rngTest("cdf for a point distribution should be a perfect step from 0 to 1") {
    val pointValue = 42.0
    val pointDist  = Uncertain.always(pointValue)

    // No sampling needed for a deterministic distribution.
    assertEquals(
      obtained = pointDist.cdf(pointValue - 0.01, 100),
      expected = 0.0,
      clue = "CDF just before the point value must be 0."
    )
    assertEquals(
      obtained = pointDist.cdf(pointValue, 100),
      expected = 1.0,
      clue = "CDF at the point value must be 1."
    )
    assertEquals(
      obtained = pointDist.cdf(pointValue + 0.01, 100),
      expected = 1.0,
      clue = "CDF just after the point value must be 1."
    )
  }

  // --- Input Validation Tests ---

  rngTest("cdf should throw IllegalArgumentException for non-positive sample count") {
    val dist = Uncertain.normal(0, 1)
    intercept[IllegalArgumentException] {
      dist.cdf(0.5, 0)
    }
    intercept[IllegalArgumentException] {
      dist.cdf(0.5, -100)
    }
  }
}
