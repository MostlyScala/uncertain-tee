/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee

import mostly.uncertaintee.syntax.*

import scala.math.{abs, pow}

class NegativeBinomialDistributionSpec extends RngSuite {

  // To allow variance to get close together, we sample... a lot.
  private val sampleCount       = 1_250_000
  private val tolerance         = 0.01
  private val varianceTolerance = 0.08

  // --- Statistical Properties Tests ---

  rngTest("NegativeBinomial sample mean should approximate theoretical mean r*(1-p)/p") {
    val r           = 5   // Number of successes
    val p           = 0.4 // Probability of success
    val negBinomial = Uncertain.negativeBinomial(r, p)

    // Theoretical mean (number of failures) = r * (1-p) / p
    // See: https://en.wikipedia.org/wiki/Negative_binomial_distribution
    val theoreticalMean = r * (1.0 - p) / p
    val sampleMean      = negBinomial.expectedValue(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for NegBinomial(r=$r, p=$p)."
    )
  }

  rngTest("NegativeBinomial sample variance should approximate theoretical variance r*(1-p)/p^2") {
    val r           = 10  // Number of successes
    val p           = 0.7 // Probability of success
    val negBinomial = Uncertain.negativeBinomial(r, p)

    // Theoretical variance = r * (1-p) / p^2
    val theoreticalVariance = r * (1.0 - p) / pow(p, 2)
    val sampleVariance      = pow(negBinomial.standardDeviation(sampleCount), 2)

    assert(
      cond = abs(sampleVariance - theoreticalVariance) < varianceTolerance,
      clue = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for NegBinomial(r=$r, p=$p)."
    )
  }

  rngTest("NegativeBinomial(50, 0.5) stats should match theoretical values") {
    val r           = 50
    val p           = 0.5
    val negBinomial = Uncertain.negativeBinomial(r, p)

    // Theoretical mean = r * (1-p) / p = 50 * 0.5 / 0.5 = 50.0
    val theoreticalMean = r * (1.0 - p) / p
    val sampleMean      = negBinomial.mean(sampleCount)
    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for NegBinomial(r=$r, p=$p)."
    )

    // Theoretical variance = r * (1-p) / p^2 = 50 * 0.5 / (0.5^2) = 25 / 0.25 = 100.0
    val theoreticalVariance = r * (1.0 - p) / pow(p, 2)
    val sampleVariance      = pow(negBinomial.standardDeviation(sampleCount), 2)
    assert(
      cond = abs(sampleVariance - theoreticalVariance) < varianceTolerance,
      clue = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for NegBinomial(r=$r, p=$p)."
    )
  }

  rngTest("NegativeBinomial with small p should handle rare successes efficiently") {
    val r           = 10
    val p           = 0.01 // Very low success probability
    val negBinomial = Uncertain.negativeBinomial(r, p)

    // Theoretical mean = r * (1-p) / p = 10 * 0.99 / 0.01 = 990.0
    val theoreticalMean = r * (1.0 - p) / p

    // Use fewer samples for this test since we expect large values
    val smallerSampleCount = 50_000
    val sampleMean         = negBinomial.expectedValue(smallerSampleCount)

    // Use larger tolerance for low p due to higher variance
    val lowPTolerance = 2.0
    assert(
      cond = abs(sampleMean - theoreticalMean) < lowPTolerance,
      clue = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for NegBinomial(r=$r, p=$p) with low p."
    )

    // Theoretical variance = r * (1-p) / p^2 = 10 * 0.99 / (0.01^2) = 99000.0
    val theoreticalVariance = r * (1.0 - p) / pow(p, 2)
    val sampleVariance      = pow(negBinomial.standardDeviation(smallerSampleCount), 2)

    // Variance tolerance needs to be proportional to the variance itself
    val lowPVarianceTolerance = theoreticalVariance * 0.05 // 5% relative tolerance
    assert(
      cond = abs(sampleVariance - theoreticalVariance) < lowPVarianceTolerance,
      clue = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for NegBinomial(r=$r, p=$p) with low p."
    )
  }

  rngTest("NegativeBinomial with very small p should still produce valid samples") {
    val r           = 5
    val p           = 0.001 // Extremely low success probability
    val negBinomial = Uncertain.negativeBinomial(r, p)

    // Just verify we can sample without hanging or errors
    val samples = negBinomial.take(1000)

    // All samples should be non-negative integers
    assert(
      cond = samples.forall(_ >= 0),
      clue = "All samples from NegBinomial should be non-negative"
    )

    // With p=0.001, we expect roughly 1000*(1-p)/p = ~999 failures per success
    // So for 5 successes, we expect ~4995 failures on average
    // Just verify samples are in a reasonable range (not stuck at 0 or infinity)
    val theoreticalMean = r * (1.0 - p) / p
    val sampleMean      = samples.sum.toDouble / samples.length

    // Very loose check - just make sure it's order-of-magnitude correct
    assert(
      cond = sampleMean > theoreticalMean * 0.1 && sampleMean < theoreticalMean * 10,
      clue = s"Sample mean ($sampleMean) should be roughly in the ballpark of theoretical mean ($theoreticalMean) for NegBinomial(r=$r, p=$p)"
    )
  }

  // --- Edge Case Tests ---
  rngTest("NegativeBinomial with p=1.0 should always produce 0 failures") {
    val r           = 5
    val p           = 1.0
    val negBinomial = Uncertain.negativeBinomial(r, p)
    val samples     = negBinomial.take(sampleCount)

    assert(
      cond = samples.forall(_ == 0),
      clue = "NegBinomial(r, p=1.0) must always result in 0 failures."
    )
    assertEquals(
      obtained = negBinomial.expectedValue(1000),
      expected = 0.0
    )
    assertEquals(
      obtained = negBinomial.standardDeviation(1000),
      expected = 0.0
    )
  }

  rngTest("NegativeBinomial with r=1 should behave like Geometric (counting failures)") {
    val r                 = 1
    val p                 = 0.3
    val negBinomial       = Uncertain.negativeBinomial(r, p)
    val geometricFailures = Uncertain.geometric(p).map(_ - 1)

    // Compare means
    val meanNB = negBinomial.expectedValue(sampleCount)
    val meanG  = geometricFailures.expectedValue(sampleCount)
    assert(
      cond = abs(meanNB - meanG) < tolerance,
      clue = s"Mean of NegBinomial(r=1, p=$p) ($meanNB) should match mean of Geometric($p) failures ($meanG)."
    )

    // Compare variances
    val varNB = pow(negBinomial.standardDeviation(sampleCount), 2)
    val varG  = pow(geometricFailures.standardDeviation(sampleCount), 2)
    assert(
      cond = abs(varNB - varG) < varianceTolerance,
      clue = s"Variance of NegBinomial(r=1, p=$p) ($varNB) should match variance of Geometric($p) failures ($varG)."
    )
  }

  // --- Input Validation Tests ---

  test("NegativeBinomial constructor should throw IllegalArgumentException for invalid parameters") {
    intercept[IllegalArgumentException] {
      Uncertain.negativeBinomial(r = 0, probability = 0.5) // r must be > 0
    }
    intercept[IllegalArgumentException] {
      Uncertain.negativeBinomial(r = -5, probability = 0.5) // r must be > 0
    }
    intercept[IllegalArgumentException] {
      Uncertain.negativeBinomial(r = 5, probability = 0.0) // p must be > 0
    }
    intercept[IllegalArgumentException] {
      Uncertain.negativeBinomial(r = 5, probability = -0.1) // p must be in (0, 1]
    }
    intercept[IllegalArgumentException] {
      Uncertain.negativeBinomial(r = 5, probability = 1.1) // p must be in (0, 1]
    }
  }
}
