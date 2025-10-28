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

import scala.math.{abs, pow, sqrt}

class NormalGaussianDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.05

  // --- Statistical Properties Tests ---

  rngTest("Normal distribution's sample mean should approximate its theoretical mean (μ)") {
    val mean   = 5.0
    val stdDev = 2.0
    val normal = Uncertain.normal(mean, stdDev)

    // The mean (or expected value) of a Normal(μ, σ²) distribution is μ.
    val theoreticalMean = mean
    val sampleMean      = normal.mean(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Normal(μ=$mean, σ=$stdDev)."
    )
  }

  rngTest("Normal distribution's sample variance should approximate its theoretical variance (σ²)") {
    val mean   = -10.0
    val stdDev = 3.0
    val normal = Uncertain.normal(mean, stdDev)

    // The variance measures the spread of the distribution.
    // For a Normal(μ, σ²) distribution, the variance is σ².
    // See: https://en.wikipedia.org/wiki/Variance
    val theoreticalVariance = pow(stdDev, 2)
    // The library's `standardDeviation` returns sqrt(variance), so we square it.
    val sampleVariance      = pow(normal.standardDeviation(sampleCount), 2)

    assert(
      cond = abs(sampleVariance - theoreticalVariance) < tolerance,
      clue = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Normal(μ=$mean, σ=$stdDev)."
    )
  }

  rngTest("Normal distribution's sample skewness should be approximately 0") {
    val mean    = 0.0
    val stdDev  = 1.0
    val normal  = Uncertain.normal(mean, stdDev)
    val samples = normal.take(sampleCount)

    // Skewness measures the asymmetry of the probability distribution.
    // For any Normal distribution, the theoretical skewness is 0, as it is perfectly symmetric.
    // See: https://en.wikipedia.org/wiki/Skewness
    val theoreticalSkewness = 0.0

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    assert(
      cond = abs(sampleSkewness - theoreticalSkewness) < tolerance,
      clue = s"Sample skewness ($sampleSkewness) for a symmetric Normal distribution should be close to 0."
    )
  }

  rngTest("Normal distribution's sample excess kurtosis should be approximately 0") {
    val mean    = 0.0
    val stdDev  = 1.0
    val normal  = Uncertain.normal(mean, stdDev)
    val samples = normal.take(sampleCount)

    // Excess kurtosis measures the "tailedness" of the distribution compared to the Normal distribution itself.
    // By definition, the excess kurtosis of a Normal distribution is 0.
    // See: https://en.wikipedia.org/wiki/Kurtosis
    val theoreticalKurtosis = 0.0

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    // The "- 3" at the end is to calculate *excess* kurtosis (as the kurtosis of a normal distribution is 3).
    val sampleKurtosis = (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / sampleCount) - 3.0

    // Kurtosis estimation has high variance, so a larger tolerance is justified.
    assert(
      cond = abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2,
      clue = s"Sample excess kurtosis ($sampleKurtosis) for a Normal distribution should be close to 0."
    )
  }

  // --- Statistical Functions ---

  rngTest("CDF of a normal distribution should approximate the error function") {
    val mean   = 0.0
    val stdDev = 1.0
    val normal = Uncertain.normal(mean, stdDev)

    // For a standard normal distribution, CDF(μ) = 0.5.
    // See: https://en.wikipedia.org/wiki/Cumulative_distribution_function
    val cdfAtMean = normal.cdf(mean, sampleCount)
    assert(abs(cdfAtMean - 0.5) < tolerance, s"CDF at mean should be ~0.5, but was $cdfAtMean")

    // Approx. 68% of values are within 1 standard deviation of the mean.
    // This means P(μ-σ <= X <= μ+σ) ≈ 0.68.
    // By extension, CDF(μ+σ) ≈ 0.841 and CDF(μ-σ) ≈ 0.159.
    val cdfAtPlusOneStdDev = normal.cdf(mean + stdDev, sampleCount)
    assert(
      cond = abs(cdfAtPlusOneStdDev - 0.841) < tolerance,
      clue = s"CDF at μ+σ should be ~0.841, but was $cdfAtPlusOneStdDev"
    )

    val cdfAtMinusOneStdDev = normal.cdf(mean - stdDev, sampleCount)
    assert(
      cond = abs(cdfAtMinusOneStdDev - 0.159) < tolerance,
      clue = s"CDF at μ-σ should be ~0.159, but was $cdfAtMinusOneStdDev"
    )
  }

  // --- Edge Case and Special Value Tests ---

  rngTest("Normal distribution with zero standard deviation should produce a constant value") {
    val mean             = 10.0
    val degenerateNormal = Uncertain.normal(mean, Numeric[Double].zero)
    val samples          = degenerateNormal.take(10_000)

    assert(
      cond = samples.forall(_ == mean),
      clue = s"Normal(μ=$mean, σ=0) must always produce the value $mean."
    )
    assertEquals(
      obtained = degenerateNormal.mean(10_000),
      expected = mean,
      clue = s"The expected value of Normal(μ=$mean, σ=0) must be exactly $mean."
    )
    assertEquals(
      obtained = degenerateNormal.standardDeviation(10_000),
      expected = 0.0,
      clue = s"The standard deviation of Normal(μ=$mean, σ=0) must be 0.0."
    )
  }

  // --- Arithmetic Operations Tests ---

  rngTest("Sum of two independent Normal distributions should be a Normal distribution") {
    val x1  = Uncertain.normal(3.0, 4.0) // μ=3, σ=4, var=16
    val x2  = Uncertain.normal(7.0, 3.0) // μ=7, σ=3, var=9
    val sum = x1 + x2

    // If X ~ N(μ₁, σ₁²) and Y ~ N(μ₂, σ₂²), then X + Y ~ N(μ₁ + μ₂, σ₁² + σ₂²).
    // See: https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables
    val expectedMean     = 3.0 + 7.0                 // 10.0
    val expectedVariance = pow(4.0, 2) + pow(3.0, 2) // 16 + 9 = 25.0
    val expectedStdDev   = sqrt(expectedVariance)    // 5.0

    val sampleMean   = sum.mean(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(
      cond = abs(sampleMean - expectedMean) < tolerance,
      clue = s"Mean of sum should be $expectedMean, but was $sampleMean."
    )
    assert(
      cond = abs(sampleStdDev - expectedStdDev) < tolerance,
      clue = s"StdDev of sum should be $expectedStdDev, but was $sampleStdDev."
    )
  }

  rngTest("Scaling and shifting a Normal distribution should yield a new Normal distribution") {
    val scale       = 2.0
    val shift       = 10.0
    val x           = Uncertain.normal(5.0, 3.0) // μ=5, σ=3
    val transformed = x * scale + shift

    // If X ~ N(μ, σ²), then aX + b ~ N(aμ + b, (aσ)²).
    val expectedMean   = 5.0 * scale + shift // 5*2 + 10 = 20.0
    val expectedStdDev = 3.0 * scale         // 3*2 = 6.0

    val sampleMean   = transformed.mean(sampleCount)
    val sampleStdDev = transformed.standardDeviation(sampleCount)

    assert(
      cond = abs(sampleMean - expectedMean) < tolerance,
      clue = s"Mean of transformed should be $expectedMean, but was $sampleMean."
    )
    assert(
      cond = abs(sampleStdDev - expectedStdDev) < tolerance,
      clue = s"StdDev of transformed should be $expectedStdDev, but was $sampleStdDev."
    )
  }

  rngTest("Dividing a Normal distribution by a scalar should yield a new Normal distribution") {
    val divisor     = 2.0
    val x           = Uncertain.normal(20.0, 8.0) // μ=20, σ=8
    val transformed = x / divisor

    // If X ~ N(μ, σ²), then X / c ~ N(μ / c, σ² / c²).
    val expectedMean   = 20.0 / divisor // 10.0
    val expectedStdDev = 8.0 / divisor  // 4.0

    val sampleMean   = transformed.mean(sampleCount)
    val sampleStdDev = transformed.standardDeviation(sampleCount)

    assert(
      cond = abs(sampleMean - expectedMean) < tolerance,
      clue = s"Mean of transformed should be $expectedMean, but was $sampleMean."
    )
    assert(
      cond = abs(sampleStdDev - expectedStdDev) < tolerance,
      clue = s"StdDev of transformed should be $expectedStdDev, but was $sampleStdDev."
    )
  }

  // --- Comparison and Hypothesis Tests ---

  rngTest("Comparison `normal > mean` should yield a Bernoulli distribution with p=0.5") {
    val mean   = 50.0
    val stdDev = 10.0
    val normal = Uncertain.normal(mean, stdDev)

    // Due to the symmetry of the Normal distribution, the probability of a sample
    // being greater than the mean is exactly 0.5.
    val isGreaterThanMean = normal > mean

    // The resulting Uncertain[Boolean] should therefore have an expected value of 0.5.
    val sampleProbability = isGreaterThanMean.mean(sampleCount)
    assert(
      cond = abs(sampleProbability - 0.5) < tolerance,
      clue = s"P(normal > mean) should be ~0.5, but was $sampleProbability"
    )
  }

  rngTest("Hypothesis test `probability(exceeds=...)` should correctly identify high-probability events") {
    val mean   = 100.0
    val stdDev = 15.0
    val iq     = Uncertain.normal(mean, stdDev)

    // The probability of a value being greater than μ - σ is ~0.841.
    // We test the hypothesis that P(iq > 85) > 0.8.
    // This hypothesis should be accepted.
    val isAbove85 = iq > (mean - stdDev) // Theoretical P(true) is ~0.841

    assert(
      cond = isAbove85.probability(exceeds = 0.8, maxSamples = sampleCount),
      clue = "Should be confident that P(iq > 85) exceeds 80%"
    )

    // Conversely, a hypothesis that the probability exceeds 0.9 should be rejected.
    assert(
      cond = !isAbove85.probability(exceeds = 0.9, maxSamples = sampleCount),
      clue = "Should not be confident that P(iq > 85) exceeds 90%"
    )
  }

  // --- Correlation Tests (Crucial for `Uncertain`'s core logic) ---

  rngTest("Correlation: an uncertain value subtracted from itself (x - x) should be exactly 0") {
    val x = Uncertain.normal(10.0, 5.0)

    // Due to the computation graph, `x` is sampled only once per evaluation.
    // Therefore, `x - x` is a deterministic operation resulting in 0.
    val difference = x - x

    val samples = difference.take(sampleCount)
    assert(
      cond = samples.forall(_ == 0.0),
      clue = "`x - x` must always evaluate to 0.0 due to correlation."
    )
    assertEquals(
      obtained = difference.mean(sampleCount),
      expected = 0.0,
      clue = "The expected value of `x - x` must be exactly 0.0."
    )
    assertEquals(
      obtained = difference.standardDeviation(sampleCount),
      expected = 0.0,
      clue = "The standard deviation of `x - x` must be exactly 0.0."
    )
  }

  rngTest("Correlation: `x + x` should be equivalent to `2 * x`") {
    val x = Uncertain.normal(5.0, 3.0) // μ=5, σ=3

    // In a correlated context, Var(X + X) = Var(2X) = 4 * Var(X).
    // An independent sum would yield Var(X₁ + X₂) = Var(X₁) + Var(X₂) = 2 * Var(X).
    // This test correctly verifies the correlated case.
    val sum = x + x

    val expectedMean   = 2 * 5.0 // 10.0
    val expectedStdDev = 2 * 3.0 // 6.0

    val sampleMean   = sum.mean(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(
      cond = abs(sampleMean - expectedMean) < tolerance,
      clue = s"Mean of correlated sum `x+x` should be ${expectedMean}, but was $sampleMean."
    )
    assert(
      cond = abs(sampleStdDev - expectedStdDev) < tolerance,
      clue = s"StdDev of correlated sum `x+x` should be ${expectedStdDev}, but was $sampleStdDev."
    )
  }

  rngTest("Correlation: an uncertain value divided by itself (x / x) should be exactly 1") {
    // We choose a mean far from zero to avoid sampling values near zero, which would cause division instability.
    val x = Uncertain.normal(100.0, 5.0)

    // Due to the computation graph, `x` is sampled only once per evaluation.
    // Therefore, `x / x` is a deterministic operation resulting in 1.0 (given x != 0).
    val ratio = x / x

    val samples = ratio.take(sampleCount)
    assert(
      cond = samples.forall(v => abs(v - 1.0) < 1e-9),
      clue = "`x / x` must always evaluate to 1.0 due to correlation."
    )
    assertEquals(
      obtained = ratio.mean(sampleCount),
      expected = 1.0,
      clue = "The expected value of `x / x` must be exactly 1.0."
    )
    assertEquals(
      obtained = ratio.standardDeviation(sampleCount),
      expected = 0.0,
      clue = "The standard deviation of `x / x` must be exactly 0.0."
    )
  }
}
