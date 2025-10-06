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

class PoissonDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.05

  // --- Statistical Properties Tests ---

  rngTest("Poisson distribution's sample mean should approximate its theoretical mean (lambda)") {
    val lambda  = 10.0
    val poisson = Uncertain.poisson(lambda)

    // The mean (or expected value) of a Poisson(λ) distribution is λ.
    // See: https://en.wikipedia.org/wiki/Poisson_distribution#Properties
    val theoreticalMean = lambda
    val sampleMean      = poisson.expectedValue(sampleCount)

    val hint =
      s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Poisson(λ=$lambda)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Poisson distribution's sample variance should approximate its theoretical variance (lambda)") {
    val lambda  = 7.5
    val poisson = Uncertain.poisson(lambda)

    // The variance of a Poisson(λ) distribution is also λ.
    val theoreticalVariance = lambda
    val sampleVariance      = pow(poisson.standardDeviation(sampleCount), 2)

    val hint =
      s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Poisson(λ=$lambda)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Poisson distribution's sample skewness should approximate its theoretical skewness (1/sqrt(lambda))") {
    val lambda  = 5.0
    val poisson = Uncertain.poisson(lambda)
    val samples = poisson.take(sampleCount).map(_.toDouble)

    // The theoretical skewness is 1 / sqrt(λ).
    val theoreticalSkewness = 1.0 / sqrt(lambda)

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) should be close to theoretical skewness ($theoreticalSkewness)."
    assert(abs(sampleSkewness - theoreticalSkewness) < tolerance, hint)
  }

  rngTest(
    "Poisson distribution's sample excess kurtosis should approximate its theoretical excess kurtosis (1/lambda)"
  ) {
    val lambda  = 10.0
    val poisson = Uncertain.poisson(lambda)
    val samples = poisson.take(sampleCount).map(_.toDouble)

    // The theoretical excess kurtosis is 1 / λ.
    val theoreticalKurtosis = 1.0 / lambda

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleKurtosis = (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / sampleCount) - 3.0

    val hint = s"Sample excess kurtosis ($sampleKurtosis) should be close to theoretical ($theoreticalKurtosis)."
    // Kurtosis estimation has high variance, so a larger tolerance is justified.
    assert(abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2, hint)
  }

  // --- Edge Case and Special Value Tests ---

  rngTest("Poisson(lambda=0) should always produce 0") {
    val zeroLambda = Uncertain.poisson(0.0)
    val samples    = zeroLambda.take(sampleCount)
    assert(samples.forall(_ == 0), "Poisson(0.0) must always produce 0.")
    assertEquals(zeroLambda.expectedValue(sampleCount), 0.0)
    assertEquals(zeroLambda.standardDeviation(sampleCount), 0.0)
  }

  // --- Arithmetic Operations Tests ---

  rngTest("Sum of two independent Poisson distributions should be a Poisson distribution") {
    val lambda1 = 5.0
    val lambda2 = 10.0
    val p1      = Uncertain.poisson(lambda1)
    val p2      = Uncertain.poisson(lambda2)
    val sum     = p1 + p2

    // If X ~ Pois(λ₁) and Y ~ Pois(λ₂), then X + Y ~ Pois(λ₁ + λ₂).
    val expectedMean     = lambda1 + lambda2
    val expectedVariance = lambda1 + lambda2
    val expectedStdDev   = sqrt(expectedVariance)

    val sampleMean   = sum.expectedValue(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(abs(sampleMean - expectedMean) < tolerance, s"Mean of sum should be $expectedMean, but was $sampleMean.")
    assert(
      abs(sampleStdDev - expectedStdDev) < tolerance,
      s"StdDev of sum should be $expectedStdDev, but was $sampleStdDev."
    )
  }

  rngTest("Correlation: an uncertain value subtracted from itself (x - x) should be exactly 0") {
    val x          = Uncertain.poisson(5.0)
    val difference = x - x
    val samples    = difference.take(sampleCount)
    assert(samples.forall(_ == 0), "`x - x` must always evaluate to 0 due to correlation.")
    assertEquals(difference.expectedValue(sampleCount), 0.0)
    assertEquals(difference.standardDeviation(sampleCount), 0.0)
  }

  rngTest("Correlation: `x + x` should be equivalent to `2 * x`") {
    val lambda = 4.0
    val x      = Uncertain.poisson(lambda) // Mean=4, Var=4
    val sum    = x + x

    // E[2x] = 2 * E[x] = 2 * 4 = 8
    // Var(2x) = 2² * Var(x) = 4 * 4 = 16
    val expectedMean   = 2.0 * lambda
    val expectedStdDev = 2.0 * sqrt(lambda)

    val sampleMean   = sum.expectedValue(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(
      abs(sampleMean - expectedMean) < tolerance,
      s"Mean of correlated sum `x+x` should be $expectedMean, but was $sampleMean."
    )
    assert(
      abs(sampleStdDev - expectedStdDev) < tolerance,
      s"StdDev of correlated sum `x+x` should be $expectedStdDev, but was $sampleStdDev."
    )
  }

}
