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

import munit.FunSuite

import scala.math.{abs, pow, sqrt}
import mostly.uncertaintee.syntax.*

class BinomialDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Statistical Properties Tests ---

  rngTest("Binomial distribution's sample mean should approximate its theoretical mean (n*p)") {
    val trials      = 20
    val probability = 0.4
    val binomial    = Uncertain.binomial(trials, probability)

    // The mean (or expected value) of a Binomial(n, p) distribution is n * p.
    // See: https://en.wikipedia.org/wiki/Binomial_distribution#Properties
    val theoreticalMean = trials * probability
    val sampleMean      = binomial.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Binomial(n=$trials, p=$probability)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Binomial distribution's sample variance should approximate its theoretical variance (n*p*(1-p))") {
    val trials      = 50
    val probability = 0.25
    val binomial    = Uncertain.binomial(trials, probability)

    // The variance of a Binomial(n, p) distribution is n * p * (1 - p).
    val theoreticalVariance = trials * probability * (1.0 - probability)
    val sampleVariance      = pow(binomial.standardDeviation(sampleCount), 2)

    val hint = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Binomial(n=$trials, p=$probability)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Binomial distribution's sample skewness should approximate its theoretical skewness") {
    val trials      = 30
    val probability = 0.7
    val binomial    = Uncertain.binomial(trials, probability)
    val samples     = binomial.take(sampleCount).map(_.toDouble)

    // The theoretical skewness is (1 - 2p) / sqrt(n * p * (1 - p)).
    val theoreticalSkewness = (1.0 - 2.0 * probability) / sqrt(trials * probability * (1.0 - probability))

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) should be close to theoretical skewness ($theoreticalSkewness)."
    assert(abs(sampleSkewness - theoreticalSkewness) < tolerance, hint)
  }

  rngTest("Binomial distribution's sample excess kurtosis should approximate its theoretical excess kurtosis") {
    val trials      = 100
    val probability = 0.5
    val binomial    = Uncertain.binomial(trials, probability)
    val samples     = binomial.take(sampleCount).map(_.toDouble)

    // The theoretical excess kurtosis is (1 - 6p(1 - p)) / (n * p * (1 - p)).
    val theoreticalKurtosis = (1.0 - 6.0 * probability * (1.0 - probability)) / (trials * probability * (1.0 - probability))

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleKurtosis = (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / sampleCount) - 3.0

    val hint = s"Sample excess kurtosis ($sampleKurtosis) should be close to theoretical ($theoreticalKurtosis)."
    // Kurtosis estimation has high variance, so a larger tolerance is justified.
    assert(abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2, hint)
  }

  // --- Edge Case and Special Value Tests ---

  rngTest("Binomial(n, p=0) should always produce 0 successes") {
    val trials   = 10
    val zeroProb = Uncertain.binomial(trials, 0.0)
    val samples  = zeroProb.take(1000)
    assert(samples.forall(_ == 0), "Binomial(n, 0.0) must always produce 0.")
    assertEquals(zeroProb.expectedValue(1000), 0.0)
    assertEquals(zeroProb.standardDeviation(1000), 0.0)
  }

  rngTest("Binomial(n, p=1) should always produce n successes") {
    val trials      = 15
    val certainProb = Uncertain.binomial(trials, 1.0)
    val samples     = certainProb.take(1000)
    assert(samples.forall(_ == trials), s"Binomial($trials, 1.0) must always produce $trials.")
    assertEquals(certainProb.expectedValue(1000), trials.toDouble)
    assertEquals(certainProb.standardDeviation(1000), 0.0)
  }

  rngTest("Binomial(n=0, p) should always produce 0 successes") {
    val probability = 0.3
    val zeroTrials  = Uncertain.binomial(0, probability)
    val samples     = zeroTrials.take(1000)
    assert(samples.forall(_ == 0), "Binomial(0, p) must always produce 0.")
    assertEquals(zeroTrials.expectedValue(1000), 0.0)
  }

  // --- Arithmetic Operations Tests ---

  rngTest("Sum of two independent Binomial distributions with the same p should be a Binomial distribution") {
    val p   = 0.6
    val n1  = 10
    val n2  = 20
    val b1  = Uncertain.binomial(n1, p)
    val b2  = Uncertain.binomial(n2, p)
    val sum = b1 + b2

    // If X ~ B(n, p) and Y ~ B(m, p), then X + Y ~ B(n + m, p).
    val expectedMean     = (n1 + n2) * p
    val expectedVariance = (n1 + n2) * p * (1.0 - p)
    val expectedStdDev   = sqrt(expectedVariance)

    val sampleMean   = sum.expectedValue(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(abs(sampleMean - expectedMean) < tolerance, s"Mean of sum should be $expectedMean, but was $sampleMean.")
    assert(abs(sampleStdDev - expectedStdDev) < tolerance, s"StdDev of sum should be $expectedStdDev, but was $sampleStdDev.")
  }

  rngTest("Correlation: an uncertain value subtracted from itself (x - x) should be exactly 0") {
    val x          = Uncertain.binomial(10, 0.5)
    val difference = x - x
    val samples    = difference.take(sampleCount)
    assert(samples.forall(_ == 0), "`x - x` must always evaluate to 0 due to correlation.")
    assertEquals(difference.expectedValue(sampleCount), 0.0)
    assertEquals(difference.standardDeviation(sampleCount), 0.0)
  }

  rngTest("Correlation: `x + x` should be equivalent to `2 * x`") {
    val x   = Uncertain.binomial(10, 0.5) // Mean=5, Var=2.5
    val sum = x + x

    // E[2x] = 2 * E[x] = 2 * 5 = 10
    // Var(2x) = 2² * Var(x) = 4 * 2.5 = 10
    val expectedMean   = 2.0 * (10 * 0.5)
    val expectedStdDev = 2.0 * sqrt(10 * 0.5 * 0.5)

    val sampleMean   = sum.expectedValue(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(abs(sampleMean - expectedMean) < tolerance, s"Mean of correlated sum `x+x` should be $expectedMean, but was $sampleMean.")
    assert(abs(sampleStdDev - expectedStdDev) < tolerance, s"StdDev of correlated sum `x+x` should be $expectedStdDev, but was $sampleStdDev.")
  }
}
