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

class TriangularDistributionSpec extends RngSuite {

  private val sampleCount = 500_000
  private val tolerance   = 0.01

  // --- Sanity and Range Tests ---

  rngTest("Triangular distribution samples should always be within the [min, max] range") {
    val min        = 10.0
    val peak       = 15.0
    val max        = 30.0
    val triangular = Uncertain.triangular(min, peak, max)
    val samples    = triangular.take(sampleCount)

    assert(
      samples.forall(x => x >= min && x <= max),
      s"All samples must be within the [$min, $max] interval."
    )
  }

  // --- Statistical Properties Tests ---

  rngTest("Triangular distribution's sample mean should approximate its theoretical mean") {
    val min        = 0.0
    val peak       = 5.0
    val max        = 10.0
    val triangular = Uncertain.triangular(min, peak, max)

    // The mean of a triangular distribution is (min + peak + max) / 3.
    // See: https://en.wikipedia.org/wiki/Triangular_distribution#Properties
    val theoreticalMean = (min + peak + max) / 3.0
    val sampleMean      = triangular.expectedValue(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue =
        s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Triangular($min, $peak, $max)."
    )
  }

  rngTest("Triangular distribution's sample variance should approximate its theoretical variance") {
    val min        = 0.0
    val peak       = 10.0
    val max        = 20.0
    val triangular = Uncertain.triangular(min, peak, max)

    // The variance is (min² + peak² + max² - min*peak - min*max - peak*max) / 18.
    val theoreticalVariance =
      (pow(min, 2) + pow(peak, 2) + pow(max, 2) - min * peak - min * max - peak * max) / 18.0
    val sampleVariance      = pow(triangular.standardDeviation(sampleCount), 2)

    assert(
      cond = abs(sampleVariance - theoreticalVariance) < tolerance,
      clue =
        s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Triangular($min, $peak, $max)."
    )
  }

  rngTest("Triangular distribution should be densest around its peak (mode)") {
    val min        = 0.0
    val peak       = 3.0
    val max        = 10.0
    val triangular = Uncertain.triangular(min, peak, max)
    val samples    = triangular.take(sampleCount)

    val epsilon = 0.1

    val centerBin = samples.count(x => abs(x - peak) < epsilon)
    val leftBin   = samples.count(x => (x >= peak - 2 * epsilon) && (x < peak - epsilon))
    val rightBin  = samples.count(x => (x > peak + epsilon) && (x <= peak + 2 * epsilon))

    assert(
      cond = centerBin > leftBin && centerBin > rightBin,
      clue =
        s"The bin around the peak ($centerBin) should contain more samples than its neighbors (left: $leftBin, right: $rightBin)."
    )
  }

  // --- Edge Cases and Special Value Tests ---

  rngTest("Symmetric triangular distribution (peak is midpoint) should have a skewness of 0") {
    val min       = 0.0
    val max       = 10.0
    val peak      = (min + max) / 2.0 // 5.0
    val symmetric = Uncertain.triangular(min, peak, max)
    val samples   = symmetric.take(sampleCount)

    // Theoretical skewness for a symmetric distribution is 0.
    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    assert(
      cond = abs(sampleSkewness) < tolerance,
      clue = s"Sample skewness ($sampleSkewness) for a symmetric triangular distribution should be close to 0."
    )
  }

  rngTest("Degenerate triangular distribution (min == peak == max) should be a point distribution") {
    val value      = 42.0
    val degenerate = Uncertain.triangular(value, value, value)
    val samples    = degenerate.take(1000)

    assert(
      cond = samples.forall(_ == value),
      clue = s"Triangular($value, $value, $value) must always produce $value."
    )
    assertEquals(degenerate.expectedValue(1000), value)
    assertEquals(degenerate.standardDeviation(1000), 0.0)
  }

  // --- Input Validation ---

  rngTest("Triangular distribution constructor should throw IllegalArgumentException for invalid parameters") {
    intercept[IllegalArgumentException] {
      Uncertain.triangular(min = 10.0, peak = 5.0, max = 5.0) // min > max
    }
    intercept[IllegalArgumentException] {
      Uncertain.triangular(min = 0.0, peak = -1.0, max = 10.0) // peak < min
    }
    intercept[IllegalArgumentException] {
      Uncertain.triangular(min = 0.0, peak = 11.0, max = 10.0) // peak > max
    }
  }
}
