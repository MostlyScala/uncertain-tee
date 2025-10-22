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
import mostly.uncertaintee.syntax.*

import scala.math.*

class EntropySpec extends RngSuite {

  private val sampleCount             = 100_000
  private val tolerance               = 0.01
  private def log2(x: Double): Double = log(x) / log(2.0)

  // --- Deterministic and Zero-Entropy Tests ---

  test("Entropy of a deterministic Uncertain.point value must be exactly 0 bits") {
    val deterministic = Uncertain.always("A single outcome")
    // A distribution with only one possible outcome has no uncertainty.
    // H = - (1.0 * log2(1.0)) = 0.0
    assertEquals(deterministic.entropy(sampleCount), 0.0, "Entropy of a point value should be zero.")
  }

  rngTest("Entropy of a deterministic Bernoulli(0) or Bernoulli(1) must be 0 bits") {
    val bernoulli0 = Uncertain.bernoulli(0.0)
    val bernoulli1 = Uncertain.bernoulli(1.0)

    assertEquals(bernoulli0.entropy(sampleCount), 0.0, "Bernoulli(0) is predictable and its entropy must be zero.")
    assertEquals(bernoulli1.entropy(sampleCount), 0.0, "Bernoulli(1) is predictable and its entropy must be zero.")
  }

  // --- Discrete Distribution Tests ---

  rngTest("Entropy of a fair coin flip (Bernoulli(0.5)) should be 1 bit") {
    val fairCoin = Uncertain.bernoulli(0.5)

    // For two equally likely outcomes, p=0.5.
    // H = - (0.5 * log2(0.5) + 0.5 * log2(0.5)) = 1.0 bit.
    // This is the maximum possible entropy for a two-outcome system.
    val theoreticalEntropy = 1.0
    val sampleEntropy      = fairCoin.entropy(sampleCount)

    val hint =
      s"Sample entropy ($sampleEntropy) of a fair coin should be close to the theoretical maximum of $theoreticalEntropy bit."
    assert(abs(sampleEntropy - theoreticalEntropy) < tolerance, hint)
  }

  rngTest("Entropy of a biased coin (Bernoulli(0.1)) should be less than 1 bit") {
    val biasedCoin = Uncertain.bernoulli(0.1)
    val p          = 0.1

    // For a biased coin, uncertainty is reduced.
    // H = - (p * log2(p) + (1-p) * log2(1-p))
    val theoreticalEntropy = -(p * log2(p) + (1.0 - p) * log2(1.0 - p))
    val sampleEntropy      = biasedCoin.entropy(sampleCount)

    val hint =
      s"Sample entropy ($sampleEntropy) of a biased coin should be close to its theoretical entropy ($theoreticalEntropy)."
    assert(abs(sampleEntropy - theoreticalEntropy) < tolerance, hint)
    assert(sampleEntropy < 1.0, "Entropy of a biased coin must be less than the maximum of 1 bit.")
  }

  rngTest("Entropy of a uniform discrete distribution should be log2(N)") {
    val outcomes         = List("A", "B", "C", "D", "E", "F", "G", "H") // N=8 outcomes
    val uniformEmpirical = Uncertain.empirical(outcomes)

    // For N equally likely outcomes, H = log2(N).
    // Here, H = log2(8) = 3.0 bits.
    val theoreticalEntropy = log2(outcomes.length)
    val sampleEntropy      = uniformEmpirical.entropy(sampleCount)

    val hint =
      s"Sample entropy ($sampleEntropy) for ${outcomes.length} uniform outcomes should be close to log2(N) ($theoreticalEntropy bits)."
    assert(abs(sampleEntropy - theoreticalEntropy) < tolerance, hint)
  }

  rngTest("Entropy of a categorical distribution should match its theoretical value") {
    val outcomes    = Map("A" -> 0.5, "B" -> 0.25, "C" -> 0.25)
    val categorical = Uncertain.categorical(outcomes)

    // H = - sum(p_i * log2(p_i))
    // H = - (0.5 * log2(0.5) + 0.25 * log2(0.25) + 0.25 * log2(0.25)) = 1.5 bits.
    val theoreticalEntropy = outcomes.values.map(p => -p * log2(p)).sum
    val sampleEntropy      = categorical.entropy(sampleCount)

    val hint =
      s"Sample entropy ($sampleEntropy) for the categorical distribution should be close to its theoretical value ($theoreticalEntropy bits)."
    assert(abs(sampleEntropy - theoreticalEntropy) < tolerance, hint)
  }

  // --- Continuous Distribution Relative Test (Using Colors) ---

  // A real-world enum to serve as discrete bins (colors) for the continuous data.
  private enum Color {
    case RED, GREEN, BLUE, YELLOW, ORANGE, PINK, BROWN
  }
  private val allColors = Color.values

  rngTest("Entropy of a continuous distribution should increase with its variance (after mapping to colors)") {
    val narrowNormal = Uncertain.normal(mean = 0.0, standardDeviation = 1.0)
    val wideNormal   = Uncertain.normal(mean = 0.0, standardDeviation = 5.0)

    // This mapping function discretizes a continuous value into one of 7 colors.
    val mapToColor = (sample: Double) => {
      val min       = -15.0 // ~3 std devs for the wide distribution
      val max       = 15.0
      val numColors = allColors.length

      val clamped    = math.max(min, math.min(sample, max))
      val normalized = (clamped - min) / (max - min)
      val colorIndex = math.min(numColors - 1, floor(normalized * numColors).toInt)

      allColors(colorIndex)
    }

    // Pre-discretize the continuous distributions into colors before calling entropy.
    val narrowColored = narrowNormal.map(mapToColor)
    val wideColored   = wideNormal.map(mapToColor)

    val narrowEntropy = narrowColored.entropy(sampleCount)
    val wideEntropy   = wideColored.entropy(sampleCount)

    // The wide distribution spreads its samples more evenly across the 7 colors,
    // resulting in higher entropy. The narrow distribution concentrates most
    // samples into just a few central colors, resulting in lower entropy.
    val hint =
      s"A wide normal distribution's color entropy ($wideEntropy) should be greater than a narrow one's ($narrowEntropy)."
    assert(wideEntropy > narrowEntropy, hint)
  }
}
