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

import mostly.uncertaintee.quantiles.*
import mostly.uncertaintee.syntax.*

import scala.math.abs

class QuantilesSpec extends RngSuite {

  private val tolerance   = 0.05
  private val sampleCount = 250_000

  rngTest("Quantiles.ofSize should create correct quantile intervals") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quantiles = Quantiles.ofSize(5, uniform, sampleCount)

    assert(
      quantiles.quantileIntervals == 5,
      "Should have 5 quantile intervals"
    )
  }

  rngTest("Quantiles.ofSize should handle uniform distribution correctly") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quantiles = Quantiles.ofSize(4, uniform, sampleCount) // quartiles

    val q0 = quantiles(0) // min
    val q1 = quantiles(1) // 25th percentile
    val q2 = quantiles(2) // 50th percentile (median)
    val q3 = quantiles(3) // 75th percentile
    val q4 = quantiles(4) // max

    assert(abs(q0 - 0.0) < tolerance * 100, s"Min should be close to 0, was $q0")
    assert(abs(q1 - 25.0) < tolerance * 100, s"Q1 should be close to 25, was $q1")
    assert(abs(q2 - 50.0) < tolerance * 100, s"Q2 (median) should be close to 50, was $q2")
    assert(abs(q3 - 75.0) < tolerance * 100, s"Q3 should be close to 75, was $q3")
    assert(abs(q4 - 100.0) < tolerance * 100, s"Max should be close to 100, was $q4")
  }

  rngTest("Quantiles min and max should return correct boundary values") {
    val uniform   = Uncertain.uniform(10.0, 90.0)
    val quantiles = Quantiles.ofSize(3, uniform, sampleCount)

    assert(abs(quantiles.min - 10.0) < tolerance * 100, s"Min should be close to 10, was ${quantiles.min}")
    assert(abs(quantiles.max - 90.0) < tolerance * 100, s"Max should be close to 90, was ${quantiles.max}")
    assert(quantiles.min == quantiles(0), "Min should equal quantile(0)")
    assert(quantiles.max == quantiles(quantiles.quantileIntervals), "Max should equal quantile(n)")
  }

  rngTest("Quantiles.toList should return correct sequence based on inclusion parameters") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quantiles = Quantiles.ofSize(4, uniform, sampleCount)

    val fullSeq   = quantiles.toList(includeMin = true, includeMax = true)
    val noMinSeq  = quantiles.toList(includeMin = false, includeMax = true)
    val noMaxSeq  = quantiles.toList(includeMin = true, includeMax = false)
    val middleSeq = quantiles.toList(includeMin = false, includeMax = false)

    assert(fullSeq.length == 5, s"Full sequence should have 5 elements, had ${fullSeq.length}")
    assert(noMinSeq.length == 4, s"No-min sequence should have 4 elements, had ${noMinSeq.length}")
    assert(noMaxSeq.length == 4, s"No-max sequence should have 4 elements, had ${noMaxSeq.length}")
    assert(middleSeq.length == 3, s"Middle sequence should have 3 elements, had ${middleSeq.length}")

    assert(fullSeq.head == quantiles.min, "Full sequence should start with min")
    assert(fullSeq.last == quantiles.max, "Full sequence should end with max")
    assert(!noMinSeq.contains(quantiles.min), "No-min sequence should not contain min")
    assert(!noMaxSeq.contains(quantiles.max), "No-max sequence should not contain max")
  }

  rngTest("Quantiles.toMap should return correct map based on inclusion parameters") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quantiles = Quantiles.ofSize(4, uniform, sampleCount)

    val fullMap   = quantiles.toMap(includeMin = true, includeMax = true)
    val noMinMap  = quantiles.toMap(includeMin = false, includeMax = true)
    val noMaxMap  = quantiles.toMap(includeMin = true, includeMax = false)
    val middleMap = quantiles.toMap(includeMin = false, includeMax = false)

    assert(fullMap.size == 5, s"Full map should have 5 entries, had ${fullMap.size}")
    assert(noMinMap.size == 4, s"No-min map should have 4 entries, had ${noMinMap.size}")
    assert(noMaxMap.size == 4, s"No-max map should have 4 entries, had ${noMaxMap.size}")
    assert(middleMap.size == 3, s"Middle map should have 3 entries, had ${middleMap.size}")

    assert(fullMap.contains(0), "Full map should contain key 0")
    assert(fullMap.contains(4), "Full map should contain key 4")
    assert(!noMinMap.contains(0), "No-min map should not contain key 0")
    assert(!noMaxMap.contains(4), "No-max map should not contain key 4")

    // Verify map values match quantile values
    fullMap.foreach { case (index, value) =>
      assert(value == quantiles(index), s"Map value at index $index should match quantile($index)")
    }
  }

  rngTest("Quantiles.asDiscreteUncertain should be stable after reconstruction") {
    val original = Uncertain.uniform(0.0, 100.0)
    val q1       = Quantiles.ofSize(4, original, sampleCount)
    val u1       = q1.reconstructFast
    val q2       = Quantiles.ofSize(4, u1, sampleCount)

    // Quantile boundaries should be approximately the same after reconstruction
    (0 to 4).foreach { i =>
      val diff = abs(q1(i) - q2(i))
      assert(diff < tolerance * 100, s"Quantile $i should be stable: q1=${q1(i)}, q2=${q2(i)}, diff=$diff")
    }
  }

  // --- Tests for companion object methods ---

  rngTest("Quantiles.tertiles should create tertiles with correct intervals") {
    val uniform  = Uncertain.uniform(0.0, 100.0)
    val tertiles = Quantiles.tertiles(uniform, sampleCount)

    assert(tertiles.quantileIntervals == 3, "Tertiles should have 3 intervals")
  }

  rngTest("Quantiles.quartiles should create quartiles with correct intervals") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quartiles = Quantiles.quartiles(uniform, sampleCount)

    assert(quartiles.quantileIntervals == 4, "Quartiles should have 4 intervals")
  }

  rngTest("Quantiles.quintiles should create quintiles with correct intervals") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quintiles = Quantiles.quintiles(uniform, sampleCount)

    assert(quintiles.quantileIntervals == 5, "Quintiles should have 5 intervals")
  }

  rngTest("Quantiles.deciles should create deciles with correct intervals") {
    val uniform = Uncertain.uniform(0.0, 100.0)
    val deciles = Quantiles.deciles(uniform, sampleCount)

    assert(deciles.quantileIntervals == 10, "Deciles should have 10 intervals")
  }

  rngTest("Quantiles.percentiles should create percentiles with correct intervals") {
    val uniform     = Uncertain.uniform(0.0, 100.0)
    val percentiles = Quantiles.percentiles(uniform, sampleCount)

    assert(percentiles.quantileIntervals == 100, "Percentiles should have 100 intervals")
  }

  // --- Edge cases and error conditions ---

  rngTest("Quantiles.ofSize should require positive quantile size") {
    val uniform = Uncertain.uniform(0.0, 100.0)

    intercept[IllegalArgumentException] {
      Quantiles.ofSize(0, uniform, sampleCount)
    }

    intercept[IllegalArgumentException] {
      Quantiles.ofSize(-1, uniform, sampleCount)
    }
  }

  rngTest("Quantiles.ofSize should require sufficient sample count") {
    val uniform = Uncertain.uniform(0.0, 100.0)

    intercept[IllegalArgumentException] {
      Quantiles.ofSize(5, uniform, 5) // sampleCount == n
    }

    intercept[IllegalArgumentException] {
      Quantiles.ofSize(5, uniform, 3) // sampleCount < n
    }
  }

  rngTest("Quantiles companion methods should require sufficient sample count") {
    val u = Uncertain.uniform(0.0, 100.0)

    intercept[IllegalArgumentException] {
      Quantiles.ofSize(3000, u, 2999)
    }
    intercept[IllegalArgumentException] {
      Quantiles.ofSize(3000, u, 3000)
    }

    intercept[IllegalArgumentException] {
      Quantiles.tertiles(u, 3)
    }

    intercept[IllegalArgumentException] {
      Quantiles.quartiles(u, 4)
    }

    intercept[IllegalArgumentException] {
      Quantiles.quintiles(u, 5)
    }

    intercept[IllegalArgumentException] {
      Quantiles.deciles(u, 10)
    }

    intercept[IllegalArgumentException] {
      Quantiles.percentiles(u, 100)
    }
  }

  rngTest("Quantiles.quantile should validate index bounds") {
    val uniform   = Uncertain.uniform(0.0, 100.0)
    val quantiles = Quantiles.ofSize(4, uniform, sampleCount)

    intercept[IllegalArgumentException] {
      quantiles(-1)
    }

    intercept[IllegalArgumentException] {
      quantiles(5) // > quantileIntervals
    }

    // Valid indices should work
    val q0 = quantiles(0)
    val q4 = quantiles(4)
    assert(q0 >= 0.0, "quantile(0) should return a valid value")
    assert(q4 <= 100.0, "quantile(4) should return a valid value")
  }

  rngTest("Quantiles should handle point distributions correctly") {
    val point     = Uncertain.always(42.0)
    val quantiles = Quantiles.ofSize(4, point, sampleCount)

    (0 to 4).foreach { i =>
      assert(quantiles(i) == 42.0, s"All quantiles should be 42.0 for point distribution, quantile($i) was ${quantiles(i)}")
    }

    assert(quantiles.min == 42.0, "Min should be 42.0")
    assert(quantiles.max == 42.0, "Max should be 42.0")
  }

  rngTest("Quantiles should work with discrete distributions") {
    val discrete  = Uncertain.fromRange(1 to 10)
    val quantiles = Quantiles.ofSize(4, discrete, sampleCount)

    // All quantile values should be integers from 1 to 10
    (0 to 4).foreach { i =>
      val value = quantiles(i)
      assert(value >= 1 && value <= 10, s"Quantile $i should be between 1 and 10, was $value")
      assert(value == value.toDouble, s"Quantile $i should be an integer value, was $value")
    }
  }
}
