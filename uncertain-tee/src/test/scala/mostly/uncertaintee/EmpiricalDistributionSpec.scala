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

import scala.math.abs

class EmpiricalDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  rngTest("sampling frequency should match the element frequency in the source data") {
    // An empirical distribution samples directly from a collection of existing data,
    // preserving the proportions of each unique element in that data.
    val sourceData = List("dog", "cat", "cat", "dog", "cat", "bird")
    val empirical  = Uncertain.empirical(sourceData)

    // Calculate theoretical frequencies from the source data
    val theoreticalFrequencies =
      sourceData.groupBy(identity).view.mapValues(_.length.toDouble / sourceData.length).toMap

    val samples           = empirical.take(sampleCount)
    val sampleFrequencies = samples.groupBy(identity).view.mapValues(_.length.toDouble / sampleCount).toMap

    theoreticalFrequencies.foreach { case (item, theoreticalFreq) =>
      val sampleFreq = sampleFrequencies.getOrElse(item, 0.0)
      assert(
        cond = abs(sampleFreq - theoreticalFreq) < tolerance,
        clue = s"Sample frequency of '$item' ($sampleFreq) should match source data frequency ($theoreticalFreq)."
      )
    }
  }

  rngTest("should work with different data types like Int") {
    val sourceData             = List(1, 2, 2, 3, 3, 3)
    val empirical              = Uncertain.empirical(sourceData)
    val theoreticalFrequencies = Map(1 -> 1.0 / 6.0, 2 -> 2.0 / 6.0, 3 -> 3.0 / 6.0)

    val samples           = empirical.take(sampleCount)
    val sampleFrequencies = samples.groupBy(identity).view.mapValues(_.length.toDouble / sampleCount).toMap

    theoreticalFrequencies.foreach { case (item, theoreticalFreq) =>
      val sampleFreq = sampleFrequencies.getOrElse(item, 0.0)
      assert(
        cond = abs(sampleFreq - theoreticalFreq) < tolerance,
        clue = s"Sample frequency of '$item' ($sampleFreq) should match source data frequency ($theoreticalFreq)."
      )
    }
  }

  rngTest("with a single item should behave like a point distribution") {
    val distribution = Uncertain.empirical(List(42))
    val samples      = distribution.take(1000)
    assert(
      cond = samples.forall(_ == 42),
      clue = "An empirical distribution with one item must always return that item."
    )
    assertEquals(
      obtained = distribution.mode(1000),
      expected = 42
    )
  }

  rngTest("with all identical items should behave like a point distribution") {
    val distribution = Uncertain.empirical(List("A", "A", "A", "A"))
    val samples      = distribution.take(1000)
    assert(
      cond = samples.forall(_ == "A"),
      clue = "An empirical distribution with all identical items must always return that item."
    )
    assertEquals(
      obtained = distribution.mode(1000),
      expected = "A"
    )
  }

  // --- Input Validation Tests ---

  rngTest("constructor should throw if the source collection is empty") {
    intercept[IllegalArgumentException] {
      Uncertain.empirical(List.empty[Int])
    }
  }
}
