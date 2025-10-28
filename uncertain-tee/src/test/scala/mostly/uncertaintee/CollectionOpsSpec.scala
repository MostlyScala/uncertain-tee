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

import mostly.uncertaintee.ops.CollectionOps
import mostly.uncertaintee.syntax.*

class CollectionOpsSpec extends RngSuite with CollectionOps {

  private val sampleCount = 250_000

  rngTest("Interleaving a list of size 1 with large lists should result in an equal distribution of positions for the small list") {
    val positions = Uncertain
      .interleave(
        List("A"),          // 1 A
        List.fill(10)("B"), // 10 Bs
        List.fill(10)("C"), // 10 Cs
        List.fill(10)("D") // 10 Ds
      )
      .map(_.indexOf("A")) // if interleaving goes well, A should be equally likely everywhere

    val largeSampleCount = sampleCount * 10
    // Total length is 1 + 10 + 10 + 10 = 31, so "A" can be in positions 0-30
    val positionCounts   = positions.histogram(largeSampleCount) // large sample count for this one

    // Each position should appear roughly equally (1/31 of the time)
    val expectedFrequency = largeSampleCount.toDouble / 31
    val tolerance         = expectedFrequency * 0.01 // Allow 1% deviation - pretty strict in this test.

    // Check that all positions 0-30 appear
    val allPositions = 0 to 30
    allPositions.foreach { pos =>
      val actualCount = positionCounts.getOrElse(pos, 0)
      assert(
        actualCount > 0,
        s"Position $pos should appear at least once in $sampleCount samples"
      )
      assert(
        math.abs(actualCount - expectedFrequency) < tolerance,
        s"Position $pos appeared $actualCount times, expected ~$expectedFrequency (tolerance: $tolerance)"
      )
    }
  }

  rngTest("subrange should return the original range when range is empty") {
    val emptyRange = 10 until 10
    val result     = Uncertain.subrange(emptyRange)

    val samples = result.take(sampleCount)
    assert(samples.forall(_ == emptyRange), "All samples from empty range should be the empty range itself")
  }

  rngTest("subrange should return valid sub-ranges within bounds") {
    val originalRange = 0 until 10
    val result        = Uncertain.subrange(originalRange)

    val samples = result.take(sampleCount)
    samples.foreach { subRange =>
      assert(subRange.start >= originalRange.start, s"Sub-range start ${subRange.start} should be >= ${originalRange.start}")
      assert(subRange.end <= originalRange.end, s"Sub-range end ${subRange.end} should be <= ${originalRange.end}")
      assert(subRange.step == originalRange.step, s"Sub-range step ${subRange.step} should match original step ${originalRange.step}")
      assert(subRange.start <= subRange.end, s"Sub-range start ${subRange.start} should be <= end ${subRange.end}")
    }
  }

  rngTest("subrange should preserve step size for ranges with custom step") {
    val customStepRange = 0 to 20 by 5
    val result          = Uncertain.subrange(customStepRange)

    val samples = result.take(sampleCount)
    samples.foreach { subRange =>
      assert(subRange.step == 5, s"Sub-range should preserve step size of 5, got ${subRange.step}")
      assert(subRange.start % 5 == 0, s"Sub-range start ${subRange.start} should be multiple of step size")
      if (subRange.nonEmpty) {
        assert(subRange.end % 5 == 0, s"Sub-range end ${subRange.end} should be multiple of step size")
      }
    }
  }

  rngTest("subrange should handle negative step ranges") {
    val negativeStepRange = 10 to 0 by -2
    val result            = Uncertain.subrange(negativeStepRange)

    val samples = result.take(sampleCount)
    samples.foreach { subRange =>
      assert(subRange.step == -2, s"Sub-range should preserve negative step size, got ${subRange.step}")
      assert(subRange.start >= negativeStepRange.end, s"Sub-range start ${subRange.start} should be >= ${negativeStepRange.end}")
      assert(subRange.end <= negativeStepRange.start, s"Sub-range end ${subRange.end} should be <= ${negativeStepRange.start}")
    }
  }

  // --- permutations tests ---

  rngTest("permutations should return empty list when input is empty") {
    val emptyList = List.empty[Int]
    val result    = Uncertain.permutations(emptyList)

    val samples = result.take(sampleCount)
    assert(samples.forall(_.isEmpty), "Shuffling empty list should always return empty list")
  }

  rngTest("permutations should return single element when input has one element") {
    val singleElementList = List(42)
    val result            = Uncertain.permutations(singleElementList)

    val samples = result.take(sampleCount)
    assert(samples.forall(_ == singleElementList), "Shuffling single element list should return the same list")
  }

  rngTest("permutations should preserve all elements") {
    val originalList = List(1, 2, 3, 4, 5)
    val result       = Uncertain.permutations(originalList)

    val samples = result.take(sampleCount)
    samples.foreach { shuffled =>
      assert(shuffled.length == originalList.length, "Shuffled list should have same length")
      assert(shuffled.sorted == originalList.sorted, "Shuffled list should contain same elements")
    }
  }

  rngTest("permutations should produce different permutations") {
    val originalList = List(1, 2, 3, 4)
    val result       = Uncertain.permutations(originalList)

    val samples = result.take(sampleCount).toSet
    assert(samples.size > 1, "Should produce different permutations")
    assert(samples.contains(originalList) || samples.size > 10, "Should have reasonable variety in permutations")
  }

  // --- permutations tests ---

  rngTest("combinationsOfVaryingSize should respect size constraints") {
    val originalList = List("A", "B", "C", "D")
    val sizeRange    = 2 to 3
    val result       = Uncertain.combinationsOfVaryingSize(sizeRange, originalList)

    val samples = result.take(sampleCount)
    samples.foreach { perm =>
      assert(perm.length >= sizeRange.start && perm.length <= sizeRange.last, s"Permutation length ${perm.length} should be in range ${sizeRange}")
      assert(perm.distinct == perm, "Permutation should not have duplicate elements")
      assert(perm.forall(originalList.contains), "All elements should be from original list")
    }
  }

  rngTest("combinationsOfVaryingSize should handle edge case of size 0") {
    val originalList = List(1, 2, 3)
    val result       = Uncertain.combinationsOfVaryingSize(0 to 0, originalList)

    val samples = result.take(sampleCount)
    assert(samples.forall(_.isEmpty), "Permutations of size 0 should be empty")
  }

  rngTest("combinationsOfVaryingSize should throw for invalid size ranges") {
    val originalList = List(1, 2, 3)

    intercept[IllegalArgumentException] {
      Uncertain.combinationsOfVaryingSize(0 to 5, originalList) // size > list size
    }

    intercept[IllegalArgumentException] {
      Uncertain.combinationsOfVaryingSize(-1 to 2, originalList) // negative start
    }
  }

  // --- slices tests ---

  rngTest("slices should respect size and position constraints") {
    val originalList = List(10, 20, 30, 40, 50, 60)
    val sizeRange    = 2 to 4
    val result       = Uncertain.slicesOfVaryingSize(sizeRange, originalList)

    val samples = result.take(sampleCount)
    samples.foreach { slice =>
      assert(slice.length >= sizeRange.start && slice.length <= sizeRange.last, s"Slice length ${slice.length} should be in range ${sizeRange}")

      // Check that slice maintains original order
      if (slice.nonEmpty) {
        val startIdx = originalList.indexOf(slice.head)
        assert(startIdx >= 0, "Slice start element should exist in original list")
        assert(originalList.slice(startIdx, startIdx + slice.length) == slice, "Slice should be contiguous subsequence of original list")
      }
    }
  }

  rngTest("slices should handle edge cases") {
    val originalList = List(1, 2, 3)

    // Size 0
    val zeroSizeResult  = Uncertain.slicesOfVaryingSize(0 to 0, originalList)
    val zeroSizeSamples = zeroSizeResult.take(sampleCount)
    assert(zeroSizeSamples.forall(_.isEmpty), "Slices of size 0 should be empty")

    // Full size
    val fullSizeResult  = Uncertain.slicesOfVaryingSize(3 to 3, originalList)
    val fullSizeSamples = fullSizeResult.take(sampleCount)
    assert(fullSizeSamples.forall(_ == originalList), "Slices of full size should equal original list")
  }

  rngTest("slices should throw for invalid size ranges") {
    val originalList = List(1, 2, 3)

    intercept[IllegalArgumentException] {
      Uncertain.slicesOfVaryingSize(0 to 5, originalList) // size > list size
    }

    intercept[IllegalArgumentException] {
      Uncertain.slicesOfVaryingSize(-1 to 2, originalList) // negative start
    }
  }

  // --- combinations tests ---

  rngTest("combinations should select without replacement") {
    val originalList = List("A", "B", "C", "D")
    val sizeRange    = 2 to 3
    val result       = Uncertain.combinationsOfVaryingSize(sizeRange, originalList)

    val samples = result.take(sampleCount)
    samples.foreach { combo =>
      assert(combo.length >= sizeRange.start && combo.length <= sizeRange.last, s"Combination length ${combo.length} should be in range ${sizeRange}")
      assert(combo.distinct == combo, "Combination should not have duplicate elements")
      assert(combo.forall(originalList.contains), "All elements should be from original list")
    }
  }

  rngTest("combinations should handle duplicates in input correctly") {
    val originalList = List("A", "A", "B", "C")
    val result       = Uncertain.combinationsOfVaryingSize(2 to 2, originalList)

    val samples = result.take(sampleCount)
    samples.foreach { combo =>
      assert(combo.length == 2, "Combination should have length 2")
      // Each element should correspond to a unique index in the original list
      // so we can get List("A", "A") if both A's are selected
    }

    // Should be able to get both A's
    val hasBothAs = samples.exists(combo => combo.count(_ == "A") == 2)
    assert(hasBothAs, "Should be able to select both duplicate A's")
  }

  rngTest("combinations should throw for invalid size ranges") {
    val originalList = List(1, 2, 3)

    intercept[IllegalArgumentException] {
      Uncertain.combinationsOfVaryingSize(0 to 5, originalList) // size > list size
    }

    intercept[IllegalArgumentException] {
      Uncertain.combinationsOfVaryingSize(-1 to 2, originalList) // negative start
    }
  }

  // --- subsets tests ---

  rngTest("subsets should return proper subsets") {
    val originalSet = Set("X", "Y", "Z", "W")
    val sizeRange   = 2 to 3
    val result      = Uncertain.subsetsOfVaryingSize(sizeRange, originalSet)

    val samples = result.take(sampleCount)
    samples.foreach { subset =>
      assert(subset.size >= sizeRange.start && subset.size <= sizeRange.last, s"Subset size ${subset.size} should be in range ${sizeRange}")
      assert(subset.subsetOf(originalSet), "Result should be subset of original set")
    }
  }

  rngTest("subsets should handle edge cases") {
    val originalSet = Set(1, 2, 3)

    // Empty subset
    val emptyResult  = Uncertain.subsetsOfVaryingSize(0 to 0, originalSet)
    val emptySamples = emptyResult.take(sampleCount)
    assert(emptySamples.forall(_.isEmpty), "Subsets of size 0 should be empty")

    // Full size subset
    val fullResult  = Uncertain.subsetsOfVaryingSize(3 to 3, originalSet)
    val fullSamples = fullResult.take(sampleCount)
    assert(fullSamples.forall(_ == originalSet), "Subsets of full size should equal original set")
  }

  rngTest("subsets should throw for invalid size ranges") {
    val originalSet = Set(1, 2, 3)

    intercept[IllegalArgumentException] {
      Uncertain.subsetsOfVaryingSize(0 to 5, originalSet) // size > set size
    }

    intercept[IllegalArgumentException] {
      Uncertain.subsetsOfVaryingSize(-1 to 2, originalSet) // negative start
    }
  }

  rngTest("subsets should produce all possible combinations") {
    val originalSet = Set("A", "B", "C", "D", "E")
    val result      = Uncertain.subsetsOfVaryingSize(0 to 5, originalSet)

    val samples = result.take(sampleCount).toSet
    assert(samples.size > 1, "Should produce different subsets")
    assert(
      originalSet.subsets().toSet == samples,
      s"10 possible subsets of size 3, so we should see all of these subsets when sampling $sampleCount"
    )
  }

  // --- Fixed-size method tests ---

  rngTest("subsetsOfFixedSize should work correctly") {
    val originalSet = Set(1, 2, 3, 4, 5)
    val result      = Uncertain.subsetsOfFixedSize(3, originalSet)

    val samples = result.take(100)
    samples.foreach { subset =>
      assert(subset.size == 3, s"Fixed size subset should have size 3, got ${subset.size}")
      assert(subset.subsetOf(originalSet), "Result should be subset of original set")
    }
  }

  rngTest("slicesOfFixedSize should work correctly") {
    val originalList = List(1, 2, 3, 4, 5)
    val result       = Uncertain.slicesOfFixedSize(3, originalList)

    val samples = result.take(100)
    samples.foreach { slice =>
      assert(slice.length == 3, s"Fixed size slice should have length 3, got ${slice.length}")
      if (slice.nonEmpty) {
        val startIdx = originalList.indexOf(slice.head)
        assert(startIdx >= 0, "Slice start element should exist in original list")
        assert(originalList.slice(startIdx, startIdx + slice.length) == slice, "Slice should be contiguous subsequence of original list")
      }
    }
  }

  rngTest("combinationsOfFixedSize should work correctly") {
    val originalList = List("A", "B", "C", "D")
    val result       = Uncertain.combinationsOfFixedSize(2, originalList)

    val samples = result.take(100)
    samples.foreach { combo =>
      assert(combo.length == 2, s"Fixed size combination should have length 2, got ${combo.length}")
      assert(combo.distinct == combo, "Combination should not have duplicate elements")
      assert(combo.forall(originalList.contains), "All elements should be from original list")
    }
  }

  // --- Complete operations tests ---

  rngTest("slices (complete) should generate slices of all possible sizes") {
    val originalList = List(1, 2, 3)
    val result       = Uncertain.slices(originalList)

    val samples = result.take(1000)
    val sizes   = samples.map(_.length).toSet
    assert(sizes.contains(0), "Should generate empty slices")
    assert(sizes.contains(originalList.length), "Should generate full-size slices")
    assert(sizes.size > 1, "Should generate slices of different sizes")
  }

  rngTest("subsets (complete) should generate subsets of all possible sizes") {
    val originalSet = Set(1, 2, 3)
    val result      = Uncertain.subsets(originalSet)

    val samples = result.take(1000)
    val sizes   = samples.map(_.size).toSet
    assert(sizes.contains(0), "Should generate empty subsets")
    assert(sizes.contains(originalSet.size), "Should generate full-size subsets")
    assert(sizes.size > 1, "Should generate subsets of different sizes")
  }

  rngTest("combinations (complete) should generate combinations of all possible sizes") {
    val originalList = List(1, 2, 3)
    val result       = Uncertain.combinations(originalList)

    val samples = result.take(1000)
    val sizes   = samples.map(_.length).toSet
    assert(sizes.contains(0), "Should generate empty combinations")
    assert(sizes.contains(originalList.length), "Should generate full-size combinations")
    assert(sizes.size > 1, "Should generate combinations of different sizes")
  }

  // --- Error handling tests for fixed-size methods ---

  rngTest("fixed-size methods should throw for invalid sizes") {
    val list = List(1, 2, 3)
    val set  = Set(1, 2, 3)

    intercept[IllegalArgumentException] {
      Uncertain.subsetsOfFixedSize(5, set) // size > set size
    }

    intercept[IllegalArgumentException] {
      Uncertain.subsetsOfFixedSize(-1, set) // negative size
    }

    intercept[IllegalArgumentException] {
      Uncertain.slicesOfFixedSize(5, list) // size > list size
    }

    intercept[IllegalArgumentException] {
      Uncertain.slicesOfFixedSize(-1, list) // negative size
    }

    intercept[IllegalArgumentException] {
      Uncertain.combinationsOfFixedSize(5, list) // size > list size
    }

    intercept[IllegalArgumentException] {
      Uncertain.combinationsOfFixedSize(-1, list) // negative size
    }
  }
}
