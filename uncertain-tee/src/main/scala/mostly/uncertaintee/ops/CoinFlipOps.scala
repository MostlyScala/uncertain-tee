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

package mostly.uncertaintee.ops

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

import scala.util.Random

/** Operations for simulating coin flips and analyzing sequences of Boolean outcomes.
  *
  * Import with:
  * {{{
  * import mostly.uncertaintee.syntax.coinflip.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait CoinFlipOps {

  extension (u: Uncertain.type) {

    /** Simulates a single coin flip.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param chanceOfHeads
      *   Probability of getting heads (true), defaults to 0.5 for a fair coin
      * @return
      *   An uncertain Boolean value representing the coin flip result
      * @example
      *   {{{
      *   val fairCoin = Uncertain.coinFlip()
      *   val biasedCoin = Uncertain.coinFlip(chanceOfHeads = 0.7)
      *
      *   // Multiple independent flips
      *   val flips = List.fill(10)(Uncertain.coinFlip()).map(_.sample())
      *   }}}
      */
    def coinFlip(chanceOfHeads: Double = 0.5)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      Uncertain.bernoulliViaDouble(chanceOfHeads)
    }

    /** Tests if ALL flips in a sequence would be heads.
      *
      * This is equivalent to asking: "What's the probability that every flip is heads?" For a fair coin, flipping all heads becomes exponentially unlikely: 50%, 25%, 12.5%,
      * 6.25%...
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param totalFlips
      *   Number of coin flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each individual flip
      * @return
      *   An uncertain Boolean that's true if all flips are heads
      * @example
      *   {{{
      *   // Probability of 5 heads in a row (fair coin)
      *   val allHeads = Uncertain.coinFlipAll(totalFlips = 5)
      *   println(s"P(5 heads): ${allHeads.mean()}")  // 0.03125 = 3.125%
      *
      *   // Multiple systems all working (95% reliability each)
      *   val allSystemsUp = Uncertain.coinFlipAll(
      *     totalFlips = 3,
      *     chanceOfHeads = 0.95
      *   )
      *   }}}
      */
    def coinFlipAll(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")

      if (totalFlips == 0) {
        Uncertain.always(true) // Vacuous truth: all zero flips are heads
      } else {
        // Probability of all heads = chanceOfHeads^totalFlips
        val probabilityAllHeads = math.pow(chanceOfHeads, totalFlips)
        Uncertain.bernoulliViaDouble(probabilityAllHeads)
      }
    }

    /** Tests if a sequence of flips results in exactly N heads.
      *
      * This uses the binomial distribution to efficiently determine if exactly the specified number of heads occurred, without simulating each individual flip.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param totalFlips
      *   Total number of coin flips
      * @param exactHeads
      *   The exact number of heads we're testing for
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if we get exactly the specified number of heads
      * @example
      *   {{{
      *   // Did we get exactly 5 heads in 10 flips?
      *   val exactly5 = Uncertain.coinFlipExact(
      *     totalFlips = 10,
      *     exactHeads = 5
      *   )
      *
      *   // Testing for perfect 50/50 split
      *   val perfectSplit = Uncertain.coinFlipExact(
      *     totalFlips = 100,
      *     exactHeads = 50
      *   )
      *   println(s"P(exactly 50/100): ${perfectSplit.mean()}")  // ~7.96%
      *   }}}
      */
    def coinFlipExact(
      totalFlips: Int,
      exactHeads: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(exactHeads >= 0, "Exact heads must be non-negative")
      require(exactHeads <= totalFlips, s"Cannot get $exactHeads heads in only $totalFlips flips")

      // Use binomial distribution to get count of heads, then check if equal
      Uncertain
        .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
        .map(_ == exactHeads)
    }

    /** Tests if a sequence contains a streak of consecutive heads.
      *
      * Simulates flipping a coin multiple times and checks whether there exists any consecutive run of heads of the specified length.
      *
      * Note: This is computed via simulation, not analytically.
      *
      * @param streakLength
      *   Length of the consecutive heads streak to look for
      * @param totalFlips
      *   Total number of flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if a streak of the specified length occurs
      * @example
      *   {{{
      *   // What's the chance of getting 3 heads in a row within 10 flips?
      *   val hasStreak = Uncertain.coinFlipStreak(
      *     streakLength = 3,
      *     totalFlips = 10
      *   )
      *
      *   // Looking for 5 consecutive successes in 20 attempts
      *   val fiveInARow = Uncertain.coinFlipStreak(
      *     streakLength = 5,
      *     totalFlips = 20,
      *     chanceOfHeads = 0.8
      *   )
      *   }}}
      */
    def coinFlipStreak(
      streakLength: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(streakLength > 0, "Streak length must be positive")
      require(streakLength <= totalFlips, s"Cannot find streak of $streakLength in only $totalFlips flips")

      if (streakLength == 1) {
        // At least one heads in totalFlips
        coinFlipAtLeast(minHeads = 1, totalFlips = totalFlips, chanceOfHeads = chanceOfHeads)
      } else {
        Uncertain { () =>
          // Simulate flips and check for streak using sliding window
          val flips = List.fill(totalFlips)(coinFlip(chanceOfHeads).sample())
          flips.sliding(streakLength).exists(window => window.forall(identity))
        }
      }
    }

    /** Tests if a sequence contains at least N heads.
      *
      * Uses the binomial distribution to efficiently determine if we meet or exceed the minimum number of heads, without simulating each individual flip.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param minHeads
      *   Minimum number of heads required
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if we get at least minHeads heads
      * @example
      *   {{{
      *   // At least 7 out of 10 correct?
      *   val passTest = Uncertain.coinFlipAtLeast(
      *     minHeads = 7,
      *     totalFlips = 10,
      *     chanceOfHeads = 0.7
      *   )
      *
      *   // Quality threshold: at least 95 good items out of 100
      *   val meetsQuality = Uncertain.coinFlipAtLeast(
      *     minHeads = 95,
      *     totalFlips = 100,
      *     chanceOfHeads = 0.98
      *   )
      *
      *   // Majority vote (more than half)
      *   val majority = Uncertain.coinFlipAtLeast(
      *     minHeads = 6,
      *     totalFlips = 10
      *   )
      *   }}}
      */
    def coinFlipAtLeast(
      minHeads: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(minHeads >= 0, "Minimum heads must be non-negative")
      require(minHeads <= totalFlips, s"Cannot get $minHeads heads in only $totalFlips flips")

      if (minHeads == 0) {
        Uncertain.always(true) // Always get at least 0 heads
      } else {
        // Use binomial distribution to get count of heads, then check if >= minHeads
        Uncertain
          .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
          .map(_ >= minHeads)
      }
    }

    /** Tests if a sequence contains at most N heads.
      *
      * The complement of "at least" — useful for modeling upper bounds or failure conditions.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param maxHeads
      *   Maximum number of heads allowed
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if we get at most maxHeads heads
      * @example
      *   {{{
      *   // Quality control: at most 2 defects in 100 items
      *   val acceptable = Uncertain.coinFlipAtMost(
      *     maxHeads = 2,
      *     totalFlips = 100,
      *     chanceOfHeads = 0.01  // 1% defect rate
      *   )
      *
      *   // Security: no more than 3 failed login attempts
      *   val notLocked = Uncertain.coinFlipAtMost(
      *     maxHeads = 3,
      *     totalFlips = 10,
      *     chanceOfHeads = 0.1
      *   )
      *   }}}
      */
    def coinFlipAtMost(
      maxHeads: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(maxHeads >= 0, "Maximum heads must be non-negative")

      if (maxHeads >= totalFlips) {
        Uncertain.always(true) // Always get at most totalFlips heads
      } else {
        // Use binomial distribution to get count of heads, then check if <= maxHeads
        Uncertain
          .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
          .map(_ <= maxHeads)
      }
    }

    /** Tests if a sequence contains between min and max heads (inclusive).
      *
      * Useful for modeling acceptable ranges or tolerance bands.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param minHeads
      *   Minimum number of heads (inclusive)
      * @param maxHeads
      *   Maximum number of heads (inclusive)
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if heads count is in [minHeads, maxHeads]
      * @example
      *   {{{
      *   // Acceptable range: 45-55 successes out of 100
      *   val withinTolerance = Uncertain.coinFlipBetween(
      *     minHeads = 45,
      *     maxHeads = 55,
      *     totalFlips = 100
      *   )
      *
      *   // Test score range: 7-9 correct out of 10
      *   val goodGrade = Uncertain.coinFlipBetween(
      *     minHeads = 7,
      *     maxHeads = 9,
      *     totalFlips = 10,
      *     chanceOfHeads = 0.8
      *   )
      *   }}}
      */
    def coinFlipBetween(
      minHeads: Int,
      maxHeads: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(minHeads >= 0, "Minimum heads must be non-negative")
      require(maxHeads >= minHeads, s"Maximum heads ($maxHeads) must be >= minimum heads ($minHeads)")
      require(maxHeads <= totalFlips, s"Maximum heads ($maxHeads) cannot exceed total flips ($totalFlips)")

      // Use binomial distribution to get count of heads, then check if in range
      Uncertain
        .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
        .map(heads => heads >= minHeads && heads <= maxHeads)
    }
  }
}
