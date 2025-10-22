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

    // ===== Basic Operations =====

    /** Simulates a single coin flip.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param chanceOfHeads
      *   Probability of getting heads (true), defaults to 0.5 for a fair coin
      * @return
      *   An uncertain Boolean value representing the coin flip result
      * @note
      *   Runtime: O(1) - uses Bernoulli distribution
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

    /** Simulates a single flip of a fair coin (50/50 chance).
      *
      * @return
      *   An uncertain Boolean value (true for heads, false for tails)
      * @note
      *   Runtime: O(1)
      * @example
      *   {{{
      * val fairCoin = Uncertain.fairCoin()
      * val result = fairCoin.sample()
      *   }}}
      */
    def fairCoin(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulliViaDouble(0.5)

    // ===== Count-Based Operations =====

    /** Tests if a sequence of flips results in exactly N heads.
      *
      * This uses the binomial distribution to efficiently determine if exactly the specified number of heads occurred, without simulating each individual flip.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param heads
      *   The exact number of heads we're testing for
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if we get exactly the specified number of heads
      * @note
      *   Runtime: O(totalFlips) - samples from binomial distribution
      * @example
      *   {{{
      *   // Did we get exactly 5 heads in 10 flips?
      *   val exactly5 = Uncertain.coinFlipExactly(
      *     heads = 5,
      *     totalFlips = 10
      *   )
      *
      *   // Testing for perfect 50/50 split
      *   val perfectSplit = Uncertain.coinFlipExactly(
      *     heads = 50,
      *     totalFlips = 100
      *   )
      *   println(s"P(exactly 50/100): ${perfectSplit.mean()}")  // ~7.96%
      *   }}}
      */
    def coinFlipExactly(
      heads: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(heads >= 0, "Exact heads must be non-negative")
      require(heads <= totalFlips, s"Cannot get $heads heads in only $totalFlips flips")

      Uncertain
        .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
        .map(_ == heads)
    }

    /** Tests if a sequence contains at least N heads.
      *
      * Uses the binomial distribution to efficiently determine if we meet or exceed the minimum number of heads, without simulating each individual flip.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param heads
      *   Minimum number of heads required
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if we get at least the specified number of heads
      * @note
      *   Runtime: O(totalFlips) - samples from binomial distribution
      * @example
      *   {{{
      *   // At least 7 out of 10 correct?
      *   val passTest = Uncertain.coinFlipAtLeast(
      *     heads = 7,
      *     totalFlips = 10,
      *     chanceOfHeads = 0.7
      *   )
      *
      *   // Quality threshold: at least 95 good items out of 100
      *   val meetsQuality = Uncertain.coinFlipAtLeast(
      *     heads = 95,
      *     totalFlips = 100,
      *     chanceOfHeads = 0.98
      *   )
      *   }}}
      */
    def coinFlipAtLeast(
      heads: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(heads >= 0, "Minimum heads must be non-negative")
      require(heads <= totalFlips, s"Cannot get $heads heads in only $totalFlips flips")

      if (heads == 0) {
        Uncertain.always(true) // Always get at least 0 heads
      } else {
        Uncertain
          .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
          .map(_ >= heads)
      }
    }

    /** Tests if a sequence contains at most N heads.
      *
      * The complement of "at least" — useful for modeling upper bounds or failure conditions.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param heads
      *   Maximum number of heads allowed
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if we get at most the specified number of heads
      * @note
      *   Runtime: O(totalFlips) - samples from binomial distribution
      * @example
      *   {{{
      *   // Quality control: at most 2 defects in 100 items
      *   val acceptable = Uncertain.coinFlipAtMost(
      *     heads = 2,
      *     totalFlips = 100,
      *     chanceOfHeads = 0.01  // 1% defect rate
      *   )
      *
      *   // Security: no more than 3 failed login attempts
      *   val notLocked = Uncertain.coinFlipAtMost(
      *     heads = 3,
      *     totalFlips = 10,
      *     chanceOfHeads = 0.1
      *   )
      *   }}}
      */
    def coinFlipAtMost(
      heads: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(heads >= 0, "Maximum heads must be non-negative")

      if (heads >= totalFlips) {
        Uncertain.always(true) // Always get at most totalFlips heads
      } else {
        Uncertain
          .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
          .map(_ <= heads)
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
      * @note
      *   Runtime: O(totalFlips) - samples from binomial distribution
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

      Uncertain
        .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
        .map(h => h >= minHeads && h <= maxHeads)
    }

    // ===== Special Count Cases =====

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
      * @note
      *   Runtime: O(1) - analytically computed via probability formula
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
        val probabilityAllHeads = math.pow(chanceOfHeads, totalFlips)
        Uncertain.bernoulliViaDouble(probabilityAllHeads)
      }
    }

    /** Tests if NO flips in a sequence are heads (all tails).
      *
      * The complement of getting any heads at all.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param totalFlips
      *   Number of coin flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each individual flip
      * @return
      *   An uncertain Boolean that's true if all flips are tails
      * @note
      *   Runtime: O(1) - analytically computed via probability formula
      * @example
      *   {{{
      *   // Probability of no successes in 5 attempts
      *   val allFailed = Uncertain.coinFlipNone(
      *     totalFlips = 5,
      *     chanceOfHeads = 0.2
      *   )
      *
      *   // System never working in 10 checks (1% uptime)
      *   val neverUp = Uncertain.coinFlipNone(
      *     totalFlips = 10,
      *     chanceOfHeads = 0.01
      *   )
      *   }}}
      */
    def coinFlipNone(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")

      if (totalFlips == 0) {
        Uncertain.always(true) // Vacuous truth: all zero flips are tails
      } else {
        val probabilityAllTails = math.pow(1.0 - chanceOfHeads, totalFlips)
        Uncertain.bernoulliViaDouble(probabilityAllTails)
      }
    }

    /** Tests if at least one flip in a sequence is heads.
      *
      * The complement of getting no heads at all. This is one of the most common queries.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param totalFlips
      *   Number of coin flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each individual flip
      * @return
      *   An uncertain Boolean that's true if at least one flip is heads
      * @note
      *   Runtime: O(1) - analytically computed via probability formula
      * @example
      *   {{{
      *   // At least one success in 5 attempts
      *   val anySuccess = Uncertain.coinFlipAny(
      *     totalFlips = 5,
      *     chanceOfHeads = 0.2
      *   )
      *
      *   // Did the system work at least once?
      *   val workedOnce = Uncertain.coinFlipAny(
      *     totalFlips = 10,
      *     chanceOfHeads = 0.99
      *   )
      *   }}}
      */
    def coinFlipAny(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")

      if (totalFlips == 0) {
        Uncertain.always(false) // No flips means no heads
      } else {
        // P(at least one heads) = 1 - P(all tails)
        val probabilityAtLeastOne = 1.0 - math.pow(1.0 - chanceOfHeads, totalFlips)
        Uncertain.bernoulliViaDouble(probabilityAtLeastOne)
      }
    }

    /** Tests if more than half of the flips are heads (strict majority).
      *
      * Useful for voting, consensus, and other majority-rule scenarios. Requires more than 50% heads.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param totalFlips
      *   Total number of coin flips
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if strictly more than half are heads
      * @note
      *   Runtime: O(totalFlips) - samples from binomial distribution
      * @example
      *   {{{
      *   // Majority vote in 10 person committee
      *   val majority = Uncertain.coinFlipMajority(
      *     totalFlips = 10,
      *     chanceOfHeads = 0.6
      *   )
      *
      *   // Most tests passed
      *   val mostPassed = Uncertain.coinFlipMajority(
      *     totalFlips = 20,
      *     chanceOfHeads = 0.8
      *   )
      *   }}}
      */
    def coinFlipMajority(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")

      if (totalFlips == 0) {
        Uncertain.always(false) // No majority with zero flips
      } else {
        val requiredHeads = (totalFlips / 2) + 1
        Uncertain
          .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
          .map(_ >= requiredHeads)
      }
    }

    /** Tests if exactly half of the flips are heads (perfect 50/50 split).
      *
      * Only meaningful for even numbers of flips. Returns always false for odd numbers.
      *
      * Uses a binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution
      *
      * @param totalFlips
      *   Total number of coin flips (should be even for meaningful results)
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Boolean that's true if exactly half are heads
      * @note
      *   Runtime: O(totalFlips) for even totalFlips, O(1) for odd totalFlips
      * @example
      *   {{{
      *   // Perfect tie in 10 votes
      *   val tie = Uncertain.coinFlipExactlyHalf(totalFlips = 10)
      *
      *   // Evenly split 100 items
      *   val perfectSplit = Uncertain.coinFlipExactlyHalf(totalFlips = 100)
      *   println(s"P(50/100): ${perfectSplit.mean()}")  // ~7.96%
      *   }}}
      */
    def coinFlipExactlyHalf(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")

      if (totalFlips % 2 == 1) {
        Uncertain.always(false) // Odd number can't be exactly half
      } else if (totalFlips == 0) {
        Uncertain.always(true) // Vacuous: 0 is half of 0
      } else {
        val halfFlips = totalFlips / 2
        Uncertain
          .binomialViaDouble(trials = totalFlips, probability = chanceOfHeads)
          .map(_ == halfFlips)
      }
    }

    // ===== Pattern Operations =====

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
      * @note
      *   Runtime: O(totalFlips) - requires simulating all flips and checking sliding windows
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
        // At least one heads in totalFlips - use analytical formula
        coinFlipAny(totalFlips = totalFlips, chanceOfHeads = chanceOfHeads)
      } else if (streakLength == totalFlips) {
        // All heads - use analytical formula
        coinFlipAll(totalFlips = totalFlips, chanceOfHeads = chanceOfHeads)
      } else {
        Uncertain { () =>
          // Simulate flips and check for streak using sliding window
          val flips = List.fill(totalFlips)(coinFlip(chanceOfHeads).sample())
          flips.sliding(streakLength).exists(window => window.forall(identity))
        }
      }
    }

    /** Tests if a sequence contains a streak of consecutive tails.
      *
      * The complement operation to streak of heads - looks for consecutive failures rather than successes.
      *
      * Note: This is computed via simulation, not analytically (except for special cases).
      *
      * @param streakLength
      *   Length of the consecutive tails streak to look for
      * @param totalFlips
      *   Total number of flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each flip (tails = 1 - chanceOfHeads)
      * @return
      *   An uncertain Boolean that's true if a streak of the specified length of tails occurs
      * @note
      *   Runtime: O(totalFlips) - requires simulating all flips (delegates to coinFlipStreak with inverted probability)
      * @example
      *   {{{
      *   // What's the chance of getting 3 failures in a row within 10 attempts?
      *   val hasFailureStreak = Uncertain.coinFlipStreakTails(
      *     streakLength = 3,
      *     totalFlips = 10,
      *     chanceOfHeads = 0.8  // 80% success means 20% failure
      *   )
      *
      *   // System down for 5 consecutive checks
      *   val extendedOutage = Uncertain.coinFlipStreakTails(
      *     streakLength = 5,
      *     totalFlips = 20,
      *     chanceOfHeads = 0.95
      *   )
      *   }}}
      */
    def coinFlipStreakTails(
      streakLength: Int,
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")
      require(streakLength > 0, "Streak length must be positive")
      require(streakLength <= totalFlips, s"Cannot find streak of $streakLength in only $totalFlips flips")

      // Flip the probability - looking for tails streak is like looking for heads streak with inverted probability
      coinFlipStreak(
        streakLength = streakLength,
        totalFlips = totalFlips,
        chanceOfHeads = 1.0 - chanceOfHeads
      )
    }

    // ===== Measurement Operations (return Int instead of Boolean) =====

    /** Returns the length of the longest consecutive heads streak in a sequence.
      *
      * Unlike the Boolean operations, this returns an Uncertain[Int] representing the length of the longest run of heads found.
      *
      * Note: This is computed via simulation, not analytically.
      *
      * @param totalFlips
      *   Total number of flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Int representing the longest streak length
      * @note
      *   Runtime: O(totalFlips) - requires simulating all flips and tracking streaks
      * @example
      *   {{{
      *   // What's the distribution of longest streaks in 100 flips?
      *   val longestStreak = Uncertain.coinFlipLongestStreak(
      *     totalFlips = 100
      *   )
      *   println(s"Average longest streak: ${longestStreak.mean()}")
      *
      *   // Check if longest streak exceeds threshold
      *   val hasLongStreak = longestStreak.map(_ >= 5)
      *   }}}
      */
    def coinFlipLongestStreak(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(chanceOfHeads >= 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips >= 0, "Total flips must be non-negative")

      if (totalFlips == 0) {
        Uncertain.always(0)
      } else {
        Uncertain { () =>
          val flips = List.fill(totalFlips)(coinFlip(chanceOfHeads).sample())

          var currentStreak = 0
          var maxStreak     = 0

          flips.foreach { isHeads =>
            if (isHeads) {
              currentStreak += 1
              maxStreak = math.max(maxStreak, currentStreak)
            } else {
              currentStreak = 0
            }
          }

          maxStreak
        }
      }
    }

    /** Returns the position (1-indexed) of the first heads in a sequence, or 0 if no heads occur.
      *
      * Useful for "time to first success" scenarios. Returns 0 if all flips are tails.
      *
      * This models a truncated geometric distribution.
      *
      * Note: This is computed analytically, not via simulation.
      *
      * @param totalFlips
      *   Total number of flips to simulate
      * @param chanceOfHeads
      *   Probability of heads on each flip
      * @return
      *   An uncertain Int representing the 1-indexed position of first heads (0 if none)
      * @note
      *   Runtime: O(1) - uses geometric distribution sampler
      * @example
      *   {{{
      * // How long until first success?
      * val firstSuccess = Uncertain.coinFlipFirstHeadsAt(
      * totalFlips = 20,
      * chanceOfHeads = 0.1
      * )
      * println(s"Expected position: ${firstSuccess.mean()}")
      *
      * // Check if success happens in first 5 attempts
      * val earlySuccess = firstSuccess.map(pos => pos > 0 && pos <= 5)
      *   }}}
      */
    def coinFlipFirstHeadsAt(
      totalFlips: Int,
      chanceOfHeads: Double = 0.5
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(chanceOfHeads > 0 && chanceOfHeads <= 1, s"chanceOfHeads ($chanceOfHeads) must be between 0 and 1")
      require(totalFlips > 0, "Total flips must be a positive int")

      if (chanceOfHeads == 1) {
        Uncertain.always(0)
      } else {
        // Model as a Truncated Geometric Distribution
        // 1. Get a sample from the (unbounded) geometric distribution
        // 2. If the position is > totalFlips, it means no success occurred
        //    within the truncated limit, so we return 0.
        Uncertain.geometric(chanceOfHeads).map { position =>
          if (position > totalFlips) 0 else position
        }
      }
    }
  }
}
