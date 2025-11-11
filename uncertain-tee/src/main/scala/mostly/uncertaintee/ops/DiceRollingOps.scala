/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

import scala.util.Random

/** Operations for simulating dice rolls in tabletop gaming, probability exercises, and Monte Carlo simulations.
  *
  * Import with:
  * {{{
  * import mostly.uncertaintee.syntax.dice.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait DiceRollingOps {

  extension (u: Uncertain.type) {

    // ---------------------------------
    // ----- Standard Polyhedral Dice ----
    // ---------------------------------

    /** Rolls a 2-sided die (equivalent to a coin flip, but returns 1 or 2).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer: 1 or 2.
      * @example
      *   {{{
      * val coinLike = Uncertain.d2()
      * val isHeads = coinLike.map(_ == 2)  // Treat 2 as heads
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      */
    def d2(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 2)

    /** Rolls a 4-sided die (pyramid die).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 4.
      * @example
      *   {{{
      * val d4Roll = Uncertain.d4()
      * val damage = d4Roll.map(_ + 2)  // d4 + 2 damage
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      */
    def d4(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 4)

    /** Rolls a 6-sided die (standard cube die).
      *
      * This is the most common die, used in countless board games and probability examples.
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 6.
      * @example
      *   {{{
      * val monopolyDice = Uncertain.d6() + Uncertain.d6()
      * val yahtzee = List.fill(5)(Uncertain.d6())
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      * @see
      *   [[rollSeveral]] for rolling multiple d6.
      */
    def d6(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 6)

    /** Rolls an 8-sided die (octahedron).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 8.
      * @example
      *   {{{
      * val longswordDamage = Uncertain.d8()
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      */
    def d8(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 8)

    /** Rolls a 10-sided die (pentagonal trapezohedron).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 10.
      * @example
      *   {{{
      * val d10Roll = Uncertain.d10()
      * // Often used in pairs for percentile rolls
      * val twoD10 = d10 + d10
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      * @see
      *   [[d100]] for percentile rolls.
      */
    def d10(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 10)

    /** Rolls a 12-sided die (dodecahedron).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 12.
      * @example
      *   {{{
      * val greataxeDamage = Uncertain.d12()
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      */
    def d12(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.rollDie(sides = 12)

    /** Rolls a 20-sided die (icosahedron) — the iconic RPG die.
      *
      * In D&D and similar games, this is used for attack rolls, saving throws, and ability checks. Rolling a 20 is a "natural 20" or critical success; rolling a 1 is a critical
      * failure.
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 20.
      * @example
      *   {{{
      * val attackRoll = Uncertain.d20()
      * val isCriticalHit = attackRoll.map(_ == 20)
      * val isCriticalFail = attackRoll.map(_ == 1)
      *
      * // Attack roll with modifier
      * val attack = Uncertain.d20().map(_ + 5)  // +5 to attack
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      * @see
      *   [[rollKeepHighest]] for advantage mechanics.
      * @see
      *   [[rollKeepLowest]] for disadvantage mechanics.
      */
    def d20(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 20)

    /** Rolls a 100-sided die (percentile die).
      *
      * Often simulated by rolling two d10s (one for tens, one for ones). Used for percentile-based systems and random tables.
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 100.
      * @example
      *   {{{
      * val percentileRoll = Uncertain.d100()
      *
      * // Random encounter table
      * val encounter = percentileRoll.map {
      * case n if n <= 20 => "Nothing"
      * case n if n <= 60 => "Common enemy"
      * case n if n <= 90 => "Rare enemy"
      * case _ => "Boss!"
      * }
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice.
      */
    def d100(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 100)

    // -------------------------
    // ----- Descriptive Aliases ----
    // -------------------------

    /** Rolls a 2-sided die (1 or 2).
      *
      * This is an alias for [[d2]].
      * @return
      *   An uncertain integer: 1 or 2.
      */
    def twoSidedDie(using random: Random = new Random()): Uncertain[Int] = d2

    /** Rolls a 4-sided die (1 to 4).
      *
      * This is an alias for [[d4]].
      * @return
      *   An uncertain integer from 1 to 4.
      */
    def fourSidedDie(using random: Random = new Random()): Uncertain[Int] = d4

    /** Rolls a 6-sided die (1 to 6).
      *
      * This is an alias for [[d6]].
      * @return
      *   An uncertain integer from 1 to 6.
      */
    def sixSidedDie(using random: Random = new Random()): Uncertain[Int] = d6

    /** Rolls an 8-sided die (1 to 8).
      *
      * This is an alias for [[d8]].
      * @return
      *   An uncertain integer from 1 to 8.
      */
    def eightSidedDie(using random: Random = new Random()): Uncertain[Int] = d8

    /** Rolls a 10-sided die (1 to 10).
      *
      * This is an alias for [[d10]].
      * @return
      *   An uncertain integer from 1 to 10.
      */
    def tenSidedDie(using random: Random = new Random()): Uncertain[Int] = d10

    /** Rolls a 12-sided die (1 to 12).
      *
      * This is an alias for [[d12]].
      * @return
      *   An uncertain integer from 1 to 12.
      */
    def twelveSidedDie(using random: Random = new Random()): Uncertain[Int] = d12

    /** Rolls a 20-sided die (1 to 20).
      *
      * This is an alias for [[d20]].
      * @return
      *   An uncertain integer from 1 to 20.
      */
    def twentySidedDie(using random: Random = new Random()): Uncertain[Int] = d20

    /** Rolls a 100-sided die (1 to 100).
      *
      * This is an alias for [[d100]].
      * @return
      *   An uncertain integer from 1 to 100.
      */
    def oneHundredSidedDie(using random: Random = new Random()): Uncertain[Int] = d100

    // ------------------------------
    // ----- Core Rolling Mechanics ----
    // ------------------------------

    /** Rolls a single die with the specified number of sides.
      *
      * This is the fundamental building block for most other dice operations. Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @param sides
      *   Number of sides on the die (must be ≥ 1).
      * @return
      *   An uncertain integer from 1 to `sides` (inclusive).
      * @example
      *   {{{
      * val d6 = Uncertain.rollDie(6)
      * val d30 = Uncertain.rollDie(30)  // Rare but exists!
      * val d1000 = Uncertain.rollDie(1000)
      *   }}}
      */
    def rollDie(sides: Int)(using random: Random = new Random()): Uncertain[Int] = {
      require(sides >= 1, s"Die must have at least one side, got: $sides")
      Uncertain.fromRange(
        from = 1 to sides
      )
    }

    /** Rolls multiple different dice and sums their results.
      *
      * This is useful for RPG-style dice notation like "d20 + 2d6 + d8". For rolling multiple *identical* dice, [[rollSeveral]] is more efficient.
      *
      * @param dice
      *   The number of sides for each die to roll (e.g., 20, 6, 6, 8 for "d20 + 2d6 + d8").
      * @return
      *   An uncertain integer representing the sum of all dice.
      * @example
      *   {{{
      * // Attack: d20 + d6 (weapon) + d8 (sneak attack)
      * val sneakAttack = Uncertain.roll(20, 6, 8)
      *
      * // This is equivalent to, but more concise than:
      * val sneakAttack = d20 + d6 + d8
      *   }}}
      * @see
      *   [[rollSeveral]] for rolling multiple identical dice (e.g., "8d6").
      * @see
      *   [[rollDie]] for rolling a single die.
      */
    def roll(dice: Int*)(using random: Random = new Random()): Uncertain[Int] = {
      require(dice.nonEmpty, "Must specify at least one die")
      require(dice.forall(_ >= 1), s"All dice must have at least one side, got: ${dice.mkString(", ")}")
      dice.map(rollDie).reduce(_ + _)
    }

    /** Rolls multiple identical dice and sums their results.
      *
      * This simulates the common "XdY" dice notation (e.g., "3d6", "8d6").
      *
      * @param numberOfDice
      *   How many dice to roll (must be ≥ 0).
      * @param sides
      *   Number of sides on each die (must be ≥ 1).
      * @return
      *   An uncertain integer representing the sum (range: `numberOfDice` to `numberOfDice * sides`).
      * @example
      *   {{{
      * // 2d6 (common in many board games)
      * val monopoly = Uncertain.rollSeveral(numberOfDice = 2, sides = 6)
      *
      * // 3d6 (e.g. D&D ability scores)
      * val abilityScore = Uncertain.rollSeveral(numberOfDice = 3, sides = 6)
      *
      * // 8d6 fireball damage
      * val fireball = Uncertain.rollSeveral(numberOfDice = 8, sides = 6)
      *
      * println(s"Average 2d6: ${monopoly.mean()}")  // ~7.0
      *   }}}
      * @see
      *   [[roll]] for rolling different types of dice (e.g., "d20 + d6").
      * @see
      *   [[rollDie]] for rolling a single die.
      */
    def rollSeveral(
      numberOfDice: Int,
      sides: Int
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 0, s"Cannot roll negative number of dice, got: $numberOfDice")
      require(sides >= 1, s"Die must have at least one side, got: $sides")
      if (numberOfDice == 0) Uncertain.always(0)
      else if (numberOfDice == 1) rollDie(sides)
      else Uncertain(() => rollDie(sides).take(numberOfDice).sum)
    }

    // ------------------------------------------
    // ----- Keep/Drop Mechanics (Advantage) ----
    // ------------------------------------------

    /** Rolls multiple dice and keeps only the highest N results, summing them.
      *
      * This is used for "advantage" mechanics (roll 2, keep 1) or ability score generation (roll 4, keep 3).
      *
      * @param numberOfDice
      *   Total number of dice to roll (must be ≥ 1).
      * @param sides
      *   Number of sides on each die.
      * @param keep
      *   How many of the highest dice to keep and sum (must be `1 <= keep <= numberOfDice`).
      * @return
      *   An uncertain integer representing the sum of the highest `keep` dice.
      * @example
      *   {{{
      * // D&D 5e Advantage: roll 2d20, keep highest 1
      * val withAdvantage = Uncertain.rollKeepHighest(
      * numberOfDice = 2,
      * sides = 20,
      * keep = 1
      * )
      *
      * // Roll 4d6, drop lowest (i.e., keep highest 3) for ability scores
      * val abilityScore = Uncertain.rollKeepHighest(
      * numberOfDice = 4,
      * sides = 6,
      * keep = 3
      * )
      *   }}}
      * @see
      *   [[rollWithAdvantage]] for the specific 2-dice-keep-1 case.
      * @see
      *   [[rollKeepLowest]] for disadvantage mechanics.
      * @see
      *   [[rollSeveral]] for rolling without dropping.
      */
    def rollKeepHighest(
      numberOfDice: Int,
      sides: Int,
      keep: Int = 1
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 1, s"Must roll at least one die, got: $numberOfDice")
      require(sides >= 1, s"Die must have at least one side, got: $sides")
      require(keep >= 1, s"Must keep at least one die, got: $keep")
      require(keep <= numberOfDice, s"Cannot keep $keep dice when only rolling $numberOfDice")
      Uncertain { () =>
        rollDie(sides)
          .take(numberOfDice)
          .sorted
          .takeRight(keep)
          .sum
      }
    }

    /** Rolls multiple dice and keeps only the lowest N results, summing them.
      *
      * This is used for "disadvantage" mechanics (roll 2, keep 1).
      *
      * @param numberOfDice
      *   Total number of dice to roll (must be ≥ 1).
      * @param sides
      *   Number of sides on each die.
      * @param keep
      *   How many of the lowest dice to keep and sum (must be `1 <= keep <= numberOfDice`).
      * @return
      *   An uncertain integer representing the sum of the lowest `keep` dice.
      * @example
      *   {{{
      * // D&D 5e Disadvantage: roll 2d20, keep lowest 1
      * val withDisadvantage = Uncertain.rollKeepLowest(
      * numberOfDice = 2,
      * sides = 20,
      * keep = 1
      * )
      *
      * // Roll 4d6, drop highest (i.e., keep lowest 3)
      * val lowScore = Uncertain.rollKeepLowest(
      * numberOfDice = 4,
      * sides = 6,
      * keep = 3
      * )
      *   }}}
      * @see
      *   [[rollWithDisadvantage]] for the specific 2-dice-keep-1 case.
      * @see
      *   [[rollKeepHighest]] for advantage mechanics.
      * @see
      *   [[rollSeveral]] for rolling without dropping.
      */
    def rollKeepLowest(
      numberOfDice: Int,
      sides: Int,
      keep: Int = 1
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 1, s"Must roll at least one die, got: $numberOfDice")
      require(sides >= 1, s"Die must have at least one side, got: $sides")
      require(keep >= 1, s"Must keep at least one die, got: $keep")
      require(keep <= numberOfDice, s"Cannot keep $keep dice when only rolling $numberOfDice")
      Uncertain { () =>
        rollDie(sides)
          .take(numberOfDice)
          .sorted
          .take(keep)
          .sum
      }
    }

    /** Rolls two dice and keeps the highest result (D&D 5e "Advantage").
      *
      * This is a common mechanic where circumstances favor the roller. It is a specific alias for `rollKeepHighest(numberOfDice = 2, sides = sides, keep = 1)`.
      *
      * @param sides
      *   The number of sides on the die (e.g., 20 for a d20).
      * @return
      *   An uncertain integer representing the higher of two rolls.
      * @example
      *   {{{
      * // Attack with advantage
      * val attack = Uncertain.rollWithAdvantage(sides = 20)
      * val check = Uncertain.rollWithAdvantage(sides = 20).map(_ + 5) // +5 modifier
      *   }}}
      * @see
      *   [[rollWithDisadvantage]] for the opposite mechanic.
      * @see
      *   [[rollKeepHighest]] for the general-purpose function.
      */
    def rollWithAdvantage(sides: Int)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.rollKeepHighest(
        numberOfDice = 2,
        sides = sides,
        keep = 1
      )(using random)

    /** Rolls two dice and keeps the lowest result (D&D 5e "Disadvantage").
      *
      * This is a common mechanic where circumstances hinder the roller. It is a specific alias for `rollKeepLowest(numberOfDice = 2, sides = sides, keep = 1)`.
      *
      * @param sides
      *   The number of sides on the die (e.g., 20 for a d20).
      * @return
      *   An uncertain integer representing the lower of two rolls.
      * @example
      *   {{{
      * // Attack with disadvantage
      * val attack = Uncertain.rollWithDisadvantage(sides = 20)
      * val check = Uncertain.rollWithDisadvantage(sides = 20).map(_ + 3) // +3 modifier
      *   }}}
      * @see
      *   [[rollWithAdvantage]] for the opposite mechanic.
      * @see
      *   [[rollKeepLowest]] for the general-purpose function.
      */
    def rollWithDisadvantage(sides: Int)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.rollKeepLowest(
        numberOfDice = 2,
        sides = sides,
        keep = 1
      )(using random)

    // -----------------------------------
    // ----- Exploding Dice Mechanics ----
    // -----------------------------------

    /** Rolls a die with "exploding" mechanics — if you roll the maximum, roll again and add.
      *
      * This is a common mechanic in games like Savage Worlds. It is an alias for `rollExplodingAtThreshold(sides, explodeThreshold = sides, maxExplosions)`.
      *
      * @param sides
      *   Number of sides on the die (e.g., 6). The die explodes on this value.
      * @param maxExplosions
      *   Maximum number of times the die can explode (prevents infinite loops). Defaults to 100.
      * @return
      *   An uncertain integer that can exceed the normal maximum.
      * @example
      *   {{{
      * // Exploding d6: if you roll 6, roll again and add
      * val explodingD6 = Uncertain.rollExploding(sides = 6)
      * // Can result in 1-6, or 7-12 (on explosion), or 13-18 (two explosions), etc.
      *
      * val skillCheck = Uncertain.rollExploding(sides = 8, maxExplosions = 10)
      *   }}}
      * @see
      *   [[rollExplodingAtThreshold]] for a version with a custom threshold.
      * @see
      *   [[rollDie]] for standard non-exploding dice.
      */
    def rollExploding(
      sides: Int,
      maxExplosions: Int = 100
    )(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.rollExplodingAtThreshold(
        sides = sides,
        explodeThreshold = sides,
        maxExplosions = maxExplosions
      )(using random)

    /** Rolls a die with "exploding" mechanics, rerolling and adding when the roll meets or exceeds a threshold.
      *
      * This is a flexible version of the "exploding dice" mechanic, where the explosion can happen on values other than the maximum.
      *
      * @param sides
      *   Number of sides on the die (must be ≥ 2).
      * @param explodeThreshold
      *   The minimum roll value (inclusive) that triggers an explosion. Must be `1 <= explodeThreshold <= sides`.
      * @param maxExplosions
      *   Maximum number of times the die can explode (prevents infinite loops). Defaults to 100.
      * @return
      *   An uncertain integer that can exceed the normal maximum.
      * @example
      *   {{{
      * // Standard exploding d6: if you roll 6, roll again and add
      * val explodingD6 = Uncertain.rollExplodingAtThreshold(sides = 6, explodeThreshold = 6)
      *
      * // World of Darkness: Explode on 10s for a d10
      * val wodRoll = Uncertain.rollExplodingAtThreshold(sides = 10, explodeThreshold = 10)
      *
      * // Generous exploding d8: Explodes on 7 or 8
      * val generousD8 = Uncertain.rollExplodingAtThreshold(sides = 8, explodeThreshold = 7)
      *   }}}
      * @see
      *   [[rollExploding]] for the simpler version that explodes only on the max value.
      * @see
      *   [[rollDie]] for standard non-exploding dice.
      */
    def rollExplodingAtThreshold(
      sides: Int,
      explodeThreshold: Int,
      maxExplosions: Int = 100
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(sides >= 2, s"Exploding die must have at least 2 sides, got: $sides")
      require(maxExplosions >= 0, s"Max explosions must be non-negative, got: $maxExplosions")
      require(
        explodeThreshold >= 1 && explodeThreshold <= sides,
        s"Explode threshold must be within the die's range [1, $sides], got: $explodeThreshold"
      )
      Uncertain { () =>
        var total       = 0
        var explosions  = 0
        var keepRolling = true
        val die         = Uncertain.rollDie(sides)(using random)
        while (keepRolling && explosions <= maxExplosions) {
          val roll = die.sample()
          total += roll
          if (roll >= explodeThreshold && explosions < maxExplosions) {
            explosions += 1
          } else {
            keepRolling = false
          }
        }
        total
      }
    }

    // ----------------------------------------
    // ----- Dice Pool Mechanics (Counting) ----
    // ----------------------------------------

    /** Rolls a pool of dice and counts how many meet or exceed a success threshold.
      *
      * This is the core mechanic for many "dice pool" systems (e.g., World of Darkness, Shadowrun).
      *
      * @param numberOfDice
      *   The total number of dice to roll in the pool (must be ≥ 0).
      * @param sides
      *   The number of sides on each die (e.g., 10 for WoD).
      * @param successThreshold
      *   The minimum roll value (inclusive) to count as one success.
      * @return
      *   An uncertain integer representing the total number of successes.
      * @example
      *   {{{
      * // World of Darkness: Roll 5 d10s, success on 8+
      * val skillCheck = Uncertain.rollCountSuccesses(
      * numberOfDice = 5,
      * sides = 10,
      * successThreshold = 8
      * )
      *
      * // Shadowrun: Roll 7 d6s, success on 5+
      * val shadowrunCheck = Uncertain.rollCountSuccesses(
      * numberOfDice = 7,
      * sides = 6,
      * successThreshold = 5
      * )
      *   }}}
      * @see
      *   [[rollCountFailures]] to count non-successes.
      * @see
      *   [[rollCountMatches]] to count exact matches.
      */
    def rollCountSuccesses(
      numberOfDice: Int,
      sides: Int,
      successThreshold: Int
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 0, s"Cannot roll negative number of dice (was: $numberOfDice)")
      require(sides >= 1, s"Cannot roll dice with less than 1 side (was: $sides)")
      require(
        successThreshold >= 1 && successThreshold <= sides,
        s"Success threshold must be within the die's range [1, $sides], got: $successThreshold"
      )
      Uncertain { () =>
        Uncertain.rollDie(sides)(using random).take(numberOfDice).count(_ >= successThreshold)
      }
    }

    /** Rolls a pool of dice and counts how many *fail* to meet a success threshold.
      *
      * This is the inverse of [[rollCountSuccesses]]. It counts all dice that roll *less than* the threshold.
      *
      * @param numberOfDice
      *   The total number of dice to roll in the pool.
      * @param sides
      *   The number of sides on each die.
      * @param successThreshold
      *   The minimum roll value (inclusive) that *would* be a success.
      * @return
      *   An uncertain integer representing the total number of failures (rolls < `successThreshold`).
      * @example
      *   {{{
      * // Count failures: 5 d10s, success on 8+ (failures are 1-7)
      * val failures = Uncertain.rollCountFailures(
      * numberOfDice = 5,
      * sides = 10,
      * successThreshold = 8
      * )
      *
      * // Failures and successes should sum to the total dice
      * val successes = Uncertain.rollCountSuccesses(5, 10, 8)
      * val total = successes + failures // This will always be 5
      *   }}}
      * @see
      *   [[rollCountSuccesses]] for counting successes.
      */
    def rollCountFailures(
      numberOfDice: Int,
      sides: Int,
      successThreshold: Int
    )(using random: Random = new Random()): Uncertain[Int] = {
      // Use rollCountSuccesses's requirements
      val successes = Uncertain.rollCountSuccesses(numberOfDice, sides, successThreshold)(using random)
      numberOfDice - successes
    }

    /** Rolls a pool of dice and counts how many exactly match a target number.
      *
      * Useful for systems that look for specific numbers (e.g., "counting 6s" in Yahtzee, or "botches" on 1s in WoD).
      *
      * @param numberOfDice
      *   The total number of dice to roll in the pool (must be ≥ 0).
      * @param sides
      *   The number of sides on each die.
      * @param target
      *   The exact number to match.
      * @return
      *   An uncertain integer representing the total number of matching dice.
      * @example
      *   {{{
      *     // Count how many 6s are rolled in a pool of 10d6
      *     val sixes = Uncertain.rollCountMatches(
      *       numberOfDice = 10,
      *       sides = 6,
      *       target = 6
      *     )
      *     // Count botches (1s) in a 5d10 pool
      *       val botches = Uncertain.rollCountMatches(
      *       numberOfDice = 5,
      *       sides = 10,
      *       target = 1
      *     )
      *   }}}
      * @see
      *   [[rollCountSuccesses]] for counting thresholds.
      */
    def rollCountMatches(
      numberOfDice: Int,
      sides: Int,
      target: Int
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 0, s"Cannot roll negative number of dice (was: $numberOfDice)")
      require(sides >= 1, s"Cannot roll dice with less than 1 side (was: $sides)")
      require(
        target >= 1 && target <= sides,
        s"Target must be within the die's range [1, $sides], got: $target"
      )
      Uncertain { () =>
        Uncertain.rollDie(sides)(using random).take(numberOfDice).count(_ == target)
      }
    }

    // ----------------------------------------
    // ----- Exploding Dice Pool Mechanics ----
    // ----------------------------------------

    /** Rolls a pool of exploding dice and counts how many meet or exceed a success threshold.
      *
      * This combines dice pool counting with exploding dice mechanics: each die that rolls at or above the `explodeThreshold` (here specified indirectly by `successThreshold`)
      * explodes and is rolled again, adding to that die's total before the success comparison is made. This mirrors systems where high rolls can chain into even higher results
      * before being checked against a target.
      *
      * Uses [[rollExplodingAtThreshold]] internally and counts successes like [[rollCountSuccesses]].
      *
      * @param numberOfDice
      *   The total number of dice to roll in the pool (must be ≥ 0).
      * @param sides
      *   The number of sides on each die (must be ≥ 2 for exploding dice).
      * @param successThreshold
      *   The minimum roll value (inclusive) on the final per-die total to count as one success. Also used as the explosion threshold when rolling each die.
      * @param maxExplosions
      *   Maximum number of explosion chains per die (must be ≥ 0). Prevents unbounded looping on frequent explosions.
      * @return
      *   An uncertain integer representing the total number of successes in the pool.
      * @example
      *   {{ // World of Darkness-like: 10-sided dice, successes on 8+, explode on 10s (adjust threshold as desired) val pool = Uncertain.rollExplodingCountSuccesses( numberOfDice =
      *   5, sides = 10, successThreshold = 8, maxExplosions = 10 ) }}
      * @see
      *   [[rollCountSuccesses]] for non-exploding pools.
      * @see
      *   [[rollExplodingAtThreshold]] for details of the exploding mechanic.
      */
    def rollExplodingCountSuccesses(
      numberOfDice: Int,
      sides: Int,
      successThreshold: Int,
      maxExplosions: Int
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 0, s"Cannot roll negative number of dice (was: $numberOfDice)")
      require(sides >= 2, s"Exploding die must have at least 2 sides, got: $sides")
      require(successThreshold >= 1 && successThreshold <= sides, s"Success threshold must be within the die's range [1, $sides], got: $successThreshold")
      require(maxExplosions >= 0, s"Max explosions must be non-negative, got: $maxExplosions")
      Uncertain { () =>
        Uncertain
          .rollExplodingAtThreshold(sides = sides, explodeThreshold = successThreshold, maxExplosions = maxExplosions)(using random)
          .take(numberOfDice)
          .count(_ >= successThreshold)
      }
    }

    /** Rolls a pool of exploding dice and counts how many fail to meet a success threshold.
      *
      * This is the inverse of [[rollExplodingCountSuccesses]]: it counts all dice whose final exploding total is strictly less than `successThreshold`.
      *
      * Uses [[rollExplodingAtThreshold]] internally and counts failures like [[rollCountFailures]].
      *
      * @param numberOfDice
      *   The total number of dice to roll in the pool (must be ≥ 0).
      * @param sides
      *   The number of sides on each die (must be ≥ 2 for exploding dice).
      * @param successThreshold
      *   The minimum roll value (inclusive) that would be considered a success; failures are values below this. Also used as the explosion threshold when rolling each die.
      * @param maxExplosions
      *   Maximum number of explosion chains per die (must be ≥ 0).
      * @return
      *   An uncertain integer representing the total number of failures in the pool.
      * @example
      *   {{ val failures = Uncertain.rollExplodingCountFailures( numberOfDice = 7, sides = 6, successThreshold = 5, maxExplosions = 5 ) }}
      * @see
      *   [[rollCountFailures]] for non-exploding pools.
      * @see
      *   [[rollExplodingAtThreshold]] for details of the exploding mechanic.
      */
    def rollExplodingCountFailures(
      numberOfDice: Int,
      sides: Int,
      successThreshold: Int,
      maxExplosions: Int
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 0, s"Cannot roll negative number of dice (was: $numberOfDice)")
      require(sides >= 2, s"Exploding die must have at least 2 sides, got: $sides")
      require(successThreshold >= 1 && successThreshold <= sides, s"Success threshold must be within the die's range [1, $sides], got: $successThreshold")
      require(maxExplosions >= 0, s"Max explosions must be non-negative, got: $maxExplosions")
      Uncertain { () =>
        Uncertain
          .rollExplodingAtThreshold(
            sides = sides,
            explodeThreshold = successThreshold,
            maxExplosions = maxExplosions
          )(using random)
          .take(numberOfDice)
          .count(_ < successThreshold)
      }
    }
  }
}
