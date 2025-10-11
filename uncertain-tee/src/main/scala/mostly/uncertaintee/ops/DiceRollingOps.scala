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

    // ============================================================================
    // STANDARD POLYHEDRAL DICE (Common in RPGs)
    // ============================================================================

    /** Rolls a 2-sided die (equivalent to a coin flip, but returns 1 or 2).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer: 1 or 2
      * @example
      *   {{{
      *   val coinLike = Uncertain.d2()
      *   val isHeads = coinLike.map(_ == 2)  // Treat 2 as heads
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      */
    def d2(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 2)

    /** Rolls a 4-sided die (pyramid die).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 4
      * @example
      *   {{{
      *   val d4Roll = Uncertain.d4()
      *   val damage = d4Roll.map(_ + 2)  // d4 + 2 damage
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      */
    def d4(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 4)

    /** Rolls a 6-sided die (standard cube die).
      *
      * This is the most common die, used in countless board games and probability examples.
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 6
      * @example
      *   {{{
      *   val monopolyDice = Uncertain.d6() + Uncertain.d6()
      *   val yahtzee = List.fill(5)(Uncertain.d6())
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      * @see
      *   [[rollSeveral]] for rolling multiple d6
      */
    def d6(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 6)

    /** Rolls an 8-sided die (octahedron).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 8
      * @example
      *   {{{
      *   val longswordDamage = Uncertain.d8()
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      */
    def d8(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 8)

    /** Rolls a 10-sided die (pentagonal trapezohedron).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 10
      * @example
      *   {{{
      *   val d10Roll = Uncertain.d10()
      *   // Often used in pairs for percentile rolls
      *   val twoD10 = d10 + d10
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      * @see
      *   [[d100]] for percentile rolls
      */
    def d10(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 10)

    /** Rolls a 12-sided die (dodecahedron).
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 12
      * @example
      *   {{{
      *   val greataxeDamage = Uncertain.d12()
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
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
      *   An uncertain integer from 1 to 20
      * @example
      *   {{{
      *   val attackRoll = Uncertain.d20()
      *   val isCriticalHit = attackRoll.map(_ == 20)
      *   val isCriticalFail = attackRoll.map(_ == 1)
      *
      *   // Attack roll with modifier
      *   val attack = Uncertain.d20().map(_ + 5)  // +5 to attack
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      * @see
      *   [[rollKeepHighest]] for advantage mechanics
      * @see
      *   [[rollKeepLowest]] for disadvantage mechanics
      */
    def d20(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 20)

    /** Rolls a 100-sided die (percentile die).
      *
      * Often simulated by rolling two d10s (one for tens, one for ones). Used for percentile-based systems and random tables.
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @return
      *   An uncertain integer from 1 to 100
      * @example
      *   {{{
      *   val percentileRoll = Uncertain.d100()
      *
      *   // Random encounter table
      *   val encounter = percentileRoll.map {
      *     case n if n <= 20 => "Nothing"
      *     case n if n <= 60 => "Common enemy"
      *     case n if n <= 90 => "Rare enemy"
      *     case _ => "Boss!"
      *   }
      *   }}}
      * @see
      *   [[rollDie]] for custom-sided dice
      */
    def d100(using random: Random = new Random()): Uncertain[Int] = Uncertain.rollDie(sides = 100)

    // ============================================================================
    // DESCRIPTIVE ALIASES
    // ============================================================================

    /** Alias for [[d2]] — rolls a 2-sided die. */
    def twoSidedDie(using random: Random = new Random()): Uncertain[Int] = d2

    /** Alias for [[d4]] — rolls a 4-sided die. */
    def fourSidedDie(using random: Random = new Random()): Uncertain[Int] = d4

    /** Alias for [[d6]] — rolls a standard 6-sided die. */
    def sixSidedDie(using random: Random = new Random()): Uncertain[Int] = d6

    /** Alias for [[d8]] — rolls an 8-sided die. */
    def eightSidedDie(using random: Random = new Random()): Uncertain[Int] = d8

    /** Alias for [[d10]] — rolls a 10-sided die. */
    def tenSidedDie(using random: Random = new Random()): Uncertain[Int] = d10

    /** Alias for [[d12]] — rolls a 12-sided die. */
    def twelveSidedDie(using random: Random = new Random()): Uncertain[Int] = d12

    /** Alias for [[d20]] — rolls a 20-sided die. */
    def twentySidedDie(using random: Random = new Random()): Uncertain[Int] = d20

    /** Alias for [[d100]] — rolls a 100-sided die (percentile). */
    def oneHundredSidedDie(using random: Random = new Random()): Uncertain[Int] = d100

    // ============================================================================
    // GENERAL DICE OPERATIONS
    // ============================================================================

    /** Rolls a single die with the specified number of sides.
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @param sides
      *   Number of sides on the die (must be ≥ 1)
      * @return
      *   An uncertain integer from 1 to sides (inclusive)
      * @example
      *   {{{
      *   val d6 = Uncertain.rollDie(6)
      *   val d30 = Uncertain.rollDie(30)  // Rare but exists!
      *   val d1000 = Uncertain.rollDie(1000)  // For when d100 isn't enough
      *   }}}
      */
    def rollDie(sides: Int)(using random: Random = new Random()): Uncertain[Int] = {
      require(sides >= 1, s"Die must have at least one side, got: $sides")
      Uncertain.uniformInt(minInclusive = 1, maxExclusive = sides + 1)
    }

    /** Rolls multiple different dice and sums their results.
      *
      * This is useful for RPG-style dice notation like "d20 + 2d6 + d8".
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @param dice
      *   The number of sides for each die to roll (e.g., 20, 6, 6, 8 for "d20 + 2d6 + d8")
      * @return
      *   An uncertain integer representing the sum of all dice
      * @example
      *   {{{
      *   // Fireball damage: 8d6
      *   val fireball = Uncertain.roll(6, 6, 6, 6, 6, 6, 6, 6)
      *   //... or more readable for repeated dice
      *   val eightD6 = Uncertain.rollSeveral(8, sides = 6)
      *
      *   // Attack: d20 + d6 (weapon) + d8 (sneak attack)
      *   val sneakAttack = Uncertain.roll(20, 6, 8)
      *   // ... or just use arithmetic
      *   val sneakAttack = d20 + d6 + d8
      *
      *   }}}
      * @see
      *   [[rollSeveral]] for rolling multiple identical dice
      * @see
      *   [[rollDie]] for rolling a single die
      */
    def roll(dice: Int*)(using random: Random = new Random()): Uncertain[Int] = {
      require(dice.nonEmpty, "Must specify at least one die")
      require(dice.forall(_ >= 1), s"All dice must have at least one side, got: ${dice.mkString(", ")}")
      dice.map(rollDie).reduce(_ + _)
    }

    /** Rolls multiple identical dice and sums their results.
      *
      * This is the most common dice notation in RPGs: "XdY" means "roll X dice with Y sides each."
      *
      * Uses a discrete uniform distribution: https://en.wikipedia.org/wiki/Discrete_uniform_distribution
      *
      * @param numberOfDice
      *   How many dice to roll (must be ≥ 0)
      * @param sides
      *   Number of sides on each die (defaults to 6 for standard dice)
      * @return
      *   An uncertain integer representing the sum (range: numberOfDice to numberOfDice × sides)
      * @example
      *   {{{
      *   // 2d6 (common in many board games)
      *   val monopoly = Uncertain.rollSeveral(2, sides = 6)
      *
      *   // 3d6 (e.g. D&D ability scores)
      *   val abilityScore = Uncertain.rollSeveral(3, sides = 6)
      *
      *   // 8d6 fireball damage
      *   val fireball = Uncertain.rollSeveral(8, sides = 6)
      *
      *   println(s"Average 2d6: ${monopoly.mean()}")  // ~7.0
      *   }}}
      * @see
      *   [[roll]] for rolling different types of dice
      * @see
      *   [[rollDie]] for rolling a single die
      * @see
      *   [[rollKeepHighest]] for advantage mechanics
      */
    def rollSeveral(numberOfDice: Int, sides: Int = 6)(using random: Random = new Random()): Uncertain[Int] = {
      require(numberOfDice >= 0, s"Cannot roll negative number of dice, got: $numberOfDice")
      require(sides >= 1, s"Die must have at least one side, got: $sides")
      Uncertain(() => rollDie(sides).take(numberOfDice).sum)
    }

    /** Rolls multiple dice and keeps only the highest N results (advantage mechanic).
      *
      * Common in D&D 5e "advantage" (roll 2d20, keep highest) and in dice pool systems.
      *
      * @param numberOfDice
      *   Total number of dice to roll
      * @param sides
      *   Number of sides on each die
      * @param keep
      *   How many of the highest dice to keep
      * @return
      *   An uncertain integer representing the sum of the highest N dice
      * @example
      *   {{{
      *   // D&D 5e Advantage: roll 2d20, keep highest
      *   val withAdvantage = Uncertain.rollKeepHighest(
      *     numberOfDice = 2,
      *     sides = 20,
      *     keep = 1
      *   )
      *
      *   // Roll 4d6, drop lowest (ability score generation)
      *   val abilityScore = Uncertain.rollKeepHighest(
      *     numberOfDice = 4,
      *     sides = 6,
      *     keep = 3
      *   )
      *   }}}
      * @see
      *   [[rollKeepLowest]] for disadvantage mechanics
      * @see
      *   [[rollSeveral]] for rolling without dropping
      */
    def rollKeepHighest(
      numberOfDice: Int,
      sides: Int,
      keep: Int
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

    /** Rolls multiple dice and keeps only the lowest N results (disadvantage mechanic).
      *
      * Common in D&D 5e "disadvantage" (roll 2d20, keep lowest) and in penalty systems.
      *
      * @param numberOfDice
      *   Total number of dice to roll
      * @param sides
      *   Number of sides on each die
      * @param keep
      *   How many of the lowest dice to keep
      * @return
      *   An uncertain integer representing the sum of the lowest N dice
      * @example
      *   {{{
      *   // D&D 5e Disadvantage: roll 2d20, keep lowest
      *   val withDisadvantage = Uncertain.rollKeepLowest(
      *     numberOfDice = 2,
      *     sides = 20,
      *     keep = 1
      *   )
      *   }}}
      * @see
      *   [[rollKeepHighest]] for advantage mechanics
      * @see
      *   [[rollSeveral]] for rolling without dropping
      */
    def rollKeepLowest(
      numberOfDice: Int,
      sides: Int,
      keep: Int
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

    /** Rolls a die with "exploding" mechanics — if you roll the maximum, roll again and add.
      *
      * Common in games like Savage Worlds and some Shadowrun mechanics.
      *
      * @param sides
      *   Number of sides on the die
      * @param maxExplosions
      *   Maximum number of times the die can explode (prevents infinite loops)
      * @return
      *   An uncertain integer that can exceed the normal maximum
      * @example
      *   {{{
      *   // Exploding d6: if you roll 6, roll again and add
      *   val explodingD6 = Uncertain.rollExploding(sides = 6)
      *   // Can result in 1-6, or 7-12 (on explosion), or 13-18 (two explosions), etc.
      *
      *   // Savage Worlds-style "ace" mechanic
      *   val skillCheck = Uncertain.rollExploding(sides = 8, maxExplosions = 10)
      *   }}}
      * @see
      *   [[rollDie]] for standard non-exploding dice
      */
    def rollExploding(
      sides: Int,
      maxExplosions: Int = 10
    )(using random: Random = new Random()): Uncertain[Int] = {
      require(sides >= 2, s"Exploding die must have at least 2 sides, got: $sides")
      require(maxExplosions >= 0, s"Max explosions must be non-negative, got: $maxExplosions")

      Uncertain { () =>
        var total       = 0
        var explosions  = 0
        var keepRolling = true
        val die         = Uncertain.rollDie(sides)
        while (keepRolling && explosions <= maxExplosions) {
          val roll = die.sample()
          total += roll
          if (roll == sides && explosions < maxExplosions) {
            explosions += 1
          } else {
            keepRolling = false
          }
        }
        total
      }
    }
  }
}
