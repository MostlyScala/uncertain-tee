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

import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.{Uncertain, Zero}

import scala.util.Random

/** Operations for "X in Y", odds (10:1 odds) and percentages.
  *
  * Import with:
  * {{{
  * import mostly.uncertaintee.syntax.chance.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ChanceOps {

  extension (u: Uncertain.type) {

    // ----------------------
    // -- Probability (X in Y) --
    // ----------------------

    /** Models a simple "X in Y" probability.
      *
      * This models (favorable / total).
      *
      * @param x
      *   The number of favorable outcomes (the "X")
      * @param y
      *   The total number of outcomes (the "Y")
      * @return
      *   An uncertain Boolean that's true with probability X/Y
      * @note
      *   Runtime: O(1)
      * @example
      *   {{{
      * // A 3 in 5 chance
      * val chance = Uncertain.xInY(3, 5)
      * println(s"P(success): ${chance.mean()}") // ~0.6
      *   }}}
      */
    def xInY(x: Int, y: Int)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(x >= 0, "x must be non-negative")
      require(y > 0, "y must be positive (cannot divide by zero)")
      require(x <= y, "x must be <= y")
      if (x == 0) {
        Uncertain.always(false)
      } else if (x == y) {
        Uncertain.always(true)
      } else {
        Uncertain.bernoulliViaDouble(x.toDouble / y.toDouble)(using random)
      }
    }

    /** Models a simple "1 in Y" probability.
      *
      * @param y
      *   The total number of outcomes (the "Y")
      * @return
      *   An uncertain Boolean that's true with probability 1/Y
      * @note
      *   Runtime: O(1)
      * @example
      *   {{{
      * // A 1 in 5 chance
      * val chance = Uncertain.oneIn(5)
      *   }}}
      */
    def oneIn(y: Int)(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.xInY(1, y)(using random)

    // ------------------
    // -- "One in Y" Helpers
    // ------------------
    def oneInTwo(using random: Random = new Random()): Uncertain[Boolean]   = Uncertain.oneIn(2)(using random)
    def oneInThree(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.oneIn(3)(using random)
    def oneInFour(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.oneIn(4)(using random)
    def oneInFive(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.oneIn(5)(using random)
    def oneInSix(using random: Random = new Random()): Uncertain[Boolean]   = Uncertain.oneIn(6)(using random)
    def oneInSeven(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.oneIn(7)(using random)
    def oneInEight(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.oneIn(8)(using random)
    def oneInNine(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.oneIn(9)(using random)
    def oneInTen(using random: Random = new Random()): Uncertain[Boolean]   = Uncertain.oneIn(10)(using random)

    // ------------------
    // -- Odds (X to Y) --
    // ------------------

    /** Models a simple "X to Y" odds probability.
      *
      * This calculates probability from odds (favorable:unfavorable). For example, `odds(3, 1)` (3-to-1 odds) means 3 favorable outcomes and 1 unfavorable, for a total of 4. The
      * resulting probability is 3 / (3 + 1) = 0.75.
      *
      * @param favourable
      *   The number of favorable outcomes
      * @param unfavourable
      *   The number of unfavorable outcomes
      * @return
      *   An uncertain Boolean with probability X / (X + Y)
      */
    def odds(favourable: Int, unfavourable: Int)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(favourable >= 0, "x (favorable) must be non-negative")
      require(unfavourable >= 0, "y (unfavorable) must be non-negative")
      val total = favourable + unfavourable
      if (total == 0) {
        // 0-to-0 odds is undefined, return false
        Uncertain.always(false)
      } else {
        // Delegates to xInY, which already handles x=0 and x=total
        Uncertain.xInY(favourable, total)(using random)
      }
    }

    // ------------------
    // -- Common Odds Helpers
    // ------------------

    /** 1-to-1 odds ("even chance" or "fifty-fifty"). 50% probability. */
    def oddsOneToOne(using random: Random = new Random()): Uncertain[Boolean] = odds(1, 1)(using random)

    /** 2-to-1 odds. ~66.7% probability. */
    def oddsTwoToOne(using random: Random = new Random()): Uncertain[Boolean] = odds(2, 1)(using random)

    /** 3-to-1 odds. 75% probability. */
    def oddsThreeToOne(using random: Random = new Random()): Uncertain[Boolean] = odds(3, 1)(using random)

    /** 4-to-1 odds. 80% probability. */
    def oddsFourToOne(using random: Random = new Random()): Uncertain[Boolean] = odds(4, 1)(using random)

    /** 5-to-1 odds. ~83.3% probability. */
    def oddsFiveToOne(using random: Random = new Random()): Uncertain[Boolean] = odds(5, 1)(using random)

    /** 10-to-1 odds. ~90.9% probability. */
    def oddsTenToOne(using random: Random = new Random()): Uncertain[Boolean] = odds(10, 1)(using random)

    /** 1-to-2 odds. ~33.3% probability. */
    def oddsOneToTwo(using random: Random = new Random()): Uncertain[Boolean] = odds(1, 2)(using random)

    /** 1-to-3 odds. 25% probability. */
    def oddsOneToThree(using random: Random = new Random()): Uncertain[Boolean] = odds(1, 3)(using random)

    /** 1-to-4 odds. 20% probability. */
    def oddsOneToFour(using random: Random = new Random()): Uncertain[Boolean] = odds(1, 4)(using random)

    /** 1-to-5 odds. ~16.7% probability. */
    def oddsOneToFive(using random: Random = new Random()): Uncertain[Boolean] = odds(1, 5)(using random)

    /** 1-to-10 odds. ~9.1% probability. */
    def oddsOneToTen(using random: Random = new Random()): Uncertain[Boolean] = odds(1, 10)(using random)

    /** Models a simple "P percent" probability.
      *
      * negative percentages are always false, percentages larger than 100 are always true.
      *
      * @param p
      *   The percentage chance (e.g., 25.5 for 25.5%)
      * @return
      *   An uncertain Boolean that's true with probability P/100
      * @note
      *   Runtime: O(1)
      */
    def percent(p: Double)(using random: Random = new Random()): Uncertain[Boolean] =
      if (p <= Zero) {
        Uncertain.always(false)
      } else if (p >= 100.0) {
        Uncertain.always(true)
      } else {
        Uncertain.bernoulli(p / 100.0)(using random)
      }

    /** Models a simple "P permille" (parts per thousand) probability.
      *
      * You may know this as per mil, per mill, permil,permill, pro mill, pro mille, promill... you get the drift. A lot of langugaes and dictionaries (even within english)
      * disagree.
      *
      * https://en.wikipedia.org/wiki/Per_mille
      *
      * Negative values are always false, values larger than 1000 are always true.
      *
      * @param p
      *   The permille chance (e.g., 25.5 for 25.5‰ or 2.55%)
      * @return
      *   An uncertain Boolean that's true with probability P/1000
      * @note
      *   Runtime: O(1)
      * @example
      *   {{{
      * // A 15‰ (1.5%) chance
      * val chance = Uncertain.permille(15)
      * println(s"P(success): ${chance.mean()}") // ~0.015
      *   }}}
      */
    def permille(p: Double)(using random: Random = new Random()): Uncertain[Boolean] =
      if (p <= 0.0) {
        Uncertain.always(false)
      } else if (p >= 1000.0) {
        Uncertain.always(true)
      } else {
        Uncertain.bernoulliViaDouble(p / 1000.0)(using random)
      }

    /** a common synonym in germanic languages for [[permille]]. */
    def promille(p: Double)(using random: Random = new Random()): Uncertain[Boolean] = permille(p)(using random)

    // 0-9%
    def percent0(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.always(false)
    def percent1(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.01)(using random)
    def percent2(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.02)(using random)
    def percent3(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.03)(using random)
    def percent4(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.04)(using random)
    def percent5(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.05)(using random)
    def percent6(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.06)(using random)
    def percent7(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.07)(using random)
    def percent8(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.08)(using random)
    def percent9(using random: Random = new Random()): Uncertain[Boolean]  = Uncertain.bernoulli(0.09)(using random)
    // 10-19%
    def percent10(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.1)(using random)
    def percent11(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.11)(using random)
    def percent12(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.12)(using random)
    def percent13(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.13)(using random)
    def percent14(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.14)(using random)
    def percent15(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.15)(using random)
    def percent16(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.16)(using random)
    def percent17(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.17)(using random)
    def percent18(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.18)(using random)
    def percent19(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.29)(using random)
    // 20-29%
    def percent20(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.2)(using random)
    def percent21(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.21)(using random)
    def percent22(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.22)(using random)
    def percent23(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.23)(using random)
    def percent24(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.24)(using random)
    def percent25(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.25)(using random)
    def percent26(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.26)(using random)
    def percent27(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.27)(using random)
    def percent28(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.28)(using random)
    def percent29(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.29)(using random)
    // 30-39%
    def percent30(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.3)(using random)
    def percent31(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.31)(using random)
    def percent32(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.32)(using random)
    def percent33(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.33)(using random)
    def percent34(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.34)(using random)
    def percent35(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.35)(using random)
    def percent36(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.36)(using random)
    def percent37(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.37)(using random)
    def percent38(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.38)(using random)
    def percent39(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.39)(using random)
    // 40-49%
    def percent40(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.4)(using random)
    def percent41(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.41)(using random)
    def percent42(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.42)(using random)
    def percent43(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.43)(using random)
    def percent44(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.44)(using random)
    def percent45(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.45)(using random)
    def percent46(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.46)(using random)
    def percent47(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.47)(using random)
    def percent48(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.48)(using random)
    def percent49(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.49)(using random)
    // 50-59%
    def percent50(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.5)(using random)
    def percent51(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.51)(using random)
    def percent52(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.52)(using random)
    def percent53(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.53)(using random)
    def percent54(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.54)(using random)
    def percent55(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.55)(using random)
    def percent56(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.56)(using random)
    def percent57(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.57)(using random)
    def percent58(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.58)(using random)
    def percent59(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.59)(using random)
    // 60-69%
    def percent60(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.6)(using random)
    def percent61(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.61)(using random)
    def percent62(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.62)(using random)
    def percent63(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.63)(using random)
    def percent64(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.64)(using random)
    def percent65(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.65)(using random)
    def percent66(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.66)(using random)
    def percent67(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.67)(using random)
    def percent68(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.68)(using random)
    def percent69(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.69)(using random)
    // 70-79%
    def percent70(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.7)(using random)
    def percent71(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.71)(using random)
    def percent72(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.72)(using random)
    def percent73(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.73)(using random)
    def percent74(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.74)(using random)
    def percent75(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.75)(using random)
    def percent76(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.76)(using random)
    def percent77(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.77)(using random)
    def percent78(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.78)(using random)
    def percent79(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.79)(using random)
    // 80-89%
    def percent80(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.8)(using random)
    def percent81(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.81)(using random)
    def percent82(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.82)(using random)
    def percent83(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.83)(using random)
    def percent84(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.84)(using random)
    def percent85(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.85)(using random)
    def percent86(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.86)(using random)
    def percent87(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.87)(using random)
    def percent88(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.88)(using random)
    def percent89(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.89)(using random)
    // 90-99%
    def percent90(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.9)(using random)
    def percent91(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.91)(using random)
    def percent92(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.92)(using random)
    def percent93(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.93)(using random)
    def percent94(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.94)(using random)
    def percent95(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.95)(using random)
    def percent96(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.96)(using random)
    def percent97(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.97)(using random)
    def percent98(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.98)(using random)
    def percent99(using random: Random = new Random()): Uncertain[Boolean] = Uncertain.bernoulli(0.99)(using random)
  }
}
