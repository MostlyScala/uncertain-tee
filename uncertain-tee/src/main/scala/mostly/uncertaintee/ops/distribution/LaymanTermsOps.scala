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

package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

import scala.util.Random

/** Contains more "friendly" names than hardcore statistics naming; e.g. normal-distribution as bellcurve(centeredOn = ..., withSpread =...)
  */
trait LaymanTermsOps {

  extension (u: Uncertain.type) {

    /** Creates a bell curve (normal/Gaussian) distribution.
      *
      * This is the classic "bell curve" shape you see in many natural phenomena - heights, test scores, measurement errors, etc.
      *
      * Uses a normal/Gaussian distribution: https://en.wikipedia.org/wiki/Normal_distribution
      *
      * @param centeredOn
      *   The center/average value (where the peak of the bell is)
      * @param withSpread
      *   How spread out the values are (larger = more spread out)
      * @return
      *   An uncertain value following a bell curve distribution
      * @example
      *   {{{
      *   val humanHeight = Uncertain.bellCurve(centeredOn = 170.0, withSpread = 10.0)
      *   val testScore = Uncertain.bellCurve(centeredOn = 75.0, withSpread = 8.0)
      *   }}}
      */
    def bellCurve(centeredOn: Double, withSpread: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.normalDouble(mean = centeredOn, standardDeviation = withSpread)

    /** Creates a value that's uniformly random between two numbers (decimals).
      *
      * For integers, use `fromRange`
      *
      * Every value in the range has an equal chance of being selected.
      *
      * Uses a continuous uniform distribution: https://en.wikipedia.org/wiki/Continuous_uniform_distribution
      *
      * @param start
      *   Lower bound (inclusive)
      * @param end
      *   Upper bound (inclusive)
      * @return
      *   An uncertain double uniformly distributed in [start, end]
      * @example
      *   {{{
      *   val temperature = Uncertain.between(15.0, 30.0)
      *   val price = Uncertain.between(9.99, 19.99)
      *   }}}
      */
    def between(start: Double, end: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.uniformDouble(min = start, max = end)

    /** Picks one item from a collection with equal probability for each.
      *
      * Uses a categorical distribution: https://en.wikipedia.org/wiki/Categorical_distribution
      *
      * @param items
      *   The items to choose from
      * @return
      *   An uncertain value that will be one of the items
      * @example
      *   {{{
      *   val fruit = Uncertain.oneOf("apple", "banana", "orange")
      *   val direction = Uncertain.oneOf("north", "south", "east", "west")
      *   }}}
      */
    def oneOf[T](items: T*)(using random: Random = new Random()): Uncertain[T] = {
      require(items.nonEmpty, "Need at least one item to choose from")
      Uncertain.empirical(items)
    }

    /** Picks one item from a collection with specified weights/probabilities.
      *
      * Higher weights mean higher probability of selection. Weights don't need to sum to 1.
      *
      * Uses a categorical distribution: https://en.wikipedia.org/wiki/Categorical_distribution
      *
      * @param weightedItems
      *   Tuples of (item, weight) where weight represents relative probability
      * @return
      *   An uncertain value that will be one of the items
      * @example
      *   {{{
      *   val weather = Uncertain.oneOfWeighted(
      *     ("sunny", 0.6),
      *     ("cloudy", 0.3),
      *     ("rainy", 0.1)
      *   )
      *   }}}
      */
    def oneOfWeighted[T](weightedItems: (T, Double)*)(using random: Random = new Random()): Uncertain[T] = {
      require(weightedItems.nonEmpty, "Need at least one item to choose from")
      Uncertain.categoricalViaDouble(weightedItems.toMap)
    }

    /** "Lightning strikes twice" - models coinciding events. */
    def bothHappen(
      firstChance: Double,
      secondChance: Double
    ): Uncertain[Boolean] = {
      require(firstChance >= 0 && firstChance <= 1, s"Probability ($probabilityExceeds) must be between 0 and 1.")
      require(secondChance >= 0 && secondChance <= 1, s"Probability ($probabilityExceeds) must be between 0 and 1.")
      Uncertain.bernoulli(firstChance) && Uncertain.bernoulli(secondChance)
    }

    /** "Backup plan" - try first option, if it fails try second. (alias for [[orElse]]) */
    def withFallback[T](
      primaryOption: Uncertain[Option[T]],
      fallback: Uncertain[T]
    ): Uncertain[T] = primaryOption.orElse(fallback)
  }
}
