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

    /** Creates a 50/50 random true/false outcome.
      *
      * This is equivalent to a fair coin flip, but with clearer intent for yes/no decisions.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @return
      *   An uncertain Boolean with equal probability of true or false
      * @example
      *   {{{
      *   val shouldRetry = Uncertain.maybe()
      *   if (shouldRetry.sample()) {
      *     println("Retrying...")
      *   }
      *
      *   val optionalFeature = Uncertain.maybe()
      *   // Use in A/B testing, random branching, etc.
      *   }}}
      */
    def maybe()(using random: Random = new Random()): Uncertain[Boolean] =
      Uncertain.bernoulliViaDouble(probability = 0.5)

    /** Picks N random items from a collection without replacement.
      *
      * @param items
      *   The collection to pick from. For performance sensitive code, prefer IndexedSeq (Vector, Array, etc.) for O(1) random access.
      * @param n
      *   Number of items to pick (must be <= items.size)
      * @return
      *   An uncertain List containing n randomly selected items
      * @example
      *   {{{
      *   val contestants = List("Alice", "Bob", "Charlie", "Diana", "Eve")
      *   val winners = Uncertain.pickN(contestants, 3)
      *   // Sample: List("Diana", "Alice", "Charlie")
      *
      *   val deck = (1 to 52).toList
      *   val hand = Uncertain.pickN(deck, 5)
      *   // Deal a poker hand
      *
      *   val playlist = List("Song A", "Song B", "Song C", "Song D")
      *   val shuffled = Uncertain.pickN(playlist, playlist.size)
      *   }}}
      */
    def pickWithoutReplacement[T](items: Seq[T], n: Int)(using random: Random = new Random()): Uncertain[List[T]] = {
      require(n >= 0, "Cannot pick negative number of items")
      require(n <= items.size, s"Cannot pick $n items from collection of size ${items.size}")
      if (n > items.size / 2) {
        // For large N we do a shuffle since there's no point optimizing
        Uncertain(() => random.shuffle(items).take(n).toList)
      } else {
        // For small N, partial Fisher-Yates avoids unnecessary work. Is this faster than std lib shuffle? Maybe we should actually benchmark it.
        Uncertain { () =>
          val arrayOfShuffledIndexes: Array[Int] = items.indices.toArray
          // Partial Fisher-Yates: only shuffle the first N elements
          for (originalIndex <- 0 until n) {
            val indexToSwapWith = originalIndex + random.nextInt(items.size - originalIndex)
            // Swap the values at originalIndex and indexToSwapWith
            val temp            = arrayOfShuffledIndexes(indexToSwapWith)
            arrayOfShuffledIndexes(indexToSwapWith) = arrayOfShuffledIndexes(originalIndex)
            arrayOfShuffledIndexes(originalIndex) = temp
          }
          // Take the first N (the shuffled indexes) and retrieve the corresponding items
          arrayOfShuffledIndexes.iterator
            .take(n)
            .map(randomizedIndex => items(randomizedIndex))
            .toList
        }
      }

    }

    /** Draws a given number of items randomly from a collection, without replacement.
      *
      * This is like reaching into a hat and pulling out a few items — once an item is drawn, it can't be drawn again in the same sample.
      *
      * @param items
      *   The collection of items to draw from. For performance, prefer `IndexedSeq` (Vector, Array, etc.) for O(1) random access.
      * @param itemsToDraw
      *   Number of items to pick (must be <= items.size)
      * @return
      *   An uncertain `List` containing `itemsToDraw` randomly selected items
      * @example
      *   {{{
      *   val contestants = List("Alice", "Bob", "Charlie", "Diana")
      *   val winners = Uncertain.drawFromHat(contestants, 2)
      *   // Sample result: List("Charlie", "Alice")
      *
      *   val deck = (1 to 52).toList
      *   val hand = Uncertain.drawFromHat(deck, 5)
      *   // Deals a random poker hand
      *   }}}
      */
    def drawFromHat[T](items: Seq[T], itemsToDraw: Int)(using random: Random = Random()): Uncertain[List[T]] =
      Uncertain.pickWithoutReplacement(items = items, n = itemsToDraw)

    /** Simulates drawing straws and tells you if you drew one of the "short" straws.
      *
      * Each straw has an equal chance of being a short straw. By default, drawing a short straw returns `true`.
      *
      * @param totalStraws
      *   Total number of straws in the pool (must be >= 1)
      * @param shortStraws
      *   How many straws are considered "short" (must be >= 0 and <= total straws)
      * @return
      *   An uncertain `Boolean` — `true` if you drew a short straw (or as controlled by `treatShortAsTrue`)
      * @example
      *   {{{
      *   // One short straw among ten
      *   val drewShort = Uncertain.drawStraws(10)
      *   // Multiple short straws
      *   val drewOneOfTwoShort: Uncertaion[Boolean] = Uncertain.drawStraws(shortStraws = 2, totalStraws = 10)
      *   }}}
      */
    def drawStraws(
      shortStraws: Int = 1,
      totalStraws: Int
    )(using random: Random = new Random()): Uncertain[Boolean] = {
      require(totalStraws > 0, "Must have at least one straw")
      require(shortStraws >= 0, "Must have non-negative number of short straws")
      require(shortStraws <= totalStraws, "Cannot have more short straws than total straws")
      if (shortStraws == 0) {
        Uncertain.always(false)
      } else if (shortStraws == totalStraws) {
        Uncertain.always(true)
      } else {
        val probability: Double = shortStraws.toDouble / totalStraws.toDouble
        Uncertain.bernoulliViaDouble(probability)
      }
    }

    /** Combines all uncertain values in a collection into a single uncertain collection.
      *
      * This is useful when you have multiple uncertain values and want to work with them together while preserving their correlations.
      *
      * @param uncertainValues
      *   A collection of uncertain values
      * @return
      *   An uncertain List containing samples from all input uncertains
      * @example
      *   {{{
      *   val roll1 = Uncertain.diceRoll(6)
      *   val roll2 = Uncertain.diceRoll(6)
      *   val roll3 = Uncertain.diceRoll(6)
      *
      *   val allRolls = Uncertain.combineAll(List(roll1, roll2, roll3))
      *   val total = allRolls.map(_.sum)
      *   // Sample: 14 (from rolls of 5, 6, 3)
      *
      *   val temperatures = List(
      *     Uncertain.bellCurve(centeredOn = 20.0, withSpread = 2.0),
      *     Uncertain.bellCurve(centeredOn = 22.0, withSpread = 2.0),
      *     Uncertain.bellCurve(centeredOn = 19.0, withSpread = 2.0)
      *   )
      *   val avgTemp = Uncertain.combineAll(temperatures).map(temps => temps.sum / temps.length)
      *   }}}
      */
    def combineAll[T](uncertainValues: Iterable[Uncertain[T]])(using random: Random = new Random()): Uncertain[List[T]] =
      Uncertain.sequence(uncertainValues)

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

    /** Creates a yes/no outcome based on odds like "1 in 10" chance.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param n
      *   The denominator of the probability (e.g., 10 for "1 in 10")
      * @return
      *   An uncertain Boolean that's true with probability 1/oneIn
      * @example
      *   {{{
      *   val rareEvent = Uncertain.oneIn(n = 100)  // 1% chance
      *   val coinFlip = Uncertain.onIn(n = 2)     // 50% chance
      *   }}}
      */
    def oneIn(n: Int)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(n > 0, "Must be at least 1 in 1 chance")
      Uncertain.bernoulliViaDouble(probability = 1.0 / n)
    }

    /** Creates a yes/no outcome based on odds like "2 out of 33" chance.
      *
      * Uses a Bernoulli distribution: https://en.wikipedia.org/wiki/Bernoulli_distribution
      *
      * @param n
      *   The number of favorable outcomes (e.g., 1 for "1 out of 10")
      * @param outOf
      *   The total number of possible outcomes (e.g., 10 for "1 out of 10")
      * @return
      *   An uncertain Boolean that's true with probability favorable/outOf
      * @example
      *   {{{
      *   val rareEvent = Uncertain.chanceOf(2, outOf = 333)  // 2 out of 333 chance
      *   val coinFlip = Uncertain.chanceOf(1, outOf = 2)      // 1 out of 2 chance
      *   val impossible = Uncertain.chanceOf(0, outOf = 100)  // 0% chance
      *   }}}
      */
    def nOutOf(n: Int, outOf: Int)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(n >= 0, s"Favorable outcomes must be non-negative, got $n")
      require(outOf > 0, s"Total outcomes must be positive, got $outOf")
      require(n <= outOf, s"Favorable outcomes ($n) cannot exceed total outcomes ($outOf)")
      Uncertain.bernoulliViaDouble(probability = n.toDouble / outOf)
    }

    /** "Lightning strikes twice" - models coinciding events. */
    def bothHappen(
      firstChance: Double,
      secondChance: Double
    ): Uncertain[Boolean] = {
      require(firstChance >= 0 && firstChance <= 1, s"Probability ($probability) must be between 0 and 1.")
      require(secondChance >= 0 && secondChance <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain.bernoulli(firstChance) && Uncertain.bernoulli(secondChance)
    }

    /** "Backup plan" - try first option, if it fails try second. */
    def withFallback[T](
      primaryOption: Uncertain[Option[T]],
      fallback: Uncertain[T]
    ): Uncertain[T] = primaryOption.flatMap {
      case Some(value) => Uncertain.always(value)
      case None        => fallback
    }
  }
}
