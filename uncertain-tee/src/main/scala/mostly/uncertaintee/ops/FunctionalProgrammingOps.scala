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
import mostly.uncertaintee.syntax.point

import scala.util.Random

/** {{{
  * import mostly.uncertaintee.syntax.functional.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  *
  * Adds "advanced" operators to Uncertain (from a classical FP-domain)
  */
trait FunctionalProgrammingOps {

  extension (uncertainType: Uncertain.type) {

    /** Converts a `List[Uncertain[T]]` (or another iterable) into a single `Uncertain[List[T]]`.
      *
      * This is useful for aggregating a collection of uncertain values into a single uncertain result.
      *
      * @example
      *   Correlation preservation across multiple uncertain values:
      *   {{{
      *   val temp1 = Uncertain.normal(20, 2)
      *   val temp2 = temp1.map(_ + 5)  // Always 5 degrees warmer
      *   val temp3 = temp1.map(_ - 3)  // Always 3 degrees cooler
      *
      *   val allTemps = Uncertain.sequence(List(temp1, temp2, temp3))
      *
      *   // The relationships are preserved in every sample
      *   val sample = allTemps.sample() // e.g., List(18.5, 23.5, 15.5)
      *   // Notice: temp2 = temp1 + 5, temp3 = temp1 - 3
      *   }}}
      *
      * {{{
      * import mostly.uncertaintee.Uncertain
      * import mostly.uncertaintee.syntax.*
      *
      * // Model three independent dice rolls
      * val dieRollD6 = Uncertain.uniform(1, 7).map(_.toInt)
      * val dieRoll12 = Uncertain.uniform(1, 13).map(_.toInt)
      * val dieRollD20 = Uncertain.uniform(1, 21).map(_.toInt)
      *
      * // Sequence them into a single Uncertain List
      * val uncertainSum: Uncertain[Int] = Uncertain.sequence(
      *   List(dieRollD20, dieRoll12, dieRollD6)
      * ).map(_.sum)
      *
      * // Now you can analyze the distribution of the sum of three dice rolls
      * println(s"Expected sum of three dice rolls: ${uncertainSum.mean()}")
      * }}}
      * @param uncertainTs
      *   a sequence of `Uncertain` values.
      * @return
      *   A single `Uncertain` that, when sampled, produces a list of samples — one from each of the input `Uncertain` instances, preserving all correlations between them.
      */
    def sequence[T](uncertainTs: Iterable[Uncertain[T]])(using random: Random = new Random()): Uncertain[List[T]] =
      uncertainTs.foldRight {
        Uncertain.point(List.empty[T])(using random)
      } { (elem, acc) =>
        for {
          h <- elem
          t <- acc
        } yield h :: t
      }

    /** Applies a function to each element in a collection, where the function returns an `Uncertain` value, then sequences the results into a single `Uncertain` collection.
      *
      * @example
      *   Correlation in dependent processing:
      *   {{{
      *   val baseEfficiency = Uncertain.normal(0.8, 0.1)
      *
      *   def processTask(taskComplexity: Double): Uncertain[Double] =
      *     baseEfficiency.map(eff => taskComplexity / eff)  // Same efficiency for all tasks
      *
      *   val complexities = List(1.0, 2.5, 1.8)
      *   val processingTimes = Uncertain.traverse(complexities)(processTask)
      *
      *   // All processing times are correlated through baseEfficiency
      *   val sample = processingTimes.sample()
      *   // If efficiency is low, ALL tasks take longer proportionally
      *   }}}
      *
      * {{{
      * // Assume we have a function that models the uncertain time to process a task
      * def processTask(taskId: Int): Uncertain[Double] =
      * Uncertain.normal(mean = taskId * 1.5, standardDeviation = 0.5)
      *
      * val taskIds = List(1, 2, 3)
      *
      * // Use traverse to get the distribution of total processing time
      * val totalTime: Uncertain[Double] = Uncertain.traverse(taskIds)(processTask).map(_.sum)
      *
      * println(s"Expected total processing time: ${totalTime.mean()}")
      * }}}
      * @param items
      *   The collection of values to map over.
      * @param f
      *   The function to apply to each element, which returns an `Uncertain` value.
      * @return
      *   An `Uncertain` value containing the collection of results, preserving correlations.
      */
    def traverse[A, T](
      items: Iterable[A]
    )(f: A => Uncertain[T])(using random: Random = new Random()): Uncertain[List[T]] =
      items.foldRight {
        Uncertain.point(List.empty[T])(using random)
      } { (elem, acc) =>
        for {
          h <- f(elem)
          t <- acc
        } yield h :: t
      }
  }

  extension [T](uncertainT: Uncertain[T]) {

    /** Combines two uncertain values into an uncertain pair, preserving any correlation between them.
      *
      * This is useful when you need to work with two related uncertain quantities together. It is the fundamental building block for `zipWith` and applicative-style `mapN`
      * operations.
      *
      * @example
      *   {{{
      * val height = Uncertain.normal(175, 10) // cm
      * val weight = Uncertain.normal(70, 5)   // kg
      *
      * // Creates a distribution of (height, weight) pairs
      * val personStats: Uncertain[(Double, Double)] = height.product(weight)
      *
      * personStats.sample() // e.g., (172.5, 73.1)
      *   }}}
      *
      * @param uncertainB
      *   The other `Uncertain` value.
      * @tparam B
      *   The type of the other `Uncertain` value.
      * @return
      *   An `Uncertain` of a tuple containing samples from both inputs.
      * @see
      *   [[zipWith]] to combine values with a function immediately.
      */
    def product[B](uncertainB: Uncertain[B]): Uncertain[(T, B)] = (uncertainT, uncertainB).mapN((_, _))

    /** Combines two uncertain values using a "zipper" function, preserving any correlation between them.
      *
      * This is a convenient shorthand for `product` followed by `map`.
      *
      * @example
      *   {{{
      * val height = Uncertain.normal(1.75, 0.1) // meters
      * val weight = Uncertain.normal(70, 5)     // kg
      *
      * def calculateBmi(h: Double, w: Double): Double = w / (h * h)
      *
      * // Creates a distribution of BMI values
      * val bmiDistribution = height.zipWith(weight)(calculateBmi)
      *
      * bmiDistribution.sample() // e.g., 22.8
      *   }}}
      *
      * @param uncertainB
      *   The other `Uncertain` value.
      * @param f
      *   The function to apply to the paired samples.
      * @return
      *   An `Uncertain` containing the result of applying `f` to the samples.
      * @see
      *   [[product]] to get the pair without applying a function.
      */
    def zipWith[B, C](uncertainB: Uncertain[B])(f: (T, B) => C): Uncertain[C] = (uncertainT, uncertainB).mapN(f)

    /** Filters and maps the uncertain value using a partial function.
      *
      * For each sample, this method checks if the partial function is defined. If it is, it applies the function to produce a `Some(value)`. If not, it produces `None`. This is
      * useful for transforming only a subset of the possible outcomes.
      *
      * @example
      *   {{{
      * import scala.math.sqrt
      *
      * val distribution = Uncertain.uniform(-5, 6)
      *
      * // Calculate the square root, but only for positive values.
      * val sqrtOfPositives: Uncertain[Option[Double]] = distribution.collect {
      * case x if x > 0 => sqrt(x)
      * }
      *
      * sqrtOfPositives.sample() // e.g., Some(1.82), or None if sample was -3.5
      *   }}}
      *
      * @param pf
      *   The partial function to apply.
      * @return
      *   An `Uncertain[Option[B]]` which is `Some(value)` if the partial function was defined for the sample, and `None` otherwise.
      * @see
      *   [[filter]] for filtering without transforming the value.
      */
    def collect[B](pf: PartialFunction[T, B]): Uncertain[Option[B]] =
      uncertainT.map(pf.lift)

    /** An alias for [[collect]].
      * @see
      *   [[collect]]
      */
    def filterWith[B](pf: PartialFunction[T, B]): Uncertain[Option[B]] = collect(pf)
  }

  extension [T](nested: Uncertain[Uncertain[T]]) {

    /** Flattens a nested `Uncertain[Uncertain[T]]` into a single `Uncertain[T]`.
      *
      * This is the standard monadic flatten operation. It is useful in scenarios where you first have a distribution over several possible models, and then you want to sample from
      * the resulting combined model.
      *
      * @example
      *   {{{
      * // Model selection: 70% chance of a stable model, 30% of a volatile one.
      * val stableModel = Uncertain.normal(10, 1)
      * val volatileModel = Uncertain.normal(10, 5)
      *
      * val chosenModel: Uncertain[Uncertain[Double]] = Uncertain.categorical(
      * Map(stableModel -> 0.7, volatileModel -> 0.3)
      * )
      *
      * // The flattened distribution represents the overall mixture of outcomes.
      * val finalDistribution: Uncertain[Double] = chosenModel.flatten
      *
      * finalDistribution.sample() // A sample from the combined (mixture) distribution
      *   }}}
      *
      * @return
      *   An `Uncertain[T]` representing the flattened distribution.
      */
    def flatten: Uncertain[T] = nested.flatMap(identity)
  }

  // ----------
  // These are a lot of mapN implementations; we're trying to stay dependency-free in this code,
  // while staying similar to common FP patterns such as seen in the `cats` library.
  // Hence, 22 ts. It's just boilerplate, no real logic here, so a reasonable tradeoff to
  // having to write macros or pull in dependencies.
  // ----------

  extension [T1](t: Tuple1[Uncertain[T1]]) {
    def mapN[B](f: T1 => B): Uncertain[B] = t._1.map(f)
  }

  extension [T1, T2](t: (Uncertain[T1], Uncertain[T2])) {
    def mapN[B](f: (T1, T2) => B): Uncertain[B] = for {
      a <- t._1
      b <- t._2
    } yield f(a, b)
  }

  // format: off
  extension [T1, T2, T3](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3])) {
    def mapN[B](f: (T1, T2, T3) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _)))
  }

  extension [T1, T2, T3, T4](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4])) {
    def mapN[B](f: (T1, T2, T3, T4) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5])) {
    def mapN[B](f: (T1, T2, T3, T4, T5) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19], Uncertain[T20])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19], Uncertain[T20], Uncertain[T21])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19], Uncertain[T20], Uncertain[T21], Uncertain[T22])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => B): Uncertain[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }
  
  // TUPLED - from (Uncertain[A], Uncertain[B]) to Uncertain[(A, B)] for all tuples between 2 and 22
  extension [T1, T2](t: (Uncertain[T1], Uncertain[T2])) {
    def tupled: Uncertain[(T1, T2)] = t.mapN((t1, t2) => (t1,t2))
  }
  extension [T1, T2, T3](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3])) {
    def tupled: Uncertain[(T1, T2, T3)] = t.mapN((t1, t2, t3) => (t1, t2, t3))
  }
  extension [T1, T2, T3, T4](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4])) {
    def tupled: Uncertain[(T1, T2, T3, T4)] = t.mapN((t1, t2, t3, t4) => (t1, t2, t3, t4))
  }
  extension [T1, T2, T3, T4, T5](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5])) {
    def tupled: Uncertain[(T1, T2, T3, T4, T5)] = t.mapN((t1, t2, t3, t4, t5) => (t1, t2, t3, t4, t5))
  }
  extension [T1, T2, T3, T4, T5, T6](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6])) {
    def tupled: Uncertain[(T1, T2, T3, T4, T5, T6)] = t.mapN((t1, t2, t3, t4, t5, t6) => (t1, t2, t3, t4, t5, t6))
  }
  extension [T1, T2, T3, T4, T5, T6, T7](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7])) {
    def tupled: Uncertain[(T1, T2, T3, T4, T5, T6, T7)] = t.mapN((t1, t2, t3, t4, t5, t6, t7) => (t1, t2, t3, t4, t5, t6, t7))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8])) {
    def tupled: Uncertain[(T1, T2, T3, T4, T5, T6, T7, T8)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8) => (t1, t2, t3, t4, t5, t6, t7, t8))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9])) {
    def tupled: Uncertain[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9) => (t1, t2, t3, t4, t5, t6, t7, t8, t9))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9, T10)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4, T5, T6, T7, T8, T9, T10, T11)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16, T17)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16, T17, T18)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16, T17, T18, T19)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19], Uncertain[T20])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16, T17, T18, T19, T20)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19], Uncertain[T20], Uncertain[T21])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16, T17, T18, T19, T20, T21)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
  }
  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (Uncertain[T1], Uncertain[T2], Uncertain[T3], Uncertain[T4], Uncertain[T5], Uncertain[T6], Uncertain[T7], Uncertain[T8], Uncertain[T9], Uncertain[T10], Uncertain[T11], Uncertain[T12], Uncertain[T13], Uncertain[T14], Uncertain[T15], Uncertain[T16], Uncertain[T17], Uncertain[T18], Uncertain[T19], Uncertain[T20], Uncertain[T21], Uncertain[T22])) {
    def tupled: Uncertain[(T1, T2, T3 ,T4 ,T5 ,T6 ,T7 ,T8 ,T9 ,T10 ,T11 ,T12, T13, T14 ,T15, T16, T17, T18, T19, T20, T21, T22)] = t.mapN((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22))
  }

}
