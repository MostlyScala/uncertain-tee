package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

/** {{{
  * import mostly.uncertaintee.syntax.functional.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  *
  * Adds "advanced" operators to Uncertain (from a classical FP-domain)
  */
trait FunctionalProgrammingOps {

  extension [T](uncertainT: Uncertain[T]) {

    /** Combines two uncertain values into an uncertain pair, preserving any correlation between them.
      *
      * This is useful when you need to work with two related uncertain quantities together. It is the fundamental
      * building block for `zipWith` and applicative-style `mapN` operations.
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
      * For each sample, this method checks if the partial function is defined. If it is, it applies the function to
      * produce a `Some(value)`. If not, it produces `None`. This is useful for transforming only a subset of the
      * possible outcomes.
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
      *   An `Uncertain[Option[B]]` which is `Some(value)` if the partial function was defined for the sample, and
      *   `None` otherwise.
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
      * This is the standard monadic flatten operation. It is useful in scenarios where you first have a distribution
      * over several possible models, and then you want to sample from the resulting combined model.
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
  /** Just to keep the larger mapN impls legible */
  private type U[T] = Uncertain[T]
  
  extension [T1, T2, T3](t: (U[T1], U[T2], U[T3])) {
    def mapN[B](f: (T1, T2, T3) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _)))
  }

  extension [T1, T2, T3, T4](t: (U[T1], U[T2], U[T3], U[T4])) {
    def mapN[B](f: (T1, T2, T3, T4) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5](t: (U[T1], U[T2], U[T3], U[T4], U[T5])) {
    def mapN[B](f: (T1, T2, T3, T4, T5) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16], U[T17])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16], U[T17], U[T18])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16], U[T17], U[T18], U[T19])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16], U[T17], U[T18], U[T19], U[T20])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16], U[T17], U[T18], U[T19], U[T20], U[T21])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (U[T1], U[T2], U[T3], U[T4], U[T5], U[T6], U[T7], U[T8], U[T9], U[T10], U[T11], U[T12], U[T13], U[T14], U[T15], U[T16], U[T17], U[T18], U[T19], U[T20], U[T21], U[T22])) {
    def mapN[B](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => B): U[B] = t.head.flatMap(h => t.tail.mapN(f(h, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)))
  }
  // format: off
}
