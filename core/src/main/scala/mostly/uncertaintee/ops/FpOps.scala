package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

/** {{{
  * import mostly.uncertaintee.syntax.functional.*
  * // or just import all the syntax
  * import mostly.uncertaintee.syntax.*
  * }}}
  *
  * Adds "advanced" operators to Uncertain (from a classical FP-domain)
  */
trait FpOps {

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
    def product[B](uncertainB: Uncertain[B]): Uncertain[(T, B)] = for {
      t <- uncertainT
      u <- uncertainB
    } yield (t, u)

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
    def zipWith[B, C](uncertainB: Uncertain[B])(f: (T, B) => C): Uncertain[C] = for {
      t <- uncertainT
      u <- uncertainB
    } yield f(t, u)

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
}
