package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

/** {{{
  * import mostly.uncertaintee.syntax.option.*
  * // or just import all the syntax
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait OptionOps {
  extension [T](uncertainOption: Uncertain[Option[T]]) {

    /** If this uncertain value results in `Some(v)`, the result is `v`. Otherwise, it falls back to sampling from the
      * provided alternative `Uncertain` value.
      *
      * This is useful for handling the result of operations like `filter` or `collect` by providing a default
      * probabilistic model when a sample is filtered out. It allows you to chain alternative computations.
      *
      * @example
      *   {{{
      * val speed = Uncertain.normal(120, 40) // A sensor reading that might be out of range
      * val validSpeed = speed.filter(s => s > 0 && s < 130) // Produces Uncertain[Option[Double]]
      *
      * // If the reading was invalid (None), fall back to a different model.
      * val fallbackModel = Uncertain.uniform(40, 60)
      * val finalSpeed = validSpeed.orElse(fallbackModel)
      *
      * // Now, finalSpeed will either contain a valid reading or a sample from the fallback model.
      * finalSpeed.sample()
      *   }}}
      *
      * @param fallback
      *   The `Uncertain[T]` to use if this `Uncertain[Option[T]]` produces a `None` sample. It is passed by-name to
      *   avoid evaluation unless necessary.
      * @return
      *   A new `Uncertain[T]` that resolves the `Option` by using the fallback for `None` cases.
      * @see
      *   [[getOrElse]] for falling back to a plain, constant value instead of another uncertain one.
      */
    def orElse(fallback: => Uncertain[T]): Uncertain[T] = uncertainOption.flatMap(_.fold(fallback)(Uncertain.point))

    /** If this uncertain value results in `Some(v)`, the result is `v`. Otherwise, it falls back to the provided
      * default value.
      *
      * This method is the standard way to unwrap the inner `Option` by providing a single, constant fallback value for
      * any samples that have been filtered out.
      *
      * @example
      *   {{{
      *   val temperature = Uncertain.normal(25, 10) // Celsius
      *   val plausibleTemp = temperature.filter(t => t > -10 && t < 40) // Produces Uncertain[Option[Double]]
      *
      *   // If the temperature reading was implausible (None), use a fixed default of 15.0.
      *   val finalTemp = plausibleTemp.getOrElse(15.0)
      *
      *   // finalTemp will now either contain a plausible reading or the constant value 15.0.
      *   finalTemp.sample()
      *   }}}
      *
      * @param default
      *   The plain value of type `T` to use if this `Uncertain[Option[T]]` produces a `None` sample. It is passed
      *   by-name to avoid evaluation unless necessary.
      * @return
      *   A new `Uncertain[T]` that unwraps the `Option` by using the default value for `None` cases.
      * @see
      *   [[orElse]] for falling back to another `Uncertain` value instead of a plain, constant one.
      */
    def getOrElse(default: => T): Uncertain[T] = uncertainOption.map(_.getOrElse(default))

  }
}
