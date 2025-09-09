package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

trait OptionalOps {
  extension [T](uncertainOption: Uncertain[Option[T]]) {

    /** Transforms the `Uncertain[Option[T]]` into an `Uncertain[T]` by providing a default value to use in the case of
      * `None`.
      */
    def getOrElse(default: => T): Uncertain[T] =
      uncertainOption.map(_.getOrElse(default))
  }

}
