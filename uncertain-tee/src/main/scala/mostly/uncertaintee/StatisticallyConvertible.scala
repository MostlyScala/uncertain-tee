package mostly.uncertaintee

/** A typeclass for types that can be safely converted to a Double for statistical analysis. */
trait StatisticallyConvertible[T] {
  def toDouble(value: T): Double
}

object StatisticallyConvertible {

  /** Any type T that is already Numeric can be converted. */
  given statisticalNumeric[T](using num: Numeric[T]): StatisticallyConvertible[T] =
    (value: T) => num.toDouble(value)

  /** The specific conversion rule for Boolean: true -> 1.0, false -> 0.0. */
  given statisticalBool: StatisticallyConvertible[Boolean] =
    (value: Boolean) => if (value) 1.0 else 0.0
}
