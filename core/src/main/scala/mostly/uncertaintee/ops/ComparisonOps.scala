package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

/** {{{
  *    import mostly.uncertaintee.syntax.comparison.*
  *    // or just import all the syntax (recommended)
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ComparisonOps {

  /** Equality comparisons for uncertain values. */
  extension [T](uncertain: Uncertain[T]) {

    /** Compares two uncertain values sample-by-sample. */
    def ===(other: Uncertain[T]): Uncertain[Boolean] = for {
      a <- uncertain
      b <- other
    } yield a == b

    /** Sample-wise inequality comparison (opposite of ===). */
    def !==(other: Uncertain[T]): Uncertain[Boolean] = for {
      a <- uncertain
      b <- other
    } yield a != b
  }

  /** Comparison operations for uncertain values with ordered types. */
  extension [T](lhs: Uncertain[T])(using ord: Ordering[T]) {
    def gt(value: T): Uncertain[Boolean]  = lhs.map(a => ord.gt(a, value))
    def lt(value: T): Uncertain[Boolean]  = lhs.map(a => ord.lt(a, value))
    def gte(value: T): Uncertain[Boolean] = lhs.map(a => ord.gteq(a, value))
    def lte(value: T): Uncertain[Boolean] = lhs.map(a => ord.lteq(a, value))

    def >(value: T): Uncertain[Boolean]  = gt(value)
    def <(value: T): Uncertain[Boolean]  = lt(value)
    def >=(value: T): Uncertain[Boolean] = gte(value)
    def <=(value: T): Uncertain[Boolean] = lte(value)

    def gt(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.gt(lhsSample, rhsSample)

    def lt(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.lt(lhsSample, rhsSample)

    def gte(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.gteq(lhsSample, rhsSample)

    def lte(other: Uncertain[T]): Uncertain[Boolean] = for {
      lhsSample <- lhs; rhsSample <- other
    } yield ord.lteq(lhsSample, rhsSample)

    def >(other: Uncertain[T]): Uncertain[Boolean]  = gt(other)
    def <(other: Uncertain[T]): Uncertain[Boolean]  = lt(other)
    def >=(other: Uncertain[T]): Uncertain[Boolean] = gte(other)
    def <=(other: Uncertain[T]): Uncertain[Boolean] = lte(other)
  }
}
