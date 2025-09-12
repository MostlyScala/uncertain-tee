package mostly.uncertaintee.ops

/** A trait that mixes in all available syntax traits for easy importing.
  *
  * Best utilized via:
  *
  * {{{
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait AllOps
    extends ArithmeticOps
    with BooleanOps
    with ComparisonOps
    with DistributionOps
    with FpOps
    with OptionOps
    with StatisticalOps
