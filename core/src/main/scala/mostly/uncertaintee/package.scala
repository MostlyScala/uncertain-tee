package mostly

package object uncertaintee {

  /** Allows access to syntax for operating on [[Uncertain]]
    *
    * {{{
    *   // recommended, let the compiler sort out your imports instead
    *   // of picking-and-mixing
    *   import mostly.uncertaintee.syntax.*
    *   // ... equivalent to importing .all (which is familiar to many libraries), which is also supported
    *   import mostly.uncertaintee.syntax.all.*
    *
    *   // If you want to pick-and-mix:
    *   import mostly.uncertaintee.syntax.all.*
    *   import mostly.uncertaintee.syntax.arithmetic.*
    *   import mostly.uncertaintee.syntax.boolean.*
    *   import mostly.uncertaintee.syntax.functional.*
    *   import mostly.uncertaintee.syntax.optionalOps.*
    *   import mostly.uncertaintee.syntax.comparison.*
    *   import mostly.uncertaintee.syntax.statistical.*
    * }}}
    */
  object syntax extends ops.AllOps {
    val all: ops.AllOps                 = this
    val arithmetic: ops.ArithmeticOps   = this
    val boolean: ops.BooleanOps         = this
    val comparison: ops.ComparisonOps   = this
    val functional: ops.FpOps           = this
    val option: ops.OptionOps           = this
    val statistical: ops.StatisticalOps = this
  }

  // To avoid any funky accidents where I mistype numbers or round floats or something
  // incorrect; locking these down as constants. Not exposed to end user of library.
  private[uncertaintee] val Zero: Double     = Numeric[Double].zero
  private[uncertaintee] val One: Double      = Numeric[Double].one
  private[uncertaintee] val Two: Double      = Numeric[Double].fromInt(2)
  private[uncertaintee] val MinusTwo: Double = Numeric[Double].fromInt(-2)
}
