/*
 * Copyright (c) 2025 Mostly Codes
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
    val all: ops.AllOps                   = this
    val arithmetic: ops.ArithmeticOps     = this
    val boolean: ops.BooleanOps           = this
    val distribution: ops.AllDistributionOps = this
    val comparison: ops.ComparisonOps     = this
    val functional: ops.FunctionalProgrammingOps             = this
    val option: ops.OptionOps             = this
    val statistical: ops.StatisticalOps   = this
  }

  // To avoid any funky accidents where I mistype numbers or round floats or something
  // incorrect; locking these down as constants. Not exposed to end user of library.
  private[uncertaintee] val Zero: Double     = Numeric[Double].zero
  private[uncertaintee] val One: Double      = Numeric[Double].one
  private[uncertaintee] val Two: Double      = Numeric[Double].fromInt(2)
  private[uncertaintee] val MinusTwo: Double = Numeric[Double].fromInt(-2)
}
