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

package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

/** {{{
  *    import mostly.uncertaintee.syntax.arithmetic.*
  *    // or just import all the syntax (recommended)
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ArithmeticOps {

  /** Arithmetic operations for uncertain numeric values. */
  extension [T](lhs: Uncertain[T])(using num: Numeric[T]) {

    /** Adds two uncertain values sample-by-sample. */
    def +(rhs: Uncertain[T]): Uncertain[T] = for {
      lhsSample <- lhs
      rhsSample <- rhs
    } yield num.plus(lhsSample, rhsSample)

    /** Subtracts two uncertain values sample-by-sample. */
    def -(rhs: Uncertain[T]): Uncertain[T] = for {
      lhsSample <- lhs
      rhsSample <- rhs
    } yield num.minus(lhsSample, rhsSample)

    /** Multiplies two uncertain values sample-by-sample. */
    def *(rhs: Uncertain[T]): Uncertain[T] = for {
      lhsSample <- lhs
      rhsSample <- rhs
    } yield num.times(lhsSample, rhsSample)

    /** Adds a constant to an uncertain value. */
    def +(rhs: T): Uncertain[T] = lhs.map(l => num.plus(l, rhs))

    /** Subtracts a constant from an uncertain value. */
    def -(rhs: T): Uncertain[T] = lhs.map(l => num.minus(l, rhs))

    /** Multiplies an uncertain value by a constant. */
    def *(rhs: T): Uncertain[T] = lhs.map(l => num.times(l, rhs))
  }

  /** Division operations for uncertain values with fractional types. */
  extension [T](lhs: Uncertain[T])(using frac: Fractional[T]) {

    /** Divides uncertain value by a fixed value. */
    def /(rhs: T): Uncertain[T] = lhs.map(a => frac.div(a, rhs))

    /** Divides two uncertain values sample-by-sample. */
    def /(rhs: Uncertain[T]): Uncertain[T] = for {
      lhsSample <- lhs
      rhsSample <- rhs
    } yield frac.div(lhsSample, rhsSample)
  }
}
