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
