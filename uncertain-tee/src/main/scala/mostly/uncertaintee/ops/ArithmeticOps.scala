/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
