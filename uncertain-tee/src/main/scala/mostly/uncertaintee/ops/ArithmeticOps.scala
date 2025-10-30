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
import mostly.uncertaintee.syntax.functional.*

/** Allows basic common numeric operations on Uncertain[T] types, e.g.
  *
  * {{{
  *   (uncertaintyA + 3.5 - uncertaintyB) % uncertaintyC
  * }}}
  *
  * {{{
  * import mostly.uncertaintee.syntax.distribution.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ArithmeticOps {

  /** A type alias for all supported numeric types. */
  type SupportedNum = Double | Float | Long | Int | Char | Short | Byte

  /** This is a private, "dummy" instance.
    *
    * Its only job is to help the compiler differentiate between overloaded methods.
    *
    * It must be in scope for the compiler to allow arithmetic ops between Uncertain instances.
    */
  sealed trait Left2Right_Uncertain_Uncertain

  /** This is a private, "dummy" instance.
    *
    * Its only job is to help the compiler differentiate between overloaded methods.
    *
    * It must be in scope for the compiler to allow arithmetic ops between Uncertain instances.
    */
  sealed trait Left2Right_SupportedNum_Uncertain

  /** @see
    *   [[Left2Right_Uncertain_Uncertain]]
    * @see
    *   [[mostly.uncertaintee.styledocs.ImplicitsVsGivenExplanation]]
    */
  given Left2Right_Uncertain_Uncertain = new Left2Right_Uncertain_Uncertain {}

  /** @see
    *   [[Left2Right_SupportedNum_Uncertain]]
    * @see
    *   [[mostly.uncertaintee.styledocs.ImplicitsVsGivenExplanation]]
    */
  given Left2Right_SupportedNum_Uncertain = new Left2Right_SupportedNum_Uncertain {}

  /** Allows SupportedNum-on-LHS operations, e.g.
    * {{{
    *   1.6 * uncertain
    *   1f + (3 * uncertain)
    *   -1 * uncertain
    * }}}
    */
  extension [N1 <: SupportedNum](lhs: N1) {

    /** Add `+` between a numeric type and an uncertain of a numeric type */
    transparent inline def +[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_SupportedNum_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Double, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Double, r: Uncertain[Float])  => r.map(l + _): Uncertain[Double]
        case (l: Double, r: Uncertain[Long])   => r.map(l + _): Uncertain[Double]
        case (l: Double, r: Uncertain[Int])    => r.map(l + _): Uncertain[Double]
        case (l: Double, r: Uncertain[Short])  => r.map(l + _): Uncertain[Double]
        case (l: Double, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Double]
        case (l: Double, r: Uncertain[Char])   => r.map(l + _): Uncertain[Double]

        case (l: Float, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Float, r: Uncertain[Float])  => r.map(l + _): Uncertain[Float]
        case (l: Float, r: Uncertain[Long])   => r.map(l + _): Uncertain[Float]
        case (l: Float, r: Uncertain[Int])    => r.map(l + _): Uncertain[Float]
        case (l: Float, r: Uncertain[Short])  => r.map(l + _): Uncertain[Float]
        case (l: Float, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Float]
        case (l: Float, r: Uncertain[Char])   => r.map(l + _): Uncertain[Float]

        case (l: Long, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Long, r: Uncertain[Float])  => r.map(l + _): Uncertain[Float]
        case (l: Long, r: Uncertain[Long])   => r.map(l + _): Uncertain[Long]
        case (l: Long, r: Uncertain[Int])    => r.map(l + _): Uncertain[Long]
        case (l: Long, r: Uncertain[Short])  => r.map(l + _): Uncertain[Long]
        case (l: Long, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Long]
        case (l: Long, r: Uncertain[Char])   => r.map(l + _): Uncertain[Long]

        case (l: Int, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Int, r: Uncertain[Float])  => r.map(l + _): Uncertain[Float]
        case (l: Int, r: Uncertain[Long])   => r.map(l + _): Uncertain[Long]
        case (l: Int, r: Uncertain[Int])    => r.map(l + _): Uncertain[Int]
        case (l: Int, r: Uncertain[Short])  => r.map(l + _): Uncertain[Int]
        case (l: Int, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Int]
        case (l: Int, r: Uncertain[Char])   => r.map(l + _): Uncertain[Int]

        case (l: Short, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Short, r: Uncertain[Float])  => r.map(l + _): Uncertain[Float]
        case (l: Short, r: Uncertain[Long])   => r.map(l + _): Uncertain[Long]
        case (l: Short, r: Uncertain[Int])    => r.map(l + _): Uncertain[Int]
        case (l: Short, r: Uncertain[Short])  => r.map(l + _): Uncertain[Int]
        case (l: Short, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Int]
        case (l: Short, r: Uncertain[Char])   => r.map(l + _): Uncertain[Int]

        case (l: Byte, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Byte, r: Uncertain[Float])  => r.map(l + _): Uncertain[Float]
        case (l: Byte, r: Uncertain[Long])   => r.map(l + _): Uncertain[Long]
        case (l: Byte, r: Uncertain[Int])    => r.map(l + _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Short])  => r.map(l + _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Char])   => r.map(l + _): Uncertain[Int]

        case (l: Char, r: Uncertain[Double]) => r.map(l + _): Uncertain[Double]
        case (l: Char, r: Uncertain[Float])  => r.map(l + _): Uncertain[Float]
        case (l: Char, r: Uncertain[Long])   => r.map(l + _): Uncertain[Long]
        case (l: Char, r: Uncertain[Int])    => r.map(l + _): Uncertain[Int]
        case (l: Char, r: Uncertain[Short])  => r.map(l + _): Uncertain[Int]
        case (l: Char, r: Uncertain[Byte])   => r.map(l + _): Uncertain[Int]
        case (l: Char, r: Uncertain[Char])   => r.map(l + _): Uncertain[Int]
      }

    /** Add `-` between a numeric type and an uncertain of a numeric type */
    transparent inline def -[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_SupportedNum_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Double, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Double, r: Uncertain[Float])  => r.map(l - _): Uncertain[Double]
        case (l: Double, r: Uncertain[Long])   => r.map(l - _): Uncertain[Double]
        case (l: Double, r: Uncertain[Int])    => r.map(l - _): Uncertain[Double]
        case (l: Double, r: Uncertain[Short])  => r.map(l - _): Uncertain[Double]
        case (l: Double, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Double]
        case (l: Double, r: Uncertain[Char])   => r.map(l - _): Uncertain[Double]

        case (l: Float, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Float, r: Uncertain[Float])  => r.map(l - _): Uncertain[Float]
        case (l: Float, r: Uncertain[Long])   => r.map(l - _): Uncertain[Float]
        case (l: Float, r: Uncertain[Int])    => r.map(l - _): Uncertain[Float]
        case (l: Float, r: Uncertain[Short])  => r.map(l - _): Uncertain[Float]
        case (l: Float, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Float]
        case (l: Float, r: Uncertain[Char])   => r.map(l - _): Uncertain[Float]

        case (l: Long, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Long, r: Uncertain[Float])  => r.map(l - _): Uncertain[Float]
        case (l: Long, r: Uncertain[Long])   => r.map(l - _): Uncertain[Long]
        case (l: Long, r: Uncertain[Int])    => r.map(l - _): Uncertain[Long]
        case (l: Long, r: Uncertain[Short])  => r.map(l - _): Uncertain[Long]
        case (l: Long, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Long]
        case (l: Long, r: Uncertain[Char])   => r.map(l - _): Uncertain[Long]

        case (l: Int, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Int, r: Uncertain[Float])  => r.map(l - _): Uncertain[Float]
        case (l: Int, r: Uncertain[Long])   => r.map(l - _): Uncertain[Long]
        case (l: Int, r: Uncertain[Int])    => r.map(l - _): Uncertain[Int]
        case (l: Int, r: Uncertain[Short])  => r.map(l - _): Uncertain[Int]
        case (l: Int, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Int]
        case (l: Int, r: Uncertain[Char])   => r.map(l - _): Uncertain[Int]

        case (l: Short, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Short, r: Uncertain[Float])  => r.map(l - _): Uncertain[Float]
        case (l: Short, r: Uncertain[Long])   => r.map(l - _): Uncertain[Long]
        case (l: Short, r: Uncertain[Int])    => r.map(l - _): Uncertain[Int]
        case (l: Short, r: Uncertain[Short])  => r.map(l - _): Uncertain[Int]
        case (l: Short, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Int]
        case (l: Short, r: Uncertain[Char])   => r.map(l - _): Uncertain[Int]

        case (l: Byte, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Byte, r: Uncertain[Float])  => r.map(l - _): Uncertain[Float]
        case (l: Byte, r: Uncertain[Long])   => r.map(l - _): Uncertain[Long]
        case (l: Byte, r: Uncertain[Int])    => r.map(l - _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Short])  => r.map(l - _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Char])   => r.map(l - _): Uncertain[Int]

        case (l: Char, r: Uncertain[Double]) => r.map(l - _): Uncertain[Double]
        case (l: Char, r: Uncertain[Float])  => r.map(l - _): Uncertain[Float]
        case (l: Char, r: Uncertain[Long])   => r.map(l - _): Uncertain[Long]
        case (l: Char, r: Uncertain[Int])    => r.map(l - _): Uncertain[Int]
        case (l: Char, r: Uncertain[Short])  => r.map(l - _): Uncertain[Int]
        case (l: Char, r: Uncertain[Byte])   => r.map(l - _): Uncertain[Int]
        case (l: Char, r: Uncertain[Char])   => r.map(l - _): Uncertain[Int]
      }

    /** Add `*` between a numeric type and an uncertain of a numeric type */
    transparent inline def *[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_SupportedNum_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Double, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Double, r: Uncertain[Float])  => r.map(l * _): Uncertain[Double]
        case (l: Double, r: Uncertain[Long])   => r.map(l * _): Uncertain[Double]
        case (l: Double, r: Uncertain[Int])    => r.map(l * _): Uncertain[Double]
        case (l: Double, r: Uncertain[Short])  => r.map(l * _): Uncertain[Double]
        case (l: Double, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Double]
        case (l: Double, r: Uncertain[Char])   => r.map(l * _): Uncertain[Double]

        case (l: Float, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Float, r: Uncertain[Float])  => r.map(l * _): Uncertain[Float]
        case (l: Float, r: Uncertain[Long])   => r.map(l * _): Uncertain[Float]
        case (l: Float, r: Uncertain[Int])    => r.map(l * _): Uncertain[Float]
        case (l: Float, r: Uncertain[Short])  => r.map(l * _): Uncertain[Float]
        case (l: Float, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Float]
        case (l: Float, r: Uncertain[Char])   => r.map(l * _): Uncertain[Float]

        case (l: Long, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Long, r: Uncertain[Float])  => r.map(l * _): Uncertain[Float]
        case (l: Long, r: Uncertain[Long])   => r.map(l * _): Uncertain[Long]
        case (l: Long, r: Uncertain[Int])    => r.map(l * _): Uncertain[Long]
        case (l: Long, r: Uncertain[Short])  => r.map(l * _): Uncertain[Long]
        case (l: Long, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Long]
        case (l: Long, r: Uncertain[Char])   => r.map(l * _): Uncertain[Long]

        case (l: Int, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Int, r: Uncertain[Float])  => r.map(l * _): Uncertain[Float]
        case (l: Int, r: Uncertain[Long])   => r.map(l * _): Uncertain[Long]
        case (l: Int, r: Uncertain[Int])    => r.map(l * _): Uncertain[Int]
        case (l: Int, r: Uncertain[Short])  => r.map(l * _): Uncertain[Int]
        case (l: Int, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Int]
        case (l: Int, r: Uncertain[Char])   => r.map(l * _): Uncertain[Int]

        case (l: Short, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Short, r: Uncertain[Float])  => r.map(l * _): Uncertain[Float]
        case (l: Short, r: Uncertain[Long])   => r.map(l * _): Uncertain[Long]
        case (l: Short, r: Uncertain[Int])    => r.map(l * _): Uncertain[Int]
        case (l: Short, r: Uncertain[Short])  => r.map(l * _): Uncertain[Int]
        case (l: Short, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Int]
        case (l: Short, r: Uncertain[Char])   => r.map(l * _): Uncertain[Int]

        case (l: Byte, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Byte, r: Uncertain[Float])  => r.map(l * _): Uncertain[Float]
        case (l: Byte, r: Uncertain[Long])   => r.map(l * _): Uncertain[Long]
        case (l: Byte, r: Uncertain[Int])    => r.map(l * _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Short])  => r.map(l * _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Char])   => r.map(l * _): Uncertain[Int]

        case (l: Char, r: Uncertain[Double]) => r.map(l * _): Uncertain[Double]
        case (l: Char, r: Uncertain[Float])  => r.map(l * _): Uncertain[Float]
        case (l: Char, r: Uncertain[Long])   => r.map(l * _): Uncertain[Long]
        case (l: Char, r: Uncertain[Int])    => r.map(l * _): Uncertain[Int]
        case (l: Char, r: Uncertain[Short])  => r.map(l * _): Uncertain[Int]
        case (l: Char, r: Uncertain[Byte])   => r.map(l * _): Uncertain[Int]
        case (l: Char, r: Uncertain[Char])   => r.map(l * _): Uncertain[Int]
      }

    /** Add `/` between a numeric type and an uncertain of a numeric type */
    transparent inline def /[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_SupportedNum_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Double, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Double, r: Uncertain[Float])  => r.map(l / _): Uncertain[Double]
        case (l: Double, r: Uncertain[Long])   => r.map(l / _): Uncertain[Double]
        case (l: Double, r: Uncertain[Int])    => r.map(l / _): Uncertain[Double]
        case (l: Double, r: Uncertain[Short])  => r.map(l / _): Uncertain[Double]
        case (l: Double, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Double]
        case (l: Double, r: Uncertain[Char])   => r.map(l / _): Uncertain[Double]

        case (l: Float, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Float, r: Uncertain[Float])  => r.map(l / _): Uncertain[Float]
        case (l: Float, r: Uncertain[Long])   => r.map(l / _): Uncertain[Float]
        case (l: Float, r: Uncertain[Int])    => r.map(l / _): Uncertain[Float]
        case (l: Float, r: Uncertain[Short])  => r.map(l / _): Uncertain[Float]
        case (l: Float, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Float]
        case (l: Float, r: Uncertain[Char])   => r.map(l / _): Uncertain[Float]

        case (l: Long, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Long, r: Uncertain[Float])  => r.map(l / _): Uncertain[Float]
        case (l: Long, r: Uncertain[Long])   => r.map(l / _): Uncertain[Long]
        case (l: Long, r: Uncertain[Int])    => r.map(l / _): Uncertain[Long]
        case (l: Long, r: Uncertain[Short])  => r.map(l / _): Uncertain[Long]
        case (l: Long, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Long]
        case (l: Long, r: Uncertain[Char])   => r.map(l / _): Uncertain[Long]

        case (l: Int, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Int, r: Uncertain[Float])  => r.map(l / _): Uncertain[Float]
        case (l: Int, r: Uncertain[Long])   => r.map(l / _): Uncertain[Long]
        case (l: Int, r: Uncertain[Int])    => r.map(l / _): Uncertain[Int]
        case (l: Int, r: Uncertain[Short])  => r.map(l / _): Uncertain[Int]
        case (l: Int, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Int]
        case (l: Int, r: Uncertain[Char])   => r.map(l / _): Uncertain[Int]

        case (l: Short, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Short, r: Uncertain[Float])  => r.map(l / _): Uncertain[Float]
        case (l: Short, r: Uncertain[Long])   => r.map(l / _): Uncertain[Long]
        case (l: Short, r: Uncertain[Int])    => r.map(l / _): Uncertain[Int]
        case (l: Short, r: Uncertain[Short])  => r.map(l / _): Uncertain[Int]
        case (l: Short, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Int]
        case (l: Short, r: Uncertain[Char])   => r.map(l / _): Uncertain[Int]

        case (l: Byte, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Byte, r: Uncertain[Float])  => r.map(l / _): Uncertain[Float]
        case (l: Byte, r: Uncertain[Long])   => r.map(l / _): Uncertain[Long]
        case (l: Byte, r: Uncertain[Int])    => r.map(l / _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Short])  => r.map(l / _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Char])   => r.map(l / _): Uncertain[Int]

        case (l: Char, r: Uncertain[Double]) => r.map(l / _): Uncertain[Double]
        case (l: Char, r: Uncertain[Float])  => r.map(l / _): Uncertain[Float]
        case (l: Char, r: Uncertain[Long])   => r.map(l / _): Uncertain[Long]
        case (l: Char, r: Uncertain[Int])    => r.map(l / _): Uncertain[Int]
        case (l: Char, r: Uncertain[Short])  => r.map(l / _): Uncertain[Int]
        case (l: Char, r: Uncertain[Byte])   => r.map(l / _): Uncertain[Int]
        case (l: Char, r: Uncertain[Char])   => r.map(l / _): Uncertain[Int]
      }

    /** Add `%` between a numeric type and an uncertain of a numeric type */
    transparent inline def %[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_SupportedNum_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Double, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Double, r: Uncertain[Float])  => r.map(l % _): Uncertain[Double]
        case (l: Double, r: Uncertain[Long])   => r.map(l % _): Uncertain[Double]
        case (l: Double, r: Uncertain[Int])    => r.map(l % _): Uncertain[Double]
        case (l: Double, r: Uncertain[Short])  => r.map(l % _): Uncertain[Double]
        case (l: Double, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Double]
        case (l: Double, r: Uncertain[Char])   => r.map(l % _): Uncertain[Double]

        case (l: Float, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Float, r: Uncertain[Float])  => r.map(l % _): Uncertain[Float]
        case (l: Float, r: Uncertain[Long])   => r.map(l % _): Uncertain[Float]
        case (l: Float, r: Uncertain[Int])    => r.map(l % _): Uncertain[Float]
        case (l: Float, r: Uncertain[Short])  => r.map(l % _): Uncertain[Float]
        case (l: Float, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Float]
        case (l: Float, r: Uncertain[Char])   => r.map(l % _): Uncertain[Float]

        case (l: Long, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Long, r: Uncertain[Float])  => r.map(l % _): Uncertain[Float]
        case (l: Long, r: Uncertain[Long])   => r.map(l % _): Uncertain[Long]
        case (l: Long, r: Uncertain[Int])    => r.map(l % _): Uncertain[Long]
        case (l: Long, r: Uncertain[Short])  => r.map(l % _): Uncertain[Long]
        case (l: Long, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Long]
        case (l: Long, r: Uncertain[Char])   => r.map(l % _): Uncertain[Long]

        case (l: Int, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Int, r: Uncertain[Float])  => r.map(l % _): Uncertain[Float]
        case (l: Int, r: Uncertain[Long])   => r.map(l % _): Uncertain[Long]
        case (l: Int, r: Uncertain[Int])    => r.map(l % _): Uncertain[Int]
        case (l: Int, r: Uncertain[Short])  => r.map(l % _): Uncertain[Int]
        case (l: Int, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Int]
        case (l: Int, r: Uncertain[Char])   => r.map(l % _): Uncertain[Int]

        case (l: Short, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Short, r: Uncertain[Float])  => r.map(l % _): Uncertain[Float]
        case (l: Short, r: Uncertain[Long])   => r.map(l % _): Uncertain[Long]
        case (l: Short, r: Uncertain[Int])    => r.map(l % _): Uncertain[Int]
        case (l: Short, r: Uncertain[Short])  => r.map(l % _): Uncertain[Int]
        case (l: Short, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Int]
        case (l: Short, r: Uncertain[Char])   => r.map(l % _): Uncertain[Int]

        case (l: Byte, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Byte, r: Uncertain[Float])  => r.map(l % _): Uncertain[Float]
        case (l: Byte, r: Uncertain[Long])   => r.map(l % _): Uncertain[Long]
        case (l: Byte, r: Uncertain[Int])    => r.map(l % _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Short])  => r.map(l % _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Int]
        case (l: Byte, r: Uncertain[Char])   => r.map(l % _): Uncertain[Int]

        case (l: Char, r: Uncertain[Double]) => r.map(l % _): Uncertain[Double]
        case (l: Char, r: Uncertain[Float])  => r.map(l % _): Uncertain[Float]
        case (l: Char, r: Uncertain[Long])   => r.map(l % _): Uncertain[Long]
        case (l: Char, r: Uncertain[Int])    => r.map(l % _): Uncertain[Int]
        case (l: Char, r: Uncertain[Short])  => r.map(l % _): Uncertain[Int]
        case (l: Char, r: Uncertain[Byte])   => r.map(l % _): Uncertain[Int]
        case (l: Char, r: Uncertain[Char])   => r.map(l % _): Uncertain[Int]
      }

  }

  /** Allows SupportedNum-on-RHS operations with Uncertain[T]
    * {{{
    *    uncertain % 2
    *   (3 * uncertainA) / uncertainB
    *   uncertain + 44
    * }}}
    */
  extension [N1 <: SupportedNum](lhs: Uncertain[N1]) {

    /** Add negating unary `-` for uncertains of numeric types */
    transparent inline def unary_- =
      inline lhs match {
        case (l: Uncertain[Double]) => l.map(x => -x): Uncertain[Double]
        case (l: Uncertain[Float])  => l.map(x => -x): Uncertain[Float]
        case (l: Uncertain[Long])   => l.map(x => -x): Uncertain[Long]
        case (l: Uncertain[Int])    => l.map(x => -x): Uncertain[Int]
      }

    /** Add `+` between an uncer  tain of a numeric type and a numeric type */
    transparent inline def +[N2 <: SupportedNum](rhs: N2) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ + r): Uncertain[Double]

        case (l: Uncertain[Float], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)   => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)    => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)   => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)   => l.map(_ + r): Uncertain[Float]

        case (l: Uncertain[Long], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)   => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)    => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)  => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)   => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)   => l.map(_ + r): Uncertain[Long]

        case (l: Uncertain[Int], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)   => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)  => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)   => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)   => l.map(_ + r): Uncertain[Int]

        case (l: Uncertain[Short], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)   => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)  => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)   => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)   => l.map(_ + r): Uncertain[Int]

        case (l: Uncertain[Byte], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)   => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)  => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)   => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)   => l.map(_ + r): Uncertain[Int]

        case (l: Uncertain[Char], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)  => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)   => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)  => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)   => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)   => l.map(_ + r): Uncertain[Int]
      }

    /** Add `+` between two uncertains of numeric types */
    transparent inline def +[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_Uncertain_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Double]

        case (l: Uncertain[Float], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Float]

        case (l: Uncertain[Long], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Long]

        case (l: Uncertain[Int], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Int]

        case (l: Uncertain[Short], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Int]

        case (l: Uncertain[Byte], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Int]

        case (l: Uncertain[Char], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Int]
      }

    /** Add `+` between an uncertain of a numeric type and a numeric type */
    transparent inline def -[N2 <: SupportedNum](rhs: N2) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ - r): Uncertain[Double]

        case (l: Uncertain[Float], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)   => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)    => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)   => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)   => l.map(_ - r): Uncertain[Float]

        case (l: Uncertain[Long], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)   => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)    => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)  => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)   => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)   => l.map(_ - r): Uncertain[Long]

        case (l: Uncertain[Int], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)   => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)  => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)   => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)   => l.map(_ - r): Uncertain[Int]

        case (l: Uncertain[Short], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)   => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)  => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)   => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)   => l.map(_ - r): Uncertain[Int]

        case (l: Uncertain[Byte], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)   => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)  => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)   => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)   => l.map(_ - r): Uncertain[Int]

        case (l: Uncertain[Char], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)  => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)   => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)  => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)   => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)   => l.map(_ - r): Uncertain[Int]
      }

    /** Add `+` between two uncertains of numeric types */
    transparent inline def -[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_Uncertain_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Double]

        case (l: Uncertain[Float], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Float]

        case (l: Uncertain[Long], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Long]

        case (l: Uncertain[Int], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Int]

        case (l: Uncertain[Short], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Int]

        case (l: Uncertain[Byte], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Int]

        case (l: Uncertain[Char], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Int]
      }

    /** Add `*` between an uncertain of a numeric type and a numeric type */
    transparent inline def *[N2 <: SupportedNum](rhs: N2) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ * r): Uncertain[Double]

        case (l: Uncertain[Float], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)   => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)    => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)   => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)   => l.map(_ * r): Uncertain[Float]

        case (l: Uncertain[Long], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)   => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)    => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)  => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)   => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)   => l.map(_ * r): Uncertain[Long]

        case (l: Uncertain[Int], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)   => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)  => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)   => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)   => l.map(_ * r): Uncertain[Int]

        case (l: Uncertain[Short], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)   => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)  => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)   => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)   => l.map(_ * r): Uncertain[Int]

        case (l: Uncertain[Byte], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)   => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)  => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)   => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)   => l.map(_ * r): Uncertain[Int]

        case (l: Uncertain[Char], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)  => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)   => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)  => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)   => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)   => l.map(_ * r): Uncertain[Int]
      }

    /** Add `*` between two uncertains of numeric types */
    transparent inline def *[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_Uncertain_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Double]

        case (l: Uncertain[Float], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Float]

        case (l: Uncertain[Long], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Long]

        case (l: Uncertain[Int], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Int]

        case (l: Uncertain[Short], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Int]

        case (l: Uncertain[Byte], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Int]

        case (l: Uncertain[Char], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Int]
      }

    // --- DIVISION (/) ---

    /** Add `/` between an uncertain of a numeric type and a numeric type */
    transparent inline def /[N2 <: SupportedNum](rhs: N2) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ / r): Uncertain[Double]

        case (l: Uncertain[Float], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)   => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)    => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)   => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)   => l.map(_ / r): Uncertain[Float]

        case (l: Uncertain[Long], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)   => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)    => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)  => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)   => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)   => l.map(_ / r): Uncertain[Long]

        case (l: Uncertain[Int], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)   => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)  => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)   => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)   => l.map(_ / r): Uncertain[Int]

        case (l: Uncertain[Short], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)   => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)  => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)   => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)   => l.map(_ / r): Uncertain[Int]

        case (l: Uncertain[Byte], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)   => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)  => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)   => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)   => l.map(_ / r): Uncertain[Int]

        case (l: Uncertain[Char], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)  => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)   => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)  => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)   => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)   => l.map(_ / r): Uncertain[Int]
      }

    /** Add `/` between two uncertains of numeric types */
    transparent inline def /[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_Uncertain_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Double]

        case (l: Uncertain[Float], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Float]

        case (l: Uncertain[Long], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Long]

        case (l: Uncertain[Int], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Int]

        case (l: Uncertain[Short], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Int]

        case (l: Uncertain[Byte], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Int]

        case (l: Uncertain[Char], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Int]
      }

    /** Add `%` between an uncertain of a numeric type and a numeric type */
    transparent inline def %[N2 <: SupportedNum](rhs: N2) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ % r): Uncertain[Double]

        case (l: Uncertain[Float], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)   => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)    => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)   => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)   => l.map(_ % r): Uncertain[Float]

        case (l: Uncertain[Long], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)   => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)    => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)  => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)   => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)   => l.map(_ % r): Uncertain[Long]

        case (l: Uncertain[Int], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)   => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)  => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)   => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)   => l.map(_ % r): Uncertain[Int]

        case (l: Uncertain[Short], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)   => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)  => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)   => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)   => l.map(_ % r): Uncertain[Int]

        case (l: Uncertain[Byte], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)   => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)  => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)   => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)   => l.map(_ % r): Uncertain[Int]

        case (l: Uncertain[Char], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)  => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)   => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)  => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)   => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)   => l.map(_ % r): Uncertain[Int]
      }

    /** Add `%` between two uncertains of numeric types */
    transparent inline def %[N2 <: SupportedNum](rhs: Uncertain[N2])(using Left2Right_Uncertain_Uncertain) =
      inline (lhs, rhs) match {

        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Double]

        case (l: Uncertain[Float], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Float]

        case (l: Uncertain[Long], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Long]

        case (l: Uncertain[Int], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Int]

        case (l: Uncertain[Short], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Int]

        case (l: Uncertain[Byte], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Int]

        case (l: Uncertain[Char], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Int]
      }
  }
}
