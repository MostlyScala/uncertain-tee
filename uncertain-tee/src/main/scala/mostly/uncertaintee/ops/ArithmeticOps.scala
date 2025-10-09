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
  type AllNumerics = Double | Float | Long | Int | Short | Byte | Char

  /** This is a private, "dummy" given instance.
    *
    * Its only job is to help the compiler differentiate between overloaded methods.
    *
    * It must be in scope for the compiler to allow arithmetic ops between Uncertain instances.
    */
  trait UncertainOpDummy

  /** @see [[UncertainOpDummy]] */
  given UncertainOpDummy = new UncertainOpDummy {}

  extension [N1 <: AllNumerics](lhs: Uncertain[N1]) {

    // --- ADDITION (+) ---

    transparent inline def +[N2 <: AllNumerics](rhs: N2) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Double) => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ + r): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Double)  => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)   => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)    => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)     => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)   => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)    => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)    => l.map(_ + r): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Double)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)    => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)     => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)      => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)    => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)     => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)     => l.map(_ + r): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Double)    => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)     => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)      => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)       => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)     => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)      => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)      => l.map(_ + r): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Double)  => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)   => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)    => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)     => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)   => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)    => l.map(_ + r): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Double)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)    => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)     => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)      => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)     => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)     => l.map(_ + r): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Double)   => l.map(_ + r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)    => l.map(_ + r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)     => l.map(_ + r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)      => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)    => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)     => l.map(_ + r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)     => l.map(_ + r): Uncertain[Int]
      }

    transparent inline def +[N2 <: AllNumerics](rhs: Uncertain[N2])(using UncertainOpDummy) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ + _): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Uncertain[Double])  => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])   => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])    => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])     => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])   => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])    => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])    => l.zipWith(r)(_ + _): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Uncertain[Double])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])    => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])     => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])      => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])    => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])     => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])     => l.zipWith(r)(_ + _): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Uncertain[Double])    => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])     => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])      => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])       => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])     => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])      => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])      => l.zipWith(r)(_ + _): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Uncertain[Double])  => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])   => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])    => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])     => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])   => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])    => l.zipWith(r)(_ + _): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Uncertain[Double])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])    => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])     => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])      => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])     => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])     => l.zipWith(r)(_ + _): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Uncertain[Double])   => l.zipWith(r)(_ + _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])    => l.zipWith(r)(_ + _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])     => l.zipWith(r)(_ + _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])      => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])    => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])     => l.zipWith(r)(_ + _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])     => l.zipWith(r)(_ + _): Uncertain[Int]
      }

    // --- SUBTRACTION (-) ---

    transparent inline def -[N2 <: AllNumerics](rhs: N2) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Double) => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ - r): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Double)  => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)   => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)    => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)     => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)   => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)    => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)    => l.map(_ - r): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Double)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)    => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)     => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)      => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)    => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)     => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)     => l.map(_ - r): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Double)    => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)     => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)      => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)       => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)     => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)      => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)      => l.map(_ - r): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Double)  => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)   => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)    => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)     => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)   => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)    => l.map(_ - r): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Double)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)    => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)     => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)      => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)     => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)     => l.map(_ - r): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Double)   => l.map(_ - r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)    => l.map(_ - r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)     => l.map(_ - r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)      => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)    => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)     => l.map(_ - r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)     => l.map(_ - r): Uncertain[Int]
      }

    transparent inline def -[N2 <: AllNumerics](rhs: Uncertain[N2])(using UncertainOpDummy) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ - _): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Uncertain[Double])  => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])   => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])    => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])     => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])   => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])    => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])    => l.zipWith(r)(_ - _): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Uncertain[Double])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])    => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])     => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])      => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])    => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])     => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])     => l.zipWith(r)(_ - _): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Uncertain[Double])    => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])     => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])      => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])       => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])     => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])      => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])      => l.zipWith(r)(_ - _): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Uncertain[Double])  => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])   => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])    => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])     => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])   => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])    => l.zipWith(r)(_ - _): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Uncertain[Double])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])    => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])     => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])      => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])     => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])     => l.zipWith(r)(_ - _): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Uncertain[Double])   => l.zipWith(r)(_ - _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])    => l.zipWith(r)(_ - _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])     => l.zipWith(r)(_ - _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])      => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])    => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])     => l.zipWith(r)(_ - _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])     => l.zipWith(r)(_ - _): Uncertain[Int]
      }

    // --- MULTIPLICATION (*) ---

    transparent inline def *[N2 <: AllNumerics](rhs: N2) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Double) => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ * r): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Double)  => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)   => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)    => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)     => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)   => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)    => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)    => l.map(_ * r): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Double)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)    => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)     => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)      => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)    => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)     => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)     => l.map(_ * r): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Double)    => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)     => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)      => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)       => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)     => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)      => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)      => l.map(_ * r): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Double)  => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)   => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)    => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)     => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)   => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)    => l.map(_ * r): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Double)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)    => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)     => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)      => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)     => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)     => l.map(_ * r): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Double)   => l.map(_ * r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)    => l.map(_ * r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)     => l.map(_ * r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)      => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)    => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)     => l.map(_ * r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)     => l.map(_ * r): Uncertain[Int]
      }

    transparent inline def *[N2 <: AllNumerics](rhs: Uncertain[N2])(using UncertainOpDummy) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ * _): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Uncertain[Double])  => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])   => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])    => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])     => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])   => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])    => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])    => l.zipWith(r)(_ * _): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Uncertain[Double])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])    => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])     => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])      => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])    => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])     => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])     => l.zipWith(r)(_ * _): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Uncertain[Double])    => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])     => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])      => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])       => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])     => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])      => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])      => l.zipWith(r)(_ * _): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Uncertain[Double])  => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])   => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])    => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])     => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])   => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])    => l.zipWith(r)(_ * _): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Uncertain[Double])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])    => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])     => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])      => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])     => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])     => l.zipWith(r)(_ * _): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Uncertain[Double])   => l.zipWith(r)(_ * _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])    => l.zipWith(r)(_ * _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])     => l.zipWith(r)(_ * _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])      => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])    => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])     => l.zipWith(r)(_ * _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])     => l.zipWith(r)(_ * _): Uncertain[Int]
      }

    // --- DIVISION (/) ---

    transparent inline def /[N2 <: AllNumerics](rhs: N2) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Double) => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ / r): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Double)  => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)   => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)    => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)     => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)   => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)    => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)    => l.map(_ / r): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Double)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)    => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)     => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)      => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)    => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)     => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)     => l.map(_ / r): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Double)    => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)     => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)      => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)       => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)     => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)      => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)      => l.map(_ / r): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Double)  => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)   => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)    => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)     => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)   => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)    => l.map(_ / r): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Double)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)    => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)     => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)      => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)     => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)     => l.map(_ / r): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Double)   => l.map(_ / r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)    => l.map(_ / r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)     => l.map(_ / r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)      => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)    => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)     => l.map(_ / r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)     => l.map(_ / r): Uncertain[Int]
      }

    transparent inline def /[N2 <: AllNumerics](rhs: Uncertain[N2])(using UncertainOpDummy) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ / _): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Uncertain[Double])  => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])   => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])    => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])     => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])   => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])    => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])    => l.zipWith(r)(_ / _): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Uncertain[Double])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])    => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])     => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])      => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])    => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])     => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])     => l.zipWith(r)(_ / _): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Uncertain[Double])    => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])     => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])      => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])       => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])     => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])      => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])      => l.zipWith(r)(_ / _): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Uncertain[Double])  => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])   => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])    => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])     => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])   => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])    => l.zipWith(r)(_ / _): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Uncertain[Double])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])    => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])     => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])      => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])     => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])     => l.zipWith(r)(_ / _): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Uncertain[Double])   => l.zipWith(r)(_ / _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])    => l.zipWith(r)(_ / _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])     => l.zipWith(r)(_ / _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])      => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])    => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])     => l.zipWith(r)(_ / _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])     => l.zipWith(r)(_ / _): Uncertain[Int]
      }

    // --- REMAINDER (%) ---

    transparent inline def %[N2 <: AllNumerics](rhs: N2) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Double) => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Float)  => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Long)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Int)    => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Short)  => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Byte)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Double], r: Char)   => l.map(_ % r): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Double)  => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Float], r: Float)   => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Long)    => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Int)     => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Short)   => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Byte)    => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Float], r: Char)    => l.map(_ % r): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Double)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Long], r: Float)    => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Long], r: Long)     => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Int)      => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Short)    => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Byte)     => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Long], r: Char)     => l.map(_ % r): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Double)    => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Int], r: Float)     => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Int], r: Long)      => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Int], r: Int)       => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Int], r: Short)     => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Int], r: Byte)      => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Int], r: Char)      => l.map(_ % r): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Double)  => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Short], r: Float)   => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Short], r: Long)    => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Short], r: Int)     => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Short], r: Short)   => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Short], r: Byte)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Short], r: Char)    => l.map(_ % r): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Double)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Byte], r: Float)    => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Byte], r: Long)     => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Byte], r: Int)      => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Short)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Byte)     => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Byte], r: Char)     => l.map(_ % r): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Double)   => l.map(_ % r): Uncertain[Double]
        case (l: Uncertain[Char], r: Float)    => l.map(_ % r): Uncertain[Float]
        case (l: Uncertain[Char], r: Long)     => l.map(_ % r): Uncertain[Long]
        case (l: Uncertain[Char], r: Int)      => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Char], r: Short)    => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Char], r: Byte)     => l.map(_ % r): Uncertain[Int]
        case (l: Uncertain[Char], r: Char)     => l.map(_ % r): Uncertain[Int]
      }

    transparent inline def %[N2 <: AllNumerics](rhs: Uncertain[N2])(using UncertainOpDummy) =
      inline (lhs, rhs) match {
        // Double LHS -> Double
        case (l: Uncertain[Double], r: Uncertain[Double]) => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Float])  => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Long])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Int])    => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Short])  => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Byte])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Double], r: Uncertain[Char])   => l.zipWith(r)(_ % _): Uncertain[Double]
        // Float LHS -> Double | Float
        case (l: Uncertain[Float], r: Uncertain[Double])  => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Float], r: Uncertain[Float])   => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Long])    => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Int])     => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Short])   => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Byte])    => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Float], r: Uncertain[Char])    => l.zipWith(r)(_ % _): Uncertain[Float]
        // Long LHS -> Double | Float | Long
        case (l: Uncertain[Long], r: Uncertain[Double])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Long], r: Uncertain[Float])    => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Long], r: Uncertain[Long])     => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Int])      => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Short])    => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Byte])     => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Long], r: Uncertain[Char])     => l.zipWith(r)(_ % _): Uncertain[Long]
        // Int LHS -> Double | Float | Long | Int
        case (l: Uncertain[Int], r: Uncertain[Double])    => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Int], r: Uncertain[Float])     => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Int], r: Uncertain[Long])      => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Int], r: Uncertain[Int])       => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Short])     => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Byte])      => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Int], r: Uncertain[Char])      => l.zipWith(r)(_ % _): Uncertain[Int]
        // Short LHS -> Double | Float | Long | Int
        case (l: Uncertain[Short], r: Uncertain[Double])  => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Short], r: Uncertain[Float])   => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Short], r: Uncertain[Long])    => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Short], r: Uncertain[Int])     => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Short])   => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Byte])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Short], r: Uncertain[Char])    => l.zipWith(r)(_ % _): Uncertain[Int]
        // Byte LHS -> Double | Float | Long | Int
        case (l: Uncertain[Byte], r: Uncertain[Double])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Byte], r: Uncertain[Float])    => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Byte], r: Uncertain[Long])     => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Byte], r: Uncertain[Int])      => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Short])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Byte])     => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Byte], r: Uncertain[Char])     => l.zipWith(r)(_ % _): Uncertain[Int]
        // Char LHS -> Double | Float | Long | Int
        case (l: Uncertain[Char], r: Uncertain[Double])   => l.zipWith(r)(_ % _): Uncertain[Double]
        case (l: Uncertain[Char], r: Uncertain[Float])    => l.zipWith(r)(_ % _): Uncertain[Float]
        case (l: Uncertain[Char], r: Uncertain[Long])     => l.zipWith(r)(_ % _): Uncertain[Long]
        case (l: Uncertain[Char], r: Uncertain[Int])      => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Short])    => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Byte])     => l.zipWith(r)(_ % _): Uncertain[Int]
        case (l: Uncertain[Char], r: Uncertain[Char])     => l.zipWith(r)(_ % _): Uncertain[Int]
      }
  }
}
