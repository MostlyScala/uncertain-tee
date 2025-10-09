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
  *    import mostly.uncertaintee.syntax.conversion.*
  *    // or just import all the syntax (recommended)
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ConversionOps {

  extension [T](uncertainT: Uncertain[T]) {

    /** Converts the uncertain value from type `T` to type `U`, if a [[scala.Conversion]] given instance is available (provides a more fluent alternative to `.map` for standard
      * type conversions).
      */
    def to[B](using conv: Conversion[T, B]): Uncertain[B] =
      uncertainT.map(conv)
  }
}
