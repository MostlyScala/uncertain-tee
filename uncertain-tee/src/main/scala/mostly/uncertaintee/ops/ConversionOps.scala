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

  /** 'implicit', not 'given' is a deliberate choice.
    *
    * In scala 3, a wildcard import does not mean importing everything.
    *
    * {{{
    *   mostly.uncertaintee.syntax.{given, *}
    * }}}
    *
    * `given` needs to be explicitly imported: https://docs.scala-lang.org/scala3/reference/contextual/given-imports.html
    *
    * `implicit` still works, though this may change in future versions of scala 3 LTS according to the link above; at which point we will change this to be given, and consumers
    * (and all documentation) will have to make the breaking change to `import mostly.uncertaintee.syntax.{given, *}`
    */
  implicit val convertBoolToInt: Conversion[Boolean, Int] = {
    case true  => 1
    case false => 0
  }

  extension [T](uncertainT: Uncertain[T]) {

    /** Converts the uncertain value from type `T` to type `U`, if a [[scala.Conversion]] given instance is available (provides a more fluent alternative to `.map` for standard
      * type conversions).
      */
    def to[B](using conv: Conversion[T, B]): Uncertain[B] =
      uncertainT.map(conv.convert)
  }
}
