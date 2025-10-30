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
package mostly.uncertaintee.styledocs

/** ==Why We Use `implicit` instead of `given`==
  *
  * In Scala 3, a wildcard import does not mean "importing everything"—it means everything except `given` instances.
  *
  * {{{
  * object librarycode {
  *   implicit val b2i: Conversion[Boolean, Int] = ???
  *   given d2i: Conversion[Double, Int] = ???
  *   def doSomething[A, B](a: A)(using conversion: Conversion[A, B]): B = ???
  * }
  *
  * // inside BusinessLogic.scala
  * import librarycode.*
  *
  * val x: Int = doSomething(true) // compiles: implicit instance found
  * val y: Int = doSomething(1.0)  // fails: given instance not imported
  * }}}
  *
  * To make both work, end users must explicitly import `given` instances:
  *
  * {{{
  * import librarycode.{given, *}
  *
  * val x: Int = doSomething(true) // compiles: implicit instance found
  * val y: Int = doSomething(1.0)  // compiles: given instance now imported
  * }}}
  *
  * This is by design (see [[https://docs.scala-lang.org/scala3/reference/contextual/given-imports.html Scala 3 docs: given imports]]), likely to make implicits less surprising.
  * Unfortunately, this prevents established libraries from migrating from `implicit` to `given` if they rely on wildcard imports—which most existing Scala libraries do—meaning all
  * existing documentation would need to be rewritten.
  *
  * We find it simpler and less surprising for end users to understand that wildcard means "everything" rather than "everything (except givens)".
  *
  * '''This project will remain on `implicit`''' until a future LTS version of Scala drops support for the old behavior and emits compiler warnings/errors, which appears to be the
  * standard approach for most established libraries. The common expectation is that a wildcard import should import everything.
  *
  * The documentation linked above outlines a migration plan where old-style implicits accessed through wildcard imports would eventually become a compiler error. As of October
  * 2025 (nearly 5 years after publication), this doesn't seem to be the case yet; we will adapt when necessary.
  */
object WhyIsThisImplicitAndNotGiven
