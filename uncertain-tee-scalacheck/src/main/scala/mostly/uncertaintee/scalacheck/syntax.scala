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

package mostly.uncertaintee.scalacheck

import mostly.uncertaintee.*
import mostly.uncertaintee.scalacheck.ops.*
import org.scalacheck.*

import scala.util.Random

object syntax extends ArbitraryUncertainOps with GenUncertainOps with PropUncertainOps with UncertainInstances {

  val arbitrary: ArbitraryUncertainOps = this
  val gen: GenUncertainOps             = this
  val prop: PropUncertainOps           = this
  val instances: UncertainInstances    = this

  extension [T](ga: Gen[T] | Arbitrary[T]) {

    /** Converts a Gen/Arbitrary to an Uncertain distribution.
      *
      * @param p
      *   Gen parameters (size, etc).
      * @param retries
      *   Number of retries for Gen generation.
      * @param random
      *   The RNG to drive the simulation. If you need reproducible results, pass a seeded Random here.
      */
    def toUncertain(
      p: Gen.Parameters = Gen.Parameters.default,
      retries: Int = 100
    )(using random: Random = new Random()): Uncertain[T] =
      ga match {
        case g: Gen[T]       => Uncertain.fromGen(g, p, retries)(using random)
        case a: Arbitrary[T] => Uncertain.fromArbitrary(a, p, retries)(using random)
      }
  }
}
