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

package mostly.uncertaintee.scalacheck.ops

import mostly.uncertaintee.*
import mostly.uncertaintee.scalacheck.syntax.*
import org.scalacheck.*

import scala.util.Random

trait ArbitraryUncertainOps {

  extension (a: Arbitrary.type) {

    /** WARNING: This arbitrary is non-deterministic regarding ScalaCheck seeds.
      *
      * @note
      *   Because `Uncertain` manages its own internal RNG state, failure reproduction using ScalaCheck seeds/printing will not work for this generator. To ensure determinism, you
      *   must instantiate the underlying `Uncertain` with a seeded `scala.util.Random`.
      */
    def fromUncertain[T](uncertain: Uncertain[T]): Arbitrary[T] =
      Arbitrary(Gen.resultOf[Unit, T](_ => uncertain.sample()))
  }

  extension (u: Uncertain.type) {

    /** Creates an Uncertain from a ScalaCheck Arbitrary.
      *
      * @param arb
      *   The Arbitrary to convert.
      * @param p
      *   Gen parameters (size, etc).
      * @param retries
      *   Number of retries for Gen generation.
      * @param random
      *   The RNG to drive the simulation. If you need reproducible results, pass a seeded Random here.
      */
    def fromArbitrary[T](
      arb: Arbitrary[T],
      p: Gen.Parameters = Gen.Parameters.default,
      retries: Int = 100
    )(using random: Random = new Random()): Uncertain[T] =
      Uncertain.fromGen(
        gen = arb.arbitrary,
        p = p,
        retries = retries
      )
  }

  extension [T](u: Uncertain[T]) {

    /** WARNING: This arbitrary is non-deterministic regarding ScalaCheck seeds.
      *
      * @note
      *   Because `Uncertain` manages its own internal RNG state, failure reproduction using ScalaCheck seeds/printing will not work for this generator. To ensure determinism, you
      *   must instantiate the underlying `Uncertain` with a seeded `scala.util.Random`.
      */
    def toArbitrary: Arbitrary[T] = Arbitrary.fromUncertain(u)
  }

}
