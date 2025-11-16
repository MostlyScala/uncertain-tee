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
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import scala.util.Random

trait GenUncertainOps {

  extension (g: Gen.type) {

    /** The resulting generator is non-deterministic regarding ScalaCheck seeds - this may or may not impact how you use this for testing.
      *
      * @note
      *   Because `Uncertain` manages its own internal RNG state, failure reproduction using ScalaCheck seeds/printing will not work for this generator. To ensure determinism, you
      *   must instantiate the underlying `Uncertain` with a seeded `scala.util.Random`.
      */
    def fromUncertain[T](uncertain: Uncertain[T]): Gen[T] =
      Gen.resultOf[Unit, T](_ => uncertain.sample())
  }

  extension (u: Uncertain.type) {

    /** Creates an Uncertain from a ScalaCheck Gen.
      *
      * @param gen
      *   The Gen to convert.
      * @param p
      *   Gen parameters (size, etc).
      * @param retries
      *   Number of retries for Gen generation.
      * @param random
      *   The RNG to drive the simulation. Each call to sample() will consume a new seed from this Random. If you need reproducible results, pass a seeded Random here.
      */
    def fromGen[T](
      gen: Gen[T],
      p: Gen.Parameters = Gen.Parameters.default,
      retries: Int = 100
    )(using random: Random = new Random()): Uncertain[T] =
      Uncertain { () =>
        gen.pureApply(
          p = p,
          seed = Seed(random.nextLong()),
          retries = retries
        )
      }(using random)
  }
  extension [T](u: Uncertain[T]) {

    /** WARNING: This generator is non-deterministic regarding ScalaCheck seeds.
      *
      * @note
      *   Because `Uncertain` manages its own internal RNG state, failure reproduction using ScalaCheck seeds/printing will not work for this generator. To ensure determinism, you
      *   must instantiate the underlying `Uncertain` with a seeded `scala.util.Random`.
      */
    def toGen: Gen[T] = Gen.fromUncertain(u)

  }
}
