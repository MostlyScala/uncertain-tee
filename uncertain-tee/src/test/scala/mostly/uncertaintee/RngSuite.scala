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

package mostly.uncertaintee

import munit.FunSuite

import scala.util.Random

/** A base trait for MUnit test suites that require a deterministic, seeded Random instance for each test */
trait RngSuite extends FunSuite {

  val seededRandomFixture = FunFixture[Random](
    setup = { _ => new Random(42L) },
    teardown = { _ => () }
  )

  def rngTest(name: String)(testLogic: Random ?=> Any): Unit =
    seededRandomFixture.test(name)(rng => testLogic(using rng))
}
