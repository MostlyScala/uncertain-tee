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
import mostly.uncertaintee.syntax.*

class StackSafetyTest extends FunSuite {

  test("recursive samples should be stack safe") {
    def foo(value: Uncertain[Int]): Uncertain[Int] =
      value.flatMap(i =>
        if (i == 1_000_000) Uncertain.point(i)
        else foo(Uncertain.point(i + 1))
      )

    assertEquals(foo(Uncertain.point(0)).sample(), 1_000_000)
  }
}
