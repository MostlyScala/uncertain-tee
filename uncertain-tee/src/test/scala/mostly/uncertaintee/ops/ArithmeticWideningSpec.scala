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

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

class ArithmeticWideningSpec extends munit.FunSuite {

  test("Int + Int results in Int") {
    val result1 = Uncertain.point(1) + 2
    assertEquals(result1.sample(), 3)
    assert(result1.sample().isInstanceOf[Int])
  }

  test("Int + Uncertain[Int] results in Int") {
    val result1 = Uncertain.point(1) + Uncertain.point(2)
    assertEquals(result1.sample(), 3)
    assert(result1.sample().isInstanceOf[Int])
  }
  test("Int + Long results in Long") {
    val result1: Uncertain[Long] = Uncertain.point(1) + 2L
    assertEquals(result1.sample(), 3L)
    assert(result1.sample().isInstanceOf[Long])
  }

  test("Int + Uncertaion[Long] results in Long") {
    val result1: Uncertain[Long] = Uncertain.point(1) + Uncertain.point(2L)
    assertEquals(result1.sample(), 3L)
    assert(result1.sample().isInstanceOf[Long])
  }

}
