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
import mostly.uncertaintee.syntax.*
import munit.FunSuite

class ArithmeticOpsWideningSpec extends FunSuite {

  // ==================== ADDITION TESTS ====================

  test("Addition: Double + Double -> Double") {
    val resLhsScalar  = 2.0 + Uncertain.always(1.0)
    val resRhsScalar  = Uncertain.always(1.0) + 2.0
    val resUncertains = Uncertain.always(1.0) + Uncertain.always(2.0)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Addition: Float + Float -> Float") {
    val resLhsScalar  = 2.0f + Uncertain.always(1.0f)
    val resRhsScalar  = Uncertain.always(1.0f) + 2.0f
    val resUncertains = Uncertain.always(1.0f) + Uncertain.always(2.0f)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 3.0f)
    assertEquals(resRhsScalar.sample(), 3.0f)
    assertEquals(resUncertains.sample(), 3.0f)
  }

  test("Addition: Long + Long -> Long") {
    val resLhsScalar  = 2L + Uncertain.always(1L)
    val resRhsScalar  = Uncertain.always(1L) + 2L
    val resUncertains = Uncertain.always(1L) + Uncertain.always(2L)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 3L)
    assertEquals(resRhsScalar.sample(), 3L)
    assertEquals(resUncertains.sample(), 3L)
  }

  test("Addition: Int + Int -> Int") {
    val resLhsScalar  = 2 + Uncertain.always(1)
    val resRhsScalar  = Uncertain.always(1) + 2
    val resUncertains = Uncertain.always(1) + Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 3)
    assertEquals(resRhsScalar.sample(), 3)
    assertEquals(resUncertains.sample(), 3)
  }

  test("Addition: Short + Short -> Int") {
    val resLhsScalar  = 2.toShort + Uncertain.always(1.toShort)
    val resRhsScalar  = Uncertain.always(1.toShort) + 2.toShort
    val resUncertains = Uncertain.always(1.toShort) + Uncertain.always(2.toShort)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 3)
    assertEquals(resRhsScalar.sample(), 3)
    assertEquals(resUncertains.sample(), 3)
  }

  test("Addition: Byte + Byte -> Int") {
    val resLhsScalar  = 2.toByte + Uncertain.always(1.toByte)
    val resRhsScalar  = Uncertain.always(1.toByte) + 2.toByte
    val resUncertains = Uncertain.always(1.toByte) + Uncertain.always(2.toByte)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 3)
    assertEquals(resRhsScalar.sample(), 3)
    assertEquals(resUncertains.sample(), 3)
  }

  test("Addition: Double + Float -> Double") {
    val resLhsScalar  = 2.0 + Uncertain.always(1.0f)
    val resRhsScalar  = Uncertain.always(1.0) + 2.0f
    val resUncertains = Uncertain.always(1.0) + Uncertain.always(2.0f)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Addition: Double + Int -> Double") {
    val resLhsScalar  = 2.0 + Uncertain.always(1)
    val resRhsScalar  = Uncertain.always(1.0) + 2
    val resUncertains = Uncertain.always(1.0) + Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Addition: Float + Int -> Float") {
    val resLhsScalar  = 2.0f + Uncertain.always(1)
    val resRhsScalar  = Uncertain.always(1.0f) + 2
    val resUncertains = Uncertain.always(1.0f) + Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 3.0f)
    assertEquals(resRhsScalar.sample(), 3.0f)
    assertEquals(resUncertains.sample(), 3.0f)
  }

  test("Addition: Long + Int -> Long") {
    val resLhsScalar  = 2L + Uncertain.always(1)
    val resRhsScalar  = Uncertain.always(1L) + 2
    val resUncertains = Uncertain.always(1L) + Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 3L)
    assertEquals(resRhsScalar.sample(), 3L)
    assertEquals(resUncertains.sample(), 3L)
  }

  // ==================== SUBTRACTION TESTS ====================

  test("Subtraction: Double - Double -> Double") {
    val resLhsScalar  = 5.0 - Uncertain.always(2.0)
    val resRhsScalar  = Uncertain.always(5.0) - 2.0
    val resUncertains = Uncertain.always(5.0) - Uncertain.always(2.0)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Subtraction: Float - Float -> Float") {
    val resLhsScalar  = 5.0f - Uncertain.always(2.0f)
    val resRhsScalar  = Uncertain.always(5.0f) - 2.0f
    val resUncertains = Uncertain.always(5.0f) - Uncertain.always(2.0f)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 3.0f)
    assertEquals(resRhsScalar.sample(), 3.0f)
    assertEquals(resUncertains.sample(), 3.0f)
  }

  test("Subtraction: Long - Long -> Long") {
    val resLhsScalar  = 5L - Uncertain.always(2L)
    val resRhsScalar  = Uncertain.always(5L) - 2L
    val resUncertains = Uncertain.always(5L) - Uncertain.always(2L)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 3L)
    assertEquals(resRhsScalar.sample(), 3L)
    assertEquals(resUncertains.sample(), 3L)
  }

  test("Subtraction: Int - Int -> Int") {
    val resLhsScalar  = 5 - Uncertain.always(2)
    val resRhsScalar  = Uncertain.always(5) - 2
    val resUncertains = Uncertain.always(5) - Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 3)
    assertEquals(resRhsScalar.sample(), 3)
    assertEquals(resUncertains.sample(), 3)
  }

  test("Subtraction: Double - Float -> Double") {
    val resLhsScalar  = 5.0 - Uncertain.always(2.0f)
    val resRhsScalar  = Uncertain.always(5.0) - 2.0f
    val resUncertains = Uncertain.always(5.0) - Uncertain.always(2.0f)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Subtraction: Long - Int -> Long") {
    val resLhsScalar  = 5L - Uncertain.always(2)
    val resRhsScalar  = Uncertain.always(5L) - 2
    val resUncertains = Uncertain.always(5L) - Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 3L)
    assertEquals(resRhsScalar.sample(), 3L)
    assertEquals(resUncertains.sample(), 3L)
  }

  // ==================== MULTIPLICATION TESTS ====================

  test("Multiplication: Double * Double -> Double") {
    val resLhsScalar  = 2.0 * Uncertain.always(3.0)
    val resRhsScalar  = Uncertain.always(2.0) * 3.0
    val resUncertains = Uncertain.always(2.0) * Uncertain.always(3.0)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 6.0)
    assertEquals(resRhsScalar.sample(), 6.0)
    assertEquals(resUncertains.sample(), 6.0)
  }

  test("Multiplication: Float * Float -> Float") {
    val resLhsScalar  = 2.0f * Uncertain.always(3.0f)
    val resRhsScalar  = Uncertain.always(2.0f) * 3.0f
    val resUncertains = Uncertain.always(2.0f) * Uncertain.always(3.0f)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 6.0f)
    assertEquals(resRhsScalar.sample(), 6.0f)
    assertEquals(resUncertains.sample(), 6.0f)
  }

  test("Multiplication: Long * Long -> Long") {
    val resLhsScalar  = 2L * Uncertain.always(3L)
    val resRhsScalar  = Uncertain.always(2L) * 3L
    val resUncertains = Uncertain.always(2L) * Uncertain.always(3L)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 6L)
    assertEquals(resRhsScalar.sample(), 6L)
    assertEquals(resUncertains.sample(), 6L)
  }

  test("Multiplication: Int * Int -> Int") {
    val resLhsScalar  = 2 * Uncertain.always(3)
    val resRhsScalar  = Uncertain.always(2) * 3
    val resUncertains = Uncertain.always(2) * Uncertain.always(3)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 6)
    assertEquals(resRhsScalar.sample(), 6)
    assertEquals(resUncertains.sample(), 6)
  }

  test("Multiplication: Long * Int -> Long") {
    val resLhsScalar  = 2L * Uncertain.always(3)
    val resRhsScalar  = Uncertain.always(2L) * 3
    val resUncertains = Uncertain.always(2L) * Uncertain.always(3)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 6L)
    assertEquals(resRhsScalar.sample(), 6L)
    assertEquals(resUncertains.sample(), 6L)
  }

  test("Multiplication: Float * Long -> Float") {
    val resLhsScalar  = 2.0f * Uncertain.always(3L)
    val resRhsScalar  = Uncertain.always(2.0f) * 3L
    val resUncertains = Uncertain.always(2.0f) * Uncertain.always(3L)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 6.0f)
    assertEquals(resRhsScalar.sample(), 6.0f)
    assertEquals(resUncertains.sample(), 6.0f)
  }

  // ==================== DIVISION TESTS ====================

  test("Division: Double / Double -> Double") {
    val resLhsScalar  = 6.0 / Uncertain.always(2.0)
    val resRhsScalar  = Uncertain.always(6.0) / 2.0
    val resUncertains = Uncertain.always(6.0) / Uncertain.always(2.0)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Division: Float / Float -> Float") {
    val resLhsScalar  = 6.0f / Uncertain.always(2.0f)
    val resRhsScalar  = Uncertain.always(6.0f) / 2.0f
    val resUncertains = Uncertain.always(6.0f) / Uncertain.always(2.0f)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 3.0f)
    assertEquals(resRhsScalar.sample(), 3.0f)
    assertEquals(resUncertains.sample(), 3.0f)
  }

  test("Division: Long / Long -> Long") {
    val resLhsScalar  = 6L / Uncertain.always(2L)
    val resRhsScalar  = Uncertain.always(6L) / 2L
    val resUncertains = Uncertain.always(6L) / Uncertain.always(2L)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 3L)
    assertEquals(resRhsScalar.sample(), 3L)
    assertEquals(resUncertains.sample(), 3L)
  }

  test("Division: Int / Int -> Int") {
    val resLhsScalar  = 6 / Uncertain.always(2)
    val resRhsScalar  = Uncertain.always(6) / 2
    val resUncertains = Uncertain.always(6) / Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 3)
    assertEquals(resRhsScalar.sample(), 3)
    assertEquals(resUncertains.sample(), 3)
  }

  test("Division: Double / Int -> Double") {
    val resLhsScalar  = 6.0 / Uncertain.always(2)
    val resRhsScalar  = Uncertain.always(6.0) / 2
    val resUncertains = Uncertain.always(6.0) / Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 3.0)
    assertEquals(resRhsScalar.sample(), 3.0)
    assertEquals(resUncertains.sample(), 3.0)
  }

  test("Division: Long / Int -> Long") {
    val resLhsScalar  = 6L / Uncertain.always(2)
    val resRhsScalar  = Uncertain.always(6L) / 2
    val resUncertains = Uncertain.always(6L) / Uncertain.always(2)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 3L)
    assertEquals(resRhsScalar.sample(), 3L)
    assertEquals(resUncertains.sample(), 3L)
  }

  // ==================== REMAINDER TESTS ====================

  test("Remainder: Double % Double -> Double") {
    val resLhsScalar  = 7.0                  % Uncertain.always(3.0)
    val resRhsScalar  = Uncertain.always(7.0) % 3.0
    val resUncertains = Uncertain.always(7.0) % Uncertain.always(3.0)

    assert(resLhsScalar.sample().isInstanceOf[Double])
    assert(resRhsScalar.sample().isInstanceOf[Double])
    assert(resUncertains.sample().isInstanceOf[Double])
    assertEquals(resLhsScalar.sample(), 1.0)
    assertEquals(resRhsScalar.sample(), 1.0)
    assertEquals(resUncertains.sample(), 1.0)
  }

  test("Remainder: Float % Float -> Float") {
    val resLhsScalar  = 7.0f                  % Uncertain.always(3.0f)
    val resRhsScalar  = Uncertain.always(7.0f) % 3.0f
    val resUncertains = Uncertain.always(7.0f) % Uncertain.always(3.0f)

    assert(resLhsScalar.sample().isInstanceOf[Float])
    assert(resRhsScalar.sample().isInstanceOf[Float])
    assert(resUncertains.sample().isInstanceOf[Float])
    assertEquals(resLhsScalar.sample(), 1.0f)
    assertEquals(resRhsScalar.sample(), 1.0f)
    assertEquals(resUncertains.sample(), 1.0f)
  }

  test("Remainder: Long % Long -> Long") {
    val resLhsScalar  = 7L                  % Uncertain.always(3L)
    val resRhsScalar  = Uncertain.always(7L) % 3L
    val resUncertains = Uncertain.always(7L) % Uncertain.always(3L)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 1L)
    assertEquals(resRhsScalar.sample(), 1L)
    assertEquals(resUncertains.sample(), 1L)
  }

  test("Remainder: Int % Int -> Int") {
    val resLhsScalar  = 7                  % Uncertain.always(3)
    val resRhsScalar  = Uncertain.always(7) % 3
    val resUncertains = Uncertain.always(7) % Uncertain.always(3)

    assert(resLhsScalar.sample().isInstanceOf[Int])
    assert(resRhsScalar.sample().isInstanceOf[Int])
    assert(resUncertains.sample().isInstanceOf[Int])
    assertEquals(resLhsScalar.sample(), 1)
    assertEquals(resRhsScalar.sample(), 1)
    assertEquals(resUncertains.sample(), 1)
  }

  test("Remainder: Long % Int -> Long") {
    val resLhsScalar  = 7L                  % Uncertain.always(3)
    val resRhsScalar  = Uncertain.always(7L) % 3
    val resUncertains = Uncertain.always(7L) % Uncertain.always(3)

    assert(resLhsScalar.sample().isInstanceOf[Long])
    assert(resRhsScalar.sample().isInstanceOf[Long])
    assert(resUncertains.sample().isInstanceOf[Long])
    assertEquals(resLhsScalar.sample(), 1L)
    assertEquals(resRhsScalar.sample(), 1L)
    assertEquals(resUncertains.sample(), 1L)
  }

  // ==================== COMPLEX EXPRESSION TESTS ====================

  test("Complex expression: (Uncertain[Int] + 3) * 2 - 5") {
    val res = (Uncertain.always(1) + 3) * 2 - 5
    assert(res.sample().isInstanceOf[Int])
    assertEquals(res.sample(), 3)
  }

  test("Complex expression: (3 + Uncertain[Int]) * 2 - 5") {
    val res = (3 + Uncertain.always(1)) * 2 - 5
    assert(res.sample().isInstanceOf[Int])
    assertEquals(res.sample(), 3)
  }

  test("Complex expression with mixed types") {
    val res1 = Uncertain.always(10.0) / 2 + Uncertain.always(3L) * 2
    val res2 = 10.0 / Uncertain.always(2) + 3L * Uncertain.always(2)

    assert(res1.sample().isInstanceOf[Double])
    assert(res2.sample().isInstanceOf[Double])
    assertEquals(res1.sample(), 11.0)
    assertEquals(res2.sample(), 11.0)
  }

  test("Type promotion: Int -> Long -> Float -> Double") {
    val res1 = Uncertain.always(1) + Uncertain.always(2L) + Uncertain.always(3.0f) + Uncertain.always(4.0)
    val res2 = 1 + Uncertain.always(2L) + 3.0f + Uncertain.always(4.0)

    assert(res1.sample().isInstanceOf[Double])
    assert(res2.sample().isInstanceOf[Double])
    assertEquals(res1.sample(), 10.0)
    assertEquals(res2.sample(), 10.0)
  }
}
