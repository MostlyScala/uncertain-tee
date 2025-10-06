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

import scala.math.abs
import mostly.uncertaintee.syntax.*

class BooleanOperationsSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Basic Logical Operator Tests (Independent Variables) ---

  rngTest("Logical NOT `unary_!` should invert the probability of an uncertain boolean") {
    val p    = 0.75
    val b    = Uncertain.bernoulli(p)
    val notB = !b

    // P(!A) = 1 - P(A) -> 1 - 0.75 = 0.25
    val theoreticalMean = 0.25
    val sampleMean      = notB.expectedValue(sampleCount)

    val hint = s"Expected value of !Bernoulli(0.75) should be approx 0.25. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Logical AND `&&` of two independent uncertain booleans should yield the product of their probabilities") {
    val p1   = 0.6
    val p2   = 0.5
    val b1   = Uncertain.bernoulli(p1)
    val b2   = Uncertain.bernoulli(p2)
    val bAnd = b1 && b2

    // P(A and B) = P(A) * P(B) -> 0.6 * 0.5 = 0.3
    val theoreticalMean = 0.3
    val sampleMean      = bAnd.expectedValue(sampleCount)

    val hint = s"Expected value of Bernoulli(0.6) && Bernoulli(0.5) should be approx 0.3. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Logical OR `||` of two independent uncertain booleans should follow the inclusion-exclusion principle") {
    val p1  = 0.3
    val p2  = 0.4
    val b1  = Uncertain.bernoulli(p1)
    val b2  = Uncertain.bernoulli(p2)
    val bOr = b1 || b2

    // P(A or B) = P(A) + P(B) - P(A and B) -> 0.3 + 0.4 - (0.3 * 0.4) = 0.58
    val theoreticalMean = 0.58
    val sampleMean      = bOr.expectedValue(sampleCount)

    val hint = s"Expected value of Bernoulli(0.3) || Bernoulli(0.4) should be approx 0.58. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  // --- Correlation Tests (Dependent Variables) ---

  rngTest("Correlation: `b && b` should be perfectly correlated and equivalent to `b`") {
    val p     = 0.8
    val b     = Uncertain.bernoulli(p)
    val bAndB = b && b

    // The result should have the same probability as the original.
    val sampleMean = bAndB.expectedValue(sampleCount)
    val hint       = s"Expected value of correlated `b && b` where P(b)=0.8 should be approx 0.8. Got $sampleMean"
    assert(abs(sampleMean - p) < tolerance, hint)
  }

  rngTest("Correlation: `b || b` should be perfectly correlated and equivalent to `b`") {
    val p    = 0.25
    val b    = Uncertain.bernoulli(p)
    val bOrB = b || b

    // The result should have the same probability as the original.
    val sampleMean = bOrB.expectedValue(sampleCount)
    val hint       = s"Expected value of correlated `b || b` where P(b)=0.25 should be approx 0.25. Got $sampleMean"
    assert(abs(sampleMean - p) < tolerance, hint)
  }

  rngTest("Correlation: `b || !b` must always be true (Law of Excluded Middle)") {
    val p         = 0.4
    val b         = Uncertain.bernoulli(p)
    val tautology = b || !b

    // Every single sample must be true.
    val samples = tautology.take(1000)
    assert(samples.forall(_ == true), "`b || !b` must always evaluate to true due to correlation.")
    assertEquals(tautology.expectedValue(1000), 1.0, "The expected value of `b || !b` must be exactly 1.0")
  }

  rngTest("Correlation: `b && !b` must always be false (Principle of Contradiction)") {
    val p             = 0.9
    val b             = Uncertain.bernoulli(p)
    val contradiction = b && !b

    // Every single sample must be false.
    val samples = contradiction.take(1000)
    assert(samples.forall(_ == false), "`b && !b` must always evaluate to false due to correlation.")
    assertEquals(contradiction.expectedValue(1000), 0.0, "The expected value of `b && !b` must be exactly 0.0")
  }

  // --- Operations with Certain/Point Values ---

  rngTest("`b && Uncertain.point(true)` should be equivalent to `b`") {
    val p        = 0.65
    val b        = Uncertain.bernoulli(p)
    val bAndTrue = b && Uncertain.point(true)

    val sampleMean = bAndTrue.expectedValue(sampleCount)
    val hint       = s"Expected value of `b && true` where P(b)=0.65 should be approx 0.65. Got $sampleMean"
    assert(abs(sampleMean - p) < tolerance, hint)
  }

  rngTest("`b && Uncertain.point(false)` should always be false") {
    val p         = 0.7
    val b         = Uncertain.bernoulli(p)
    val bAndFalse = b && Uncertain.point(false)

    assertEquals(bAndFalse.expectedValue(1000), 0.0, "The expected value of `b && false` must be 0.0")
  }

  rngTest("`b || Uncertain.point(true)` should always be true") {
    val p       = 0.3
    val b       = Uncertain.bernoulli(p)
    val bOrTrue = b || Uncertain.point(true)

    assertEquals(bOrTrue.expectedValue(1000), 1.0, "The expected value of `b || true` must be 1.0")
  }

  rngTest("`b || Uncertain.point(false)` should be equivalent to `b`") {
    val p        = 0.55
    val b        = Uncertain.bernoulli(p)
    val bOrFalse = b || Uncertain.point(false)

    val sampleMean = bOrFalse.expectedValue(sampleCount)
    val hint       = s"Expected value of `b || false` where P(b)=0.55 should be approx 0.55. Got $sampleMean"
    assert(abs(sampleMean - p) < tolerance, hint)
  }

  // --- Complex/Chained Operations ---

  rngTest("De Morgan's Law: !(b1 && b2) should be equivalent to !b1 || !b2") {
    val p1 = 0.7
    val p2 = 0.2
    val b1 = Uncertain.bernoulli(p1)
    val b2 = Uncertain.bernoulli(p2)

    val leftSide  = !(b1 && b2)
    val rightSide = !b1 || !b2

    val leftMean  = leftSide.expectedValue(sampleCount)
    val rightMean = rightSide.expectedValue(sampleCount)

    // Theoretical mean = 1 - (p1 * p2) -> 1 - (0.7 * 0.2) = 0.86
    val theoreticalMean = 0.86
    val hint            = s"Both sides of De Morgan's law should approximate 0.86. Got L=$leftMean, R=$rightMean"
    assert(abs(leftMean - theoreticalMean) < tolerance, hint)
    assert(abs(rightMean - theoreticalMean) < tolerance, hint)
    assert(
      abs(leftMean - rightMean) < tolerance,
      "The expected values of both sides of the equation should be nearly identical."
    )
  }

  rngTest("De Morgan's Law: !(b1 || b2) should be equivalent to !b1 && !b2") {
    val p1 = 0.1
    val p2 = 0.4
    val b1 = Uncertain.bernoulli(p1)
    val b2 = Uncertain.bernoulli(p2)

    val leftSide  = !(b1 || b2)
    val rightSide = !b1 && !b2

    val leftMean  = leftSide.expectedValue(sampleCount)
    val rightMean = rightSide.expectedValue(sampleCount)

    // Theoretical mean = (1 - p1) * (1 - p2) -> (1 - 0.1) * (1 - 0.4) = 0.54
    val theoreticalMean = 0.54
    val hint            = s"Both sides of De Morgan's law should approximate 0.54. Got L=$leftMean, R=$rightMean"
    assert(abs(leftMean - theoreticalMean) < tolerance, hint)
    assert(abs(rightMean - theoreticalMean) < tolerance, hint)
    assert(
      abs(leftMean - rightMean) < tolerance,
      "The expected values of both sides of the equation should be nearly identical."
    )
  }

  rngTest("Simulated XOR should have the correct probability") {
    val p1 = 0.6
    val p2 = 0.3
    val b1 = Uncertain.bernoulli(p1)
    val b2 = Uncertain.bernoulli(p2)

    // XOR can be expressed as (b1 || b2) && !(b1 && b2)
    // or as (b1 && !b2) || (!b1 && b2)
    val xor = (b1 || b2) && !(b1 && b2)

    // P(A XOR B) = P(A) + P(B) - 2*P(A)*P(B) -> 0.6 + 0.3 - 2 * 0.6 * 0.3 = 0.54
    val theoreticalMean = 0.54
    val sampleMean      = xor.expectedValue(sampleCount)

    val hint = s"Expected value for XOR(0.6, 0.3) should be approx 0.54. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }
}
