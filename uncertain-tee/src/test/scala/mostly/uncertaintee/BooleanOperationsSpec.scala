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

import mostly.uncertaintee.syntax.*

import scala.math.abs

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
    val sampleMean      = notB.mean(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Expected value of !Bernoulli(0.75) should be approx 0.25. Got $sampleMean"
    )
  }

  rngTest("Logical AND `&&` of two independent uncertain booleans should yield the product of their probabilities") {
    val p1   = 0.6
    val p2   = 0.5
    val b1   = Uncertain.bernoulli(p1)
    val b2   = Uncertain.bernoulli(p2)
    val bAnd = b1 && b2

    // P(A and B) = P(A) * P(B) -> 0.6 * 0.5 = 0.3
    val theoreticalMean = 0.3
    val sampleMean      = bAnd.mean(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Expected value of Bernoulli(0.6) && Bernoulli(0.5) should be approx 0.3. Got $sampleMean"
    )
  }

  rngTest("Logical OR `||` of two independent uncertain booleans should follow the inclusion-exclusion principle") {
    val p1  = 0.3
    val p2  = 0.4
    val b1  = Uncertain.bernoulli(p1)
    val b2  = Uncertain.bernoulli(p2)
    val bOr = b1 || b2

    // P(A or B) = P(A) + P(B) - P(A and B) -> 0.3 + 0.4 - (0.3 * 0.4) = 0.58
    val theoreticalMean = 0.58
    val sampleMean      = bOr.mean(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Expected value of Bernoulli(0.3) || Bernoulli(0.4) should be approx 0.58. Got $sampleMean"
    )
  }

  // --- Correlation Tests (Dependent Variables) ---

  rngTest("Correlation: `b && b` should be perfectly correlated and equivalent to `b`") {
    val p     = 0.8
    val b     = Uncertain.bernoulli(p)
    val bAndB = b && b

    // The result should have the same probability as the original.
    val sampleMean = bAndB.mean(sampleCount)
    assert(
      cond = abs(sampleMean - p) < tolerance,
      clue = s"Expected value of correlated `b && b` where P(b)=0.8 should be approx 0.8. Got $sampleMean"
    )
  }

  rngTest("Correlation: `b || b` should be perfectly correlated and equivalent to `b`") {
    val p    = 0.25
    val b    = Uncertain.bernoulli(p)
    val bOrB = b || b

    // The result should have the same probability as the original.
    val sampleMean = bOrB.mean(sampleCount)
    assert(
      cond = abs(sampleMean - p) < tolerance,
      clue = s"Expected value of correlated `b || b` where P(b)=0.25 should be approx 0.25. Got $sampleMean"
    )
  }

  rngTest("Correlation: `b || !b` must always be true (Law of Excluded Middle)") {
    val p         = 0.4
    val b         = Uncertain.bernoulli(p)
    val tautology = b || !b

    // Every single sample must be true.
    val samples = tautology.take(1000)
    assert(
      cond = samples.forall(_ == true),
      clue = "`b || !b` must always evaluate to true due to correlation."
    )
    assertEquals(
      obtained = tautology.mean(1000),
      expected = 1.0,
      clue = "The expected value of `b || !b` must be exactly 1.0"
    )
  }

  rngTest("Correlation: `b && !b` must always be false (Principle of Contradiction)") {
    val p             = 0.9
    val b             = Uncertain.bernoulli(p)
    val contradiction = b && !b

    // Every single sample must be false.
    val samples = contradiction.take(1000)
    assert(
      cond = samples.forall(_ == false),
      clue = "`b && !b` must always evaluate to false due to correlation."
    )
    assertEquals(
      obtained = contradiction.mean(1000),
      expected = 0.0,
      clue = "The expected value of `b && !b` must be exactly 0.0"
    )
  }

  // --- Operations with Certain/Point Values ---

  rngTest("`b && Uncertain.always(true)` should be equivalent to `b`") {
    val p        = 0.65
    val b        = Uncertain.bernoulli(p)
    val bAndTrue = b && Uncertain.always(true)

    val sampleMean = bAndTrue.mean(sampleCount)
    assert(
      cond = abs(sampleMean - p) < tolerance,
      clue = s"Expected value of `b && true` where P(b)=0.65 should be approx 0.65. Got $sampleMean"
    )
  }

  rngTest("`b && Uncertain.always(false)` should always be false") {
    val p         = 0.7
    val b         = Uncertain.bernoulli(p)
    val bAndFalse = b && Uncertain.always(false)

    assertEquals(
      obtained = bAndFalse.mean(1000),
      expected = 0.0,
      clue = "The expected value of `b && false` must be 0.0"
    )
  }

  rngTest("`b || Uncertain.always(true)` should always be true") {
    val p       = 0.3
    val b       = Uncertain.bernoulli(p)
    val bOrTrue = b || Uncertain.always(true)

    assertEquals(
      obtained = bOrTrue.mean(1000),
      expected = 1.0,
      clue = "The expected value of `b || true` must be 1.0"
    )
  }

  rngTest("`b || Uncertain.always(false)` should be equivalent to `b`") {
    val p        = 0.55
    val b        = Uncertain.bernoulli(p)
    val bOrFalse = b || Uncertain.always(false)

    val sampleMean = bOrFalse.mean(sampleCount)
    assert(
      cond = abs(sampleMean - p) < tolerance,
      clue = s"Expected value of `b || false` where P(b)=0.55 should be approx 0.55. Got $sampleMean"
    )
  }

  // --- Complex/Chained Operations ---

  rngTest("De Morgan's Law: !(b1 && b2) should be equivalent to !b1 || !b2") {
    val p1 = 0.7
    val p2 = 0.2
    val b1 = Uncertain.bernoulli(p1)
    val b2 = Uncertain.bernoulli(p2)

    val leftSide  = !(b1 && b2)
    val rightSide = !b1 || !b2

    val leftMean  = leftSide.mean(sampleCount)
    val rightMean = rightSide.mean(sampleCount)

    // Theoretical mean = 1 - (p1 * p2) -> 1 - (0.7 * 0.2) = 0.86
    val theoreticalMean = 0.86
    assert(
      cond = abs(leftMean - theoreticalMean) < tolerance,
      clue = s"Both sides of De Morgan's law should approximate 0.86. Got L=$leftMean, R=$rightMean"
    )
    assert(
      cond = abs(rightMean - theoreticalMean) < tolerance,
      clue = s"Both sides of De Morgan's law should approximate 0.86. Got L=$leftMean, R=$rightMean"
    )
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

    val leftMean  = leftSide.mean(sampleCount)
    val rightMean = rightSide.mean(sampleCount)

    // Theoretical mean = (1 - p1) * (1 - p2) -> (1 - 0.1) * (1 - 0.4) = 0.54
    val theoreticalMean = 0.54
    assert(
      cond = abs(leftMean - theoreticalMean) < tolerance,
      clue = s"Both sides of De Morgan's law should approximate 0.54. Got L=$leftMean, R=$rightMean"
    )
    assert(
      cond = abs(rightMean - theoreticalMean) < tolerance,
      clue = s"Both sides of De Morgan's law should approximate 0.54. Got L=$leftMean, R=$rightMean"
    )
    assert(
      cond = abs(leftMean - rightMean) < tolerance,
      clue = "The expected values of both sides of the equation should be nearly identical."
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
    val sampleMean      = xor.mean(sampleCount)

    assert(
      cond = abs(sampleMean - theoreticalMean) < tolerance,
      clue = s"Expected value for XOR(0.6, 0.3) should be approx 0.54. Got $sampleMean"
    )
  }
}
