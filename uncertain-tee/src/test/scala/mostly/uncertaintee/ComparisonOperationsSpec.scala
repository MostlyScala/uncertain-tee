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

class ComparisonOperationsSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Comparisons between Independent Distributions ---

  rngTest("greater than (>) should return the correct probability for two independent normal distributions") {
    val x = Uncertain.normal(5.0, 1.0) // N(μ=5, σ=1)
    val y = Uncertain.normal(4.0, 1.0) // N(μ=4, σ=1)

    // The probability P(X > Y) is equivalent to P(X - Y > 0).
    // The difference of two independent normal distributions is another normal distribution:
    // Z = X - Y ~ N(μ_x - μ_y, σ_x² + σ_y²) -> N(1, 2).
    // We need to find P(Z > 0) for Z ~ N(1, sqrt(2)). The z-score for 0 is (0-1)/sqrt(2) ≈ -0.707.
    // The standard normal CDF for -0.707 is ~0.24. So, P(Z > 0) = 1 - 0.24 = 0.76.
    val theoreticalProb = 0.7602 // More precise value
    val sampleProb      = (x > y).expectedValue(sampleCount)

    val hint = s"Sample probability ($sampleProb) of N(5,1) > N(4,1) should be close to theoretical ($theoreticalProb)."
    assert(abs(sampleProb - theoreticalProb) < tolerance, hint)
  }

  rngTest("less than (<) should return the correct probability for two independent uniform distributions") {
    val x = Uncertain.uniform(0.0, 2.0)
    val y = Uncertain.uniform(1.0, 3.0)

    // For X ~ U(0,2) and Y ~ U(1,3), we can calculate the exact probability P(X < Y).
    // The probability of the opposite, P(X >= Y), is the area of the triangle defined by
    // the intersection of the two ranges, which is 1/8.
    // Therefore, P(X < Y) = 1 - 1/8 = 0.875.
    val theoreticalProb = 0.875
    val sampleProb      = (x < y).expectedValue(sampleCount)

    val hint = s"Sample probability ($sampleProb) of U(0,2) < U(1,3) should be close to theoretical ($theoreticalProb)."
    assert(abs(sampleProb - theoreticalProb) < tolerance, hint)
  }

  rngTest("equality (===) between two independent discrete distributions should be correct") {
    val n = 5
    val p = 0.5
    val x = Uncertain.binomial(n, p)
    val y = Uncertain.binomial(n, p)

    // The probability P(X === Y) for two independent, identical distributions is the sum
    // of the squares of the probabilities of each outcome.
    // P(X===Y) = Σ [P(X=k)]² for k from 0 to n
    // For Binomial(5, 0.5), this is 0.24609375.
    val theoreticalProb = 0.24609375
    val sampleProb      = (x === y).expectedValue(sampleCount)

    val hint =
      s"Sample probability ($sampleProb) of B(5,0.5) === B(5,0.5) should be close to theoretical ($theoreticalProb)."
    assert(abs(sampleProb - theoreticalProb) < tolerance, hint)
  }

  // --- Operator Equivalence ---

  rngTest("greater than or equal to (>=) should be equivalent to !(<)") {
    val x = Uncertain.normal(10, 2)
    val y = Uncertain.normal(11, 2)

    val probGreaterOrEqual = (x >= y).expectedValue(sampleCount)
    val probNotLess        = (!(x < y)).expectedValue(sampleCount)

    assert(
      abs(probGreaterOrEqual - probNotLess) < tolerance,
      s"P(x>=y) ($probGreaterOrEqual) should be identical to P(!(x<y)) ($probNotLess)."
    )
  }

  rngTest("less than or equal to (<=) should be equivalent to !(>)") {
    val x = Uncertain.normal(10, 2)
    val y = Uncertain.normal(9, 2)

    val probLessOrEqual = (x <= y).expectedValue(sampleCount)
    val probNotGreater  = (!(x > y)).expectedValue(sampleCount)

    assert(
      abs(probLessOrEqual - probNotGreater) < tolerance,
      s"P(x<=y) ($probLessOrEqual) should be identical to P(!(x>y)) ($probNotGreater)."
    )
  }

  rngTest("inequality (!==) should be equivalent to !(===)") {
    val x = Uncertain.binomial(10, 0.5)
    val y = Uncertain.binomial(10, 0.5)

    val probNotEqual = (x !== y).expectedValue(sampleCount)
    val probNotSame  = (!(x === y)).expectedValue(sampleCount)

    assert(
      abs(probNotEqual - probNotSame) < tolerance,
      s"P(x!==y) ($probNotEqual) should be identical to P(!(x===y)) ($probNotSame)."
    )
  }

  // --- Correlation Tests ---

  rngTest("Correlation: x > x should always be false") {
    val x       = Uncertain.normal(100, 20)
    val result  = x > x
    val samples = result.take(sampleCount)
    assert(samples.forall(_ == false), "`x > x` must always be false.")
    assertEquals(result.expectedValue(sampleCount), 0.0)
  }

  rngTest("Correlation: x >= x should always be true") {
    val x       = Uncertain.normal(100, 20)
    val result  = x >= x
    val samples = result.take(sampleCount)
    assert(samples.forall(_ == true), "`x >= x` must always be true.")
    assertEquals(result.expectedValue(sampleCount), 1.0)
  }

  rngTest("Correlation: x === x should always be true") {
    val x       = Uncertain.exponential(1.0)
    val result  = x === x
    val samples = result.take(sampleCount)
    assert(samples.forall(_ == true), "`x === x` must always be true.")
    assertEquals(result.expectedValue(sampleCount), 1.0)
  }
}
