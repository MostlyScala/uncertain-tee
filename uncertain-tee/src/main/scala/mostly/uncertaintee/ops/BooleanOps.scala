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

import scala.math.*

/** {{{
  *    import mostly.uncertaintee.syntax.boolean.*
  *    // or just import all the syntax (recommended)
  *    import mostly.uncertaintee.syntax.*
  * }}}
  */
trait BooleanOps {

  /** Boolean operations and statistical testing for uncertain Boolean values. */
  extension (lhs: Uncertain[Boolean]) {

    /** Logical NOT operation. */
    def unary_! : Uncertain[Boolean] = lhs.map(!_)

    /** Logical AND between two uncertain booleans. */
    def &&(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
      lhsSample <- lhs
      rhsSample <- rhs
    } yield lhsSample && rhsSample

    /** Logical OR between two uncertain booleans. */
    def ||(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
      lhsSample <- lhs
      rhsSample <- rhs
    } yield lhsSample || rhsSample

    /** Converts an uncertain Boolean to an uncertain integer (0 or 1).
      *
      * Mathematically, this is the **indicator function** (also called the **characteristic function**) of the event defined by the Boolean:
      *
      * {{{
      *   1_P =
      *     1, if the condition P is true
      *     0, if the condition P is false
      * }}}
      *
      * In this context, the uncertain Boolean is treated as a random variable, and `indicator` converts it into a numeric form suitable for computing means, probabilities, or
      * other statistical operations.
      *
      * See also: https://en.wikipedia.org/wiki/Indicator_function
      */

    def indicator: Uncertain[Int] = lhs.map {
      case true  => 1
      case false => 0
    }

    /** Estimates the probability that the uncertain Boolean is true.
      *
      * Formally, if X is the indicator random variable of the Boolean:
      * {{{
      *   P(X = 1) = E[X] = mean of indicator samples
      * }}}
      *
      * @param sampleCount
      *   the number of samples to approximate the probability
      * @return
      *   estimated probability of the Boolean being true
      *
      * See also: https://en.wikipedia.org/wiki/Indicator_function
      */
    def probability(sampleCount: Int): Double =
      lhs.indicator.mean(sampleCount)

    /** Performs a hypothesis test to determine whether the probability of this uncertain Boolean being true exceeds a given threshold.
      *
      * Uses a sequential probability ratio test (SPRT) or normal approximation.
      *
      * Mathematically, testing H0: p ≤ threshold versus H1: p > threshold, where p = P(Boolean = true), the method returns `true` if H1 is accepted.
      *
      * @param exceeds
      *   threshold probability to test against
      * @param alpha
      *   Type I error rate
      * @param beta
      *   Type II error rate
      * @param delta
      *   optional minimum effect size
      * @param sampleCount
      *   maximum number of samples to consider
      * @return
      *   `true` if the estimated probability exceeds the threshold with statistical confidence
      *
      * See also: https://en.wikipedia.org/wiki/Sequential_probability_ratio_test
      */
    def probabilityExceeds(
      exceeds: Double,
      alpha: Double = 0.05,
      beta: Double = 0.05,
      delta: Option[Double] = None,
      sampleCount: Int
    ): Boolean = {
      require(exceeds >= 0 && exceeds <= 1, s"Threshold ($exceeds) must be between 0 and 1.")
      require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1.")
      require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1.")
      require(sampleCount > 0, "Max samples must be positive.")

      val effectSize = delta.getOrElse(math.max(0.01, 0.1 * (One - exceeds)))
      require(exceeds + effectSize <= One, s"Threshold + effect size too large: ${exceeds + effectSize}")

      val result = evaluateHypothesis(exceeds, alpha, beta, effectSize, sampleCount)
      result.decision
    }

    /** Shorthand for testing if an uncertain Boolean is "more likely than not" (i.e., probability > 0.5).
      *
      * @param alpha
      *   Type I error rate
      * @param beta
      *   Type II error rate
      * @param sampleCount
      *   maximum number of samples to consider
      * @return
      *   `true` if estimated probability > 0.5 with statistical confidence
      */
    def isProbable(alpha: Double = 0.05, beta: Double = 0.05, sampleCount: Int): Boolean =
      probabilityExceeds(
        exceeds = 0.5,
        alpha = alpha,
        beta = beta,
        sampleCount = sampleCount
      )

    /** Performs Sequential Probability Ratio Test for hypothesis testing. */
    def evaluateHypothesis(
      threshold: Double,
      alpha: Double,
      beta: Double,
      delta: Double,
      sampleCount: Int
    ): HypothesisResult = {
      require(threshold >= 0 && threshold <= 1, s"Threshold ($threshold) must be between 0 and 1.")
      require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1.")
      require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1.")
      require(delta > 0, s"Effect size delta ($delta) must be positive.")
      require(threshold + delta <= One, s"Threshold + delta (${threshold + delta}) must be ≤ 1.0")
      require(sampleCount > 0, "Maximum samples must be positive.")

      val p0 = threshold
      val p1 = threshold + delta

      val A = log(beta / (One - alpha))
      val B = log((One - beta) / alpha)

      var successes = 0
      var samples   = 0

      while (samples < sampleCount) {
        val sample = lhs.sample()
        if (sample) successes += 1
        samples += 1

        val x = successes
        val n = samples

        val llr = if (p0 > 0 && p0 < 1 && p1 > 0 && p1 < 1) {
          x * log(p1 / p0) + (n - x) * log((One - p1) / (One - p0))
        } else {
          if (p0 == Zero) if (x > 0) Double.PositiveInfinity else (n - x) * log(One - p1)
          else if (p1 == One) {
            if (x < n) Double.NegativeInfinity // A success is impossible, so reject H1
            else n * log(One / p0) // All trials were successes, as predicted
          } else if (p1 == Zero) if (x > 0) Double.NegativeInfinity else (n - x) * log(One - p0)
          else if (x < n) Double.PositiveInfinity
          else x * log(One / p0)
        }

        if (llr <= A) {
          return HypothesisResult(false, successes.toDouble / samples, One - alpha, samples)
        } else if (llr >= B) {
          return HypothesisResult(true, successes.toDouble / samples, One - alpha, samples)
        }
      }

      val pHat = successes.toDouble / samples
      val se   = sqrt(pHat * (1.0 - pHat) / samples)

      if (se > 0) {
        val z         = (pHat - threshold) / se
        val criticalZ = approximateNormalQuantile(1.0 - alpha)
        HypothesisResult(z > criticalZ, pHat, 1.0 - alpha, samples)
      } else {
        HypothesisResult(pHat > threshold, pHat, 1.0 - alpha, samples)
      }
    }

    /** Internal: Approximates standard normal quantiles using Beasley-Springer-Moro algorithm. */
    private def approximateNormalQuantile(p: Double): Double = {
      require(p > 0 && p < 1, "Probability must be between 0 and 1")

      if (p < 0.5) {
        -approximateNormalQuantile(1.0 - p)
      } else {
        val t  = sqrt(-2.0 * log(1.0 - p))
        val c0 = 2.515517; val c1 = 0.802853; val c2 = 0.010328
        val d1 = 1.432788; val d2 = 0.189269; val d3 = 0.001308
        t - (c0 + c1 * t + c2 * t * t) / (1.0 + d1 * t + d2 * t * t + d3 * t * t * t)
      }
    }
  }
}
