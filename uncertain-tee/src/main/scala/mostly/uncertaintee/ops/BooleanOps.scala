package mostly.uncertaintee.ops

import mostly.uncertaintee.*

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

    /** Tests if the probability of this being true exceeds a threshold. */
    def probability(
      exceeds: Double,
      alpha: Double = 0.05,
      beta: Double = 0.05,
      delta: Option[Double] = None,
      maxSamples: Int = 10_000
    ): Boolean = {
      require(exceeds >= 0 && exceeds <= 1, s"Threshold ($exceeds) must be between 0 and 1.")
      require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1.")
      require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1.")
      require(maxSamples > 0, "Max samples must be positive.")

      val effectSize = delta.getOrElse(math.max(0.01, 0.1 * (One - exceeds)))
      require(exceeds + effectSize <= One, s"Threshold + effect size too large: ${exceeds + effectSize}")

      val result = evaluateHypothesis(exceeds, alpha, beta, effectSize, maxSamples)
      result.decision
    }

    /** Shorthand for testing if something is "more likely than not" (> 50% chance). */
    def isProbable(alpha: Double = 0.05, beta: Double = 0.05): Boolean =
      probability(0.5, alpha, beta)

    /** Performs Sequential Probability Ratio Test for hypothesis testing. */
    def evaluateHypothesis(
      threshold: Double,
      alpha: Double,
      beta: Double,
      delta: Double,
      maxSamples: Int
    ): HypothesisResult = {
      require(threshold >= 0 && threshold <= 1, s"Threshold ($threshold) must be between 0 and 1.")
      require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1.")
      require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1.")
      require(delta > 0, s"Effect size delta ($delta) must be positive.")
      require(threshold + delta <= One, s"Threshold + delta (${threshold + delta}) must be ≤ 1.0")
      require(maxSamples > 0, "Maximum samples must be positive.")

      val p0 = threshold
      val p1 = threshold + delta

      val A = log(beta / (One - alpha))
      val B = log((One - beta) / alpha)

      var successes = 0
      var samples   = 0

      while (samples < maxSamples) {
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
            else n * log(One / p0)             // All trials were successes, as predicted
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
