package mostly.uncertaintee

import munit.FunSuite

import scala.math.*

class KumaraswamyDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  /** Provides numerically stable implementations of log-gamma and beta functions required for calculating theoretical
    * moments of the Kumaraswamy distribution.
    */
  private object MathHelpers {
    // Lanczos approximation for the log-gamma function.
    private val g = 7.0
    private val p = Array(
      0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059,
      12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    )

    def logGamma(z: Double): Double =
      if (z < 0.5) {
        log(Pi) - log(sin(Pi * z)) - logGamma(1.0 - z)
      } else {
        val z_minus_1 = z - 1
        var x         = p(0)
        for (i <- 1 until p.length)
          x += p(i) / (z_minus_1 + i)
        val t         = z_minus_1 + g + 0.5
        (z_minus_1 + 0.5) * log(t) - t + log(sqrt(2 * Pi) * x)
      }

    def logBeta(x: Double, y: Double): Double =
      logGamma(x) + logGamma(y) - logGamma(x + y)

    def beta(x: Double, y: Double): Double =
      exp(logBeta(x, y))
  }

  // --- Sanity and Range Tests ---

  rngTest("Kumaraswamy distribution samples should always be within the [0, 1] range") {
    val a           = 2.0
    val b           = 5.0
    val kumaraswamy = Uncertain.kumaraswamy(a, b)
    val samples     = kumaraswamy.take(sampleCount)

    assert(samples.forall(x => x >= 0.0 && x <= 1.0), "All samples must be in the [0, 1] interval.")
  }

  // --- Statistical Properties Tests ---

  rngTest("Kumaraswamy distribution's sample mean should approximate its theoretical mean") {
    val a           = 2.0
    val b           = 5.0
    val kumaraswamy = Uncertain.kumaraswamy(a, b)

    // The mean of a Kumaraswamy(a, b) distribution is b * B(1 + 1/a, b),
    // where B is the Beta function.
    // See: https://en.wikipedia.org/wiki/Kumaraswamy_distribution
    val theoreticalMean = b * MathHelpers.beta(1.0 + 1.0 / a, b)
    val sampleMean      = kumaraswamy.expectedValue(sampleCount)

    val hint =
      s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Kumaraswamy(a=$a, b=$b)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Kumaraswamy distribution's sample variance should approximate its theoretical variance") {
    val a           = 3.0
    val b           = 4.0
    val kumaraswamy = Uncertain.kumaraswamy(a, b)

    // The variance is E[X^2] - (E[X])^2.
    // E[X^k] = b * B(1 + k/a, b).
    val theoreticalMean     = b * MathHelpers.beta(1.0 + 1.0 / a, b)
    val secondMoment        = b * MathHelpers.beta(1.0 + 2.0 / a, b)
    val theoreticalVariance = secondMoment - pow(theoreticalMean, 2)

    val sampleVariance = pow(kumaraswamy.standardDeviation(sampleCount), 2)

    val hint =
      s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Kumaraswamy(a=$a, b=$b)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Kumaraswamy sample CDF should approximate the theoretical CDF") {
    val a           = 2.5
    val b           = 3.5
    val kumaraswamy = Uncertain.kumaraswamy(a, b)
    val x           = 0.4 // A point at which to evaluate the CDF

    // The CDF is F(x; a, b) = 1 - (1 - x^a)^b.
    val theoreticalCdf = 1.0 - pow(1.0 - pow(x, a), b)
    val sampleCdf      = kumaraswamy.cdf(x, sampleCount)

    val hint =
      s"Sample CDF at $x ($sampleCdf) should be close to theoretical CDF ($theoreticalCdf) for Kumaraswamy(a=$a, b=$b)."
    assert(abs(sampleCdf - theoreticalCdf) < tolerance, hint)
  }

  rngTest("Kumaraswamy sample distribution should be densest around the theoretical mode") {
    val a           = 3.0
    val b           = 4.0 // Both a > 1 and b > 1 for a unique mode inside (0, 1)
    val kumaraswamy = Uncertain.kumaraswamy(a, b)

    // The mode is ((a-1)/(ab-1))^(1/a).
    val theoreticalMode = pow((a - 1.0) / (a * b - 1.0), 1.0 / a)

    // We test the mode by checking that a wider interval around it contains more
    // samples than adjacent, narrower intervals. This is robust to statistical noise.
    val samples = kumaraswamy.take(sampleCount)
    val epsilon = 0.01

    // A wider central bin to represent the "peak region".
    val centerBin = samples.count(x => abs(x - theoreticalMode) < 1.5 * epsilon)

    // Side bins are further out and narrower, representing the "slopes".
    val leftBin  = samples.count(x => (x >= theoreticalMode - 3.5 * epsilon) && (x < theoreticalMode - 1.5 * epsilon))
    val rightBin = samples.count(x => (x > theoreticalMode + 1.5 * epsilon) && (x <= theoreticalMode + 3.5 * epsilon))

    val hint =
      s"The wider bin around the mode ($centerBin) should contain more samples than its neighbors (left: $leftBin, right: $rightBin)."
    assert(centerBin > leftBin && centerBin > rightBin, hint)
  }
  // --- Precondition and Edge Case Tests ---

  test("Kumaraswamy constructor should throw IllegalArgumentException for non-positive parameters") {
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(-1.0, 5.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(0.0, 5.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(2.0, -2.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(2.0, 0.0)
    }
  }

  rngTest("Kumaraswamy(1, 1) should be equivalent to a standard Uniform(0, 1) distribution") {
    val kumaraswamyAsUniform = Uncertain.kumaraswamy(1.0, 1.0)

    // Theoretical mean and variance of a standard uniform distribution.
    val uniformMean     = 0.5
    val uniformVariance = 1.0 / 12.0

    val sampleMean     = kumaraswamyAsUniform.expectedValue(sampleCount)
    val sampleVariance = pow(kumaraswamyAsUniform.standardDeviation(sampleCount), 2)

    val meanHint = s"Mean of Kumaraswamy(1,1) ($sampleMean) should approximate Uniform(0,1) mean ($uniformMean)."
    val varHint  =
      s"Variance of Kumaraswamy(1,1) ($sampleVariance) should approximate Uniform(0,1) variance ($uniformVariance)."

    assert(abs(sampleMean - uniformMean) < tolerance, meanHint)
    assert(abs(sampleVariance - uniformVariance) < tolerance, varHint)
  }

  rngTest("Kumaraswamy(a, 1) should match the power law distribution CDF of x^a") {
    val a        = 4.0
    val b        = 1.0
    val powerLaw = Uncertain.kumaraswamy(a, b)
    val x        = 0.7

    // When b=1, the CDF simplifies to F(x) = 1 - (1 - x^a)^1 = x^a.
    val theoreticalCdf = pow(x, a)
    val sampleCdf      = powerLaw.cdf(x, sampleCount)

    val hint =
      s"CDF of Kumaraswamy($a, 1) at $x ($sampleCdf) should be close to theoretical CDF of x^a ($theoreticalCdf)."
    assert(
      cond = abs(sampleCdf - theoreticalCdf) < tolerance,
      clue = hint
    )
  }
}
