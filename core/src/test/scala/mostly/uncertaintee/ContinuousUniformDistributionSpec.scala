package mostly.uncertaintee

import scala.math.{abs, pow, sqrt}
import mostly.uncertaintee.syntax.*

class ContinuousUniformDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Statistical Properties Tests ---

  rngTest("Uniform distribution's sample mean should approximate its theoretical mean (a+b)/2") {
    val min     = 10.0
    val max     = 20.0
    val uniform = Uncertain.uniform(min, max)

    // The mean of a U(a, b) distribution is (a + b) / 2.
    val theoreticalMean = (min + max) / 2.0
    val sampleMean      = uniform.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for U($min, $max)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Uniform distribution's sample variance should approximate its theoretical variance (b-a)²/12") {
    val min     = 0.0
    val max     = 12.0
    val uniform = Uncertain.uniform(min, max)

    // The variance of a U(a, b) distribution is (b - a)² / 12.
    val theoreticalVariance = pow(max - min, 2) / 12.0
    val sampleVariance      = pow(uniform.standardDeviation(sampleCount), 2)

    val hint =
      s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for U($min, $max)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Uniform distribution's sample skewness should be 0") {
    val uniform = Uncertain.uniform(-10.0, 10.0)
    val samples = uniform.take(sampleCount)

    // A uniform distribution is perfectly symmetric, so its theoretical skewness is 0.
    val theoreticalSkewness = 0.0

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) for a symmetric uniform distribution should be close to 0."
    assert(abs(sampleSkewness - theoreticalSkewness) < tolerance, hint)
  }

  rngTest("Uniform distribution's sample excess kurtosis should be -1.2") {
    val uniform = Uncertain.uniform(0.0, 1.0)
    val samples = uniform.take(sampleCount)

    // The theoretical excess kurtosis of a uniform distribution is -6/5 = -1.2.
    val theoreticalKurtosis = -1.2

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleKurtosis = (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / sampleCount) - 3.0

    val hint = s"Sample excess kurtosis ($sampleKurtosis) for a uniform distribution should be close to -1.2."
    // Kurtosis estimation is noisy.
    assert(abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2, hint)
  }

  // --- Edge Cases and Special Values ---

  rngTest("Uniform distribution where min equals max should be a point distribution") {
    val value      = 42.0
    val degenerate = Uncertain.uniform(value, value)
    val samples    = degenerate.take(1000)

    assert(samples.forall(_ == value), s"U($value, $value) must always produce $value.")
    assertEquals(degenerate.expectedValue(1000), value)
    assertEquals(degenerate.standardDeviation(1000), 0.0)
  }

  // --- Input Validation ---

  rngTest("Uniform distribution constructor should throw IllegalArgumentException if min > max") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(10.0, 5.0)
    }
  }
}
