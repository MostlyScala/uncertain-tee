package mostly.uncertaintee

import munit.FunSuite

import scala.math.{abs, pow, sqrt}

class PopulationStandardDeviationSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Statistical Properties Tests ---

  rngTest(
    "populationStandardDeviation for a normal distribution should approximate the theoretical standard deviation"
  ) {
    val mean       = 10.0
    val stdDev     = 2.5
    val normalDist = Uncertain.normal(mean, stdDev)

    // The theoretical standard deviation of the N(μ, σ²) distribution is σ.
    // See: https://en.wikipedia.org/wiki/Normal_distribution
    val theoreticalStdDev = stdDev
    val estimatedStdDev   = normalDist.populationStandardDeviation(sampleCount)

    val hint =
      s"Estimated population std dev ($estimatedStdDev) should be close to theoretical std dev ($theoreticalStdDev) for N($mean, $stdDev)."
    assert(abs(estimatedStdDev - theoreticalStdDev) < tolerance, hint)
  }

  rngTest(
    "populationStandardDeviation for a uniform distribution should approximate its theoretical standard deviation"
  ) {
    val min         = 0.0
    val max         = 12.0
    val uniformDist = Uncertain.uniform(min, max)

    // The theoretical variance of a U(a,b) distribution is (b-a)² / 12.
    // The standard deviation is the square root of the variance.
    // See: https://en.wikipedia.org/wiki/Continuous_uniform_distribution
    val theoreticalVariance = pow(max - min, 2) / 12.0
    val theoreticalStdDev   = sqrt(theoreticalVariance)
    val estimatedStdDev     = uniformDist.populationStandardDeviation(sampleCount)

    val hint =
      s"Estimated population std dev ($estimatedStdDev) should be close to theoretical std dev ($theoreticalStdDev) for U($min, $max)."
    assert(abs(estimatedStdDev - theoreticalStdDev) < tolerance, hint)
  }

  // --- Edge Case Tests ---

  rngTest("populationStandardDeviation for a point distribution should be exactly 0") {
    val pointDist = Uncertain.point(1337.0)

    // A distribution with no variation must have a standard deviation of 0.
    val stdDev = pointDist.populationStandardDeviation(1000)
    assertEquals(stdDev, 0.0, "Standard deviation of a constant value must be 0.")
  }

  // --- Input Validation Tests ---

  rngTest("populationStandardDeviation should throw IllegalArgumentException for non-positive sample count") {
    val dist = Uncertain.normal(0, 1)
    intercept[IllegalArgumentException] {
      dist.populationStandardDeviation(0)
    }
    intercept[IllegalArgumentException] {
      dist.populationStandardDeviation(-1)
    }
  }
}
