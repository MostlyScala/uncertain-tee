package mostly.uncertaintee

import munit.FunSuite

import scala.math.*

class RayleighDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  // --- Sanity and Range Tests ---

  rngTest("Rayleigh distribution samples should always be non-negative") {
    val scale    = 2.0
    val rayleigh = Uncertain.rayleigh(scale)
    val samples  = rayleigh.take(10_000)

    assert(samples.forall(_ >= 0.0), "All samples from a Rayleigh distribution must be non-negative.")
  }

  // --- Statistical Properties Tests ---

  rngTest("Rayleigh distribution's sample mean should approximate its theoretical mean") {
    val scale    = 3.5
    val rayleigh = Uncertain.rayleigh(scale)

    // The mean of a Rayleigh(σ) distribution is σ * sqrt(π / 2).
    // See: https://en.wikipedia.org/wiki/Rayleigh_distribution
    val theoreticalMean = scale * sqrt(Pi / 2.0)
    val sampleMean      = rayleigh.expectedValue(sampleCount)

    val hint =
      s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Rayleigh(σ=$scale)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Rayleigh distribution's sample variance should approximate its theoretical variance") {
    val scale    = 2.0
    val rayleigh = Uncertain.rayleigh(scale)

    // The variance of a Rayleigh(σ) distribution is (4 - π)/2 * σ^2.
    val theoreticalVariance = (4.0 - Pi) / 2.0 * pow(scale, 2)
    val sampleVariance      = pow(rayleigh.standardDeviation(sampleCount), 2)

    val hint =
      s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Rayleigh(σ=$scale)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Rayleigh sample CDF should approximate the theoretical CDF") {
    val scale    = 1.5
    val rayleigh = Uncertain.rayleigh(scale)
    val x        = 2.0 // A point at which to evaluate the CDF

    // The CDF is F(x; σ) = 1 - exp(-x^2 / (2σ^2)).
    val theoreticalCdf = 1.0 - exp(-pow(x, 2) / (2.0 * pow(scale, 2)))
    val sampleCdf      = rayleigh.cdf(x, sampleCount)

    val hint =
      s"Sample CDF at $x ($sampleCdf) should be close to theoretical CDF ($theoreticalCdf) for Rayleigh(σ=$scale)."
    assert(abs(sampleCdf - theoreticalCdf) < tolerance, hint)
  }

  rngTest("Rayleigh distribution's median should be correctly approximated") {
    val scale    = 4.0
    val rayleigh = Uncertain.rayleigh(scale)

    // The median is σ * sqrt(2 * ln(2)). At this point, the CDF should be 0.5.
    val theoreticalMedian = scale * sqrt(2.0 * log(2.0))
    val cdfAtMedian       = rayleigh.cdf(theoreticalMedian, sampleCount)

    val hint = s"The CDF at the theoretical median ($theoreticalMedian) should be close to 0.5. Got $cdfAtMedian."
    assert(abs(cdfAtMedian - 0.5) < tolerance, hint)
  }

  rngTest("Rayleigh sample distribution should be densest around the theoretical mode") {
    val scale    = 5.0
    val rayleigh = Uncertain.rayleigh(scale)

    // The mode is simply the scale parameter, σ.
    val theoreticalMode = scale

    // Using the robust testing strategy: a wider bin around the mode should contain
    // more samples than adjacent, narrower bins, protecting against statistical noise at the peak.
    val samples = rayleigh.take(sampleCount)
    val epsilon = 0.05 // A slightly larger epsilon suitable for this distribution's shape

    // A wider central bin representing the "peak region".
    val centerBin = samples.count(x => abs(x - theoreticalMode) < 1.5 * epsilon)

    // Side bins are further out and narrower, representing the "slopes".
    val leftBin  = samples.count(x => (x >= theoreticalMode - 3.5 * epsilon) && (x < theoreticalMode - 1.5 * epsilon))
    val rightBin = samples.count(x => (x > theoreticalMode + 1.5 * epsilon) && (x <= theoreticalMode + 3.5 * epsilon))

    val hint =
      s"The wider bin around the mode ($centerBin) should contain more samples than its neighbors (left: $leftBin, right: $rightBin)."
    assert(centerBin > leftBin && centerBin > rightBin, hint)
  }

  // --- Precondition and Edge Case Tests ---

  test("Rayleigh constructor should throw IllegalArgumentException for non-positive scale parameter") {
    intercept[IllegalArgumentException] {
      Uncertain.rayleigh(-1.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.rayleigh(0.0)
    }
  }
}
