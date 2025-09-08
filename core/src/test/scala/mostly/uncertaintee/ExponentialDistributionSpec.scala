package mostly.uncertaintee

import scala.math.{abs, pow, sqrt}

class ExponentialDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.05

  // --- Statistical Properties Tests ---

  rngTest("Exponential distribution's sample mean should approximate its theoretical mean (1/rate)") {
    val rate        = 2.0
    val exponential = Uncertain.exponential(rate)

    // The mean of an Exponential(λ) distribution is 1/λ.
    // See: https://en.wikipedia.org/wiki/Exponential_distribution
    val theoreticalMean = 1.0 / rate
    val sampleMean      = exponential.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Exp(rate=$rate)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Exponential distribution's sample variance should approximate its theoretical variance (1/rate²)") {
    val rate        = 1.5
    val exponential = Uncertain.exponential(rate)

    // The variance of an Exponential(λ) distribution is 1/λ².
    val theoreticalVariance = 1.0 / pow(rate, 2)
    val sampleVariance      = pow(exponential.standardDeviation(sampleCount), 2)

    val hint =
      s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Exp(rate=$rate)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Exponential distribution's sample skewness should be 2") {
    val rate        = 0.5
    val exponential = Uncertain.exponential(rate)
    val samples     = exponential.take(sampleCount)

    // The theoretical skewness of any exponential distribution is exactly 2.
    val theoreticalSkewness = 2.0

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) for an exponential distribution should be close to 2."
    // Skewness estimation can have higher variance.
    assert(abs(sampleSkewness - theoreticalSkewness) < tolerance * 2, hint)
  }

  rngTest("Exponential distribution's sample excess kurtosis should be 6") {
    // --- Understanding Kurtosis ---
    // Excess kurtosis measures the "tailedness" of a distribution compared to a normal distribution
    // (which has an excess kurtosis of 0). It tells us about the propensity for outliers.
    // - Positive kurtosis (Leptokurtic): Heavier tails and a sharper peak. Outliers are more likely.
    // - Negative kurtosis (Platykurtic): Lighter tails and a flatter peak. Outliers are less likely.
    //
    // An exponential distribution is highly skewed with a long right tail, resulting in a theoretical
    // excess kurtosis of exactly 6. This indicates a distribution with very heavy tails and a high
    // likelihood of producing extreme values (outliers) compared to a normal distribution.
    //

    // --- Testing Methodology ---
    // This test uses multiple smaller runs and averages the kurtosis to get a stable estimate,
    // avoiding the high variance of a single, large sample.
    val iterations          = 500
    val samplesPerIteration = 1000
    val rate                = 1.0
    val exponential         = Uncertain.exponential(rate)

    val kurtosisEstimates = (1 to iterations).map { _ =>
      val samples      = exponential.take(samplesPerIteration)
      val sampleMean   = samples.sum / samplesPerIteration
      val sampleStdDev = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (samplesPerIteration - 1))
      // Check for zero std dev to avoid division by zero in rare cases
      if (sampleStdDev > 0) {
        (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / samplesPerIteration) - 3.0
      } else {
        0.0 // Kurtosis is zero if all samples are identical
      }
    }

    val averageKurtosis = kurtosisEstimates.sum / iterations

    // The theoretical excess kurtosis of any exponential distribution is exactly 6.
    val theoreticalKurtosis = 6.0

    val hint = s"Average sample excess kurtosis ($averageKurtosis) over $iterations runs should be close to 6."
    // With averaging, we can use a much more reasonable tolerance.
    assert(abs(averageKurtosis - theoreticalKurtosis) < tolerance * 10, hint)
  }

  // --- Input Validation ---

  rngTest("Exponential distribution constructor should throw IllegalArgumentException for non-positive rate") {
    intercept[IllegalArgumentException] {
      Uncertain.exponential(0.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.exponential(-10.0)
    }
  }
}
