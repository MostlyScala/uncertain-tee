package mostly.uncertaintee

import scala.math.{abs, pow, sqrt}

class UniformDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  // Tolerance for comparing floating-point statistical properties.
  // With 100k samples, we expect statistical estimates to be reasonably close to their
  // true theoretical values.
  private val tolerance   = 0.05

  // --- Statistical Properties Tests ---

  rngTest("Uniform distribution's sample mean should approximate its theoretical mean (a+b)/2") {
    val min     = 10.0
    val max     = 20.0
    val uniform = Uncertain.uniform(min, max)

    // The mean (or expected value) of a Uniform(a, b) distribution is (a + b) / 2.
    // See: https://en.wikipedia.org/wiki/Continuous_uniform_distribution#Moments_and_other_parameters
    val theoreticalMean = (min + max) / 2.0
    val sampleMean      = uniform.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should be close to theoretical mean ($theoreticalMean) for Uniform(a=$min, b=$max)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Uniform distribution's sample variance should approximate its theoretical variance (b-a)²/12") {
    val min     = -5.0
    val max     = 5.0
    val uniform = Uncertain.uniform(min, max)

    // The variance of a Uniform(a, b) distribution is (b - a)² / 12.
    val theoreticalVariance = pow(max - min, 2) / 12.0
    val sampleVariance      = pow(uniform.standardDeviation(sampleCount), 2)

    val hint = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Uniform(a=$min, b=$max)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Uniform distribution's sample skewness should be approximately 0") {
    val min     = 0.0
    val max     = 100.0
    val uniform = Uncertain.uniform(min, max)
    val samples = uniform.take(sampleCount)

    // The theoretical skewness for a uniform distribution is 0, as it is perfectly symmetric.
    val theoreticalSkewness = 0.0

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) for a symmetric Uniform distribution should be close to 0."
    assert(abs(sampleSkewness - theoreticalSkewness) < tolerance, hint)
  }

  rngTest("Uniform distribution's sample excess kurtosis should be approximately -1.2") {
    val min     = 0.0
    val max     = 1.0
    val uniform = Uncertain.uniform(min, max)
    val samples = uniform.take(sampleCount)

    // The excess kurtosis of a continuous uniform distribution is -6/5 = -1.2.
    // See: https://en.wikipedia.org/wiki/Continuous_uniform_distribution#Moments_and_other_parameters
    val theoreticalKurtosis = -1.2

    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleKurtosis = (samples.map(x => pow((x - sampleMean) / sampleStdDev, 4)).sum / sampleCount) - 3.0

    val hint = s"Sample excess kurtosis ($sampleKurtosis) should be close to theoretical (-1.2)."
    // Kurtosis estimation has high variance, so a larger tolerance is justified.
    assert(abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2, hint)
  }

  // --- Statistical Functions ---

  rngTest("CDF of a uniform distribution should be linear within its bounds") {
    val min     = 0.0
    val max     = 10.0
    val uniform = Uncertain.uniform(min, max)

    // The CDF of U(a, b) is F(x) = (x - a) / (b - a) for x in [a, b].
    // See: https://en.wikipedia.org/wiki/Continuous_uniform_distribution#Cumulative_distribution_function
    val midpoint      = min + (max - min) / 2.0 // 5.0
    val cdfAtMidpoint = uniform.cdf(midpoint, sampleCount)
    assert(abs(cdfAtMidpoint - 0.5) < tolerance, s"CDF at midpoint should be ~0.5, but was $cdfAtMidpoint")

    val quarterPoint      = min + (max - min) / 4.0 // 2.5
    val cdfAtQuarterPoint = uniform.cdf(quarterPoint, sampleCount)
    assert(abs(cdfAtQuarterPoint - 0.25) < tolerance, s"CDF at quarter point should be ~0.25, but was $cdfAtQuarterPoint")

    // CDF should be 0 below the minimum and 1 above the maximum.
    val cdfBelowMin = uniform.cdf(min - 1.0, sampleCount)
    assertEquals(cdfBelowMin, 0.0, "CDF below min should be exactly 0.0")

    val cdfAboveMax = uniform.cdf(max + 1.0, sampleCount)
    assertEquals(cdfAboveMax, 1.0, "CDF above max should be exactly 1.0")
  }

  // --- Edge Case and Special Value Tests ---

  rngTest("Uniform distribution with min == max should produce a constant value") {
    val value             = 42.0
    val degenerateUniform = Uncertain.uniform(value, value)
    val samples           = degenerateUniform.take(sampleCount)

    assert(samples.forall(_ == value), s"Uniform(a=$value, b=$value) must always produce the value $value.")
    assertEquals(degenerateUniform.expectedValue(sampleCount), value, s"The expected value of Uniform($value, $value) must be exactly $value.")
    assertEquals(degenerateUniform.standardDeviation(sampleCount), 0.0, s"The standard deviation of Uniform($value, $value) must be 0.0.")
  }

  // --- Arithmetic Operations Tests ---

  rngTest("Sum of two independent Uniform distributions should have correct mean and variance") {
    val u1  = Uncertain.uniform(0.0, 2.0) // Mean = 1.0, Var = (2-0)²/12 = 4/12
    val u2  = Uncertain.uniform(5.0, 9.0) // Mean = 7.0, Var = (9-5)²/12 = 16/12
    val sum = u1 + u2

    // For independent variables X and Y: E[X+Y] = E[X]+E[Y] and Var(X+Y) = Var(X)+Var(Y).
    val expectedMean     = 1.0 + 7.0                    // 8.0
    val expectedVariance = (4.0 / 12.0) + (16.0 / 12.0) // 20.0 / 12.0 ≈ 1.667
    val expectedStdDev   = sqrt(expectedVariance)

    val sampleMean   = sum.expectedValue(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(abs(sampleMean - expectedMean) < tolerance, s"Mean of sum should be $expectedMean, but was $sampleMean.")
    assert(abs(sampleStdDev - expectedStdDev) < tolerance, s"StdDev of sum should be $expectedStdDev, but was $sampleStdDev.")
  }

  // --- Comparison and Hypothesis Tests ---

  rngTest("Comparison `uniform > midpoint` should yield a Bernoulli distribution with p=0.5") {
    val min      = 0.0
    val max      = 100.0
    val uniform  = Uncertain.uniform(min, max)
    val midpoint = (min + max) / 2.0

    // Due to symmetry, the probability of a sample being greater than the midpoint is 0.5.
    val isGreaterThanMidpoint = uniform > midpoint
    val sampleProbability     = isGreaterThanMidpoint.expectedValue(sampleCount)
    assert(abs(sampleProbability - 0.5) < tolerance, s"P(uniform > midpoint) should be ~0.5, but was $sampleProbability")
  }

  rngTest("Hypothesis test `probability(exceeds=...)` should correctly identify probabilities") {
    val uniform = Uncertain.uniform(0.0, 10.0)

    // The probability of a value being greater than 8 is (10-8)/(10-0) = 0.2.
    val isAbove8 = uniform > 8.0 // Theoretical P(true) is 0.2

    // Test a hypothesis that should be accepted.
    assert(isAbove8.probability(exceeds = 0.15), "Should be confident that P(uniform > 8) exceeds 15%")

    // Test a hypothesis that should be rejected.
    assert(!isAbove8.probability(exceeds = 0.25), "Should not be confident that P(uniform > 8) exceeds 25%")
  }

  // --- Correlation Tests (Crucial for `Uncertain`'s core logic) ---

  rngTest("Correlation: an uncertain value subtracted from itself (x - x) should be exactly 0") {
    val x          = Uncertain.uniform(-10.0, 10.0)
    val difference = x - x
    val samples    = difference.take(sampleCount)
    assert(samples.forall(_ == Numeric[Double].zero), "`x - x` must always evaluate to 0.0 due to correlation.")
    assertEquals(difference.expectedValue(sampleCount), Numeric[Double].zero)
    assertEquals(difference.standardDeviation(sampleCount), Numeric[Double].zero)
  }

  rngTest("Correlation: `x + x` should be equivalent to `2 * x`") {
    val x   = Uncertain.uniform(5.0, 10.0) // Mean=7.5, Var=(5²)/12
    val sum = x + x

    // `2 * x` is Uniform(10, 20).
    // E[2x] = 2 * E[x] = 2 * 7.5 = 15.0
    // Var(2x) = 2² * Var(x) = 4 * (25/12)
    val expectedMean   = 2 * 7.5
    val expectedStdDev = 2 * sqrt(pow(10.0 - 5.0, 2) / 12.0)

    val sampleMean   = sum.expectedValue(sampleCount)
    val sampleStdDev = sum.standardDeviation(sampleCount)

    assert(abs(sampleMean - expectedMean) < tolerance, s"Mean of correlated sum `x+x` should be $expectedMean, but was $sampleMean.")
    assert(abs(sampleStdDev - expectedStdDev) < tolerance, s"StdDev of correlated sum `x+x` should be $expectedStdDev, but was $sampleStdDev.")
  }

  rngTest("Correlation: an uncertain value divided by itself (x / x) should be exactly 1") {
    // We choose bounds far from zero to avoid division by zero.
    val x       = Uncertain.uniform(50.0, 100.0)
    val ratio   = x / x
    val samples = ratio.take(sampleCount)
    assert(samples.forall(v => abs(v - 1.0) < 1e-9), "`x / x` must always evaluate to 1.0 due to correlation.")
    assertEquals(ratio.expectedValue(sampleCount), 1.0)
    assertEquals(ratio.standardDeviation(sampleCount), 0.0)
  }
}
