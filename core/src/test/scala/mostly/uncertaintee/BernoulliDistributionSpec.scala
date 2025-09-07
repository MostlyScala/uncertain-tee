package mostly.uncertaintee

import munit.FunSuite

import scala.math.{abs, pow, sqrt}

class BernoulliDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  // Tolerance for comparing floating-point statistical properties.
  // With 100k samples, we expect to be reasonably close to the true value.
  private val tolerance   = 0.05

  // --- Statistical Properties Tests ---

  rngTest("Bernoulli distribution's sample mean should approximate its theoretical mean (p) using the .expectedValue method") {
    val p         = 0.7
    val bernoulli = Uncertain.bernoulli(p)

    // The mean (or expected value) of a Bernoulli distribution is p.
    // See: https://en.wikipedia.org/wiki/Expected_value
    val theoreticalMean = p
    val sampleMean      = bernoulli.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) using .expectedValue should be close to theoretical mean ($theoreticalMean) for Bernoulli(p=$p)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Bernoulli distribution's sample variance should approximate its theoretical variance (p * (1 - p))") {
    val p         = 0.3
    val bernoulli = Uncertain.bernoulli(p)

    // The variance measures the spread of the distribution.
    // For a Bernoulli distribution, the variance is p * (1 - p).
    // See: https://en.wikipedia.org/wiki/Variance
    val theoreticalVariance = p * (1.0 - p)
    // The library's `standardDeviation` returns sqrt(variance), so we square it.
    val sampleVariance      = pow(bernoulli.standardDeviation(sampleCount), 2)

    val hint = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance) for Bernoulli(p=$p)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  rngTest("Bernoulli distribution's sample skewness should approximate its theoretical skewness") {
    val p         = 0.2
    val bernoulli = Uncertain.bernoulli(p)
    val samples   = bernoulli.take(sampleCount).map(if (_) 1.0 else 0.0)

    // Skewness measures the asymmetry of the probability distribution.
    // For a Bernoulli distribution, skewness is (1 - 2p) / sqrt(p * (1 - p)).
    // See: https://en.wikipedia.org/wiki/Skewness
    val theoreticalSkewness = (1.0 - 2.0 * p) / sqrt(p * (1.0 - p))

    val mean           = samples.sum / sampleCount
    val stdDev         = sqrt(samples.map(x => pow(x - mean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - mean) / stdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) should be close to theoretical skewness ($theoreticalSkewness) for Bernoulli(p=$p)."
    assert(abs(sampleSkewness - theoreticalSkewness) < tolerance, hint)
  }

  rngTest("Bernoulli distribution's sample excess kurtosis should approximate its theoretical excess kurtosis") {
    val p         = 0.4
    val bernoulli = Uncertain.bernoulli(p)
    val samples   = bernoulli.take(sampleCount).map(if (_) 1.0 else 0.0)

    // Excess kurtosis measures the "tailedness" of the distribution compared to a normal distribution.
    // For a Bernoulli distribution, it is (1 - 6p(1 - p)) / (p(1 - p)).
    // See: https://en.wikipedia.org/wiki/Kurtosis
    val theoreticalKurtosis = (1.0 - 6.0 * p * (1.0 - p)) / (p * (1.0 - p))

    val mean           = samples.sum / sampleCount
    val stdDev         = sqrt(samples.map(x => pow(x - mean, 2)).sum / (sampleCount - 1))
    // The "- 3" at the end is to calculate *excess* kurtosis.
    val sampleKurtosis = (samples.map(x => pow((x - mean) / stdDev, 4)).sum / sampleCount) - 3.0

    val hint = s"Sample excess kurtosis ($sampleKurtosis) should be close to theoretical kurtosis ($theoreticalKurtosis) for Bernoulli(p=$p)."
    // Kurtosis can have larger variance in estimation, so we use a slightly larger tolerance.
    assert(abs(sampleKurtosis - theoreticalKurtosis) < tolerance * 2, hint)
  }

  // --- Edge Case and Special Value Tests ---

  rngTest("Bernoulli(0) should always produce false") {
    val bernoulli = Uncertain.bernoulli(0.0)
    val samples   = bernoulli.take(1000) // Fewer samples needed for this deterministic test

    assert(samples.forall(_ == false), "Bernoulli(0.0) must always be false.")
    assertEquals(bernoulli.expectedValue(1000), 0.0, "The expected value of Bernoulli(0.0) must be 0.0")
    assertEquals(pow(bernoulli.standardDeviation(1000), 2), 0.0, "The variance of Bernoulli(0.0) must be 0.0")
  }

  rngTest("Bernoulli(1) should always produce true") {
    val bernoulli = Uncertain.bernoulli(1.0)
    val samples   = bernoulli.take(1000)

    assert(samples.forall(_ == true), "Bernoulli(1.0) must always be true.")
    assertEquals(bernoulli.expectedValue(1000), 1.0, "The expected value of Bernoulli(1.0) must be 1.0")
    assertEquals(pow(bernoulli.standardDeviation(1000), 2), 0.0, "The variance of Bernoulli(1.0) must be 0.0")
  }

  rngTest("Bernoulli(0.5) should be symmetric and have a skewness of approximately 0") {
    val p         = 0.5
    val bernoulli = Uncertain.bernoulli(p)
    val samples   = bernoulli.take(sampleCount).map(if (_) 1.0 else 0.0)

    // For a symmetric distribution like Bernoulli(0.5), the third central moment (skewness) is 0.
    // See: https://en.wikipedia.org/wiki/Skewness
    val mean           = bernoulli.expectedValue(sampleCount)
    val stdDev         = bernoulli.standardDeviation(sampleCount)
    val sampleSkewness = samples.map(x => pow((x - mean) / stdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness for a symmetric Bernoulli(0.5) distribution should be close to 0. Got $sampleSkewness"
    assert(abs(sampleSkewness) < tolerance, hint)
  }

  // --- Logical Operations Tests ---

  rngTest("Logical NOT `unary_!` should invert the probability") {
    val p    = 0.8
    val b    = Uncertain.bernoulli(p)
    val notB = !b

    // If P(b=true) = p, then P(!b=true) = 1 - p.
    // The expected value of !b should therefore be 1 - p.
    val theoreticalMean = 1.0 - p
    val sampleMean      = notB.expectedValue(sampleCount)

    val hint = s"Expected value of !Bernoulli($p) should be approx ${1.0 - p}. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Logical AND `&&` of independent variables should yield product of probabilities") {
    val p1 = 0.6
    val p2 = 0.5
    val b1 = Uncertain.bernoulli(p1)
    val b2 = Uncertain.bernoulli(p2)

    val bAnd = b1 && b2

    // For independent events, P(A and B) = P(A) * P(B).
    // The expected value of the resulting distribution is p1 * p2.
    // See: https://en.wikipedia.org/wiki/And_gate#Probability
    val theoreticalMean = p1 * p2
    val sampleMean      = bAnd.expectedValue(sampleCount)

    val hint = s"Expected value of Bernoulli($p1) && Bernoulli($p2) should be approx ${p1 * p2}. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Logical OR `||` of independent variables should follow inclusion-exclusion principle") {
    val p1 = 0.25
    val p2 = 0.4
    val b1 = Uncertain.bernoulli(p1)
    val b2 = Uncertain.bernoulli(p2)

    val bOr = b1 || b2

    // For independent events, P(A or B) = P(A) + P(B) - P(A and B) = p1 + p2 - (p1 * p2).
    // This is the Principle of Inclusion-Exclusion for probability.
    // See: https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle#In_probability
    val theoreticalMean = p1 + p2 - (p1 * p2)
    val sampleMean      = bOr.expectedValue(sampleCount)

    val hint = s"Expected value of Bernoulli($p1) || Bernoulli($p2) should be approx $theoreticalMean. Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  // --- Correlation Tests (Crucial for `Uncertain`'s core logic) ---

  rngTest("Correlation: an uncertain value should be perfectly correlated with itself (b && b === b)") {
    val p = 0.75
    val b = Uncertain.bernoulli(p)

    // Due to the computation graph, `b` should be sampled only once per evaluation.
    // Therefore, `b && b` is identical to `b`.
    val bAndB = b && b

    val theoreticalMean = p
    val sampleMean      = bAndB.expectedValue(sampleCount)

    val hint = s"Expected value of a correlated `b && b` should be p ($p). Got $sampleMean"
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Correlation: `b || !b` should always be true (Law of Excluded Middle)") {
    val p = 0.6
    val b = Uncertain.bernoulli(p)

    // The same sample for `b` is used on both sides of the `||`.
    // The expression is either `true || false` or `false || true`, both of which are `true`.
    // This tests the Law of the Excluded Middle in a probabilistic context.
    // See: https://en.wikipedia.org/wiki/Law_of_excluded_middle
    val tautology = b || !b

    val samples = tautology.take(1000)
    assert(samples.forall(_ == true), "`b || !b` must always evaluate to true due to correlation.")
    assertEquals(tautology.expectedValue(1000), 1.0, "The expected value of `b || !b` must be exactly 1.0")
  }

  rngTest("Correlation: `b && !b` should always be false (Principle of Non-Contradiction)") {
    val p = 0.3
    val b = Uncertain.bernoulli(p)

    // The same sample for `b` is used on both sides of the `&&`.
    // The expression is either `true && false` or `false && true`, both of which are `false`.
    // This tests the Principle of Non-Contradiction.
    // See: https://en.wikipedia.org/wiki/Law_of_non-contradiction
    val contradiction = b && !b

    val samples = contradiction.take(1000)
    assert(samples.forall(_ == false), "`b && !b` must always evaluate to false due to correlation.")
    assertEquals(contradiction.expectedValue(1000), 0.0, "The expected value of `b && !b` must be exactly 0.0")
  }
}
