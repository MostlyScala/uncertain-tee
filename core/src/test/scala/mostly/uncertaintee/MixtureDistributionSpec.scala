package mostly.uncertaintee

import scala.math.{abs, pow, sqrt}
import scala.util.Random

class MixtureDistributionSpec extends RngSuite {

  private val sampleCount = 100_000 // Increased samples for more stable variance estimates
  private val tolerance   = 0.2

  // --- Statistical Properties Tests ---

  rngTest("Mixture mean should follow the Law of Total Expectation (weighted average of means)") {
    val c1 = Uncertain.normal(10.0, 2.0)  // E[c1] = 10.0
    val c2 = Uncertain.uniform(0.0, 10.0) // E[c2] = 5.0
    val c3 = Uncertain.point(100.0)       // E[c3] = 100.0
    val components = List(c1, c2, c3)
    val weights    = List(0.5, 0.3, 0.2)
    val mixture    = Uncertain.mixture(components, Some(weights))

    // The mean of a mixture is the weighted average of the component means.
    // E[X] = Σᵢ wᵢ * E[Xᵢ]
    // See: https://en.wikipedia.org/wiki/Law_of_total_expectation
    val theoreticalMean = (0.5 * 10.0) + (0.3 * 5.0) + (0.2 * 100.0) // 5 + 1.5 + 20 = 26.5
    val sampleMean      = mixture.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should be close to the theoretical mean ($theoreticalMean) of the mixture."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Mixture variance should follow the Law of Total Variance") {
    val mean1   = -10.0
    val stdDev1 = 3.0
    val var1    = pow(stdDev1, 2)
    val c1      = Uncertain.normal(mean1, stdDev1)

    val min2  = 0.0
    val max2  = 12.0
    val mean2 = (min2 + max2) / 2.0
    val var2  = pow(max2 - min2, 2) / 12.0
    val c2    = Uncertain.uniform(min2, max2)

    val weights = List(0.6, 0.4)
    val mixture = Uncertain.mixture(List(c1, c2), Some(weights))

    // The variance of a mixture is determined by the Law of Total Variance:
    // Var(X) = E[Var(X|Y)] + Var(E[X|Y])
    // Where Y is the choice of component.
    // E[Var(X|Y)] is the weighted average of component variances.
    // Var(E[X|Y]) is the variance of the component means.
    // See: https://en.wikipedia.org/wiki/Law_of_total_variance
    val mixtureMean = (weights.head * mean1) + (weights.last * mean2)

    val expectedInnerVariance = (weights.head * var1) + (weights.last * var2)
    val varianceOfMeans       = weights.head * pow(mean1 - mixtureMean, 2) + weights.last * pow(mean2 - mixtureMean, 2)

    val theoreticalVariance = expectedInnerVariance + varianceOfMeans
    val sampleVariance      = pow(mixture.standardDeviation(sampleCount), 2)

    val hint = s"Sample variance ($sampleVariance) should be close to the theoretical variance ($theoreticalVariance) of the mixture."
    // Variance estimates require more samples and a slightly larger tolerance.
    assert(abs(sampleVariance - theoreticalVariance) < tolerance * 5, hint)
  }

  rngTest("Mixture CDF should be the weighted average of component CDFs") {
    val c1      = Uncertain.normal(0.0, 1.0)
    val c2      = Uncertain.uniform(0.0, 1.0)
    val weights = List(0.7, 0.3)
    val mixture = Uncertain.mixture(List(c1, c2), Some(weights))

    val point          = 0.5
    // F_mix(x) = Σ w_i * F_i(x)
    val theoreticalCdf = weights.head * c1.cdf(point, 10000) + weights.last * c2.cdf(point, 10000)
    val sampleCdf      = mixture.cdf(point, sampleCount)

    val hint = s"Sample CDF ($sampleCdf) should be close to theoretical CDF ($theoreticalCdf)."
    assert(abs(sampleCdf - theoreticalCdf) < tolerance, hint)
  }

  rngTest("Mixture of symmetric components with symmetric weights should have zero skewness") {
    val c1      = Uncertain.normal(-10.0, 2.0)
    val c2      = Uncertain.normal(10.0, 2.0)
    val mixture = Uncertain.mixture(List(c1, c2)) // Equal weights

    val samples        = mixture.take(sampleCount)
    val sampleMean     = samples.sum / sampleCount
    val sampleStdDev   = sqrt(samples.map(x => pow(x - sampleMean, 2)).sum / (sampleCount - 1))
    val sampleSkewness = samples.map(x => pow((x - sampleMean) / sampleStdDev, 3)).sum / sampleCount

    val hint = s"Sample skewness ($sampleSkewness) for a symmetric mixture should be close to 0."
    assert(abs(sampleSkewness) < tolerance, hint)
  }

  rngTest("Mixture of two Bernoulli distributions results in a new Bernoulli distribution") {
    // A mixture of two point masses at 1 (true) and 0 (false) is, by definition, a Bernoulli trial.
    val success = Uncertain.point(true)
    val failure = Uncertain.point(false)

    val p       = 0.7
    val mixture = Uncertain.mixture(List(success, failure), Some(List(p, 1.0 - p)))

    // The mean of this mixture is E[X] = p * E[success] + (1-p) * E[failure] = p * 1 + (1-p) * 0 = p.
    // This is the definition of a Bernoulli(p) distribution.
    val sampleMean = mixture.expectedValue(sampleCount)
    val hint       = s"A mixture of points {0, 1} with weight p should be a Bernoulli(p). Sample mean ($sampleMean) should be close to p ($p)."
    assert(abs(sampleMean - p) < tolerance, hint)
  }

  // --- Edge Case and Special Value Tests ---

  rngTest("Mixture with a single component should be identical to the component itself") {
    val component = Uncertain.normal(50.0, 5.0)(using Random(42))
    val mixture   = Uncertain.mixture(
      List(Uncertain.normal(50.0, 5.0)(using Random(42)))
    )

    assertEquals(mixture.expectedValue(10000), component.expectedValue(10000), "Mean should be identical.")
    assertEquals(mixture.standardDeviation(10000), component.standardDeviation(10000), "Standard deviation should be identical.")
  }

  rngTest("Mixture with default equal weights should have a simple average mean") {
    val c1      = Uncertain.point(10.0)
    val c2      = Uncertain.point(20.0)
    val mixture = Uncertain.mixture(List(c1, c2)) // No weights provided, defaults to equal

    val theoreticalMean = (10.0 + 20.0) / 2.0
    val sampleMean      = mixture.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) for an unweighted mixture should be the average of component means ($theoreticalMean)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Mixture should correctly handle components with zero weight") {
    val c1      = Uncertain.point(10.0)
    val c2      = Uncertain.point(1000.0) // This component should be ignored
    val c3      = Uncertain.point(30.0)
    val weights = List(0.5, 0.0, 0.5)
    val mixture = Uncertain.mixture(List(c1, c2, c3), Some(weights))

    val theoreticalMean = (10.0 + 30.0) / 2.0
    val sampleMean      = mixture.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should ignore zero-weighted components and be ($theoreticalMean)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Nested mixtures should correctly propagate expectations") {
    val innerMixture   = Uncertain.mixture(List(Uncertain.point(10.0), Uncertain.point(20.0))) // E = 15
    val outerComponent = Uncertain.point(100.0)                                                // E = 100

    // Mixture of a mixture and a point
    val nestedMixture = Uncertain.mixture(List(innerMixture, outerComponent)) // E = (15 + 100) / 2 = 57.5

    val theoreticalMean = 57.5
    val sampleMean      = nestedMixture.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) of nested mixture should be ($theoreticalMean)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Mixture should work with non-numeric types like String") {
    val c1      = Uncertain.point("Heads")
    val c2      = Uncertain.point("Tails")
    val mixture = Uncertain.mixture(List(c1, c2), Some(List(0.8, 0.2)))

    val mode = mixture.mode(10000)
    assertEquals(mode, Some("Heads"), "The mode of the string mixture should be 'Heads' with p=0.8")
  }

  rngTest("Mixture should throw IllegalArgumentException for an empty component list") {
    intercept[IllegalArgumentException] {
      Uncertain.mixture(List.empty[Uncertain[Double]])
    }
  }

  rngTest("Mixture should throw IllegalArgumentException for mismatched weights and components") {
    intercept[IllegalArgumentException] {
      val components = List(Uncertain.point(1), Uncertain.point(2))
      val weights    = Some(List(0.5)) // Only one weight for two components
      Uncertain.mixture(components, weights)
    }
  }

  // --- Logical and Hypothesis Tests ---

  rngTest("Hypothesis testing on a mixture should reflect the combined probabilities") {
    val c1      = Uncertain.uniform(0.0, 4.0)     // P(c1 > 5) = 0.0
    val c2      = Uncertain.uniform(6.0, 10.0)    // P(c2 > 5) = 1.0
    val mixture = Uncertain.mixture(List(c1, c2)) // Equal weights

    // The theoretical probability P(mixture > 5) is:
    // P(mixture > 5) = 0.5 * P(c1 > 5) + 0.5 * P(c2 > 5)
    //                = 0.5 * 0.0       + 0.5 * 1.0
    //                = 0.5
    val hypothesis  = mixture > 5.0
    val probability = hypothesis.expectedValue(sampleCount)
    val hint        = s"The probability of the mixture being > 5.0 should be ~0.5, but was $probability"
    assert(abs(probability - 0.5) < tolerance, hint)
  }

  // --- Correlation Tests ---

  rngTest("Mixture should preserve correlation between its components") {
    val base = Uncertain.normal(10.0, 2.0) // E[base] = 10, Var[base] = 4

    // The two components are perfectly correlated, as they are derived from the same `base` uncertain value.
    val component1 = base
    val component2 = base + 100.0

    val mixture = Uncertain.mixture(List(component1, component2)) // Equal weights

    // --- Check Mean ---
    // E[mix] = 0.5 * E[base] + 0.5 * E[base + 100] = 0.5 * 10 + 0.5 * (10 + 100) = 5 + 55 = 60
    val theoreticalMean = 60.0
    val sampleMean      = mixture.expectedValue(sampleCount)
    assert(abs(sampleMean - theoreticalMean) < tolerance, s"Sample mean ($sampleMean) for correlated mixture should be $theoreticalMean.")

    // --- Check Variance using Law of Total Variance ---
    // Var(Y) = E[Var(Y|X)] + Var(E[Y|X]) where X is component choice {1, 2}
    // E[Y|X=1] = E[base] = 10
    // E[Y|X=2] = E[base+100] = 110
    // Var(E[Y|X]) = 0.5*(10-60)² + 0.5*(110-60)² = 0.5*2500 + 0.5*2500 = 2500
    //
    // Var(Y|X=1) = Var(base) = 4
    // Var(Y|X=2) = Var(base+100) = Var(base) = 4
    // E[Var(Y|X)] = 0.5 * 4 + 0.5 * 4 = 4
    //
    // Total Var = 4 + 2500 = 2504
    val theoreticalVariance = 2504.0
    val sampleVariance      = pow(mixture.standardDeviation(sampleCount), 2)
    assert(
      abs(sampleVariance - theoreticalVariance) < tolerance * 100,
      s"Sample variance ($sampleVariance) for correlated mixture should be $theoreticalVariance."
    )
  }
}
