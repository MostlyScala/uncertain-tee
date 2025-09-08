package mostly.uncertaintee

import scala.math.{abs, pow}

class MixtureDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance = 0.05

  // --- General Mixture Model Tests ---

  rngTest("Mixture distribution's sample mean should be the weighted average of component means") {
    // Setup components with known means
    val normal1 = Uncertain.normal(10.0, 2.0) // E[X₁] = 10
    val normal2 = Uncertain.normal(100.0, 5.0) // E[X₂] = 100
    val uniform1 = Uncertain.uniform(0.0, 10.0) // E[X₃] = 5

    // Setup a map of components to their weights
    val components = Map(
      normal1 -> 0.5, // w₁ = 0.5
      normal2 -> 0.2, // w₂ = 0.2
      uniform1 -> 0.3 // w₃ = 0.3
    )

    // The theoretical mean of a mixture distribution is the weighted average of the component means.
    // Formula: E[X] = Σ(wᵢ * E[Xᵢ])
    // See: https://en.wikipedia.org/wiki/Mixture_distribution#Moments
    val theoreticalMean = (0.5 * 10.0) + (0.2 * 100.0) + (0.3 * 5.0) // 5 + 20 + 1.5 = 26.5

    val mixture = Uncertain.mixture(components)
    val sampleMean = mixture.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) should be close to the theoretical weighted mean ($theoreticalMean)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("Mixture distribution's sample variance should follow the law of total variance") {
    // Setup components with known means and variances
    val comp1 = Uncertain.normal(10.0, 2.0) // μ₁=10, σ₁=2, Var(X₁)=4
    val comp2 = Uncertain.uniform(0.0, 12.0) // μ₂=6, Var(X₂)=(12-0)²/12=12

    val components = Map(
      comp1 -> 0.4, // w₁ = 0.4
      comp2 -> 0.6 // w₂ = 0.6
    )

    // The theoretical variance is derived from the law of total variance.
    // Formula: Var(X) = Σ(wᵢ * (σᵢ² + μᵢ²)) - μ², where μ is the overall mixture mean.
    val mean1 = 10.0
    val var1 = pow(2.0, 2)
    val mean2 = (0.0 + 12.0) / 2.0
    val var2 = pow(12.0 - 0.0, 2) / 12.0

    val mixtureMean = (0.4 * mean1) + (0.6 * mean2) // (0.4 * 10) + (0.6 * 6) = 7.6

    val theoreticalVariance = (0.4 * (var1 + pow(mean1, 2))) + (0.6 * (var2 + pow(mean2, 2))) - pow(mixtureMean, 2)
    // = (0.4 * (4 + 100)) + (0.6 * (12 + 36)) - 7.6²
    // = (0.4 * 104) + (0.6 * 48) - 57.76
    // = 41.6 + 28.8 - 57.76 = 12.64

    val mixture = Uncertain.mixture(components)
    val sampleVariance = pow(mixture.standardDeviation(sampleCount), 2)

    val hint = s"Sample variance ($sampleVariance) should be close to theoretical variance ($theoreticalVariance)."
    assert(abs(sampleVariance - theoreticalVariance) < tolerance, hint)
  }

  // --- Equal Mixture Model Tests ---

  rngTest("EqualMixture's sample mean should be the simple average of component means") {
    val comp1 = Uncertain.normal(10.0, 1.0)
    val comp2 = Uncertain.normal(20.0, 1.0)
    val comp3 = Uncertain.normal(30.0, 1.0)

    val components = List(comp1, comp2, comp3)

    // For an equal mixture, the mean is the simple arithmetic average of component means.
    val theoreticalMean = (10.0 + 20.0 + 30.0) / 3.0 // 20.0

    val mixture = Uncertain.equalMixture(components)
    val sampleMean = mixture.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) for equal mixture should be close to theoretical mean ($theoreticalMean)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }

  rngTest("EqualMixture's sample variance should follow the law of total variance with equal weights") {
    val comp1 = Uncertain.normal(0.0, 2.0) // μ₁=0, Var₁=4
    val comp2 = Uncertain.normal(10.0, 3.0) // μ₂=10, Var₂=9

    val components = List(comp1, comp2)
    val w = 0.5 // Equal weight for two components

    val mean1 = 0.0
    val var1 = 4.0
    val mean2 = 10.0
    val var2 = 9.0

    // Theoretical Mean: (0 + 10) / 2 = 5.0
    val mixtureMean = (mean1 + mean2) / 2.0

    // Var(X) = Σ(wᵢ * (σᵢ² + μᵢ²)) - μ²
    val theoreticalVariance = (w * (var1 + pow(mean1, 2))) + (w * (var2 + pow(mean2, 2))) - pow(mixtureMean, 2)
    // = 0.5 * (4 + 0) + 0.5 * (9 + 100) - 5²
    // = 2 + 54.5 - 25 = 31.5

    val mixture = Uncertain.equalMixture(components)
    val sampleVariance = pow(mixture.standardDeviation(sampleCount), 2)

    val hint = s"Sample variance ($sampleVariance) for equal mixture should be close to theoretical variance ($theoreticalVariance)."
    // Variance estimation can be noisy, so we use a slightly larger tolerance.
    assert(abs(sampleVariance - theoreticalVariance) < tolerance * 2, hint)
  }

  // --- Edge Cases and Special Value Tests ---

  rngTest("Mixture with a single component should be identical to that component") {
    val component = Uncertain.normal(50.0, 5.0)
    val mixture = Uncertain.mixture(Map(component -> 1.0))

    val meanDiff = abs(mixture.expectedValue(sampleCount) - component.expectedValue(sampleCount))
    val stdDevDiff = abs(mixture.standardDeviation(sampleCount) - component.standardDeviation(sampleCount))

    assert(meanDiff < tolerance, s"Mean of single-component mixture should match component mean. Diff was $meanDiff.")
    assert(stdDevDiff < tolerance, s"StdDev of single-component mixture should match component StdDev. Diff was $stdDevDiff.")
  }

  rngTest("Mixture components with zero weight should not contribute to the distribution") {
    val comp1 = Uncertain.normal(10.0, 1.0) // The one that should be chosen
    val comp2 = Uncertain.normal(-100.0, 1.0) // The one that should be ignored

    val components = Map(
      comp1 -> 1.0,
      comp2 -> 0.0
    )

    val mixture = Uncertain.mixture(components)

    val meanDiff = abs(mixture.expectedValue(sampleCount) - comp1.expectedValue(sampleCount))
    val stdDevDiff = abs(mixture.standardDeviation(sampleCount) - comp1.standardDeviation(sampleCount))

    assert(meanDiff < tolerance, s"Mean of mixture with a zero-weight component should match the non-zero component's mean. Diff was $meanDiff.")
    assert(stdDevDiff < tolerance, s"StdDev should also match. Diff was $stdDevDiff.")
  }


  rngTest("Mixture should throw an exception if the components map is empty") {
    intercept[IllegalArgumentException] {
      Uncertain.mixture(Map.empty[Uncertain[Double], Double])
    }
  }

  // --- Correlation Tests ---

  rngTest("Correlation: a mixture subtracted from itself (m - m) should be exactly 0") {
    val x = Uncertain.normal(10.0, 1.0)
    val y = Uncertain.uniform(-100.0, 100.0)

    val mixture = Uncertain.equalMixture(List(x, y))
    val difference = mixture - mixture

    val samples = difference.take(1000)
    assert(samples.forall(_ == 0.0), "`m - m` must always evaluate to 0.0 due to correlation.")
    assertEquals(difference.expectedValue(1000), 0.0)
    assertEquals(difference.standardDeviation(1000), 0.0)
  }

  rngTest("Correlation: complex expression `m - x` should be correct when m is a mixture containing x") {
    val x = Uncertain.normal(10.0, 1.0) // E[x] = 10

    // We create a mixture of `x` and `-x`. E[-x] = -10.
    val minusX = x.map(_ * -1.0)

    val mixture = Uncertain.equalMixture(List(x, minusX))

    // Now consider the expression `mixture - x`. This tests that correlation is preserved
    // through the mixture's internal `flatMap` operation.
    // 50% of the time, the mixture chooses `x`, so the expression is `x - x`, which is 0.
    // 50% of the time, the mixture chooses `-x`, so the expression is `-x - x`, which is `-2x`.

    // The result is a new mixture of a point mass at 0 and the distribution -2x.
    // E[mixture - x] = 0.5 * E[0] + 0.5 * E[-2x]
    //                 = 0.5 * 0   + 0.5 * (-2 * E[x])
    //                 = 0.5 * (-2 * 10)
    //                 = -10.0
    val theoreticalMean = -10.0

    val correlatedExpression = mixture - x
    val sampleMean = correlatedExpression.expectedValue(sampleCount)

    val hint = s"Sample mean ($sampleMean) of correlated expression `m - x` should be close to theoretical mean ($theoreticalMean)."
    assert(abs(sampleMean - theoreticalMean) < tolerance, hint)
  }
}