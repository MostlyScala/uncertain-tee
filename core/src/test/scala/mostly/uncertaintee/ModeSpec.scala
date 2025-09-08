package mostly.uncertaintee

import scala.math.floor

class ModeSpec extends RngSuite {

  private val sampleCount = 100_000

  // --- Discrete Distributions ---

  rngTest("mode with a point distribution should return the point value") {
    val distribution = Uncertain.point(42)
    val mode         = distribution.mode(sampleCount)
    assertEquals(mode, 42, "The mode of a point distribution must be its value.")
  }

  rngTest("mode with a skewed Bernoulli distribution should return the most likely outcome") {
    val skewedTrue = Uncertain.bernoulli(0.8)
    assertEquals(skewedTrue.mode(sampleCount), true, "Mode of Bernoulli(0.8) should be true.")

    val skewedFalse = Uncertain.bernoulli(0.2)
    assertEquals(skewedFalse.mode(sampleCount), false, "Mode of Bernoulli(0.2) should be false.")
  }

  rngTest("mode with a Binomial distribution should approximate the theoretical mode") {
    // For a Binomial(n, p) distribution, the mode is floor((n + 1) * p).
    // See: https://en.wikipedia.org/wiki/Binomial_distribution#Mode
    val trials          = 20
    val probability     = 0.3
    val distribution    = Uncertain.binomial(trials, probability)
    val theoreticalMode = floor((trials + 1) * probability).toInt // floor(21 * 0.3) = 6

    val sampleMode = distribution.mode(sampleCount)
    assertEquals(
      sampleMode,
      theoreticalMode,
      s"The sample mode ($sampleMode) should match the theoretical mode ($theoreticalMode)."
    )
  }

  rngTest("mode with a Poisson distribution should approximate the theoretical mode") {
    // For a non-integer λ, the mode is floor(λ).
    // See: https://en.wikipedia.org/wiki/Poisson_distribution#Mode
    val lambda          = 5.7
    val distribution    = Uncertain.poisson(lambda)
    val theoreticalMode = floor(lambda).toInt

    val sampleMode = distribution.mode(sampleCount)
    assertEquals(
      sampleMode,
      theoreticalMode,
      s"The sample mode ($sampleMode) should match the theoretical mode ($theoreticalMode)."
    )
  }

  rngTest("mode with a categorical distribution should return the most probable outcome") {
    val distribution = Uncertain.categorical(Map("A" -> 0.1, "B" -> 0.8, "C" -> 0.1))
    val mode         = distribution.mode(sampleCount)
    assertEquals(mode, "B", "The mode should be the outcome with the highest probability.")
  }

  rngTest("mode with an empirical distribution should return the most frequent element") {
    val data         = List(1, 2, 2, 3, 4, 4, 4, 4, 5)
    val distribution = Uncertain.empirical(data)
    val mode         = distribution.mode(sampleCount)
    assertEquals(mode, 4, "The mode of an empirical distribution should be the most frequent data point.")
  }
}
