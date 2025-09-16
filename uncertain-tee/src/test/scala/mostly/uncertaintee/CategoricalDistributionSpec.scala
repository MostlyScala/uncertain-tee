package mostly.uncertaintee

import scala.math.abs
import mostly.uncertaintee.syntax.*

class CategoricalDistributionSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.01

  rngTest("sampling frequency should approximate the defined probabilities") {
    // A categorical distribution allows sampling from a set of outcomes, where each
    // outcome has a specific, user-defined probability.
    val probabilities = Map("A" -> 0.1, "B" -> 0.6, "C" -> 0.3)
    val categorical   = Uncertain.categorical(probabilities)

    val samples     = categorical.take(sampleCount)
    val frequencies = samples.groupBy(identity).view.mapValues(_.length.toDouble / sampleCount).toMap

    probabilities.foreach { case (outcome, theoreticalProb) =>
      val sampleProb = frequencies.getOrElse(outcome, 0.0)
      val hint       =
        s"Frequency of '$outcome' ($sampleProb) should be close to its theoretical probability ($theoreticalProb)."
      assert(abs(sampleProb - theoreticalProb) < tolerance, hint)
    }
  }

  rngTest("with a single outcome should behave like a point distribution") {
    val distribution = Uncertain.categorical(Map("Winner" -> 1.0))
    val samples      = distribution.take(1000)
    assert(
      samples.forall(_ == "Winner"),
      "A categorical distribution with one outcome at 100% probability must always return that outcome."
    )
    assertEquals(distribution.mode(1000), "Winner")
  }

  // --- Input Validation Tests ---

  rngTest("constructor should throw if probabilities are negative") {
    // The sum of probabilities for all possible outcomes must be exactly 1.
    intercept[IllegalArgumentException] {
      // Sum is negative
      Uncertain.categorical(Map("A" -> -0.1, "B" -> -0.2))
    }
  }

  rngTest("constructor should throw if probabilities sum to zero") {
    // The sum of probabilities for all possible outcomes must be exactly 1.
    intercept[IllegalArgumentException] {
      // Sum is negative
      Uncertain.categorical(Map("A" -> 0, "B" -> 0))
    }
  }

  rngTest("constructor should throw if any probability is negative") {
    intercept[IllegalArgumentException] {
      // Sum is valid (1.0), but contains a negative probability.
      Uncertain.categorical(Map("A" -> 1.2, "B" -> -0.2))
    }
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map("A" -> 0.8, "B" -> 0.3, "C" -> -0.1))
    }
  }

  rngTest("constructor should throw if the probability map is empty") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map.empty[String, Double])
    }
  }
}
