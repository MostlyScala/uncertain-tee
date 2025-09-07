package mostly.uncertaintee

import munit.FunSuite

import scala.util.Random

class PreconditionSpec extends RngSuite {

  // A dummy uncertain value for testing extension methods
  private val dummyUncertainBoolean: Uncertain[Boolean] = Uncertain.bernoulli(0.5)(using new Random(42L))
  private val dummyUncertainDouble: Uncertain[Double] = Uncertain.uniform(0.0, 1.0)(using new Random(42L))

  // --- Top-Level Method Preconditions ---

  test("`take` should throw IllegalArgumentException for negative sample counts") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.take(-1)
    }
  }

  // --- Factory Method Preconditions ---

  test("`mixture` should throw IllegalArgumentException for empty component map") {
    intercept[IllegalArgumentException] {
      Uncertain.mixture(Map.empty[Uncertain[Double], Double])
    }
  }

  test("`mixture` should throw IllegalArgumentException for negative weights") {
    intercept[IllegalArgumentException] {
      val components = Map(Uncertain.point(1.0) -> 1.0, Uncertain.point(2.0) -> -0.5)
      Uncertain.mixture(components)
    }
  }

  test("`mixture` should throw IllegalArgumentException if weights sum to zero") {
    intercept[IllegalArgumentException] {
      val components = Map(Uncertain.point(1.0) -> 0.0, Uncertain.point(2.0) -> 0.0)
      Uncertain.mixture(components)
    }
  }

  test("`equalMixture` should throw IllegalArgumentException for empty component list") {
    intercept[IllegalArgumentException] {
      Uncertain.equalMixture(List.empty[Uncertain[Double]])
    }
  }

  test("`empirical` should throw IllegalArgumentException for empty data list") {
    intercept[IllegalArgumentException] {
      Uncertain.empirical(List.empty[Double])
    }
  }

  test("`categorical` should throw IllegalArgumentException for empty outcomes map") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map.empty[String, Double])
    }
  }

  test("`categorical` should throw IllegalArgumentException for negative probabilities") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map("A" -> 1.0, "B" -> -0.5))
    }
  }

  test("`categorical` should throw IllegalArgumentException if probabilities sum to zero") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map("A" -> 0.0, "B" -> 0.0))
    }
  }

  test("`normal` should throw IllegalArgumentException for negative standard deviation") {
    intercept[IllegalArgumentException] {
      Uncertain.normal(0.0, -1.0)
    }
  }

  test("`uniform` should throw IllegalArgumentException if max is less than min") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(10.0, 0.0)
    }
  }

  test("`exponential` should throw IllegalArgumentException for non-positive rate") {
    intercept[IllegalArgumentException] {
      Uncertain.exponential(0.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.exponential(-1.0)
    }
  }

  test("`bernoulli` should throw IllegalArgumentException for probability outside [0, 1]") {
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(-0.1)
    }
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(1.1)
    }
  }

  test("`kumaraswamy` should throw IllegalArgumentException for non-positive parameters") {
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(0.0, 1.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(1.0, -1.0)
    }
  }

  test("`rayleigh` should throw IllegalArgumentException for non-positive scale") {
    intercept[IllegalArgumentException] {
      Uncertain.rayleigh(0.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.rayleigh(-1.0)
    }
  }

  test("`binomial` should throw IllegalArgumentException for negative trials") {
    intercept[IllegalArgumentException] {
      Uncertain.binomial(-1, 0.5)
    }
  }

  test("`binomial` should throw IllegalArgumentException for probability outside [0, 1]") {
    intercept[IllegalArgumentException] {
      Uncertain.binomial(10, -0.1)
    }
    intercept[IllegalArgumentException] {
      Uncertain.binomial(10, 1.1)
    }
  }

  test("`poisson` should throw IllegalArgumentException for negative lambda") {
    intercept[IllegalArgumentException] {
      Uncertain.poisson(-1.0)
    }
  }

  // --- Extension Method Preconditions ---

  test("`probability` should throw IllegalArgumentException for invalid `exceeds` value") {
    intercept[IllegalArgumentException] {
      dummyUncertainBoolean.probability(exceeds = -0.1)
    }
    intercept[IllegalArgumentException] {
      dummyUncertainBoolean.probability(exceeds = 1.1)
    }
  }

  test("`probability` should throw IllegalArgumentException for non-positive `maxSamples`") {
    intercept[IllegalArgumentException] {
      dummyUncertainBoolean.probability(exceeds = 0.5, maxSamples = 0)
    }
  }
  
  test("`mode` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.mode(0)
    }
  }

  test("`histogram` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.histogram(0)
    }
  }

  test("`entropy` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.entropy(0)
    }
  }

  test("`expectedValue` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.expectedValue(0)
    }
  }

  test("`populationStandardDeviation` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.populationStandardDeviation(0)
    }
  }

  test("`sampleStandardDeviation` should throw IllegalArgumentException for sample count less than 2") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.sampleStandardDeviation(1)
    }
  }

  test("`confidenceInterval` should throw IllegalArgumentException for invalid confidence level") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.confidenceInterval(confidence = 0.0)
    }
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.confidenceInterval(confidence = 1.0)
    }
  }

  test("`confidenceInterval` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.confidenceInterval(sampleCount = 0)
    }
  }

  test("`cdf` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      dummyUncertainDouble.cdf(0.5, 0)
    }
  }
}

