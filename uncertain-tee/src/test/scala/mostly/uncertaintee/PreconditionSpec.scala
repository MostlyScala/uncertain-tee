/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee

import mostly.uncertaintee.syntax.*
import munit.FunSuite

import scala.util.Random

class PreconditionSpec extends RngSuite {

  val sampleCount = 100_000

  rngTest("`take` should throw IllegalArgumentException for negative sample counts") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).take(-1)
    }
  }

  rngTest("`mixture` should throw IllegalArgumentException for empty component map") {
    intercept[IllegalArgumentException] {
      Uncertain.mixture(Map.empty[Uncertain[Double], Double])
    }
  }

  rngTest("`mixture` should throw IllegalArgumentException for negative weights") {
    intercept[IllegalArgumentException] {
      val components = Map(Uncertain.always(1.0) -> 1.0, Uncertain.always(2.0) -> -0.5)
      Uncertain.mixture(components)
    }
  }

  rngTest("`mixture` should throw IllegalArgumentException if weights sum to zero") {
    intercept[IllegalArgumentException] {
      val components = Map(Uncertain.always(1.0) -> 0.0, Uncertain.always(2.0) -> 0.0)
      Uncertain.mixture(components)
    }
  }

  rngTest("`equalMixture` should throw IllegalArgumentException for empty component list") {
    intercept[IllegalArgumentException] {
      Uncertain.equalMixture(List.empty[Uncertain[Double]])
    }
  }

  rngTest("`empirical` should throw IllegalArgumentException for empty data list") {
    intercept[IllegalArgumentException] {
      Uncertain.empirical(List.empty[Double])
    }
  }

  rngTest("`categorical` should throw IllegalArgumentException for empty outcomes map") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map.empty[String, Double])
    }
  }

  rngTest("`categorical` should throw IllegalArgumentException for negative probabilities") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map("A" -> 1.0, "B" -> -0.5))
    }
  }

  rngTest("`categorical` should throw IllegalArgumentException if probabilities sum to zero") {
    intercept[IllegalArgumentException] {
      Uncertain.categorical(Map("A" -> 0.0, "B" -> 0.0))
    }
  }

  rngTest("`normal` should throw IllegalArgumentException for negative standard deviation") {
    intercept[IllegalArgumentException] {
      Uncertain.normal(0.0, -1.0)
    }
  }

  rngTest("`uniform` should throw IllegalArgumentException if max is less than min") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(10.0, 0.0)
    }
  }

  rngTest("`exponential` should throw IllegalArgumentException for non-positive rate") {
    intercept[IllegalArgumentException] {
      Uncertain.exponential(0.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.exponential(-1.0)
    }
  }

  rngTest("`bernoulli` should throw IllegalArgumentException for probability outside [0, 1]") {
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(-0.1)
    }
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(1.1)
    }
  }

  rngTest("`kumaraswamy` should throw IllegalArgumentException for non-positive parameters") {
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(0.0, 1.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.kumaraswamy(1.0, -1.0)
    }
  }

  rngTest("`rayleigh` should throw IllegalArgumentException for non-positive scale") {
    intercept[IllegalArgumentException] {
      Uncertain.rayleigh(0.0)
    }
    intercept[IllegalArgumentException] {
      Uncertain.rayleigh(-1.0)
    }
  }

  rngTest("`binomial` should throw IllegalArgumentException for negative trials") {
    intercept[IllegalArgumentException] {
      Uncertain.binomial(-1, 0.5)
    }
  }

  rngTest("`binomial` should throw IllegalArgumentException for probability outside [0, 1]") {
    intercept[IllegalArgumentException] {
      Uncertain.binomial(10, -0.1)
    }
    intercept[IllegalArgumentException] {
      Uncertain.binomial(10, 1.1)
    }
  }

  rngTest("`poisson` should throw IllegalArgumentException for negative lambda") {
    intercept[IllegalArgumentException] {
      Uncertain.poisson(-1.0)
    }
  }

  // --- Extension Method Preconditions ---

  rngTest("`probability` should throw IllegalArgumentException for invalid `exceeds` value") {
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(0.5).probability(exceeds = -0.1, maxSamples = sampleCount)
    }
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(0.5).probability(exceeds = 1.1, maxSamples = sampleCount)
    }
  }

  rngTest("`probability` should throw IllegalArgumentException for non-positive `maxSamples`") {
    intercept[IllegalArgumentException] {
      Uncertain.bernoulli(0.5).probability(exceeds = 0.5, maxSamples = 0)
    }
  }

  rngTest("`mode` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).mode(0)
    }
  }

  rngTest("`histogram` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).histogram(0)
    }
  }

  rngTest("`entropy` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).entropy(0)
    }
  }

  rngTest("`expectedValue` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).expectedValue(0)
    }
  }

  rngTest("`populationStandardDeviation` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).populationStandardDeviation(0)
    }
  }

  rngTest("`standardDeviation` should throw IllegalArgumentException for sample count less than 2") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).standardDeviation(1)
    }
  }

  rngTest("`confidenceInterval` should throw IllegalArgumentException for invalid confidence level") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).confidenceInterval(confidence = 0.0, sampleCount = sampleCount)
    }
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).confidenceInterval(confidence = 1.0, sampleCount = sampleCount)
    }
  }

  rngTest("`confidenceInterval` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).confidenceInterval(sampleCount = 0)
    }
  }

  rngTest("`cdf` should throw IllegalArgumentException for non-positive sample count") {
    intercept[IllegalArgumentException] {
      Uncertain.uniform(0.0, 1.0).cdf(0.5, 0)
    }
  }
}
