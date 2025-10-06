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

package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee._
import mostly.uncertaintee.syntax.point

import scala.math._
import scala.util.Random

trait DistributionOpsDouble {
  extension (u: Uncertain.type) {

    /** Creates a normal (bell curve) distribution.
      *
      * @param mean
      *   The center/average value of the distribution
      * @param standardDeviation
      *   The spread of the distribution (must be non-negative)
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain value following a normal distribution with the specified parameters
      */
    def normalDouble(
      mean: Double,
      standardDeviation: Double
    )(using random: Random = new Random()): Uncertain[Double] = {
      require(standardDeviation >= 0, "Standard deviation cannot be negative.")
      Uncertain { () =>
        if (standardDeviation == 0) mean
        else {
          // Box-Muller transform for generating normal samples
          var u1 = Zero
          while (u1 == Zero) u1 = random.nextDouble() // Avoid log(0)
          val u2 = random.nextDouble()
          val z0 = sqrt(MinusTwo * log(u1)) * cos(Two * Pi * u2)
          mean + standardDeviation * z0
        }
      }
    }

    /** Creates a uniform distribution of Doubles. */
    def uniformDouble(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(max >= min, s"max ($max) must be >= min ($min).")
      Uncertain(() => min + random.nextDouble() * (max - min))
    }

    /** Creates a triangular distribution.
      *
      * The triangular distribution is a continuous probability distribution with a lower limit `min`, an upper limit
      * `max`, and a mode `peak`.
      *
      * @param min
      *   The minimum value of the distribution (lower bound).
      * @param max
      *   The maximum value of the distribution (upper bound).
      * @param peak
      *   The mode or most frequent value, where the distribution is highest.
      * @param random
      *   Random number generator to use for sampling.
      * @return
      *   An uncertain value following a triangular distribution.
      */
    def triangularViaDouble(
      min: Double,
      peak: Double,
      max: Double
    )(using random: Random = new Random()): Uncertain[Double] = {
      require(min <= max, s"min ($min) must be <= max ($max)")
      require(min <= peak && peak <= max, s"peak ($peak) must be between min ($min) and max ($max).")
      if (min == max) {
        Uncertain.point(min)
      } else {
        Uncertain { () =>
          val u  = random.nextDouble()
          val fc = (peak - min) / (max - min)
          if (u < fc) {
            min + sqrt(u * (max - min) * (peak - min))
          } else {
            max - sqrt((1 - u) * (max - min) * (max - peak))
          }
        }
      }
    }

    /** Creates an exponential distribution. */
    def exponentialViaDouble(rate: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(rate > 0, "Rate parameter must be positive.")
      Uncertain { () =>
        var u = Zero
        while (u == Zero) u = random.nextDouble()
        -log(u) / rate
      }
    }

    /** Creates a Kumaraswamy distribution. */
    def kumaraswamyViaDouble(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(a > 0 && b > 0, "Kumaraswamy parameters must be positive")
      val reciprocalA = One / a
      val reciprocalB = One / b
      Uncertain { () =>
        val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
        pow(One - pow(One - u, reciprocalB), reciprocalA)
      }
    }

    /** Creates a Rayleigh distribution. */
    def rayleighViaDouble(scale: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(scale > 0, "Rayleigh scale parameter must be positive")
      Uncertain { () =>
        val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
        scale * sqrt(-Two * log(One - u))
      }
    }

    def binomialViaDouble(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(trials >= 0, "Number of trials cannot be negative.")
      require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
    }

    def bernoulliViaDouble(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain(() => random.nextDouble() < probability)
    }

    def categoricalViaDouble[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] = {
      require(outcomes.nonEmpty, "Need at least one outcome for categorical distribution")
      require(outcomes.forall(_._2 >= 0), "Probabilities cannot be negative")
      val totalWeight = outcomes.values.sum
      require(totalWeight > 0, "Probabilities must sum to something positive")

      val normalizedOutcomes = outcomes.map((outcome, weight) => (outcome, weight / totalWeight))
      val cumulativeProbs    = normalizedOutcomes.values.scanLeft(Zero)(_ + _).tail
      val paired             = normalizedOutcomes.keys.zip(cumulativeProbs)

      val sampler: () => T = () => {
        val u = random.nextDouble()
        paired.find { case (_, cumProb) => u <= cumProb }.get._1
      }

      Uncertain(sampler)
    }

    def mixtureViaDouble[T](
      components: Map[Uncertain[T], Double]
    )(using random: Random = new Random()): Uncertain[T] = {
      require(components.nonEmpty, "Need at least one component for a mixture.")
      require(components.values.forall(_ >= 0), "Weights cannot be negative.")
      val totalWeight = components.values.sum
      require(totalWeight > 0, "Weights must sum to something positive.")

      if (components.size == 1) return components.head._1
      categoricalViaDouble(components)(using random).flatMap(identity)
    }

    def poissonViaDouble(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(lambda >= 0, "Lambda (average rate) cannot be negative.")
      if (lambda == Zero) {
        Uncertain.point(0)
      } else {
        Uncertain { () =>
          val L = exp(-lambda)
          var k = 0
          var p = One
          while (p > L) {
            k += 1
            p *= random.nextDouble()
          }
          k - 1
        }
      }
    }
  }
}
