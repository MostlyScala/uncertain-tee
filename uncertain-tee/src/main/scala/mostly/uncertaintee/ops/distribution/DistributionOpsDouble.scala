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

import mostly.uncertaintee.*

import scala.annotation.tailrec
import scala.math.*
import scala.util.Random

trait DistributionOpsDouble {

  /** The inverse CDF maps from probability 0 to 1, to a value */
  trait InverseCdfFunction {
    def fromProbabilityToValue(zeroToOne: Double): Double
  }

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
      if (standardDeviation == 0) {
        Uncertain.always(mean)
      } else {
        Uncertain { () =>
          val gaussian = random.nextGaussian() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
          mean + (standardDeviation * gaussian)
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
      * The triangular distribution is a continuous probability distribution with a lower limit `min`, an upper limit `max`, and a mode `peak`.
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
        Uncertain.always(min)
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
      if (probability == Zero) {
        Uncertain.always(false)
      } else if (probability == One) {
        Uncertain.always(true)
      } else {
        Uncertain(() => random.nextDouble() < probability)
      }
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

    /** Models a Geometric distribution.
      *
      * This represents the number of independent trials required to get the first success.
      *
      * See: https://en.wikipedia.org/wiki/Geometric_distribution
      *
      * @param probability
      *   The probability of success ($p$) on any given trial. Must be in (0, 1].
      * @return
      *   An `Uncertain[Int]` representing the 1-indexed trial number on which the first success occurred.
      * @note
      *   Runtime: O(1) - uses inverse transform sampling.
      * @example
      *   {{{
      * // How many flips until the first heads (p=0.5)?
      * val firstHeads = Uncertain.geometric(0.5)
      *
      * // How many attempts until the first success (p=0.1)?
      * val firstSuccess = Uncertain.geometric(0.1)
      * println(s"Expected attempts: ${firstSuccess.mean()}") // ~10.0
      *   }}}
      */
    def geometricViaDouble(probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(
        probability > 0 && probability <= 1,
        s"probability ($probability) must be in (0, 1]"
      )
      if (probability == 1.0) {
        Uncertain.always(1) // Always 1 trial for 100% success
      } else {
        Uncertain { () =>
          // Inverse Transform Sampling:
          // k = ceil(log(U) / log(1-p)) where U is uniform(0, 1)
          val u      = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
          val trials = math.ceil(math.log(u) / math.log(1.0 - probability))
          trials.toInt
        }
      }
    }

    def poissonViaDouble(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(lambda >= 0, "Lambda (average rate) cannot be negative.")
      if (lambda == Zero) {
        Uncertain.always(0)
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

    /** Creates an Uncertain[Double] from an inverse CDF function.
      *
      * The inverse CDF maps from probability [0,1] to values
      *
      * @example
      *   {{{
      *    // example uniform distribution `a` to `b`
      *   val uniform = Uncertain.fromInverseCdf { p => a + p * (b - a) }
      *   // Exponential distribution with rate λ
      *   val exponential = Uncertain.fromInverseCdf { p => -math.log(1 - p) / lambda }
      *   }}}
      * @param inverseCdf
      *   Function mapping probability 0 to 1 to a value
      * @return
      *   Uncertain[Double] that samples by generating uniform random probabilities and transforming them via the inverse CDF
      */
    def fromInverseCdf(inverseCdf: InverseCdfFunction)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain { () =>
        val zeroToOne = random.nextDouble()
        inverseCdf.fromProbabilityToValue(zeroToOne)
      }

    /** Creates a Beta distribution.
      *
      * The Beta distribution is a continuous probability distribution on [0,1] parameterized by two positive shape parameters. It is widely used to model random variables limited
      * to intervals of finite length in Bayesian statistics.
      *
      * @see
      *   https://en.wikipedia.org/wiki/Beta_distribution
      *
      * @param alpha
      *   The first shape parameter (must be positive and finite). Higher values shift the distribution right.
      * @param beta
      *   The second shape parameter (must be positive and finite). Higher values shift the distribution left.
      * @param random
      *   Random number generator to use for sampling.
      * @return
      *   An uncertain value following a Beta distribution on the interval [0,1].
      * @note
      *   Common special cases:
      *   - Beta(1,1) is uniform on [0,1]
      *   - Beta(α,α) is symmetric around 0.5
      *   - Beta(0.5,0.5) is U-shaped (concentrated near 0 and 1)
      *   - Beta(2,5) is left-skewed, Beta(5,2) is right-skewed
      * @note
      *   Implementation uses the Gamma ratio method: if X ~ Gamma(α,1) and Y ~ Gamma(β,1), then X/(X+Y) ~ Beta(α,β). Gamma samples are generated using the Marsaglia-Tsang method.
      * @example
      *   {{{
      * // Symmetric distribution around 0.5
      * val symmetric = Uncertain.betaViaDouble(2.0, 2.0)
      *
      * // Right-skewed distribution
      * val rightSkewed = Uncertain.betaViaDouble(5.0, 2.0)
      *
      * // U-shaped distribution (concentrated at extremes)
      * val uShaped = Uncertain.betaViaDouble(0.5, 0.5)
      *   }}}
      */
    def betaViaDouble(alpha: Double, beta: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(alpha > 0 && !alpha.isNaN && !alpha.isInfinite, s"Alpha must be positive and finite: alpha=$alpha")
      require(beta > 0 && !beta.isNaN && !beta.isInfinite, s"Beta must be positive and finite: beta=$beta")

      // Generates a sample from Gamma(shape, 1) distribution using Marsaglia-Tsang method
      def sampleGamma(shape: Double): Double = {
        require(shape > 0, s"Gamma shape parameter must be positive: shape=$shape")

        // Marsaglia-Tsang transformation parameters
        val transformD = if (shape < One) {
          shape + One - One / 3.0
        } else {
          shape - One / 3.0
        }
        val transformC = One / sqrt(9.0 * transformD)

        @tailrec
        def acceptanceRejectionSample(maxAttempts: Int = 1000): Double = {
          require(maxAttempts > 0, "Maximum attempts exhausted in Gamma sampling. This indicates a serious rng/numerical issue with the source of randomness.")

          val standardNormal   = random.nextGaussian()
          val transformedValue = One + transformC * standardNormal

          if (transformedValue <= Zero) {
            acceptanceRejectionSample(maxAttempts - 1)
          } else {
            val cubeTransformed = transformedValue * transformedValue * transformedValue
            val uniformSample   = random.nextDouble()
            val normalSquared   = standardNormal * standardNormal

            val squeezeAccept = uniformSample < One - 0.0331 * normalSquared * normalSquared
            val logAccept     = log(uniformSample) < 0.5 * normalSquared + transformD * (One - cubeTransformed + log(cubeTransformed))

            if (squeezeAccept || logAccept) {
              transformD * cubeTransformed
            } else {
              acceptanceRejectionSample(maxAttempts - 1)
            }
          }
        }
        val gammaSample                                                = acceptanceRejectionSample()

        // For shape < 1, apply correction factor using theorem: if X ~ Gamma(a+1,1) then X*U^(1/a) ~ Gamma(a,1)
        if (shape < One) {
          val uniformCorrection = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
          gammaSample * pow(uniformCorrection, One / shape)
        } else {
          gammaSample
        }
      }

      Uncertain { () =>
        val gammaAlpha = sampleGamma(alpha)
        val gammaBeta  = sampleGamma(beta)
        gammaAlpha / (gammaAlpha + gammaBeta)
      }
    }
  }
}
