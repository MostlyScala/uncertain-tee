package mostly.uncertaintee.ops

import mostly.uncertaintee.*

import scala.math.*
import scala.util.Random

/** {{{
  * import mostly.uncertaintee.syntax.distribution.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait DistributionOps {

  /** Uncertain[T] can also be thought of as a distribution of [T] */
  type Distribution[T] = Uncertain[T]

  extension (u: Uncertain.type) {

    /** Creates an uncertain value that's always the same (no uncertainty).
      *
      * @param value
      *   The constant value to always return
      * @return
      *   An uncertain value that always samples to the given value
      */
    def point[T](value: T)(using random: Random = new Random()): Uncertain[T] = Uncertain(() => value)(using random)

    /** Converts a `List[Uncertain[T]]` (or another iterable) into a single `Uncertain[List[T]]`.
      *
      * This is useful for aggregating a collection of uncertain values into a single uncertain result.
      *
      * @example
      *   Correlation preservation across multiple uncertain values:
      *   {{{
      *   val temp1 = Uncertain.normal(20, 2)
      *   val temp2 = temp1.map(_ + 5)  // Always 5 degrees warmer
      *   val temp3 = temp1.map(_ - 3)  // Always 3 degrees cooler
      *
      *   val allTemps = Uncertain.sequence(List(temp1, temp2, temp3))
      *
      *   // The relationships are preserved in every sample
      *   val sample = allTemps.sample() // e.g., List(18.5, 23.5, 15.5)
      *   // Notice: temp2 = temp1 + 5, temp3 = temp1 - 3
      *   }}}
      *
      * {{{
      * import mostly.uncertaintee.Uncertain
      * import mostly.uncertaintee.syntax.*
      *
      * // Model three independent dice rolls
      * val dieRollD6 = Uncertain.uniform(1, 7).map(_.toInt)
      * val dieRoll12 = Uncertain.uniform(1, 13).map(_.toInt)
      * val dieRollD20 = Uncertain.uniform(1, 21).map(_.toInt)
      *
      * // Sequence them into a single Uncertain List
      * val uncertainSum: Uncertain[Int] = Uncertain.sequence(
      *   List(dieRollD20, dieRoll12, dieRollD6)
      * ).map(_.sum)
      *
      * // Now you can analyze the distribution of the sum of three dice rolls
      * println(s"Expected sum of three dice rolls: ${uncertainSum.mean()}")
      * }}}
      *
      * @param uncertainTs
      *   a sequence of `Uncertain` values.
      * @return
      *   A single `Uncertain` that, when sampled, produces a list of samples — one from each of the input `Uncertain`
      *   instances, preserving all correlations between them.
      */
    def sequence[T](uncertainTs: Iterable[Uncertain[T]])(using random: Random = new Random()): Uncertain[List[T]] =
      uncertainTs.foldRight {
        Uncertain.point(List.empty[T])(using random)
      } { (elem, acc) =>
        for {
          h <- elem
          t <- acc
        } yield h :: t
      }

    /** Applies a function to each element in a collection, where the function returns an `Uncertain` value, then
      * sequences the results into a single `Uncertain` collection.
      *
      * @example
      *   Correlation in dependent processing:
      *   {{{
      *   val baseEfficiency = Uncertain.normal(0.8, 0.1)
      *
      *   def processTask(taskComplexity: Double): Uncertain[Double] =
      *     baseEfficiency.map(eff => taskComplexity / eff)  // Same efficiency for all tasks
      *
      *   val complexities = List(1.0, 2.5, 1.8)
      *   val processingTimes = Uncertain.traverse(complexities)(processTask)
      *
      *   // All processing times are correlated through baseEfficiency
      *   val sample = processingTimes.sample()
      *   // If efficiency is low, ALL tasks take longer proportionally
      *   }}}
      *
      * {{{
      * // Assume we have a function that models the uncertain time to process a task
      * def processTask(taskId: Int): Uncertain[Double] =
      * Uncertain.normal(mean = taskId * 1.5, standardDeviation = 0.5)
      *
      * val taskIds = List(1, 2, 3)
      *
      * // Use traverse to get the distribution of total processing time
      * val totalTime: Uncertain[Double] = Uncertain.traverse(taskIds)(processTask).map(_.sum)
      *
      * println(s"Expected total processing time: ${totalTime.mean()}")
      * }}}
      *
      * @param items
      *   The collection of values to map over.
      * @param toUncertain
      *   The function to apply to each element, which returns an `Uncertain` value.
      * @return
      *   An `Uncertain` value containing the collection of results, preserving correlations.
      */
    def traverse[A, T](
      items: Iterable[A]
    )(toUncertain: A => Uncertain[T])(using random: Random = new Random()): Uncertain[List[T]] =
      items.foldRight {
        Uncertain.point(List.empty[T])(using random)
      } { (elem, acc) =>
        for {
          h <- toUncertain(elem)
          t <- acc
        } yield h :: t
      }

    /** Creates a mixture of different uncertain distributions.
      *
      * @param components
      *   Map from uncertain values to their weights in the mixture
      * @param random
      *   Random number generator to use for mixture selection
      * @return
      *   An uncertain value that samples from the mixture distribution
      */
    def mixture[T](components: Map[Uncertain[T], Double])(using random: Random = new Random()): Uncertain[T] = {
      require(components.nonEmpty, "Need at least one component for a mixture.")
      require(components.values.forall(_ >= 0), "Weights cannot be negative.")
      val totalWeight = components.values.sum
      require(totalWeight > 0, "Weights must sum to something positive.")

      if (components.size == 1) return components.head._1
      categorical(components)(using random).flatMap(identity)
    }

    /** Creates a mixture where all components have equal weight.
      *
      * @param components
      *   List of uncertain values to mix with equal probability
      * @param random
      *   Random number generator to use for mixture selection
      * @return
      *   An uncertain value that samples uniformly from the given components
      */
    def equalMixture[T](components: List[Uncertain[T]])(using random: Random = new Random()): Uncertain[T] = {
      require(components.nonEmpty, "Need at least one component for equal mixture.")
      mixture(components.map((x: Uncertain[T]) => x -> One).toMap)(using random)
    }

    /** Creates a distribution by sampling from observed data.
      *
      * @param data
      *   List of observed values to sample from
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain value that samples uniformly from the given data points
      */
    def empirical[T](data: List[T])(using random: Random = new Random()): Uncertain[T] = {
      require(data.nonEmpty, "Need at least one data point for empirical distribution.")
      Uncertain(() => data(random.nextInt(data.length)))
    }

    /** Creates a distribution that chooses from specific outcomes with given probabilities.
      *
      * @param outcomes
      *   Map from outcome values to their relative probabilities/weights
      * @param random
      *   Random number generator to use for outcome selection
      * @return
      *   An uncertain value that samples according to the specified probability distribution
      */
    def categorical[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] = {
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
    def normal(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] = {
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

    /** Creates a uniform distribution (all values equally likely within a range).
      *
      * @param min
      *   The minimum value (inclusive)
      * @param max
      *   The maximum value (exclusive)
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain value uniformly distributed between min and max
      */
    def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(max >= min, s"max ($max) must be ≥ min ($min).")
      Uncertain(() => min + random.nextDouble() * (max - min))
    }

    /** Creates an exponential distribution (models time between random events).
      *
      * @param rate
      *   The rate parameter (λ) - higher values mean events happen more frequently
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain value following an exponential distribution with the specified rate
      */
    def exponential(rate: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(rate > 0, "Rate parameter must be positive.")
      Uncertain { () =>
        var u = Zero
        while (u == Zero) u = random.nextDouble() // Avoid log(0)
        -log(u) / rate
      }
    }

    /** Creates a true/false distribution with a given probability of true.
      *
      * @param probability
      *   The probability of sampling `true` (must be between 0 and 1)
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain Boolean value that is `true` with the specified probability
      */
    def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] = {
      require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain(() => random.nextDouble() < probability)
    }

    /** Creates a Kumaraswamy distribution (similar to Beta, but simpler to sample).
      *
      * @param a
      *   First shape parameter (must be positive)
      * @param b
      *   Second shape parameter (must be positive)
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain value following a Kumaraswamy distribution on the interval [0,1]
      */
    def kumaraswamy(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(a > 0 && b > 0, "Kumaraswamy parameters must be positive")
      val reciprocalA = One / a
      val reciprocalB = One / b
      Uncertain { () =>
        val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
        pow(One - pow(One - u, reciprocalB), reciprocalA)
      }
    }

    /** Creates a Rayleigh distribution (models magnitude of 2D random vector).
      *
      * @param scale
      *   The scale parameter (σ) - must be positive
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain value following a Rayleigh distribution with the specified scale
      */
    def rayleigh(scale: Double)(using random: Random = new Random()): Uncertain[Double] = {
      require(scale > 0, "Rayleigh scale parameter must be positive")
      Uncertain { () =>
        val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
        scale * sqrt(-Two * log(One - u))
      }
    }

    /** Creates a binomial distribution (number of successes in n trials).
      *
      * @param trials
      *   Number of independent trials (must be non-negative)
      * @param probability
      *   Probability of success on each trial (must be between 0 and 1)
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain integer representing the number of successes in the trials
      */
    def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(trials >= 0, "Number of trials cannot be negative.")
      require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
    }

    /** Creates a Poisson distribution (number of events in a fixed time period).
      *
      * @param lambda
      *   The average rate of events (λ) - must be non-negative
      * @param random
      *   Random number generator to use for sampling
      * @return
      *   An uncertain integer representing the number of events in the time period
      */
    def poisson(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
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
