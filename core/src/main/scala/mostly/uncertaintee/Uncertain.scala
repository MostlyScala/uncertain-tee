package mostly.uncertaintee

import java.util.UUID
import scala.collection.mutable
import scala.math.*
import scala.util.Random

/** A type that represents uncertain data as a probability distribution using sampling-based computation with conditional semantics.
  *
  * `Uncertain` provides a way to work with probabilistic values by representing them as sampling functions with a computation graph for lazy evaluation and
  * proper uncertainty-aware conditionals. This allows for the composition of uncertain values while preserving correlations. The implementation is based on the
  * approach described in the paper "Uncertain<T>: A First-Order Type for Uncertain Data."
  *
  * @example
  *   {{{
  * // Define an uncertain speed with a normal distribution (mean 5.0, std dev 2.0)
  * val speed = Uncertain.normal(5.0, 2.0)
  *
  * // Perform a hypothesis test: is there a 90% probability that the speed exceeds 4.0?
  * if (speed.gt(4.0).probability(exceeds = 0.9)) {
  * println("90% confident you're going fast")
  * }
  *
  * // Use an implicit conditional (equivalent to .probability(exceeds = 0.5))
  * if (speed.gt(4.0).isProbable()) {
  * println("More likely than not you're going fast")
  * }
  *   }}}
  *
  * '''Performance Considerations'''
  *
  * This library uses the [[https://en.wikipedia.org/wiki/Sequential_probability_ratio_test Sequential Probability Ratio Test (SPRT)]] for efficient hypothesis
  * testing in conditionals (`.probability` and `.isProbable`). This means sample counts are automatically determined based on statistical significance rather
  * than requiring a fixed, and often unnecessarily large, number of samples.
  *
  * '''References'''
  *
  *   - James Bornholt, Todd Mytkowicz, and Kathryn S. McKinley. ''"Uncertain<T>: A First-Order Type for Uncertain Data."'' Architectural Support for
  *     Programming Languages and Operating Systems (ASPLOS), March 2014.
  *   - [[https://www.microsoft.com/en-us/research/publication/uncertaint-a-first-order-type-for-uncertain-data-2/ Paper Link]]
  *
  * @tparam T
  *   The underlying type of the uncertain value (e.g., `Double`, `Int`, `Boolean`).
  * @see
  *   [[https://en.wikipedia.org/wiki/Probability_distribution Probability Distribution]]
  * @see
  *   [[https://en.wikipedia.org/wiki/Computation_graph Computation Graph]]
  * @see
  *   [[https://en.wikipedia.org/wiki/Sequential_probability_ratio_test Sequential Probability Ratio Test]]
  */
sealed abstract class Uncertain[T] {

  /** The function that generates a single random sample from the distribution. This represents the source of uncertainty. */
  val sampler: () => T

  /** The internal computation graph node for this uncertain value. It enables lazy evaluation and correlation preservation. */
  val node: ComputationNode[T]

  /** Draws a single value from the distribution by evaluating the computation graph.
    *
    * This method ensures that if this `Uncertain` value is composed of other `Uncertain` values, their samples are coordinated within a single `SampleContext`
    * to preserve correlations. For example, in `x - x`, the result will always be zero because the same sample for `x` is used on both sides of the expression.
    *
    * @example
    *   {{{
    * val x = Uncertain.normal(10, 2)
    * val y = x - x // y.sample() will always return 0.0
    *   }}}
    *
    * @return
    *   A single sample of type `T`.
    */
  def sample(): T = {
    val context = new SampleContext()
    node.evaluate(context)
  }

  /** Transforms the uncertain value by applying a function to its samples.
    *
    * This is a fundamental operation for applying a deterministic function to a probabilistic value. This operation extends the computation graph to preserve
    * correlations.
    *
    * @example
    *   {{{
    * val speedMph = Uncertain.uniform(50, 70)
    * val speedKph = speedMph.map(_ * 1.60934) // Converts each speed sample to KPH
    *   }}}
    *
    * @param f
    *   The function to apply to each sample.
    * @return
    *   A new `Uncertain[U]` representing the transformed distribution.
    */
  def map[U](f: T => U): Uncertain[U] = {
    val newNode = ComputationUnaryOperation(this.node, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Transforms the uncertain value by applying a function that returns another uncertain value.
    *
    * This operation chains computation graphs, ensuring correlations are maintained across the boundary. It is useful for creating hierarchical or conditional
    * models.
    *
    * @example
    *   {{{
    * // Model the result of a coin flip determining which die to roll
    * val isHeads = Uncertain.bernoulli(0.5)
    * val rollResult = isHeads.flatMap { wasHeads =>
    * if (wasHeads) Uncertain.uniform(1, 6).map(_.round.toInt) // 6-sided die
    * else Uncertain.uniform(1, 20).map(_.round.toInt) // 20-sided die
    * }
    *   }}}
    *
    * @param f
    *   The function that takes a sample and returns a new `Uncertain` value.
    * @return
    *   A new `Uncertain[U]` representing the resulting distribution.
    */
  def flatMap[U](f: T => Uncertain[U]): Uncertain[U] = {
    val newNode = ComputationFlatMap(this.node, f)
    Uncertain(() => newNode.evaluate(), newNode)
  }

  /** Returns an infinite iterator of samples from this distribution. This is useful for functional-style processing of samples.
    *
    * @example
    *   {{{
    * val speed = Uncertain.normal(5.0, 2.0)
    * val firstTenSamples = speed.iterator.take(10).toList
    *   }}}
    */
  def iterator: Iterator[T] = Iterator.continually(sample())

  /** Collects a specified number of samples into a list.
    *
    * @param n
    *   The number of samples to take.
    * @return
    *   A `List[T]` containing `n` samples.
    */
  def take(n: Int): List[T] = iterator.take(n).toList
}

// =================================================================================================
// Computation Graph Nodes
// =================================================================================================

/** A node in the computation graph, representing an operation or a source of uncertainty. It is covariant. */
sealed private[uncertaintee] trait ComputationNode[+T] {
  def evaluate(context: SampleContext): T
  def evaluate(): T = this.evaluate(new SampleContext())
}

/** A leaf node in the computation graph, representing an original source of uncertainty (e.g., from `Uncertain.normal`). */
final private[uncertaintee] case class ComputationLeaf[T](id: UUID, sampler: () => T) extends ComputationNode[T] {
  override def evaluate(context: SampleContext): T =
    context.getValue[T](id) match {
      case Some(cached) => cached // Return memoized value if already sampled in this context
      case None         =>
        val value = sampler()
        context.setValue(id, value) // Store the value for this context
        value
    }
}

/** A unary operation node, used to implement `map`. It applies a function to the result of a source node. */
final private[uncertaintee] case class ComputationUnaryOperation[A, +B](source: ComputationNode[A], operation: A => B) extends ComputationNode[B] {
  override def evaluate(context: SampleContext): B = operation(source.evaluate(context))
}

/** A chaining operation node, used to implement `flatMap`. It evaluates a source node and then uses the result to generate a new computation graph, which is
  * then evaluated within the same context.
  */
final private[uncertaintee] case class ComputationFlatMap[A, B](source: ComputationNode[A], f: A => Uncertain[B]) extends ComputationNode[B] {
  override def evaluate(context: SampleContext): B = {
    val sourceValue    = source.evaluate(context)
    val innerUncertain = f(sourceValue)
    // Evaluate the inner graph using the *same context* to preserve correlation
    innerUncertain.node.evaluate(context)
  }
}

// =================================================================================================
// Factory Object
// =================================================================================================

/** Factory for creating `Uncertain` instances from various sources and distributions. */
object Uncertain {

  /** Creates an `Uncertain` value from a given sampling function (lambda). This is the most fundamental constructor.
    *
    * @param sampler
    *   A function that produces a new sample on each call.
    * @return
    *   A new `Uncertain[T]` instance.
    */
  def apply[T](sampler: () => T): Uncertain[T] = {
    val id = UUID.randomUUID()
    val s  = sampler
    new Uncertain[T] {
      override val sampler: () => T         = s
      override val node: ComputationNode[T] = ComputationLeaf(id = id, sampler = s)
    }
  }

  /** Internal constructor to create an `Uncertain` value with a pre-defined computation node. */
  private[uncertaintee] def apply[T](sampler: () => T, computationNode: ComputationNode[T]): Uncertain[T] = {
    val s = sampler
    new Uncertain[T] {
      override val sampler: () => T         = s
      override val node: ComputationNode[T] = computationNode
    }
  }

  /** Creates an `Uncertain` value that always returns the same constant value. This represents a deterministic value.
    *
    * @param value
    *   The constant value.
    * @return
    *   An `Uncertain[T]` that will always sample to `value`.
    */
  def point[T](value: T): Uncertain[T] = Uncertain(() => value)

  /** Creates a mixture model from a list of uncertain components and optional weights.
    *
    * This implementation correctly preserves correlations by using a categorical distribution and flatMap, ensuring that component selection and evaluation
    * happen within the same computation graph.
    *
    * @param components
    *   A list of `Uncertain` distributions to mix.
    * @param weights
    *   An optional list of weights for each component. If `None`, components are weighted equally.
    * @return
    *   A new `Uncertain[T]` representing the mixture distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Mixture_model Mixture Model]]
    */
  def mixture[T](components: List[Uncertain[T]], weights: Option[List[Double]] = None): Uncertain[T] = {
    require(components.nonEmpty, "At least one component is required for a mixture model.")
    if (components.length == 1) return components.head

    val componentWeights = weights.getOrElse(List.fill(components.length)(1.0))
    require(
      components.length == componentWeights.length,
      "The number of weights must match the number of components."
    )

    // 1. Create a categorical distribution to select a component.
    val totalWeight = componentWeights.sum
    val outcomes    = components
      .zip(componentWeights)
      .map { case (component, weight) =>
        component -> (weight / totalWeight)
      }
      .toMap

    val selector: Uncertain[Uncertain[T]] = categorical(outcomes)

    // 2. Use flatMap to evaluate the selected component within the same context.
    selector.flatMap(chosenComponent => chosenComponent)
  }

  /** Creates an uncertain distribution by sampling uniformly from a list of observed data.
    *
    * @example
    *   {{{
    * val surveyResults = List(3, 5, 4, 5, 5, 2)
    * val rating = Uncertain.empirical(surveyResults).get
    *   }}}
    * @param data
    *   The list of data points to sample from.
    * @return
    *   An `Option[Uncertain[T]]` which is `None` if the data list is empty.
    * @see
    *   [[https://en.wikipedia.org/wiki/Empirical_distribution_function Empirical Distribution Function]]
    */
  def empirical[T](data: List[T]): Option[Uncertain[T]] =
    if (data.isEmpty) None
    else Some(Uncertain(() => data(Random.nextInt(data.length))))

  /** Creates a categorical distribution from a map of outcomes to their probabilities.
    *
    * @example
    *   {{{
    * val weather = Uncertain.categorical(Map("Sunny" -> 0.7, "Cloudy" -> 0.2, "Rain" -> 0.1))
    *   }}}
    * @param outcomes
    *   A map where keys are outcomes and values are their probabilities (weights).
    * @return
    *   An Uncertain[T]
    * @see
    *   [[https://en.wikipedia.org/wiki/Categorical_distribution Categorical Distribution]]
    */
  def categorical[T](outcomes: Map[T, Double]): Uncertain[T] = {
    require(outcomes.nonEmpty, "Categorical distribution must have at least one outcome")
    require(math.abs(outcomes.values.sum - 1.0) < 1e-9, "Probabilities must sum to 1.0")

    val outcomesList    = outcomes.toList
    val cumulativeProbs = outcomesList.map(_._2).scanLeft(0.0)(_ + _).tail
    val paired          = outcomesList.map(_._1).zip(cumulativeProbs)

    val sampler: () => T = () => {
      val u = Random.nextDouble()
      paired.find { case (_, cumProb) => u <= cumProb }.get._1
    }
    Uncertain(sampler, ComputationLeaf[T](UUID.randomUUID(), sampler))
  }

  /** Creates a Normal (Gaussian) distribution.
    *
    * @param mean
    *   The mean (μ) of the distribution.
    * @param standardDeviation
    *   The standard deviation (σ) of the distribution.
    * @return
    *   An `Uncertain[Double]` representing the normal distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Normal_distribution Normal Distribution]]
    */
  def normal(mean: Double, standardDeviation: Double): Uncertain[Double] =
    Uncertain { () =>
      // Box-Muller transform
      val u1 = Iterator.continually(Random.nextDouble()).filter(d => d != 0.0 && d != 1.0).take(1).toList.head
      val u2 = Iterator.continually(Random.nextDouble()).filter(d => d != 0.0 && d != 1.0).take(1).toList.head
      val z0 = sqrt(-2.0 * log(u1)) * cos(2.0 * Pi * u2)
      mean + standardDeviation * z0
    }

  /** Creates a continuous Uniform distribution.
    *
    * @param min
    *   The lower bound of the distribution (inclusive).
    * @param max
    *   The upper bound of the distribution (exclusive).
    * @return
    *   An `Uncertain[Double]` representing the uniform distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Continuous_uniform_distribution Continuous Uniform Distribution]]
    */
  def uniform(min: Double, max: Double): Uncertain[Double] =
    Uncertain(() => min + Random.nextDouble() * (max - min))

  /** Creates an Exponential distribution.
    *
    * @param rate
    *   The rate parameter (λ).
    * @return
    *   An `Uncertain[Double]` representing the exponential distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Exponential_distribution Exponential Distribution]]
    */
  def exponential(rate: Double): Uncertain[Double] =
    Uncertain(() => -log(Random.nextDouble()) / rate)

  /** Creates a Bernoulli distribution, which is a distribution of a single boolean trial.
    *
    * @param probability
    *   The probability of the outcome being `true`.
    * @return
    *   An `Uncertain[Boolean]`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Bernoulli_distribution Bernoulli Distribution]]
    */
  def bernoulli(probability: Double): Uncertain[Boolean] =
    Uncertain(() => Random.nextDouble() < probability)

  /** Creates a Kumaraswamy distribution.
    *
    * @param a
    *   The `a` shape parameter (> 0).
    * @param b
    *   The `b` shape parameter (> 0).
    * @return
    *   An `Uncertain[Double]` representing the Kumaraswamy distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Kumaraswamy_distribution Kumaraswamy Distribution]]
    */
  def kumaraswamy(a: Double, b: Double): Uncertain[Double] = {
    require(a > 0 && b > 0, "Kumaraswamy distribution parameters must be positive")
    val reciprocalA = 1.0 / a
    val reciprocalB = 1.0 / b
    Uncertain { () =>
      val u = Random.nextDouble() * (1.0 - Double.MinPositiveValue) + Double.MinPositiveValue
      pow(1.0 - pow(1.0 - u, reciprocalB), reciprocalA)
    }
  }

  /** Creates a Rayleigh distribution.
    *
    * @param scale
    *   The scale parameter (σ > 0).
    * @return
    *   An `Uncertain[Double]` representing the Rayleigh distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Rayleigh_distribution Rayleigh Distribution]]
    */
  def rayleigh(scale: Double): Uncertain[Double] = {
    require(scale > 0, "Rayleigh distribution scale parameter must be positive")
    Uncertain { () =>
      val u = Random.nextDouble() * (1.0 - Double.MinPositiveValue) + Double.MinPositiveValue
      scale * sqrt(-2.0 * log(1.0 - u))
    }
  }

  /** Creates a Binomial distribution, representing the number of successes in a sequence of independent experiments.
    *
    * @param trials
    *   The number of trials (n).
    * @param probability
    *   The probability of success for each trial (p).
    * @return
    *   An `Uncertain[Int]` representing the binomial distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Binomial_distribution Binomial Distribution]]
    */
  def binomial(trials: Int, probability: Double): Uncertain[Int] =
    Uncertain(() => (0 until trials).count(_ => Random.nextDouble() < probability))

  /** Creates a Poisson distribution, expressing the probability of a given number of events occurring in a fixed interval.
    *
    * @param lambda
    *   The average number of events (λ).
    * @return
    *   An `Uncertain[Int]` representing the Poisson distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Poisson_distribution Poisson Distribution]]
    */
  def poisson(lambda: Double): Uncertain[Int] = Uncertain { () =>
    val L = exp(-lambda)
    var k = 0
    var p = 1.0
    while (p > L) {
      k += 1
      p *= Random.nextDouble()
    }
    k - 1
  }
}

// =================================================================================================
// Extensions
// =================================================================================================

/** Extension methods for equality comparisons on `Uncertain[T]`. */
extension [T](uncertain: Uncertain[T]) {

  /** Performs a sample-wise equality comparison, preserving correlation.
    *
    * @example
    *   {{{
    * val x = Uncertain.bernoulli(0.5)
    * val areSame = x === x // always true
    *   }}}
    */
  def ===(other: Uncertain[T]): Uncertain[Boolean] = for {
    a <- uncertain
    b <- other
  } yield a == b

  /** Performs a sample-wise inequality comparison, preserving correlation. */
  def !==(other: Uncertain[T]): Uncertain[Boolean] = for {
    a <- uncertain
    b <- other
  } yield a != b
}

/** Extension methods for ordered comparisons on `Uncertain[T]`. */
extension [T](lhs: Uncertain[T])(using ord: Ordering[T]) {

  // -- comparisons between uncertain and a fixed value --

  def gt(value: T): Uncertain[Boolean]  = lhs.map(a => ord.gt(a, value))
  def lt(value: T): Uncertain[Boolean]  = lhs.map(a => ord.lt(a, value))
  def gte(value: T): Uncertain[Boolean] = lhs.map(a => ord.gteq(a, value))
  def lte(value: T): Uncertain[Boolean] = lhs.map(a => ord.lteq(a, value))
  def >(value: T): Uncertain[Boolean]   = gt(value)
  def <(value: T): Uncertain[Boolean]   = lt(value)
  def >=(value: T): Uncertain[Boolean]  = gte(value)
  def <=(value: T): Uncertain[Boolean]  = lte(value)

  // -- comparisons with other Uncertain instances --
  def gt(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.gt(lhsSample, rhsSample)

  def lt(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.lt(lhsSample, rhsSample)

  def gte(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.gteq(lhsSample, rhsSample)

  def lte(other: Uncertain[T]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- other
  } yield ord.lteq(lhsSample, rhsSample)

  def >(other: Uncertain[T]): Uncertain[Boolean]  = gt(other)
  def <(other: Uncertain[T]): Uncertain[Boolean]  = lt(other)
  def >=(other: Uncertain[T]): Uncertain[Boolean] = gte(other)
  def <=(other: Uncertain[T]): Uncertain[Boolean] = lte(other)

}

/** Extension methods for fractional operations on `Uncertain[T]`. */
extension [T](lhs: Uncertain[T])(using frac: Fractional[T]) {

  def /(rhs: T): Uncertain[T] = lhs.map(a => frac.div(a, rhs))

  def /(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield frac.div(lhsSample, rhsSample)

}

/** Extension methods for Boolean operations and hypothesis testing on `Uncertain[Boolean]`. */
extension (lhs: Uncertain[Boolean]) {

  def unary_! : Uncertain[Boolean] = lhs.map(!_)

  def &&(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield lhsSample && rhsSample

  /** Performs a sample-wise logical OR on two uncertain booleans, preserving correlation. */
  def ||(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield lhsSample || rhsSample

  /** Evaluates if the probability of this uncertain boolean being `true` exceeds a given threshold. This is the primary mechanism for uncertain conditionals.
    * It uses SPRT for efficiency.
    *
    * @example
    *   {{{
    * val speed = Uncertain.normal(65, 5)
    * val isSpeeding = speed > 60
    * // Test the hypothesis: Is the probability of speeding > 80%?
    * if (isSpeeding.probability(exceeds = 0.8)) {
    * println("There is a high probability of speeding.")
    * }
    *   }}}
    * @param exceeds
    *   The probability threshold to test against (e.g., 0.9 for 90%).
    * @param confidenceLevel
    *   The desired statistical confidence for the decision (e.g., 0.95 for 95% confidence).
    * @param maxSamples
    *   A safeguard to prevent infinite sampling if a decision cannot be reached.
    * @return
    *   `true` if we are confident the probability exceeds the threshold, `false` otherwise.
    * @see
    *   [[https://en.wikipedia.org/wiki/Hypothesis_testing Hypothesis Testing]]
    */
  def probability(exceeds: Double, confidenceLevel: Double = 0.95, maxSamples: Int = 10000): Boolean = {
    val result = evaluateHypothesis(exceeds, confidenceLevel, maxSamples)
    result.decision
  }

  /** A convenient shorthand for `probability(exceeds = 0.5)`. Determines if the uncertain boolean is more likely `true` than `false`.
    *
    * @example
    *   {{{
    * val speed = Uncertain.normal(65, 10)
    * if ((speed > 68).isProbable()) {
    * println("It is more likely than not that the speed is over 68.")
    * }
    *   }}}
    */
  def isProbable(confidenceLevel: Double = 0.95): Boolean =
    probability(0.5, confidenceLevel)

  /** Performs a Sequential Probability Ratio Test (SPRT) to test the hypothesis that P(true) > `threshold`.
    *
    * @param threshold
    *   The probability threshold for the hypothesis (H1: p > threshold, H0: p <= threshold).
    * @param confidenceLevel
    *   The desired confidence level (1 - α).
    * @param maxSamples
    *   The maximum number of samples to draw before making a default decision.
    * @param epsilon
    *   The indifference region around the threshold.
    * @param alpha
    *   The Type I error rate (false positive). Defaults to `1.0 - confidenceLevel`.
    * @param beta
    *   The Type II error rate (false negative). Defaults to `alpha`.
    * @param batchSize
    *   The number of samples to draw in each iteration.
    * @return
    *   A `HypothesisResult` containing the decision, estimated probability, and samples used.
    * @see
    *   [[https://en.wikipedia.org/wiki/Sequential_probability_ratio_test Sequential Probability Ratio Test]]
    */
  def evaluateHypothesis(
    threshold: Double,
    confidenceLevel: Double,
    maxSamples: Int,
    epsilon: Double = 0.05,
    alpha: Option[Double] = None,
    beta: Option[Double] = None,
    batchSize: Int = 10
  ): HypothesisResult = {
    val alphaError = alpha.getOrElse(1.0 - confidenceLevel)
    val betaError  = beta.getOrElse(alphaError)
    val A          = log(betaError / (1.0 - alphaError))
    val B          = log((1.0 - betaError) / alphaError)
    val p0         = math.max(0.0, math.min(1.0, threshold - epsilon))
    val p1         = math.max(0.0, math.min(1.0, threshold + epsilon))
    var successes  = 0
    var samples    = 0
    while (samples < maxSamples) {
      val currentBatchSize = math.min(batchSize, maxSamples - samples)
      val batchSuccesses   = (0 until currentBatchSize).count(_ => lhs.sample())
      successes += batchSuccesses
      samples += currentBatchSize
      val n                = samples
      val x                = successes
      val p0c              = math.max(1e-10, math.min(1.0 - 1e-10, p0))
      val p1c              = math.max(1e-10, math.min(1.0 - 1e-10, p1))
      val llr              = x * log(p1c / p0c) + (n - x) * log((1.0 - p1c) / (1.0 - p0c))
      if (llr <= A) {
        val p = successes.toDouble / samples
        return HypothesisResult(false, p, confidenceLevel, samples)
      } else if (llr >= B) {
        val p = successes.toDouble / samples
        return HypothesisResult(true, p, confidenceLevel, samples)
      }
    }
    val finalP     = successes.toDouble / samples
    HypothesisResult(finalP > threshold, finalP, confidenceLevel, samples)
  }
}

/** Extension methods for calculating summary statistics on `Uncertain[T]`. */
extension [T](uncertain: Uncertain[T]) {

  /** Estimates the mode of the distribution by sampling.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation.
    * @return
    *   An `Option[T]` containing the most frequent sample, or `None` if no samples are drawn.
    * @see
    *   [[https://en.wikipedia.org/wiki/Mode_(statistics) Mode (statistics)]]
    */
  def mode(sampleCount: Int = 1000): Option[T] = {
    val samples = uncertain.take(sampleCount)
    if (samples.isEmpty) None
    else {
      val counts = samples.groupBy(identity).view.mapValues(_.length)
      counts.maxByOption(_._2).map(_._1)
    }
  }

  /** Generates a frequency map (histogram) of sample values.
    *
    * @param sampleCount
    *   The number of samples to use.
    * @return
    *   A `Map[T, Int]` where keys are sample values and values are their frequencies.
    * @see
    *   [[https://en.wikipedia.org/wiki/Histogram Histogram]]
    */
  def histogram(sampleCount: Int = 1000): Map[T, Int] = {
    val samples = uncertain.take(sampleCount)
    samples.groupBy(identity).view.mapValues(_.length).toMap
  }

  /** Estimates the Shannon entropy of the distribution by sampling.
    *
    * @param sampleCount
    *   The number of samples to use.
    * @return
    *   The estimated entropy in bits.
    * @see
    *   [[https://en.wikipedia.org/wiki/Entropy_(information_theory) Entropy (information theory)]]
    */
  def entropy(sampleCount: Int = 1000): Double = {
    val samples = uncertain.take(sampleCount)
    val counts  = samples.groupBy(identity).values.map(_.length)
    val total   = samples.length.toDouble
    counts.foldLeft(0.0) { (acc, count) =>
      val p = count / total
      acc - (if (p > 0) p * log(p) / log(2) else 0)
    }
  }
}

/** Extension methods for numeric operations and statistics on `Uncertain[T]`. */
extension [T](lhs: Uncertain[T])(using num: Numeric[T]) {

  /** Estimates the expected value (mean) of the distribution by sampling.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation.
    * @return
    *   The estimated mean as a `Double`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Expected_value Expected Value]]
    */
  def expectedValue(sampleCount: Int = 1000): Double = {
    val samples = lhs.take(sampleCount)
    if (samples.isEmpty) return 0.0
    val sum     = samples.foldLeft(num.zero)((acc, x) => num.plus(acc, x))
    num.toDouble(sum) / samples.length
  }

  /** An alias for `expectedValue`. */
  def mean(sampleCount: Int = 1000): Double = expectedValue(sampleCount)

  /** Estimates the standard deviation of the distribution by sampling.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation.
    * @return
    *   The estimated standard deviation as a `Double`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Standard_deviation Standard Deviation]]
    */
  def standardDeviation(sampleCount: Int = 1000): Double = {
    // TODO instead of NaN, requre >= 2 samples
    if (sampleCount < 2) return Double.NaN // Sample variance is undefined for n < 2

    val samples = lhs.take(sampleCount)

    // 1. Calculate the mean from THIS sample set
    val sum     = samples.foldLeft(num.zero)((acc, x) => num.plus(acc, x))
    val meanVal = num.toDouble(sum) / samples.length

    // 2. Calculate variance using the same samples and the calculated mean
    val variance = samples.foldLeft(0.0) { (acc, sample) =>
      val diff = num.toDouble(sample) - meanVal
      acc + diff * diff
    } / (samples.length - 1) // Use n-1 for sample standard deviation

    sqrt(variance)
  }

  /** Performs sample-wise addition, preserving correlation. */
  def +(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield num.plus(lhsSample, rhsSample)

  /** Performs sample-wise subtraction, preserving correlation. */
  def -(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield num.minus(lhsSample, rhsSample)

  /** Performs sample-wise multiplication, preserving correlation. */
  def *(rhs: Uncertain[T]): Uncertain[T] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield num.times(lhsSample, rhsSample)

  /** Adds a constant value to an uncertain value. */
  def +(rhs: T): Uncertain[T] = lhs.map(l => num.plus(l, rhs))

  /** Subtracts a constant value from an uncertain value. */
  def -(rhs: T): Uncertain[T] = lhs.map(l => num.minus(l, rhs))

  /** Multiplies an uncertain value by a constant. */
  def *(rhs: T): Uncertain[T] = lhs.map(l => num.times(l, rhs))
}

/** Extension methods for order-based statistics on `Uncertain[T]`. */
extension [T](uncertain: Uncertain[T])(using ord: Ordering[T]) {

  /** Estimates the confidence interval of the distribution using the percentile method on samples.
    *
    * @param confidence
    *   The desired confidence level (e.g., 0.95 for a 95% CI).
    * @param sampleCount
    *   The number of samples to use for the estimation.
    * @return
    *   A tuple `(lowerBound, upperBound)`. Returns `(None, None)` if not enough samples can be drawn.
    * @see
    *   [[https://en.wikipedia.org/wiki/Confidence_interval Confidence Interval]]
    */
  def confidenceInterval(confidence: Double = 0.95, sampleCount: Int = 1000): Option[(T, T)] = {
    if (sampleCount <= 0) return None
    val samples    = uncertain.take(sampleCount).sorted
    if (samples.isEmpty) return None
    val alpha      = 1.0 - confidence
    val lowerIndex = ((alpha / 2.0) * samples.length).toInt
    val upperIndex = ((1.0 - alpha / 2.0) * samples.length).toInt - 1
    val safeLower  = math.max(0, math.min(lowerIndex, samples.length - 1))
    val safeUpper  = math.max(0, math.min(upperIndex, samples.length - 1))
    Some((samples(safeLower), samples(safeUpper)))
  }

  /** Estimates the Cumulative Distribution Function (CDF) at a given value. This is the probability that a sample will be less than or equal to `value`.
    *
    * @param value
    *   The value at which to evaluate the CDF.
    * @param sampleCount
    *   The number of samples to use for the estimation.
    * @return
    *   The estimated probability P(X <= value).
    * @see
    *   [[https://en.wikipedia.org/wiki/Cumulative_distribution_function Cumulative Distribution Function]]
    */
  def cdf(value: T, sampleCount: Int = 1000): Double = {
    if (sampleCount <= 0) return 0.0
    val samples   = uncertain.take(sampleCount)
    val successes = samples.count(ord.lteq(_, value))
    successes.toDouble / sampleCount
  }
}

// =================================================================================================
// Helper Classes and Objects
// =================================================================================================

/** Holds the result of a hypothesis test performed by `evaluateHypothesis`.
  *
  * @param decision
  *   The outcome of the test (`true` for accepting H1, `false` for accepting H0).
  * @param probability
  *   The estimated probability P(true) based on the samples drawn.
  * @param confidenceLevel
  *   The confidence level used for the test.
  * @param samplesUsed
  *   The number of samples required to reach a decision.
  */
final case class HypothesisResult(decision: Boolean, probability: Double, confidenceLevel: Double, samplesUsed: Int)

/** A context for a single top-level `sample()` call, used to memoize values from leaf nodes. This is the key to preserving correlation. When an expression like
  * `x - x` is evaluated, the `SampleContext` ensures that the node for `x` is only sampled once, and its value is reused.
  */
final class SampleContext {
  private val memoizedValues: mutable.Map[UUID, Any] = mutable.Map.empty

  def getValue[T](id: UUID): Option[T]      = memoizedValues.get(id).map(_.asInstanceOf[T])
  def setValue[T](id: UUID, value: T): Unit = memoizedValues(id) = value
}

/** An example application demonstrating the library's features. */
object UncertainExample extends App {
  // Showcasing the fixed correlation for map
  println("--- Demonstrating map correlation fix ---")
  val x: Uncertain[Double] = Uncertain.normal(10, 2)
  val y: Uncertain[Double] = x.map(_ * 2) // y is now properly correlated with x
  // z should always be exactly 20, because the same sample from x is used to derive y.
  val z: Uncertain[Double] = y - y + 20.0
  println(s"10 samples of (x*2 - x*2 + 20): ${z.take(10)}")

  // Basic distributions
  println("\n--- Basic Example ---")
  val speed    = Uncertain.normal(5.0, 2.0)
  val distance = Uncertain.uniform(100.0, 200.0)

  // Arithmetic operations are composed into a computation graph.
  val time = distance / speed

  // Conditional probability using hypothesis testing with symbolic operators.
  if ((speed > 4.0).probability(exceeds = 0.9)) {
    println("90% confident you're going fast")
  }

  // Implicit conditional: a shorthand for checking if P(condition) > 0.5.
  if ((speed > 4.0).isProbable()) {
    println("More likely than not you're going fast")
  }

  // Statistics
  println(f"Expected time: ${time.expectedValue(5000)}%.2f")
  println(f"Speed std dev: ${speed.standardDeviation()}%.2f")
  speed.confidenceInterval().foreach { case (low, high) =>
    println(f"Speed 95%% Confidence Interval: ($low%.2f, $high%.2f)")
  }
}
