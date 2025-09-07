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
  * // Define an uncertain speed with a normal distribution (mean 5.0, std dev Two)
  * val speed = Uncertain.normal(5.0, Two)
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
    *
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
    * val speed = Uncertain.normal(5.0, Two)
    * val firstTenSamples = speed.iterator.take(10).toList
    *   }}}
    */
  def iterator: Iterator[T] = Iterator.continually(sample())

  /** Collects a specified number of samples into a list.
    *
    * @param n
    *   The number of samples to take. Must be non-negative.
    * @return
    *   A `List[T]` containing `n` samples.
    */
  def take(n: Int): List[T] = {
    require(n >= 0, "Number of samples to take must be non-negative.")
    iterator.take(n).toList
  }
}

// precise doubles for common values utilized throughout the library
private[this] val Zero: Double     = Numeric[Double].fromInt(0)
private[this] val One: Double      = Numeric[Double].fromInt(1)
private[this] val Two: Double      = Numeric[Double].fromInt(2)
private[this] val MinusTwo: Double = Numeric[Double].fromInt(-2)

// =================================================================================================
// Computation Graph Nodes
// =================================================================================================

/** A node in the computation graph, representing an operation or a source of uncertainty.
  *
  * This is the fundamental building block for lazy, correlated, uncertain values. Each `Uncertain` instance contains a reference to a `ComputationNode` that
  * represents its structure. The graph is evaluated **only** when a sample is requested.
  */
sealed private[uncertaintee] trait ComputationNode[+T] {

  /** Evaluates this node and any dependent nodes within a given sampling context.
    *
    * This is the core method that executes the computation graph.
    *
    * The `SampleContext` is passed down through the graph to ensure that any shared leaf nodes are sampled only once, thus preserving correlation.
    *
    * @param context
    *   The context for the current sampling operation (used for memoization, to preserve correlation).
    * @return
    *   The resulting sampled value of type `T`.
    */
  def evaluate(context: SampleContext = new SampleContext): T
}

/** A leaf node in the computation graph, representing an original source of uncertainty.
  *
  * This node holds a sampling function (`sampler`) and a unique identifier (`id`).
  *
  * It is the primary mechanism for preserving correlation.
  *
  * When evaluated, it first checks the `SampleContext` to see if it has already been sampled for the current evaluation run.
  *
  * @param id
  *   A unique identifier for this specific source of uncertainty.
  * @param sampler
  *   The function that generates a single random sample (e.g., from a normal distribution) which will be memoized
  * @tparam T
  *   The type of the value being sampled.
  */
final private[uncertaintee] case class ComputationLeaf[T](id: UUID, sampler: () => T) extends ComputationNode[T] {
  override def evaluate(context: SampleContext): T =
    context
      .getValue[T](id)
      .getOrElse {
        val value = sampler()
        context.setValue(id, value) // Store the value for this context
        value
      }
}

/** A unary operation node, used to implement `.map()`.
  *
  * This node represents the application of a simple, deterministic function to the result of another node (`source`).
  *
  * It acts as a transformation step in the computation graph.
  *
  * @param source
  *   The upstream node to evaluate first.
  * @param operation
  *   The function to apply to the result of the source node.
  * @tparam A
  *   The input type from the source node.
  * @tparam B
  *   The output type after the operation is applied.
  */
final private[uncertaintee] case class ComputationUnaryOperation[A, +B](source: ComputationNode[A], operation: A => B) extends ComputationNode[B] {
  override def evaluate(context: SampleContext): B = operation(source.evaluate(context))
}

/** A chaining operation node, used to implement `.flatMap()`.
  *
  * This node is used for probabilistic dependencies, where the structure of a subsequent computation depends on the sampled value of a prior one.
  *
  * It evaluates its `source` node, uses the result to generate a new `Uncertain` value (and thus a new computation subgraph), and then evaluates that new
  * subgraph within the *same* `SampleContext`.
  *
  * This ensures that correlations are preserved across the `flatMap` boundary.
  *
  * @param source
  *   The upstream node to evaluate first.
  * @param f
  *   The function that takes the result of the source node and returns the next `Uncertain` value in the chain.
  * @tparam A
  *   The input type from the source node.
  * @tparam B
  *   The output type of the entire chained operation.
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
  def apply[T](sampler: () => T)(using random: Random = new Random()): Uncertain[T] = {
    val id = UUID.nameUUIDFromBytes(random.nextBytes(16))
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

  /** Creates a mixture model from a list of uncertain components and weights.
    *
    * [[categorical]] chooses from a set of plain values, while [[mixture]] chooses from a set of other Uncertain distributions.
    *
    * In essence, mixture is a higher-level concept that is built using categorical and flatMap to create a new distribution by blending others.
    *
    * This implementation correctly preserves correlations by using a categorical distribution and flatMap, ensuring that component selection and evaluation
    * happen within the same computation graph.
    *
    * @param components
    *   A map of `Uncertain` distributions to their respective weights. The map must not be empty, and all weights must be non-negative and sum to a positive
    *   number.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   A new `Uncertain[T]` representing the mixture distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Mixture_model Mixture Model]]
    */
  def mixture[T](components: Map[Uncertain[T], Double])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "At least one component is required for a mixture model.")
    require(components.values.forall(_ >= 0), "Mixture weights cannot be negative.")
    val totalWeight = components.values.sum
    require(totalWeight > 0, "Mixture weights must sum to a positive number.")

    if (components.size == 1) return components.head._1
    categorical(components)(using random).flatMap(identity)
  }

  /** Creates a mixture model from a list of uncertain components with equal weighting.
    *
    * @param components
    *   A list of `Uncertain` distributions to mix. Must not be empty.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   A new `Uncertain[T]` representing the mixture distribution.
    * @see
    *   [[mixture]]
    */
  def equalMixture[T](components: List[Uncertain[T]])(using random: Random = new Random()): Uncertain[T] = {
    require(components.nonEmpty, "At least one component is required for an equal mixture model.")
    mixture(components.map((x: Uncertain[T]) => x -> One).toMap)(using random)
  }

  /** Creates an uncertain distribution by sampling uniformly from a list of observed data.
    *
    * @example
    *   {{{
    * val surveyResults = List(3, 5, 4, 5, 5, 2)
    * val rating = Uncertain.empirical(surveyResults)
    *   }}}
    * @param data
    *   The list of data points to sample from. Must not be empty.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[T]` representing the empirical distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Empirical_distribution_function Empirical Distribution Function]]
    */
  def empirical[T](data: List[T])(using random: Random = new Random()): Uncertain[T] = {
    require(data.nonEmpty, "At least one data point is required for an empirical distribution.")
    Uncertain(() => data(random.nextInt(data.length)))
  }

  /** Creates a categorical distribution from a map of outcomes to their probabilities. The probabilities will be normalized to sum to 1.
    *
    * @example
    *   {{{
    * // The following probabilities (0.7, 0.2, 0.1) sum to 1.0
    * val weather = Uncertain.categorical(Map("Sunny" -> 0.7, "Cloudy" -> 0.2, "Rain" -> 0.1))
    *
    * // The following weights (7, 2, 1) will be normalized to the probabilities above
    * val sameWeather = Uncertain.categorical(Map("Sunny" -> 7.0, "Cloudy" -> 2.0, "Rain" -> 1.0))
    *   }}}
    * @param outcomes
    *   A map where keys are outcomes and values are their probabilities (weights). The map must not be empty, all weights must be non-negative and sum to a
    *   positive number.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An Uncertain[T]
    * @see
    *   [[https://en.wikipedia.org/wiki/Categorical_distribution Categorical Distribution]]
    */
  def categorical[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] = {
    require(outcomes.nonEmpty, "Categorical distribution must have at least one outcome")
    require(outcomes.forall(_._2 >= 0), "Probabilities cannot be negative")
    val totalWeight        = outcomes.values.sum
    require(totalWeight > 0, "Probabilities must sum to a positive number")
    val normalizedOutcomes = outcomes.map((outcome, weight) => (outcome, weight / totalWeight))
    val cumulativeProbs    = normalizedOutcomes.values.scanLeft(Zero)(_ + _).tail
    val paired             = normalizedOutcomes.keys.zip(cumulativeProbs)

    val sampler: () => T = () => {
      val u = random.nextDouble()
      paired.find { case (_, cumProb) => u <= cumProb }.get._1
    }

    Uncertain(sampler)
  }

  /** Creates a Normal (Gaussian) distribution.
    *
    * @param mean
    *   The mean (μ) of the distribution.
    * @param standardDeviation
    *   The standard deviation (σ) of the distribution. Must be non-negative.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Double]` representing the normal distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Normal_distribution Normal Distribution]]
    */
  def normal(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(standardDeviation >= 0, "Standard deviation cannot be negative.")
    Uncertain { () =>
      if (standardDeviation == 0) mean
      else {
        // Box-Muller transform for generating standard normal samples
        var u1 = Zero
        while (u1 == Zero) u1 = random.nextDouble() // Avoid log(0) which is -Infinity
        val u2 = random.nextDouble()
        val z0 = sqrt(MinusTwo * log(u1)) * cos(Two * Pi * u2)
        mean + standardDeviation * z0
      }
    }
  }

  /** Creates a continuous Uniform distribution.
    *
    * @param min
    *   The lower bound of the distribution (inclusive).
    * @param max
    *   The upper bound of the distribution (exclusive). Must be >= min.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Double]` representing the uniform distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Continuous_uniform_distribution Continuous Uniform Distribution]]
    */
  def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(max >= min, s"max ($max) must be greater than or equal to min ($min).")
    Uncertain(() => min + random.nextDouble() * (max - min))
  }

  /** Creates an Exponential distribution.
    *
    * @param rate
    *   The rate parameter (λ), which must be positive.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Double]` representing the exponential distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Exponential_distribution Exponential Distribution]]
    */
  def exponential(rate: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(rate > 0, "Rate (lambda) must be positive.")
    Uncertain { () =>
      var u = Zero
      while (u == Zero) u = random.nextDouble() // Avoid log(0) which is -Infinity
      -log(u) / rate
    }
  }

  /** Creates a Bernoulli distribution, which is a distribution of a single boolean trial.
    *
    * @param probability
    *   The probability of the outcome being `true`. Must be between 0.0 and 1.0.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Boolean]`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Bernoulli_distribution Bernoulli Distribution]]
    */
  def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] = {
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => random.nextDouble() < probability)
  }

  /** Creates a Kumaraswamy distribution.
    *
    * @param a
    *   The `a` shape parameter (> 0).
    * @param b
    *   The `b` shape parameter (> 0).
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Double]` representing the Kumaraswamy distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Kumaraswamy_distribution Kumaraswamy Distribution]]
    */
  def kumaraswamy(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(a > 0 && b > 0, "Kumaraswamy distribution parameters must be positive")
    val reciprocalA = One / a
    val reciprocalB = One / b
    Uncertain { () =>
      val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
      pow(One - pow(One - u, reciprocalB), reciprocalA)
    }
  }

  /** Creates a Rayleigh distribution.
    *
    * @param scale
    *   The scale parameter (σ > 0).
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Double]` representing the Rayleigh distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Rayleigh_distribution Rayleigh Distribution]]
    */
  def rayleigh(scale: Double)(using random: Random = new Random()): Uncertain[Double] = {
    require(scale > 0, "Rayleigh distribution scale parameter must be positive")
    Uncertain { () =>
      val u = random.nextDouble() * (One - Double.MinPositiveValue) + Double.MinPositiveValue
      scale * sqrt(-Two * log(One - u))
    }
  }

  /** Creates a Binomial distribution, representing the number of successes in a sequence of independent experiments.
    *
    * @param trials
    *   The number of trials (n), which must be non-negative.
    * @param probability
    *   The probability of success for each trial (p), which must be between 0.0 and 1.0.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Int]` representing the binomial distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Binomial_distribution Binomial Distribution]]
    */
  def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
    require(trials >= 0, "Number of trials cannot be negative.")
    require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
    Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
  }

  /** Creates a Poisson distribution, expressing the probability of a given number of events occurring in a fixed interval.
    *
    * @param lambda
    *   The average number of events (λ), which must be non-negative.
    * @param random
    *   The source of randomness, provided implicitly.
    * @return
    *   An `Uncertain[Int]` representing the Poisson distribution.
    * @see
    *   [[https://en.wikipedia.org/wiki/Poisson_distribution Poisson Distribution]]
    */
  def poisson(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
    require(lambda >= 0, "Lambda (rate) cannot be negative.")
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

  def ||(rhs: Uncertain[Boolean]): Uncertain[Boolean] = for {
    lhsSample <- lhs
    rhsSample <- rhs
  } yield lhsSample || rhsSample

  /** Evaluates if the probability of this uncertain boolean being `true` exceeds a given threshold.
    *
    * This method uses a Sequential Probability Ratio Test (SPRT) to efficiently test the one-sided hypothesis H₁: P(true) > threshold vs H₀: P(true) ≤
    * threshold.
    *
    * @example
    *   {{{
    *
    * val speed = Uncertain.normal(65, 5)
    * val isSpeeding = speed > 60
    * // Test: Is P(speeding) > 0.8 with 95% confidence?
    * if (isSpeeding.probability(exceeds = 0.8)) {
    *   println("High confidence that speeding probability exceeds 80%")
    * }
    *   }}}
    *
    * @param exceeds
    *   The probability threshold to test against. Must be between 0.0 and 1.0.
    * @param alpha
    *   Type I error rate (probability of false positive). Must be between 0.0 and 1.0.
    * @param beta
    *   Type II error rate (probability of false negative). Must be between 0.0 and 1.0.
    * @param delta
    *   The effect size - how much greater than threshold we want to detect. Defaults to 0.1 * (1 - threshold) for reasonable power.
    * @param maxSamples
    *   Maximum samples before fallback to fixed-sample test. Must be positive.
    * @return
    *   `true` if we reject H₀ in favor of H₁ (p > threshold), `false` otherwise.
    */
  def probability(
    exceeds: Double,
    alpha: Double = 0.05,
    beta: Double = 0.05,
    delta: Option[Double] = None,
    maxSamples: Int = 10_000
  ): Boolean = {
    require(exceeds >= 0 && exceeds <= 1, s"Threshold ($exceeds) must be between 0 and 1.")
    require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1 (exclusive).")
    require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1 (exclusive).")
    require(maxSamples > 0, "Maximum samples must be positive.")

    val effectSize = delta.getOrElse(math.max(0.01, 0.1 * (1.0 - exceeds)))
    require(exceeds + effectSize <= 1.0, s"Threshold + effect size (${exceeds + effectSize}) must be ≤ 1.0")

    val result = evaluateHypothesis(exceeds, alpha, beta, effectSize, maxSamples)
    result.decision
  }

  /** Convenient shorthand for `probability(exceeds = 0.5)` with default error rates. */
  def isProbable(alpha: Double = 0.05, beta: Double = 0.05): Boolean =
    probability(0.5, alpha, beta)

  /** Performs a Sequential Probability Ratio Test for H₁: p > p₀ vs H₀: p ≤ p₀.
    *
    * The test uses two composite hypotheses:
    *   - H₀: p ≤ p₀ (represented by p = p₀)
    *   - H₁: p > p₀ (represented by p = p₁ = p₀ + δ)
    *
    * The SPRT sequentially computes the likelihood ratio and stops when there's sufficient evidence for either hypothesis.
    *
    * @param threshold
    *   The null hypothesis probability (p₀)
    * @param alpha
    *   Type I error rate P(reject H₀ | H₀ true)
    * @param beta
    *   Type II error rate P(accept H₀ | H₁ true)
    * @param delta
    *   Effect size: p₁ = p₀ + δ
    * @param maxSamples
    *   Fallback to prevent infinite sampling
    * @return
    *   HypothesisResult with decision, estimated probability, and sample count
    */
  def evaluateHypothesis(
    threshold: Double,
    alpha: Double,
    beta: Double,
    delta: Double,
    maxSamples: Int
  ): HypothesisResult = {
    require(threshold >= 0 && threshold <= 1, s"Threshold ($threshold) must be between 0 and 1.")
    require(alpha > 0 && alpha < 1, s"Alpha ($alpha) must be between 0 and 1 (exclusive).")
    require(beta > 0 && beta < 1, s"Beta ($beta) must be between 0 and 1 (exclusive).")
    require(delta > 0, s"Effect size delta ($delta) must be positive.")
    require(threshold + delta <= 1.0, s"Threshold + delta (${threshold + delta}) must be ≤ 1.0")
    require(maxSamples > 0, "Maximum samples must be positive.")

    val p0 = threshold
    val p1 = threshold + delta

    // SPRT decision boundaries
    val A = log(beta / (1.0 - alpha)) // Accept H₀ if LLR ≤ A
    val B = log((1.0 - beta) / alpha) // Accept H₁ if LLR ≥ B

    var successes = 0
    var samples   = 0

    while (samples < maxSamples) {
      // Draw a sample
      val sample = lhs.sample()
      if (sample) successes += 1
      samples += 1

      // Compute log-likelihood ratio
      // LLR = log(L(p₁)/L(p₀)) = Σᵢ log(p₁/p₀) + Σᵢ log((1-p₁)/(1-p₀))
      //     = x·log(p₁/p₀) + (n-x)·log((1-p₁)/(1-p₀))
      val x = successes
      val n = samples

      val llr = if (p0 > 0 && p0 < 1 && p1 > 0 && p1 < 1) {
        x * log(p1 / p0) + (n - x) * log((1.0 - p1) / (1.0 - p0))
      } else {
        // Handle boundary cases
        if (p0 == 0.0) {
          if (x > 0) Double.PositiveInfinity else (n - x) * log(1.0 - p1)
        } else if (p0 == 1.0) {
          if (x < n) Double.NegativeInfinity else x * log(p1)
        } else if (p1 == 0.0) {
          if (x > 0) Double.NegativeInfinity else (n - x) * log(1.0 - p0)
        } else { // p1 == 1.0
          if (x < n) Double.PositiveInfinity else x * log(1.0 / p0)
        }
      }

      // Make decision based on boundaries
      if (llr <= A) {
        // Accept H₀: p ≤ threshold
        val estimatedP = successes.toDouble / samples
        return HypothesisResult(
          decision = false,
          probability = estimatedP,
          confidenceLevel = 1.0 - alpha,
          samplesUsed = samples
        )
      } else if (llr >= B) {
        // Accept H₁: p > threshold
        val estimatedP = successes.toDouble / samples
        return HypothesisResult(
          decision = true,
          probability = estimatedP,
          confidenceLevel = 1.0 - alpha,
          samplesUsed = samples
        )
      }

      // Continue sampling if A < LLR < B
    }

    // Fallback: use normal approximation if max samples reached
    val pHat = successes.toDouble / samples
    val se   = sqrt(pHat * (1.0 - pHat) / samples)

    if (se > 0) {
      // One-sided z-test: H₀: p ≤ threshold vs H₁: p > threshold
      val z         = (pHat - threshold) / se
      val criticalZ = approximateNormalQuantile(1.0 - alpha)
      val decision  = z > criticalZ

      HypothesisResult(
        decision = decision,
        probability = pHat,
        confidenceLevel = 1.0 - alpha,
        samplesUsed = samples
      )
    } else {
      // Degenerate case: all samples identical
      HypothesisResult(
        decision = pHat > threshold,
        probability = pHat,
        confidenceLevel = 1.0 - alpha,
        samplesUsed = samples
      )
    }
  }

  /** Approximates the quantile function of standard normal distribution using Beasley-Springer-Moro algorithm */
  private def approximateNormalQuantile(p: Double): Double = {
    require(p > 0 && p < 1, "Probability must be between 0 and 1 (exclusive)")

    if (p < 0.5) {
      -approximateNormalQuantile(1.0 - p)
    } else {
      val t  = sqrt(-2.0 * log(1.0 - p))
      val c0 = 2.515517
      val c1 = 0.802853
      val c2 = 0.010328
      val d1 = 1.432788
      val d2 = 0.189269
      val d3 = 0.001308

      t - (c0 + c1 * t + c2 * t * t) / (1.0 + d1 * t + d2 * t * t + d3 * t * t * t)
    }
  }
}

/** Extension methods for calculating summary statistics on `Uncertain[T]`. */
extension [T](uncertain: Uncertain[T]) {

  /** Estimates the mode of the distribution by sampling.
    *
    * This method is intended for discrete distributions (e.g., `Int`, `Boolean`, `String`). It finds the most frequently occurring sample; for continuous
    * distributions (like `Uncertain.normal` or `Uncertain.uniform`), where each sample is likely to be unique, this method may produce a statistically
    * meaningless result.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation. Must be positive.
    * @return
    *   The most frequent sample.
    * @see
    *   [[https://en.wikipedia.org/wiki/Mode_(statistics) Mode (statistics)]]
    */
  def mode(sampleCount: Int = 1000): T = {
    require(sampleCount > 0, "Sample count must be positive.")
    uncertain
      .take(sampleCount)
      .groupBy(identity)
      .view
      .maxBy((_, elems) => elems.length)
      ._1
  }

  /** Generates a frequency map (histogram) of sample values.
    *
    * @param sampleCount
    *   The number of samples to use. Must be positive.
    * @return
    *   A `Map[T, Int]` where keys are sample values and values are their frequencies.
    * @see
    *   [[https://en.wikipedia.org/wiki/Histogram Histogram]]
    */
  def histogram(sampleCount: Int = 1000): Map[T, Int] = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples = uncertain.take(sampleCount)
    samples.groupBy(identity).view.mapValues(_.length).toMap
  }

  /** Estimates the Shannon entropy of the distribution by sampling.
    *
    * @param sampleCount
    *   The number of samples to use. Must be positive.
    * @return
    *   The estimated entropy in bits.
    * @see
    *   [[https://en.wikipedia.org/wiki/Entropy_(information_theory) Entropy (information theory)]]
    */
  def entropy(sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive.")
    val samples = uncertain.take(sampleCount)
    val counts  = samples.groupBy(identity).values.map(_.length)
    val total   = samples.length.toDouble
    counts.foldLeft(Zero) { (acc, count) =>
      val p = count / total
      acc - (if (p > 0) p * log(p) / log(2) else 0)
    }
  }
}

/** Numeric instance for Boolean to support statistical operations
  *
  * @note
  *   dangerous to use in any generic context that assumes standard arithmetic laws!
  *
  * Maps Boolean values to their standard numeric representation:
  *   - case true -> 1
  *   - case false -> 0
  *
  * This enables statistical methods like mean() and expectedValue() on Uncertain[Boolean].
  *
  * This is not a true numeric algebra, because Boolean algebra breaks on addition/subtraction/etc.
  *
  * If we wanted plus to be arithmetic addition: true would be 1, false would be 0. Then the operations would look like this:
  *
  *   - false + false -> 0 + 0 = 0
  *   - false * true + false -> 1 + 0 = 1
  *   - true * true + true -> 1 + 1 = 2 -> Nonsensical
  *
  * Logic utilising this instance should *mainly* convert to/from Boolean values, and not perform arithmetic operations.
  */
private[this] given whiteLieBooleanNumeric: Numeric[Boolean] = new Numeric[Boolean] {
  override def compare(x: Boolean, y: Boolean): Int      = x.compareTo(y)
  override def fromInt(x: Int): Boolean                  = x != 0
  override def minus(x: Boolean, y: Boolean): Boolean    = x ^ y  // Logical XOR
  override def negate(x: Boolean): Boolean               = !x
  override def plus(x: Boolean, y: Boolean): Boolean     = x || y // Logical OR
  override def times(x: Boolean, y: Boolean): Boolean    = x && y // Logical AND
  override def toDouble(x: Boolean): Double              = if (x) Numeric[Double].one else Numeric[Double].zero
  override def toFloat(x: Boolean): Float                = if (x) Numeric[Float].one else Numeric[Float].zero
  override def toInt(x: Boolean): Int                    = if (x) Numeric[Int].one else Numeric[Int].zero
  override def toLong(x: Boolean): Long                  = if (x) Numeric[Long].one else Numeric[Long].zero
  override def parseString(str: String): Option[Boolean] = str.toBooleanOption
}

/** Extension methods for numeric operations and statistics on `Uncertain[T]`. */
extension [T](lhs: Uncertain[T])(using num: Numeric[T]) {

  /** Estimates the expected value (mean) of the distribution by sampling.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation. Must be positive.
    * @return
    *   The estimated mean as a `Double`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Expected_value Expected Value]]
    */
  def expectedValue(sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive to calculate expected value.")
    val samples = lhs.take(sampleCount).map(num.toDouble)
    // Convert each sample to a Double *before* summing to ensure correct arithmetic,
    // avoiding the ambiguous `Numeric[Boolean].plus`
    val sum     = samples.sum
    sum / samples.length.toDouble
  }

  /** An alias for `expectedValue`. */
  def mean(sampleCount: Int = 1000): Double = expectedValue(sampleCount)

  /** Estimates the population standard deviation of the distribution by sampling.
    *
    * This method assumes the drawn samples represent the entire population
    *
    * If you are estimating the standard deviation of a larger population from a smaller sample, use `sampleStandardDeviation`.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation. Must be at least 1.
    * @return
    *   The estimated population standard deviation as a `Double`
    * @see
    *   [[https://en.wikipedia.org/wiki/Standard_deviation#Population_standard_deviation_of_a_finite_population Population Standard Deviation]]
    * @see
    *   `sampleStandardDeviation`
    */
  def populationStandardDeviation(sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive to calculate population standard deviation.")
    val samples  = lhs.take(sampleCount).map(num.toDouble)
    val meanVal  = samples.sum / samples.length
    val variance = samples.foldLeft(Zero) { (acc, sample) =>
      val diff = sample - meanVal
      acc + diff * diff
    } / samples.length // Use n for population standard deviation

    sqrt(variance)
  }

  /** Estimates the sample standard deviation of the distribution.
    *
    * This method applies Bessel's correction, using a denominator of `n-1` (where n is the sample count), which provides an unbiased estimate of the variance
    * of a larger, underlying population.
    *
    * @param sampleCount
    *   The number of samples to use for the estimation. Must be at least 2.
    * @return
    *   The estimated sample standard deviation as a `Double`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Bessel's_correction Bessel's Correction]]
    * @see
    *   `populationStandardDeviation`
    */
  def sampleStandardDeviation(sampleCount: Int = 1000): Double = {
    require(sampleCount >= 2, "Sample count must be at least 2 to calculate sample standard deviation.")
    val samples  = lhs.take(sampleCount).map(num.toDouble)
    val meanVal  = samples.sum / samples.length
    val variance = samples.foldLeft(Zero) { (acc, sample) =>
      val diff = sample - meanVal
      acc + diff * diff
    } / (samples.length - 1) // Use n-1 for sample standard deviation

    sqrt(variance)
  }

  /** Estimates the standard deviation of the distribution by sampling. It delegates to [[sampleStandardDeviation]] (as opposed to
    * [[populationStandardDeviation]]
    *
    * @param sampleCount
    *   The number of samples to use for the estimation.
    * @return
    *   The estimated standard deviation as a `Double`.
    * @see
    *   `sampleStandardDeviation`
    * @see
    *   `populationStandardDeviation`
    * @see
    *   [[https://en.wikipedia.org/wiki/Standard_deviation Standard Deviation]]
    */
  def standardDeviation(sampleCount: Int = 1000): Double = sampleStandardDeviation(sampleCount)

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
    *   The desired confidence level (e.g., 0.95 for a 95% CI). Must be between 0 and 1 (exclusive).
    * @param sampleCount
    *   The number of samples to use for the estimation. Must be positive.
    * @return
    *   A tuple `(lowerBound, upperBound)`.
    * @see
    *   [[https://en.wikipedia.org/wiki/Confidence_interval Confidence Interval]]
    */
  def confidenceInterval(confidence: Double = 0.95, sampleCount: Int = 1000): (T, T) = {
    require(confidence > 0 && confidence < 1, "Confidence must be between 0 and 1 (exclusive).")
    require(sampleCount > 0, "Sample count must be positive.")
    val samples    = uncertain.take(sampleCount).sorted
    val alpha      = One - confidence
    val lowerIndex = ((alpha / Two) * samples.length).toInt
    val upperIndex = ((One - alpha / Two) * samples.length).toInt - 1
    val safeLower  = math.max(0, math.min(lowerIndex, samples.length - 1))
    val safeUpper  = math.max(0, math.min(upperIndex, samples.length - 1))
    (samples(safeLower), samples(safeUpper))
  }

  /** Estimates the Cumulative Distribution Function (CDF) at a given value. This is the probability that a sample will be less than or equal to `value`.
    *
    * @param value
    *   The value at which to evaluate the CDF.
    * @param sampleCount
    *   The number of samples to use for the estimation. Must be positive.
    * @return
    *   The estimated probability P(X <= value).
    * @see
    *   [[https://en.wikipedia.org/wiki/Cumulative_distribution_function Cumulative Distribution Function]]
    */
  def cdf(value: T, sampleCount: Int = 1000): Double = {
    require(sampleCount > 0, "Sample count must be positive to estimate CDF.")
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
final case class HypothesisResult(
  decision: Boolean,
  probability: Double,
  confidenceLevel: Double,
  samplesUsed: Int
)

/** A context for a single top-level `sample()` call, used to memoize values from leaf nodes. This is the key to preserving correlation. When an expression like
  * `x - x` is evaluated, the `SampleContext` ensures that the node for `x` is only sampled once, and its value is reused.
  */
final class SampleContext {
  private val memoizedValues: mutable.Map[UUID, Any] = mutable.Map.empty
  def getValue[T](id: UUID): Option[T]               = memoizedValues.get(id).map(_.asInstanceOf[T])
  def setValue[T](id: UUID, value: T): Unit          = memoizedValues(id) = value
}
