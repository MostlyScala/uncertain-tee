package mostly.uncertaintee.ops

import mostly.uncertaintee.*
import mostly.uncertaintee.ops.distribution.*

import scala.util.Random

/** //TODO extensive scaladocs
  *
  * Constructors ("factory methods") for statistical distributions.
  *
  * {{{
  * import mostly.uncertaintee.syntax.distribution.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait AllDistributionOps
    extends DistributionOpsBigDecimal
    with DistributionOpsBigInt
    with DistributionOpsBoolean
    with DistributionOpsByte
    with DistributionOpsDouble
    with DistributionOpsInt
    with DistributionOpsLong
    with DistributionOpsShort {

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

    def mixture[T](components: Map[Uncertain[T], Double])(using random: Random = new Random()): Uncertain[T] =
      Uncertain.mixtureViaDouble(components)(using random)

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

    def empirical[T](data: List[T])(using random: Random = new Random()): Uncertain[T] = {
      require(data.nonEmpty, "Need at least one data point for empirical distribution.")
      Uncertain(() => data(random.nextInt(data.length)))
    }

    def categorical[T](outcomes: Map[T, Double])(using random: Random = new Random()): Uncertain[T] =
      Uncertain.categoricalViaDouble(outcomes)(using random)

    def normal(mean: Double, standardDeviation: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.normalDouble(mean, standardDeviation)(using random)

    def uniform(min: Double, max: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.uniformDouble(min, max)(using random)

    def triangular(min: Double, peak: Double, max: Double): Uncertain[Double] =
      Uncertain.triangularViaDouble(min, peak, max)

    def exponential(rate: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.exponentialViaDouble(rate)(using random)

    def bernoulli(probability: Double)(using random: Random = new Random()): Uncertain[Boolean] =
      Uncertain.bernoulliViaDouble(probability)(using random)

    def kumaraswamy(a: Double, b: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.kumaraswamyViaDouble(a, b)(using random)

    def rayleigh(scale: Double)(using random: Random = new Random()): Uncertain[Double] =
      Uncertain.rayleighViaDouble(scale)(using random)

    def binomial(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.binomialViaDouble(trials, probability)(using random)

    def poisson(lambda: Double)(using random: Random = new Random()): Uncertain[Int] =
      Uncertain.poissonViaDouble(lambda)(using random)

  }
}
