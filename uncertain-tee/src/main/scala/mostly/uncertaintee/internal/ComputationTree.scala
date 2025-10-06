package mostly.uncertaintee.internal

import mostly.uncertaintee.Uncertain

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.util.control.TailCalls

/** Internal: A node in the computation graph (tree).
  *
  * This is the core abstraction for building and evaluating probabilistic computations. Each node in the tree
  * represents either a source of randomness ([[ComputationLeaf]]) or a transformation of uncertain values
  * ([[ComputationFlatMapping]]).
  *
  * Evaluation is performed using trampolining to avoid stack overflow on deeply nested computations.
  *
  * @tparam T
  *   the type of value produced by evaluating this computation tree
  */
sealed private[uncertaintee] trait ComputationTree[+T] {

  /** Evaluates this computation tree to produce a concrete sample value.
    *
    * Uses the provided context to memoize samples from leaf nodes, ensuring that multiple references to the same
    * uncertain value within a computation produce the same sample (preserving correlation).
    *
    * @param context
    *   the sampling context for memoizing leaf values (defaults to a new context)
    * @return
    *   a sampled value of type T
    */
  def evaluate(context: SampleContext = new SampleContext): T =
    evaluateTrampoline(context).result

  /** Trampolined evaluation to prevent stack overflow on deep trees.
    *
    * @param context
    *   the sampling context for memoizing leaf values
    * @return
    *   a tail-recursive computation that produces a value of type T
    */
  private def evaluateTrampoline(context: SampleContext): TailCalls.TailRec[T] = this match {
    case ComputationLeaf(id, sampler) =>
      TailCalls.done {
        context.getOrComputeSample(id, sampler)
      }

    case ComputationFlatMapping(source, f) =>
      source.evaluateTrampoline(context).flatMap { sourceValue =>
        val innerUncertain = f(sourceValue)
        innerUncertain.computationTree.evaluateTrampoline(context)
      }
  }
}

/** A leaf node representing a source of uncertainty in the computation graph.
  *
  * Each leaf has a unique identifier and a sampler function. The identifier ensures that multiple evaluations of the
  * same leaf within a single sampling context return the same value, preserving correlation structure in the
  * computation.
  *
  * @param id
  *   unique identifier for this source of uncertainty
  * @param sampler
  *   function that generates a sample value when invoked
  * @tparam T
  *   the type of value produced by the sampler
  * @note
  *   Each UUID is permanently bound to exactly one sampler type. This invariant is enforced by construction: UUIDs are
  *   generated randomly per leaf and never exposed or reused.
  */
final private[uncertaintee] case class ComputationLeaf[T](
  id: UUID,
  sampler: () => T
) extends ComputationTree[T]

/** Represents a monadic bind operation (flatMap) in the computation graph.
  *
  * This node chains uncertain computations together, where the result of evaluating the source computation is passed to
  * a function that produces a new uncertain computation. This enables dependent probabilistic computations where later
  * random variables can depend on earlier ones.
  *
  * @param source
  *   the upstream computation tree to evaluate first
  * @param transformToUncertain
  *   function that takes the source result and produces a new uncertain computation
  * @tparam T
  *   the type of value produced by the source computation
  * @tparam B
  *   the final type of value produced after the transformation
  */
final private[uncertaintee] case class ComputationFlatMapping[T, B](
  source: ComputationTree[T],
  transformToUncertain: T => Uncertain[B]
) extends ComputationTree[B]

/** Thread-safe context for preserving correlation structure during sampling.
  *
  * When evaluating a computation tree, the same leaf node may be encountered multiple times. This context memoizes the
  * sampled values by their unique identifiers, ensuring that all references to the same uncertain value produce the
  * same sample within a single evaluation.
  *
  * Example: If `x = Uncertain.uniform(0, 1)` and we compute `x + x`, both references to `x` should use the same sampled
  * value to correctly compute `2 * sample`, rather than two different samples.
  *
  * @note
  *   Thread-safety is provided by ConcurrentHashMap with atomic computeIfAbsent operations, ensuring each UUID is
  *   sampled exactly once even under concurrent access.
  */
final private[uncertaintee] class SampleContext {
  private val memoizedValues = new ConcurrentHashMap[UUID, Any]()

  /** Atomically gets an existing sample or computes and stores a new one if not yet memoized.
    *
    * This method ensures that the sampler is called at most once per UUID, even under concurrent access. This prevents
    * race conditions where multiple threads might both see no existing sample and both call the sampler, which would
    * violate the correlation guarantee that the same uncertain value always produces the same sample within a context.
    *
    * @param id
    *   the unique identifier for the uncertainty source
    * @param sampler
    *   function to compute the sample if not already present
    * @tparam T
    *   the type of the sample value
    * @return
    *   the memoized or newly computed sample
    * @note
    *   Uses unchecked cast from Any to T. This is safe because each UUID is bound to exactly one sampler type at
    *   construction time (see [[ComputationLeaf]]), making type confusion impossible through the public API.
    */
  def getOrComputeSample[T](id: UUID, sampler: () => T): T = {
    val result = memoizedValues.computeIfAbsent(id, _ => sampler())
    result.asInstanceOf[T]
  }
}
