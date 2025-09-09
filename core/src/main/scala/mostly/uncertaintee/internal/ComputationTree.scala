package mostly.uncertaintee.internal

import mostly.uncertaintee.Uncertain

import java.util.UUID
import scala.collection.mutable

/** Internal: A node in the computation graph that represents operations or sources of uncertainty. */
sealed private[uncertaintee] trait ComputationTree[+T] {

  /** Evaluates this node within a sampling context. */
  def evaluate(context: SampleContext = new SampleContext): T = this match {
    case ComputationLeaf(id, sampler)          =>
      context
        .getSample[T](id)
        .getOrElse {
          val sample = sampler()
          context.setSample(id, sample)
          sample
        }
    case ComputationMapping(source, operation) =>
      operation(source.evaluate(context))
    case ComputationFlatMapping(source, f)     =>
      val sourceValue    = source.evaluate(context)
      val innerUncertain = f(sourceValue)
      innerUncertain.computationTree.evaluate(context)
  }
}

/** A "source" of uncertainty in the computation graph. */
final private[uncertaintee] case class ComputationLeaf[T](
  id: UUID,
  sampler: () => T
) extends ComputationTree[T]

/** Represents applying a function to an uncertain value (used by `map`). */
final private[uncertaintee] case class ComputationMapping[T, B](
  source: ComputationTree[T],
  fromT2B: T => B
) extends ComputationTree[B]

/** Represents chaining uncertain computations (used by `flatMap`). */
final private[uncertaintee] case class ComputationFlatMapping[T, B](
  source: ComputationTree[T],
  fromT2UncertainB: T => Uncertain[B]
) extends ComputationTree[B]

/** Context for preserving correlation during sampling. */
final private[uncertaintee] class SampleContext {
  private val memoizedValues: mutable.Map[UUID, Any] = mutable.Map.empty
  def getSample[T](id: UUID): Option[T]              = memoizedValues.get(id).map(_.asInstanceOf[T])
  def setSample[T](id: UUID, sample: T): Unit        = memoizedValues(id) = sample
}
