/*
 * Copyright (c) 2025 Mostly Codes
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package mostly.uncertaintee.internal

import mostly.uncertaintee.Uncertain

import java.util.UUID
import scala.collection.mutable
import scala.util.control.TailCalls

/** Internal: A node in the computation graph that represents operations or sources of uncertainty. */
sealed private[uncertaintee] trait ComputationTree[+T] {

  /** Evaluates this node within a sampling context. */
  def evaluate(context: SampleContext = new SampleContext): T =
    _evaluate(context).result

  private def _evaluate(context: SampleContext = new SampleContext): TailCalls.TailRec[T] = this match {
    case ComputationLeaf(id, sampler) =>
      TailCalls.done {
        context
          .getSample[T](id)
          .getOrElse {
            val sample = sampler()
            context.setSample(id, sample)
            sample
          }
      }

    case ComputationFlatMapping(source, f) =>
      source._evaluate(context).flatMap { sourceValue =>
        val innerUncertain = f(sourceValue)
        innerUncertain.computationTree._evaluate(context)
      }
  }
}

/** A "source" of uncertainty in the computation graph. */
final private[uncertaintee] case class ComputationLeaf[T](
  id: UUID,
  sampler: () => T
) extends ComputationTree[T]

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
