package mostly.uncertaintee.quantiles

import mostly.uncertaintee.Uncertain

import scala.math.floor

/** Represents N-quantile values.
  *
  * For common use cases see:
  *
  *   - [[Tertiles]] ( construct from [[Uncertain]] with [[Quantiles.tertiles]] )
  *   - [[Quartiles]] ( construct from [[Uncertain]] with [[Quantiles.quartiles]] )
  *   - [[Quintiles]] ( construct from [[Uncertain]] with [[Quantiles.quintiles]]
  *   - [[Deciles]] ( construct from [[Uncertain]] with [[Quantiles.deciles]] )
  *   - [[Percentiles]] ( construct from [[Uncertain]] with [[Quantiles.percentiles]] )
  */
trait Quantiles[T] {

  /** Represents the amount of quantities this [[Quantile]]-implementation represents */
  def n: Int

  /** Returns the value at the given quantile boundary.
    *
    * @param index
    *   quantile boundary index (0 to [[n]]th quantile)
    * @return
    *   the value at the specified quantile boundary
    */
  def quantile(index: Int): T

  /** Maximum value (Nth quantile boundary)
    *
    * @return
    *   the maximum value (equivalent to the nth quantile)
    */
  def max: T = quantile(n)

  /** Minimum value (0th quantile boundary)
    *
    * @return
    *   the minimum value (equivalent to the 0th quantile)
    */
  def min: T = quantile(0)

  /** As Sequence of quantiles up to [[n]]th quantile (including the zero-quantile) - e.g. for [[n]] == 5, this will return:
    *   - quantile0 == min (0%)
    *   - quantile1 == 20%
    *   - quantile2 == 40%
    *   - quantile3 == 60%
    *   - quantile4 == 80%
    *   - quantile5 == max (100%)
    *
    * @param includeMin
    *   whether to include the minimum (0th quantile) in the sequence
    * @param includeMax
    *   whether to include the maximum ([[n]]th quantile) in the sequence
    * @return
    *   sequence of quantile values based on inclusion parameters
    */
  def toSeq(includeMin: Boolean = true, includeMax: Boolean = true): Seq[T] =
    (includeMin, includeMax) match {
      case (true, true)   => (0 to n).map(quantile)
      case (true, false)  => (0 until n).map(quantile)
      case (false, true)  => (1 to n).map(quantile)
      case (false, false) => (1 until n).map(quantile)
    }
}

object Quantiles {

  /** Computes tertiles (3 way splits) from an uncertain value
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Tertiles]] computed from the samples
    */
  def tertiles[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Tertiles[T] =
    Tertiles.fromUncertain[T](
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

  /** Computes quartiles from an uncertain value
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Quartiles]] computed from the samples
    */
  def quartiles[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Quartiles[T] =
    Quartiles.fromUncertain[T](
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

  /** Computes quintiles (5 way splits) from an uncertain value
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Tertiles]] computed from the samples
    */
  def quintiles[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Quintiles[T] =
    Quintiles.fromUncertain[T](
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

  /** Computes deciles from an uncertain value
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Deciles]] computed from the samples
    */
  def deciles[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Deciles[T] =
    Deciles.fromUncertain[T](
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

  /** Computes percentiles from an uncertain value
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   [[Percentiles]] computed from the samples
    */
  def percentiles[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Percentiles[T] =
    Percentiles.fromUncertain[T](
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

  /** Computes quantiles (by sampling and sorting the samples)
    *
    * See also [[quartiles]], [[deciles]] and [[percentiles]] for common quantiles.
    *
    * @param n
    *   the number of quantiles (N).
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   a Quantiles instance with exact quantile values
    */
  def ofSize[T](
    n: Int,
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Quantiles[T] = {
    require(n > 0, s"quantile size must be > 0, was: $n")
    require(sampleCount > 0, s"sampleCount must be positive, was: $sampleCount")
    val sortedSamples: List[T]        = uncertain.take(sampleCount).sorted
    val capturedN                     = n
    val quantileValues: IndexedSeq[T] = (0 to n).map { i =>
      val proportion = i.toDouble / n
      val idx        = floor(proportion * (sampleCount - 1)).toInt
      sortedSamples(idx)
    }

    new Quantiles[T] {
      override val n: Int = capturedN

      override def quantile(index: Int): T = {
        require(index >= 0 && index <= n, s"quantile index must be in range 0 to $n, was: $index")
        quantileValues(index)
      }
    }
  }
}
