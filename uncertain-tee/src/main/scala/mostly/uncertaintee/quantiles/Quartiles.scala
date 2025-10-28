package mostly.uncertaintee.quantiles

import mostly.uncertaintee.Uncertain

/** Represents quartile values (4-quantiles)
  *
  * @param q0
  *   0th quartile (minimum, 0%)
  * @param q1
  *   1st quartile (25%)
  * @param q2
  *   2nd quartile (median, 50%)
  * @param q3
  *   3rd quartile (75%)
  * @param q4
  *   4th quartile (maximum, 100%)
  */
final case class Quartiles[T](
  q0: T,
  q1: T,
  q2: T,
  q3: T,
  q4: T
) extends Quantiles[T] {

  override val n: Int = 4

  /** The median value (2nd quartile)
    *
    * @return
    *   the median value (identical to [[q2]] )
    */
  def median: T = q2

  /** Returns the value at the given quartile boundary.
    *
    * Method exists for parity with [[Quantiles.quantile]], [[Deciles.decile]] and [[Percentiles.percentile]]; it may be easier to just pattern match Quartiles itself, depending on
    * your usage.
    *
    * @param n
    *   quartile boundary index (0 to 4)
    * @return
    *   the value at the specified quartile boundary
    */
  def quartile(n: Int): T = n match {
    case 0         => q0
    case 1         => q1
    case 2         => q2
    case 3         => q3
    case 4         => q4
    case otherwise => throw new IllegalArgumentException(s"Quartile must be in range (0 to 4), was: $otherwise")
  }

  override def quantile(n: Int): T = quartile(n)
}

object Quartiles {

  /** Computes Quartiles
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   quartiles computed from the samples
    */
  def fromUncertain[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Quartiles[T] = {

    val quantiles = Quantiles.ofSize[T](
      n = 4,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Quartiles[T](
      q0 = quantiles.quantile(0),
      q1 = quantiles.quantile(1),
      q2 = quantiles.quantile(2),
      q3 = quantiles.quantile(3),
      q4 = quantiles.quantile(4)
    )
  }
}
