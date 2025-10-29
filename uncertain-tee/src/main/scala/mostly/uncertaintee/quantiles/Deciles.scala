package mostly.uncertaintee.quantiles

import mostly.uncertaintee.Uncertain

/** Represents decile values (10-quantiles)
  *
  * @param d0
  *   0th decile (minimum, 0%)
  * @param d1
  *   1st decile (10%)
  * @param d2
  *   2nd decile (20%)
  * @param d3
  *   3rd decile (30%)
  * @param d4
  *   4th decile (40%)
  * @param d5
  *   5th decile (median, 50%)
  * @param d6
  *   6th decile (60%)
  * @param d7
  *   7th decile (70%)
  * @param d8
  *   8th decile (80%)
  * @param d9
  *   9th decile (90%)
  * @param d10
  *   10th decile (maximum, 100%)
  */
final case class Deciles[T](
  d0: T,
  d1: T,
  d2: T,
  d3: T,
  d4: T,
  d5: T,
  d6: T,
  d7: T,
  d8: T,
  d9: T,
  d10: T
) extends Quantiles[T] {

  override val quantileIntervals: Int = 10

  val median: T = d5

  /** Returns the value at the given decile boundary.
    *
    * @param n
    *   decile boundary index (0 to 10)
    * @return
    *   the value at the specified decile boundary
    */
  def decile(n: Int): T = n match {
    case 0         => d0
    case 1         => d1
    case 2         => d2
    case 3         => d3
    case 4         => d4
    case 5         => d5
    case 6         => d6
    case 7         => d7
    case 8         => d8
    case 9         => d9
    case 10        => d10
    case otherwise => throw new IllegalArgumentException(s"Deciles are in range (0 to 10), was $otherwise")
  }

  override def quantile(n: Int): T = decile(n)
}

object Deciles {

  /** Computes Deciles
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   deciles computed from the samples
    */
  def fromUncertain[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Deciles[T] = {

    val quantiles = Quantiles.ofSize[T](
      quantileIntervals = 10,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Deciles[T](
      d0 = quantiles.quantile(0),
      d1 = quantiles.quantile(1),
      d2 = quantiles.quantile(2),
      d3 = quantiles.quantile(3),
      d4 = quantiles.quantile(4),
      d5 = quantiles.quantile(5),
      d6 = quantiles.quantile(6),
      d7 = quantiles.quantile(7),
      d8 = quantiles.quantile(8),
      d9 = quantiles.quantile(9),
      d10 = quantiles.quantile(10)
    )
  }
}
