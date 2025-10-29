package mostly.uncertaintee.quantiles

import mostly.uncertaintee.Uncertain

/** Represents Quantile values (a 5-way split, 5-quantities)
  *
  * @param q0
  *   0th quintile (min, 0%)
  * @param q1
  *   1st quintile (min, 20%)
  * @param q2
  *   2nd quintile (min, 40%)
  * @param q3
  *   3rd quintile (min, 60%)
  * @param q4
  *   4th quintile (min, 80%)
  * @param q5
  *   5th quintile (max, 100%)
  */
final case class Quintiles[T](
  q0: T,
  q1: T,
  q2: T,
  q3: T,
  q4: T,
  q5: T
) extends Quantiles[T] {
  override val quantileIntervals = 5

  def quintile(n: Int): T = n match {
    case 0         => q0
    case 1         => q1
    case 2         => q2
    case 3         => q3
    case 4         => q4
    case 5         => q5
    case otherwise => throw new IllegalArgumentException(s"Quintile has to be in range (0 to 5), was: $otherwise")
  }

  override def quantile(n: Int): T = quintile(n)

}

object Quintiles {

  /** Computes Quintiles (5 way splits)
    *
    * @param uncertain
    *   the uncertain value to sample from
    * @param sampleCount
    *   number of samples to draw
    * @param ord
    *   implicit ordering for type T
    * @return
    *   quintiles computed from the samples
    */
  def fromUncertain[T](
    uncertain: Uncertain[T],
    sampleCount: Int
  )(using ord: Ordering[T]): Quintiles[T] = {

    val quantiles = Quantiles.ofSize[T](
      quantileIntervals = 5,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Quintiles[T](
      q0 = quantiles.quantile(0),
      q1 = quantiles.quantile(1),
      q2 = quantiles.quantile(2),
      q3 = quantiles.quantile(3),
      q4 = quantiles.quantile(4),
      q5 = quantiles.quantile(5)
    )
  }
}
