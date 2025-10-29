package mostly.uncertaintee.quantiles

import mostly.uncertaintee.Uncertain

/** Represents Tertile values (a 3-way split, 3-quantities)
  *
  * @param t0
  *   0th tertile (minimum value encountered, 0%)
  * @param t1
  *   1st tertile (1/3rd, 33.3% repeating)
  * @param t2
  *   2nd tertile (2/3rd, 66.6% repeating)
  * @param t3
  *   3rd tertile (3/3rd, maximum, 99.9% repeating, 100%)
  */
final case class Tertiles[T](
  t0: T,
  t1: T,
  t2: T,
  t3: T
) extends Quantiles[T] {

  override val quantileIntervals: Int = 3

  def tertile(n: Int): T = n match {
    case 0         => t0
    case 1         => t1
    case 2         => t2
    case 3         => t3
    case otherwise => throw new IllegalArgumentException(s"Tertile has to be in range (0 to 3), was: $otherwise")
  }

  override def quantile(n: Int): T = tertile(n)
}

object Tertiles {

  /** Computes Tertiles (3 way split)
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
  )(using ord: Ordering[T]): Tertiles[T] = {

    val quantiles = Quantiles.ofSize[T](
      quantileIntervals = 3,
      uncertain = uncertain,
      sampleCount = sampleCount
    )(using ord)

    Tertiles[T](
      t0 = quantiles.quantile(0),
      t1 = quantiles.quantile(1),
      t2 = quantiles.quantile(2),
      t3 = quantiles.quantile(3)
    )
  }
}
