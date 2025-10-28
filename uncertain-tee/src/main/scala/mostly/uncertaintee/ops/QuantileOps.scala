package mostly.uncertaintee.ops

import mostly.uncertaintee.*
import mostly.uncertaintee.quantiles.*

/** {{{
  * import mostly.uncertaintee.syntax.quantile.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait QuantileOps {

  extension [T](u: Uncertain[T])(using ord: Ordering[T]) {

    /** N-quantiles (but consider [[tertiles]], [[quartiles]], [[quintiles]], [[deciles]] and [[percentiles]] for the usual common quantiles)
      *
      * @see
      *   [[tertiles]], [[quartiles]],[[quintiles]], [[deciles]], [[percentiles]] and [[quantiles]]
      */
    def quantiles(n: Int, sampleCount: Int): Quantiles[T] = Quantiles.ofSize(n, u, sampleCount)(using ord)

    /** 3-way split / 3-quantiles
      *
      * @see
      *   [[quartiles]], [[quintiles]], [[deciles]], [[percentiles]] and [[quantiles]]
      */
    def tertiles(sampleCount: Int): Tertiles[T] = Quantiles.tertiles(u, sampleCount)(using ord)

    /** 4-way split / 4-quantiles
      * @see
      *   [[tertiles]], [[quintiles]], [[deciles]], [[percentiles]] and [[quantiles]]
      */
    def quartiles(sampleCount: Int): Quartiles[T] = Quantiles.quartiles(u, sampleCount)(using ord)

    /** 5-way split / 5-quantiles
      * @see
      *   [[tertiles]], [[quartiles]], [[deciles]], [[percentiles]] and [[quantiles]]
      */
    def quintiles(sampleCount: Int): Quintiles[T] = Quantiles.quintiles(u, sampleCount)(using ord)

    /** 10-way split / 10-quantiles
      * @see
      *   [[tertiles]], [[quartiles]],[[quintiles]], [[percentiles]] and [[quantiles]]
      */
    def deciles(sampleCount: Int): Deciles[T] = Quantiles.deciles(u, sampleCount)(using ord)

    /** 100-way split / 100-quantiles
      *
      * @see
      *   [[tertiles]], [[quartiles]],[[quintiles]], [[deciles]] and [[quantiles]]
      */
    def percentiles(sampleCount: Int): Percentiles[T] = Quantiles.percentiles(u, sampleCount)(using ord)
  }

}
