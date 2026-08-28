package mostly.uncertaintee

import org.scalacheck.{Gen, Prop}

import scala.concurrent.duration.FiniteDuration

object Proofs {

  /** Finds the limit of a function. Given a function `f(x)`, and a series `x->infinity`, it finds a value such that `|f(x+1) - f(x)| < accuracy`.
    *
    * @see
    *   [[https://en.wikipedia.org/wiki/Limit_of_a_function Limit of a function]]
    * @param f
    *   the limited function
    * @param accuracy
    *   represents the accuracy level or allowed band before you can consider a function suitably stable
    * @return
    *   the stable value (limit) of the function, if it exists
    */
  def limit(accuracy: Double)(f: Int => Double): Gen[Double] =
    for {
      size <- Gen.size
      if size > 0
      n     = f(size)
      np1   = f(size + 1)
      if math.abs(np1 - n) < accuracy
    } yield np1

  def ratioTest(accuracy: Double)(series: Int => Double): Gen[Double] =
    limit(accuracy) { n =>
      math.abs(if series(n) == 0 then 0.0 else series(n + 1) / series(n))
    }

  def provableWithin(timeout: FiniteDuration)(prop: Prop): Prop =
    Prop.within(timeout.toMillis)(prop)
}
