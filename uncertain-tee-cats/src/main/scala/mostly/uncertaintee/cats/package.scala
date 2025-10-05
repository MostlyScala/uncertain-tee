package mostly.uncertaintee

import _root_.cats.*
import _root_.cats.syntax.all.*
import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.point

import scala.util.NotGiven

/** Cats typeclass instances for Uncertain.
  *
  * These instances preserve the library's correlation guarantees. When working with instances like Monoid or Semigroup,
  * be aware that operations like `x |+| x` will use the same sample twice (correlated), while `x1 |+| x2` uses
  * independent samples.
  *
  * This is usually what you want for modeling dependent relationships, but if you need truly independent samples,
  * create separate Uncertain instances.
  */
package object cats {

  given uncertainFunctor: Functor[Uncertain] with {
    override def map[A, B](fa: Uncertain[A])(f: A => B): Uncertain[B] = fa.map(f)
  }

  given uncertainMonad: StackSafeMonad[Uncertain] with {
    override def flatMap[A, B](fa: Uncertain[A])(f: A => Uncertain[B]): Uncertain[B] =
      fa.flatMap(f)

    override def pure[A](x: A): Uncertain[A] = Uncertain.point(x)
  }

  /** Combines uncertain values element-wise using the underlying Semigroup.
    *
    * IMPORTANT: This preserves correlation. When combining an uncertain value with itself (x |+| x), the SAME sample is
    * used for both operands due to the library's correlation-preservation guarantee.
    *
    * @example
    *   Correlation in action:
    *   {{{
    *   val x = Uncertain.normal(0, 1)
    *
    *   // This combines the same sample with itself
    *   val doubled = x |+| x
    *   doubled.sample()  // Returns 2 * (some sample from x)
    *
    *   // For independent samples, create separate instances:
    *   val x1 = Uncertain.normal(0, 1)
    *   val x2 = Uncertain.normal(0, 1)
    *   val independent = x1 |+| x2
    *   independent.sample()  // Sum of two independent samples
    *   }}}
    */
  given uncertainMonoid[T](using Monoid[T]): Monoid[Uncertain[T]] with {
    override def empty: Uncertain[T] =
      Uncertain.point(Monoid[T].empty)

    override def combine(x: Uncertain[T], y: Uncertain[T]): Uncertain[T] =
      x.flatMap(a => y.map(b => Monoid[T].combine(a, b)))
  }

  /** If we don't have a full monoid for T, at least we can provide combine (|+|) capability via a semigroup. Combines
    * uncertain values element-wise using the underlying Semigroup.
    *
    * IMPORTANT: This preserves correlation. When combining an uncertain value with itself (x |+| x), the SAME sample is
    * used for both operands due to the library's correlation-preservation guarantee.
    *
    * @note
    *   This instance has a lower priority (uses `NotGiven[Monoid[T]]`) than uncertainMonoid to avoid ambiguity when
    *   both Monoid[T] and Semigroup[T] are available.
    */
  given uncertainSemigroup[T](using
    Semigroup[T],
    NotGiven[Monoid[T]]
  ): Semigroup[Uncertain[T]] =
    (x, y) =>
      for {
        sampleX <- x
        sampleY <- y
      } yield sampleX |+| sampleY

}