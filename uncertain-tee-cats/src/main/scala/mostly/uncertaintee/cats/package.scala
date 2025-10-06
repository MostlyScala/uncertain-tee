package mostly.uncertaintee

import _root_.cats.*
import _root_.cats.syntax.all.*
import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

import scala.util.NotGiven

package object cats {

  given uncertainFunctor: Functor[Uncertain] with {
    override def map[A, B](fa: Uncertain[A])(f: A => B): Uncertain[B] = fa.map(f)
  }

  given uncertainMonad: StackSafeMonad[Uncertain] with {
    override def flatMap[A, B](fa: Uncertain[A])(f: A => Uncertain[B]): Uncertain[B] =
      fa.flatMap(f)

    override def pure[A](x: A): Uncertain[A] = Uncertain.point(x)
  }

  given uncertainMonoid[T](using Monoid[T]): Monoid[Uncertain[T]] with {
    override def empty: Uncertain[T] =
      Uncertain.point(Monoid[T].empty)

    override def combine(x: Uncertain[T], y: Uncertain[T]): Uncertain[T] =
      x.flatMap(a => y.map(b => Monoid[T].combine(a, b)))
  }

  /** If we don't have a full monoid for T, at least we can provide combine (|+|) capability via a semigroup. Combines
    * uncertain values element-wise using the underlying Semigroup.
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

  given uncertainGroup[T](using Group[T]): Group[Uncertain[T]] with {
    override def empty: Uncertain[T] =
      Uncertain.point(Group[T].empty)

    override def combine(x: Uncertain[T], y: Uncertain[T]): Uncertain[T] =
      x.flatMap(a => y.map(b => Group[T].combine(a, b)))

    override def inverse(x: Uncertain[T]): Uncertain[T] =
      x.map(a => Group[T].inverse(a))
  }

}
