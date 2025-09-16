package mostly.uncertaintee.cats

import cats.{Functor, StackSafeMonad}
import mostly.uncertaintee.Uncertain

object instances {
  given Functor[Uncertain] with
    override def map[A, B](fa: Uncertain[A])(f: A => B): Uncertain[B] = fa.map(f)

  given StackSafeMonad[Uncertain] with
    override def flatMap[A, B](fa: Uncertain[A])(f: A => Uncertain[B]): Uncertain[B] =
      fa.flatMap(f)

    override def pure[A](x: A): Uncertain[A] = Uncertain.point(x)
}
