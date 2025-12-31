package mostly.uncertaintee

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.zio.prelude.*

package object zio {

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainAssociativeFlatten: AssociativeFlatten[Uncertain] =
    new AssociativeFlatten[Uncertain] {
      override def map[A, B](f: A => B): Uncertain[A] => Uncertain[B]     = _.map(f)
      override def flatten[A](ffa: Uncertain[Uncertain[A]]): Uncertain[A] = ffa.flatMap(identity)
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainIdentityFlatten: IdentityFlatten[Uncertain] =
    new IdentityFlatten[Uncertain] {
      override def map[A, B](f: A => B): Uncertain[A] => Uncertain[B]     = _.map(f)
      override def flatten[A](ffa: Uncertain[Uncertain[A]]): Uncertain[A] = ffa.flatMap(identity)
      override def any: Uncertain[Any]                                    = Uncertain.always(())
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainCovariant: Covariant[Uncertain] = new Covariant[Uncertain] {
    override def map[A, B](f: A => B): Uncertain[A] => Uncertain[B] = _.map(f)
  }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainMonad: Monad[Uncertain] = new Monad[Uncertain] {
    override def pure[A](a: A): Uncertain[A]                            = Uncertain.always(a)
    override def map[A, B](f: A => B): Uncertain[A] => Uncertain[B]     = _.map(f)
    override def flatten[A](ffa: Uncertain[Uncertain[A]]): Uncertain[A] = ffa.flatMap(identity)
    override def any: Uncertain[Any]                                    = Uncertain.always(())
  }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainForEach: ForEach[Uncertain] =
    new ForEach[Uncertain] {
      override def forEach[G[+_]: Covariant: IdentityBoth, A, B](fa: Uncertain[A])(f: A => G[B]): G[Uncertain[B]] = _.traverse(f)
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainAssociativeBoth: AssociativeBoth[Uncertain] =
    new AssociativeBoth[Uncertain] {
      override def both[A, B](fa: => Uncertain[A], fb: => Uncertain[B]): Uncertain[(A, B)] = fa.product(fb)
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainIdentityBoth: IdentityBoth[Uncertain] =
    new IdentityBoth[Uncertain] {
      override def both[A, B](fa: => Uncertain[A], fb: => Uncertain[B]): Uncertain[(A, B)] = fa.product(fb)
      override def any: Uncertain[Any]                                                     = Uncertain.always(())
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit def uncertainAssociative[T](using Associative[T]): Associative[Uncertain[T]] =
    new Associative[Uncertain[T]] {
      override def combine(l: => Uncertain[T], r: => Uncertain[T]): Uncertain[T] = l.zipWith(r)(Associative[T].combine)
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit def uncertainIdentity[T](using Identity[T]): Identity[Uncertain[T]] =
    new Identity[Uncertain[T]] {
      override def identity: Uncertain[T]                                        = Uncertain.always(Identity[T].identity)
      override def combine(l: => Uncertain[T], r: => Uncertain[T]): Uncertain[T] = l.zipWith(r)(Identity[T].combine)
    }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit def uncertainInverse[T](using Inverse[T]): Inverse[Uncertain[T]] =
    new Inverse[Uncertain[T]] {
      override def identity: Uncertain[T]                                        = Uncertain.always(Inverse[T].identity)
      override def combine(l: => Uncertain[T], r: => Uncertain[T]): Uncertain[T] = l.zipWith(r)(Inverse[T].combine)
      override def inverse(a: => Uncertain[T]): Uncertain[T]                     = a.map(Inverse[T].inverse)
    }

}
