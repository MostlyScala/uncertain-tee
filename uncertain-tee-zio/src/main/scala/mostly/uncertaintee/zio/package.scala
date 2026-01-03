package mostly.uncertaintee

import _root_.zio.prelude.coherent.CovariantIdentityBoth
import _root_.zio.prelude.{Debug, IdentityFlatten}
import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

import scala.reflect.ClassTag

/** End users are expected to import `mostly.uncertaintee.zio.*` */
package object zio {

  implicit val monad: CovariantIdentityBoth[Uncertain] with IdentityFlatten[Uncertain] =
    new CovariantIdentityBoth[Uncertain] with IdentityFlatten[Uncertain] {
      override def map[A, B](f: A => B): Uncertain[A] => Uncertain[B]                      = _.map(f)
      override def both[A, B](fa: => Uncertain[A], fb: => Uncertain[B]): Uncertain[(A, B)] = fa.product(fb)
      override def flatten[A](ffa: Uncertain[Uncertain[A]]): Uncertain[A]                  = ffa.flatMap(identity)
      override def any: Uncertain[Any]                                                     = Uncertain.always(())
    }

  /** This one poses an interesting question - what does a Debug instance look like, when the instances of A are random samples? Unlike debugging an array or list where the
    * elements are deterministic, the elements of an uncertain are pseudorandom samples from a distribution which may be discrete or may be continuous, or may be constant.
    * Equivalent to trying to debug a Random.
    *
    * One answer may be to take ~50 samples of the distribution, but the breaking of determinism might not make that entirely helpful.
    */
  implicit def debug[A: ClassTag]: Debug[Uncertain[A]] = new Debug[Uncertain[A]] {
    final private val className                     = summon[ClassTag[A]].runtimeClass.getSimpleName
    final private val repr                          = Debug.Repr.VConstructor(
      namespace = List("mostly", "uncertaintee"),
      name = s"Uncertain[$className]",
      reprs = List.empty
    )
    override def debug(a: Uncertain[A]): Debug.Repr = repr
  }
}
