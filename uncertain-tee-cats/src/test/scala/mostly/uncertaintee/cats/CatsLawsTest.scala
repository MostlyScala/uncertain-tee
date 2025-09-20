package mostly.uncertaintee.cats

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{ApplicativeTests, FunctorTests, MonadTests}
import cats.syntax.eq.*
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.cats.instances.given
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen}

class CatsLawsTest extends DisciplineSuite {
  private given [A: Arbitrary]: Arbitrary[Uncertain[A]] = Arbitrary(arbitrary[A].map(x => Uncertain.apply(() => x)))

  // Testing the laws for truly uncertain values
  private given [A: Eq]: Eq[Uncertain[A]] with
    override def eqv(x: Uncertain[A], y: Uncertain[A]): Boolean =
      x.sample() === y.sample()

  checkAll("Uncertain.FunctorLaws", FunctorTests[Uncertain].functor[Int, Int, String])
  checkAll("Uncertain.ApplicativeLaws", ApplicativeTests[Uncertain].applicative[Int, Int, String])
  checkAll("Uncertain.MonadLaws", MonadTests[Uncertain].monad[Int, Int, String])
  checkAll("Uncertain.MonoidLaws", MonoidTests[Uncertain[Int]].monoid)
}
