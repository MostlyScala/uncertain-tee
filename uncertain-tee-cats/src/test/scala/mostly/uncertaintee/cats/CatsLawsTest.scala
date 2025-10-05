package mostly.uncertaintee.cats

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{ApplicativeTests, FunctorTests, MonadTests}
import cats.syntax.eq.*
import mostly.uncertaintee.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen}

class CatsLawsTest extends DisciplineSuite {
  private given [A: Arbitrary]: Arbitrary[Uncertain[A]] = Arbitrary(arbitrary[A].map(x => Uncertain(() => x)))

  // Testing the laws for truly uncertain values.
  // Note this type of equality only matches some amount of samples against each other, so it only makes sense for this test - as in,
  // there is no good way of implementing the Eq typeclass for Uncertain.
  private given [A: Eq]: Eq[Uncertain[A]] with
    override def eqv(x: Uncertain[A], y: Uncertain[A]): Boolean =
      x.take(50) === y.take(50)

  checkAll("Uncertain.FunctorLaws", FunctorTests[Uncertain].functor[Int, Int, String])
  checkAll("Uncertain.ApplyLaws", ApplicativeTests[Uncertain].apply[Int, Int, String])
  checkAll("Uncertain.ApplicativeLaws", ApplicativeTests[Uncertain].applicative[Int, Int, String])
  checkAll("Uncertain.MonadLaws", MonadTests[Uncertain].monad[Int, Int, String])
  checkAll("Uncertain.MonoidLaws", MonoidTests[Uncertain[Int]].monoid)
  checkAll("Uncertain.SemigroupLaws", MonoidTests[Uncertain[Int]].semigroup)
}
