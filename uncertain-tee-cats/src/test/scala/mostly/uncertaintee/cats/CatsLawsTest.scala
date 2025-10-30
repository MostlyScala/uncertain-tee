/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee.cats

import cats.Eq
import cats.kernel.laws.discipline.{GroupTests, MonoidTests}
import cats.laws.discipline.{ApplicativeTests, FunctorTests, MonadTests, SemigroupalTests}
import cats.syntax.eq.*
import mostly.uncertaintee.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen}

class CatsLawsTest extends DisciplineSuite {
  implicit private def arbUncertainA[A: Arbitrary]: Arbitrary[Uncertain[A]] = Arbitrary(arbitrary[A].map(Uncertain.always))

  // Testing the laws for truly uncertain values.
  // Note this type of equality only matches some amount of samples against each other, so it only makes sense for this test - as in,
  // there is no good way of implementing the Eq typeclass for Uncertain.
  implicit private def eqUncertainA[A: Eq]: Eq[Uncertain[A]] = new Eq[Uncertain[A]] {
    def eqv(x: Uncertain[A], y: Uncertain[A]): Boolean =
      x.take(50) === y.take(50)
  }

  checkAll("Uncertain.FunctorLaws", FunctorTests[Uncertain].functor[Int, Int, String])
  checkAll("Uncertain.ApplyLaws", ApplicativeTests[Uncertain].apply[Int, Int, String])
  checkAll("Uncertain.ApplicativeLaws", ApplicativeTests[Uncertain].applicative[Int, Int, String])
  checkAll("Uncertain.MonadLaws", MonadTests[Uncertain].monad[Int, Int, String])
  checkAll("Uncertain.MonoidLaws", MonoidTests[Uncertain[Int]].monoid)
  checkAll("Uncertain.SemigroupLaws", SemigroupalTests[Uncertain].semigroupal[Int, Int, String])
  checkAll("Uncertain.GroupLaws", GroupTests[Uncertain[Int]].group)
}
