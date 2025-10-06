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
import cats.kernel.laws.discipline.GroupTests
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.ApplicativeTests
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.MonadTests
import cats.laws.discipline.SemigroupalTests
import cats.syntax.eq._
import mostly.uncertaintee._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

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
  checkAll("Uncertain.SemigroupLaws", SemigroupalTests[Uncertain].semigroupal[Int, Int, String])
  checkAll("Uncertain.GroupLaws", GroupTests[Uncertain[Int]].group)
}
