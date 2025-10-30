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

package mostly.uncertaintee

import _root_.cats.*
import _root_.cats.syntax.all.*
import mostly.uncertaintee.*

import scala.language.implicitConversions
import scala.util.NotGiven

package object cats {

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainFunctor: Functor[Uncertain] = new Functor[Uncertain] {
    override def map[A, B](fa: Uncertain[A])(f: A => B): Uncertain[B] = fa.map(f)
  }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit val uncertainMonad: StackSafeMonad[Uncertain] = new StackSafeMonad[Uncertain] {
    override def flatMap[A, B](fa: Uncertain[A])(f: A => Uncertain[B]): Uncertain[B] = fa.flatMap(f)
    override def pure[A](x: A): Uncertain[A]                                         = Uncertain.always(x)
  }

  /** @see [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]] */
  implicit def uncertainMonoid[T](using Monoid[T]): Monoid[Uncertain[T]] = new Monoid[Uncertain[T]] {
    override def empty: Uncertain[T]                                     = Uncertain.always(Monoid[T].empty)
    override def combine(x: Uncertain[T], y: Uncertain[T]): Uncertain[T] = x.flatMap(a => y.map(b => Monoid[T].combine(a, b)))
  }

  /** If we don't have a full monoid for T, at least we can provide combine (|+|) capability via a semigroup. Combines uncertain values element-wise using the underlying Semigroup.
    *
    * @note
    *   This instance has a lower priority (uses `NotGiven[Monoid[T]]`) than uncertainMonoid to avoid ambiguity when both Monoid[T] and Semigroup[T] are available.
    * @see
    *   [[mostly.uncertaintee.styledocs.WhyIsThisImplicitAndNotGiven]]
    */
  implicit def uncertainSemigroup[T](using
    Semigroup[T],
    NotGiven[Monoid[T]]
  ): Semigroup[Uncertain[T]] = (x, y) =>
    for {
      sampleX <- x
      sampleY <- y
    } yield sampleX |+| sampleY

  implicit def uncertainGroup[T](using Group[T]): Group[Uncertain[T]] = new Group[Uncertain[T]] {
    override def empty: Uncertain[T]                                     = Uncertain.always(Group[T].empty)
    override def combine(x: Uncertain[T], y: Uncertain[T]): Uncertain[T] = x.flatMap(a => y.map(b => Group[T].combine(a, b)))
    override def inverse(x: Uncertain[T]): Uncertain[T]                  = x.map(a => Group[T].inverse(a))
  }

}
