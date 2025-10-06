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

/** A typeclass for types that can be safely converted to a Double for statistical analysis. */
trait StatisticallyConvertible[T] {
  def toDouble(value: T): Double
}

object StatisticallyConvertible {

  /** Any type T that is already Numeric can be converted. */
  given statisticalNumeric[T](using num: Numeric[T]): StatisticallyConvertible[T] =
    (value: T) => num.toDouble(value)

  /** The specific conversion rule for Boolean: true -> 1.0, false -> 0.0. */
  given statisticalBool: StatisticallyConvertible[Boolean] =
    (value: Boolean) => if (value) 1.0 else 0.0
}
