/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package mostly.uncertaintee

/** Represents the kurtosis measurement of a distribution.
  *
  * Kurtosis measures the "tailedness" of a probability distribution - how much probability mass is in the tails versus the center compared to a normal distribution.
  *
  * See: https://en.wikipedia.org/wiki/Kurtosis
  */
sealed trait Kurtosis {

  /** The numerical excess kurtosis value (fourth standardized moment - 3), if defined.
    */
  def maybeExcessValue: Option[Double] = this match {
    case x: DefinedKurtosis => Some(x.excessValue)
    case Kurtosis.Undefined => None
  }

  /** The raw kurtosis value (fourth standardized moment), if defined. This is `excessValue + 3`.
    *
    * Alias for `[[fourthStandardizedMoment]]`.
    */
  def maybeRawValue: Option[Double] = maybeExcessValue.map(_ + 3.0)

  /** The raw kurtosis value (fourth standardized moment), if defined. This is `excessValue + 3`.
    *
    * Alias for `[[maybeRawValue]]`.
    */
  def maybeFourthStandardizedMoment: Option[Double] = maybeExcessValue.map(_ + 3.0)

}

/** Represents a Kurtosis value that is guaranteed to be defined (i.e., not [[Kurtosis.Undefined]]).
  */
trait DefinedKurtosis extends Kurtosis {

  /** The numerical excess kurtosis value (fourth standardized moment - 3). */
  def excessValue: Double

  /** The raw kurtosis value (the fourth standardized moment). This is `excessValue + 3`.
    *
    * Alias for `[[fourthStandardizedMoment]]`.
    */
  def rawValue: Double = excessValue + 3

  /** The raw kurtosis value (the fourth standardized moment). This is `excessValue + 3`.
    *
    * Alias for `[[rawValue]]`.
    */
  def fourthStandardizedMoment: Double = rawValue
}

object Kurtosis {

  /** Heavy-tailed distribution with excess kurtosis > 0.
    *
    * Leptokurtic distributions have more probability mass in the tails and at the peak compared to a normal distribution. Examples: exponential, t-distribution, Laplace.
    *
    * @param excessValue
    *   The positive excess kurtosis value.
    */
  final case class Leptokurtic(excessValue: Double) extends DefinedKurtosis {
    require(
      excessValue > 0,
      s"Leptokurtic excess kurtosis must be positive, got $excessValue"
    )
  }

  /** Normal-like distribution with excess kurtosis ≈ 0.
    *
    * Mesokurtic distributions have tail behavior similar to a normal distribution.
    *
    * The normal distribution is the canonical example with excess kurtosis = 0.
    *
    * @param excessValue
    *   The excess kurtosis value near zero.
    */
  final case class Mesokurtic(excessValue: Double) extends DefinedKurtosis

  /** Light-tailed distribution with excess kurtosis < 0.
    *
    * Platykurtic distributions have less probability mass in the tails and a flatter peak compared to a normal distribution.
    *
    * Example: uniform distribution (excess kurtosis = -1.2).
    *
    * @param excessValue
    *   The negative excess kurtosis value.
    */
  final case class Platykurtic(excessValue: Double) extends DefinedKurtosis {
    require(
      excessValue < 0,
      s"Platykurtic excess kurtosis must be negative, got $excessValue"
    )
  }

  /** Kurtosis is undefined, typically when standard deviation is zero (constant distribution). */
  case object Undefined extends Kurtosis

  /** Classifies an excess kurtosis value into its corresponding [[DefinedKurtosis]] type.
    *
    * This factory handles the classification logic. Note that it cannot return [[Undefined]], as it only operates on a pre-computed numerical value.
    *
    * @param excessValue
    *   The excess kurtosis value to classify.
    * @param mesokurticTolerance
    *   The non-negative threshold used to classify `Mesokurtic`. Values within `[-threshold, +threshold]` are considered mesokurtic (default: 0.5).
    * @return
    *   A `DefinedKurtosis` instance (`Leptokurtic`, `Mesokurtic`, or `Platykurtic`).
    */
  def apply(excessValue: Double, mesokurticTolerance: Double = 0.5): DefinedKurtosis =
    require(!excessValue.isNaN && !excessValue.isInfinity, s"Invalid kurtosis value: $excessValue")

    if (excessValue > mesokurticTolerance) {
      Leptokurtic(excessValue)
    } else if (excessValue < -mesokurticTolerance) {
      Platykurtic(excessValue)
    } else {
      Mesokurtic(excessValue)
    }
}
