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

package mostly.uncertaintee.scalacheck

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*
import org.scalacheck.*

import scala.util.Random

trait UncertainInstances {

  private val genRandom: Gen[Random] = Gen.long.map(Random(_))

  /** Generates a Generator-seeded Uncertain with a Gaussian (Normal) distribution.
    *
    * @param meanRange
    *   Range for the mean parameter
    * @param stdDevRange
    *   Range for the standard deviation (must be >= 0)
    */
  def gaussianUncertain(
    meanRange: Gen[Double] = Gen.choose(-1000.0, 1000.0),
    stdDevRange: Gen[Double] = Gen.choose(0.0, 100.0)
  ): Gen[Uncertain[Double]] = for {
    mean   <- meanRange
    stdDev <- stdDevRange
    rng    <- genRandom
  } yield Uncertain.gaussian(mean = mean, standardDeviation = stdDev)(using rng)

  /** Generates a generator seeded geometric distribution */
  val geometricUncertain: Gen[Uncertain[Int]] = for {
    probability <- Gen.double
    rng         <- genRandom
  } yield Uncertain.geometric(
    probability = probability
  )(using rng)

  /** Generates a generator seeded Uniform distribution in range [minRange, maxRange].
    *
    * @note
    *   ensure maxRange is >= minRange, otherwise generator will fail.
    */
  def uniformUncertain(
    minRange: Gen[Double] = Gen.choose(-1000.0, 0),
    maxRange: Gen[Double] = Gen.choose(0, 1000)
  ): Gen[Uncertain[Double]] = for {
    min <- minRange
    max <- maxRange.suchThat(min <= _)
    rng <- genRandom
  } yield Uncertain.uniform(min, max)(using rng)

  /** Generates a generator-seeded Triangular distribution with min <= peak <= max */
  def triangularUncertain(
    minRange: Gen[Double] = Gen.choose(-1000.0, 0.0),
    maxRange: Gen[Double] = Gen.choose(0.0, 1000.0)
  ): Gen[Uncertain[Double]] = for {
    min  <- minRange
    max  <- maxRange.suchThat(_ >= min)
    peak <- Gen.choose(min, max)
    rng  <- genRandom
  } yield Uncertain.triangular(min = min, peak = peak, max = max)(using rng)

  /** Generates a generator-seeded Exponential distribution (rate > 0) */
  def exponentialUncertain(
    rateRange: Gen[Double] = Gen.choose(1e-9, 100.0)
  ): Gen[Uncertain[Double]] = for {
    rate <- rateRange
    rng  <- genRandom
  } yield Uncertain.exponential(rate)(using rng)

  /** Generates a generator-seeded Kumaraswamy(a, b) distribution (a > 0, b > 0) */
  def kumaraswamyUncertain(
    aRange: Gen[Double] = Gen.choose(1e-9, 10.0),
    bRange: Gen[Double] = Gen.choose(1e-9, 10.0)
  ): Gen[Uncertain[Double]] = for {
    a   <- aRange
    b   <- bRange
    rng <- genRandom
  } yield Uncertain.kumaraswamy(a = a, b = b)(using rng)

  /** Generates a generator-seeded Rayleigh distribution (scale > 0) */
  def rayleighUncertain(
    scaleRange: Gen[Double] = Gen.choose(1e-9, 100.0)
  ): Gen[Uncertain[Double]] = for {
    scale <- scaleRange
    rng   <- genRandom
  } yield Uncertain.rayleigh(scale)(using rng)

  /** Generates an empirical distribution from a non-empty finite sample of Doubles */
  def empiricalUncertain[T](
    gen: Gen[T]
  ): Gen[Uncertain[T]] = for {
    xs  <- Gen.nonEmptyListOf(gen)
    rng <- genRandom
  } yield Uncertain.empirical(xs)(using rng)

  /** Generate an random (generator-seeded) uncertain double distribution within a reasonable range */
  def uncertainDouble: Gen[Uncertain[Double]] = Gen.oneOf(
    exponentialUncertain(),
    gaussianUncertain(),
    kumaraswamyUncertain(),
    rayleighUncertain(),
    triangularUncertain()
  )



}
