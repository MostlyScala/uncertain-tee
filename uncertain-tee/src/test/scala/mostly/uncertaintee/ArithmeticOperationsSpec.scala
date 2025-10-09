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

import mostly.uncertaintee.syntax.*
import munit.Clue.generate

import scala.math.{abs, pow, sqrt}

class ArithmeticOperationsSpec extends RngSuite {

  private val sampleCount     = 100_000
  // A tighter tolerance for arithmetic mean, which tends to converge faster.
  private val meanTolerance   = 0.05
  // A slightly looser tolerance for standard deviation, which has higher variance.
  private val stdDevTolerance = 0.1

  // --- Helper to get stats from a distribution ---
  private def getStats(u: Uncertain[Double]): (Double, Double) = {
    val samples  = u.take(sampleCount)
    val mean     = samples.sum / sampleCount
    val variance = samples.map(x => pow(x - mean, 2)).sum / (sampleCount - 1)
    (mean, sqrt(variance))
  }

  // --- Operations between two Uncertain values ---

  rngTest("Addition of two independent normal distributions should be correct") {
    val x: Uncertain[Double] = Uncertain.normal(mean = 10, standardDeviation = 2)
    val y: Uncertain[Double] = Uncertain.normal(mean = 5, standardDeviation = 3)
    val z: Uncertain[Double] = x + y

    // E[X+Y] = E[X] + E[Y] -> 10 + 5 = 15
    // Var(X+Y) = Var(X) + Var(Y) -> 2^2 + 3^2 = 13
    val theoreticalMean   = 15.0
    val theoreticalStdDev = sqrt(13.0) // approx 3.605

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("Subtraction of two independent normal distributions should be correct") {
    val x = Uncertain.normal(mean = 10, standardDeviation = 4)
    val y = Uncertain.normal(mean = 3, standardDeviation = 3)
    val z = x - y

    // E[X-Y] = E[X] - E[Y] -> 10 - 3 = 7
    // Var(X-Y) = Var(X) + Var(Y) -> 4^2 + 3^2 = 25
    val theoreticalMean   = 7.0
    val theoreticalStdDev = sqrt(25.0) // 5.0

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("Multiplication of two independent normal distributions should be correct") {
    val x = Uncertain.normal(mean = 10, standardDeviation = 2)
    val y = Uncertain.normal(mean = 5, standardDeviation = 1)
    val z = x * y

    // E[XY] = E[X]E[Y] -> 10 * 5 = 50
    // Var(XY) = (Var(X) + E[X]^2)(Var(Y) + E[Y]^2) - (E[X]E[Y])^2
    // Var(XY) = (2^2 + 10^2)(1^2 + 5^2) - (10*5)^2 = 104 * 26 - 2500 = 204
    val theoreticalMean   = 50.0
    val theoreticalStdDev = sqrt(204.0) // approx 14.28

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("Division of two independent normal distributions should be correct") {
    // Denominator is kept far from zero to ensure stability.
    val x = Uncertain.normal(mean = 100, standardDeviation = 5)
    val y = Uncertain.normal(mean = 10, standardDeviation = 1)
    val z = x / y

    // The exact mean of a ratio distribution is complex. We use a second-order
    // Taylor expansion for the approximation: E[X/Y] ≈ E[X]/E[Y] * (1 + Var(Y)/E[Y]^2)
    // -> 100/10 * (1 + 1^2/10^2) = 10 * 1.01 = 10.1
    val theoreticalMean = 10.1

    val (sampleMean, _) = getStats(z)
    // We only test the mean, as the variance of a ratio distribution is often unstable.
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
  }

  // --- Operations with a constant value ---

  rngTest("Adding a constant to an uncertain value should shift the mean correctly") {
    val x = Uncertain.normal(mean = 10, standardDeviation = 2)
    val c = 5.0
    val z = x + c

    // E[X+c] = E[X] + c -> 10 + 5 = 15
    // Var(X+c) = Var(X) -> 2^2 = 4
    val theoreticalMean   = 15.0
    val theoreticalStdDev = 2.0

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("Multiplying an uncertain value by a constant should scale the mean and stddev correctly") {
    val x = Uncertain.normal(mean = 10, standardDeviation = 2)
    val c = 3.0
    val z = x * c

    // E[c*X] = c*E[X] -> 3 * 10 = 30
    // Var(c*X) = c^2 * Var(X) -> 3^2 * 2^2 = 36
    val theoreticalMean   = 30.0
    val theoreticalStdDev = 6.0

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("Dividing an uncertain value by a constant should scale the mean and stddev correctly") {
    val x = Uncertain.normal(mean = 30, standardDeviation = 6)
    val c = 3.0
    val z = x / c

    // E[X/c] = E[X]/c -> 30 / 3 = 10
    // Var(X/c) = (1/c)^2 * Var(X) -> (1/3)^2 * 6^2 = 4
    val theoreticalMean   = 10.0
    val theoreticalStdDev = 2.0

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  // --- Correlation Tests ---

  rngTest("Correlation: `x - x` should always be exactly zero") {
    val x = Uncertain.normal(mean = 100, standardDeviation = 20)
    val z = x - x

    val samples = z.take(1000)
    assert(samples.forall(_ == 0.0), "`x - x` must always be 0.0 due to correlation")
    assertEquals(z.expectedValue(1000), 0.0)
    assertEquals(z.standardDeviation(1000), 0.0)
  }

  rngTest("Correlation: `x / x` should always be exactly one") {
    // Distribution is kept away from zero.
    val x = Uncertain.normal(mean = 50, standardDeviation = 10)
    val z = x / x

    val samples = z.take(1000)
    assert(samples.forall(_ == 1.0), "`x / x` must always be 1.0 due to correlation")
    assertEquals(z.expectedValue(1000), 1.0)
    assertEquals(z.standardDeviation(1000), 0.0)
  }

  rngTest("Correlation: `(x + y) - x` should be equivalent to y") {
    val x = Uncertain.normal(mean = 10, standardDeviation = 2)
    val y = Uncertain.normal(mean = 5, standardDeviation = 3)

    val z = (x + y) - x

    val (zMean, zStdDev) = getStats(z)
    val (yMean, yStdDev) = getStats(y)

    assert(abs(zMean - yMean) < meanTolerance, s"Mean of (x+y)-x ($zMean) should equal mean of y ($yMean)")
    assert(
      abs(zStdDev - yStdDev) < stdDevTolerance,
      s"StdDev of (x+y)-x ($zStdDev) should equal StdDev of y ($yStdDev)"
    )
  }

  // --- Complex "Real World" Scenarios ---

  rngTest("Real-world scenario with multiple uncertain variables: calculating profit per unit") {
    // Analogy: A small business with uncertainty in its costs, sales, and prices.
    val pricePerUnit        = Uncertain.normal(mean = 50, standardDeviation = 2)
    val unitsSold           = Uncertain.normal(mean = 1000, standardDeviation = 50)
    val fixedCosts          = Uncertain.normal(mean = 10000, standardDeviation = 500)
    val variableCostPerUnit = Uncertain.normal(mean = 20, standardDeviation = 1)

    // Profit Per Unit = ((Price * Units) - (FixedCosts + VarCost * Units)) / Units
    // This simplifies to (Price - VarCost) - (FixedCosts / Units)
    val profitPerUnit = (pricePerUnit - variableCostPerUnit) - (fixedCosts / unitsSold)

    // E[P-V] = E[P] - E[V] = 50 - 20 = 30
    // E[F/U] ≈ E[F]/E[U] * (1 + Var(U)/E[U]^2) = 10000/1000 * (1 + 50^2/1000^2) = 10.025
    // E[ProfitPerUnit] ≈ 30 - 10.025 = 19.975
    val theoreticalMean = 19.975

    val (sampleMean, _) = getStats(profitPerUnit)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean profit per unit should be ~${theoreticalMean}, but was $sampleMean"
    )
  }

  rngTest("Real-world scenario with static numbers: calibrating a temperature sensor") {
    // Analogy: A sensor reads in Fahrenheit with some error, and we need to convert to calibrated Celsius.
    // The true temp is 20°C ± 1°C, which is 68°F ± 1.8°F.
    val fahrenheitReading = Uncertain.normal(mean = 68, standardDeviation = 1.8)
    val calibrationOffset = -0.5 // The sensor consistently reads 0.5°C too high.

    // Formula: Calibrated Celsius = (Fahrenheit - 32) * 5/9 + Offset
    val calibratedCelsius = ((fahrenheitReading - 32.0) * 5.0 / 9.0) + calibrationOffset

    // E[C] = (E[F] - 32) * 5/9 + offset = (68 - 32) * 5/9 - 0.5 = 19.5
    // StdDev(C) = StdDev(F) * 5/9 = 1.8 * 5/9 = 1.0
    val theoreticalMean   = 19.5
    val theoreticalStdDev = 1.0

    val (sampleMean, sampleStdDev) = getStats(calibratedCelsius)
    assert(
      abs(sampleMean - theoreticalMean) < meanTolerance,
      s"Mean calibrated temp should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < stdDevTolerance,
      s"StdDev of calibrated temp should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }
}
