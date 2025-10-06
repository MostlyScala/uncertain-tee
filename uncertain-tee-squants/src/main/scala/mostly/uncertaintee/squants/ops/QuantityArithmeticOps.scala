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

package mostly.uncertaintee.squants.ops

import _root_.squants._
import _root_.squants.electro._
import _root_.squants.motion._
import mostly.uncertaintee._

trait QuantityArithmeticOps {

  /** Addition and subtraction for quantities of the same dimension */
  extension [Q <: Quantity[Q]](lhs: Uncertain[Q]) {

    /** Adds two uncertain quantities sample-by-sample. */
    def +(rhs: Uncertain[Q]): Uncertain[Q] = for {
      l <- lhs
      r <- rhs
    } yield l + r

    /** Subtracts two uncertain quantities sample-by-sample. */
    def -(rhs: Uncertain[Q]): Uncertain[Q] = for {
      l <- lhs
      r <- rhs
    } yield l - r

    /** Adds a constant quantity to an uncertain quantity. */
    def +(rhs: Q): Uncertain[Q] = lhs.map(l => l + rhs)

    /** Subtracts a constant quantity from an uncertain quantity. */
    def -(rhs: Q): Uncertain[Q] = lhs.map(l => l - rhs)

    /** Negates an uncertain quantity. */
    def unary_- : Uncertain[Q] = lhs.map(-_)

    /** Takes the absolute value of an uncertain quantity. */
    def abs: Uncertain[Q] = lhs.map(_.abs)

    /** Multiplies an uncertain quantity by a scalar (Double). */
    def *(rhs: Double): Uncertain[Q] = lhs.map(_ * rhs)

    /** Divides an uncertain quantity by a scalar (Double). */
    def /(rhs: Double): Uncertain[Q] = lhs.map(_ / rhs)

    /** Divides an uncertain quantity by a constant quantity.
      *
      * Returns a dimensionless ratio (Double).
      */
    def /(rhs: Q): Uncertain[Double] = lhs.map(l => l / rhs)

    /** Divides two uncertain quantities sample-by-sample.
      *
      * Returns an uncertain dimensionless ratio.
      */
    def /(rhs: Uncertain[Q]): Uncertain[Double] = for {
      l <- lhs
      r <- rhs
    } yield l / r
  }

  /** Generic cross-dimension multiplication and division.
    *
    * This leverages typeclasses to automatically determine the result type based on dimensional analysis.
    */
  extension [Q1 <: Quantity[Q1]](lhs: Uncertain[Q1]) {

    /** Multiplies two uncertain quantities, with result type determined by typeclass. */
    def times[Q2 <: Quantity[Q2], R <: Quantity[R]](
      rhs: Uncertain[Q2]
    )(using ev: MultiplyOp[Q1, Q2, R]): Uncertain[R] = for {
      l <- lhs
      r <- rhs
    } yield ev.multiply(l, r)

    /** Multiplies uncertain quantity by a constant. */
    def times[Q2 <: Quantity[Q2], R <: Quantity[R]](
      rhs: Q2
    )(using ev: MultiplyOp[Q1, Q2, R]): Uncertain[R] =
      lhs.map(l => ev.multiply(l, rhs))

    /** Divides two uncertain quantities, with result type determined by typeclass. */
    def dividedBy[Q2 <: Quantity[Q2], R <: Quantity[R]](
      rhs: Uncertain[Q2]
    )(using ev: DivideOp[Q1, Q2, R]): Uncertain[R] = for {
      l <- lhs
      r <- rhs
    } yield ev.divide(l, r)

    /** Divides uncertain quantity by a constant. */
    def dividedBy[Q2 <: Quantity[Q2], R <: Quantity[R]](
      rhs: Q2
    )(using ev: DivideOp[Q1, Q2, R]): Uncertain[R] =
      lhs.map(l => ev.divide(l, rhs))
  }

  /** Helper operations for Length */
  extension (lhs: Uncertain[Length]) {

    /** Squares a length to produce an area. */
    def squared: Uncertain[Area] = lhs.map(l => l * l)

    /** Cubes a length to produce a volume. */
    def cubed: Uncertain[Volume] = lhs.map(l => l * l * l)
  }
}

/** Comparison operations for uncertain quantities */
trait QuantityComparisonOps {

  extension [Q <: Quantity[Q]](lhs: Uncertain[Q]) {

    /** Sample-by-sample equality comparison. */
    def ===(rhs: Uncertain[Q]): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield l == r

    /** Sample-by-sample inequality comparison. */
    def !==(rhs: Uncertain[Q]): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield l != r

    /** Greater than comparison with constant. */
    def >(rhs: Q): Uncertain[Boolean] = lhs.map(_ > rhs)

    /** Less than comparison with constant. */
    def <(rhs: Q): Uncertain[Boolean] = lhs.map(_ < rhs)

    /** Greater than or equal comparison with constant. */
    def >=(rhs: Q): Uncertain[Boolean] = lhs.map(_ >= rhs)

    /** Less than or equal comparison with constant. */
    def <=(rhs: Q): Uncertain[Boolean] = lhs.map(_ <= rhs)

    /** Greater than comparison between two uncertain quantities. */
    def >(rhs: Uncertain[Q]): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield l > r

    /** Less than comparison between two uncertain quantities. */
    def <(rhs: Uncertain[Q]): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield l < r

    /** Greater than or equal comparison between two uncertain quantities. */
    def >=(rhs: Uncertain[Q]): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield l >= r

    /** Less than or equal comparison between two uncertain quantities. */
    def <=(rhs: Uncertain[Q]): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield l <= r

    /** Approximates a quantity (useful for avoiding exact equality checks). */
    def approx(rhs: Q, tolerance: Q): Uncertain[Boolean] =
      lhs.map(l => (l - rhs).abs <= tolerance)

    /** Approximates another uncertain quantity. */
    def approx(rhs: Uncertain[Q], tolerance: Q): Uncertain[Boolean] = for {
      l <- lhs
      r <- rhs
    } yield (l - r).abs <= tolerance
  }
}

/** Typeclass for multiplication operations that change dimensions. */
trait MultiplyOp[Q1 <: Quantity[Q1], Q2 <: Quantity[Q2], R <: Quantity[R]] {
  def multiply(a: Q1, b: Q2): R
}

object MultiplyOp {
  // Spatial operations
  given lengthTimesLength: MultiplyOp[Length, Length, Area] with {
    def multiply(a: Length, b: Length): Area = a * b
  }

  given areaTimesLength: MultiplyOp[Area, Length, Volume] with {
    def multiply(a: Area, b: Length): Volume = a * b
  }

  given lengthTimesArea: MultiplyOp[Length, Area, Volume] with {
    def multiply(a: Length, b: Area): Volume = a * b
  }

  // Mechanics - Force and Energy
  given massTimesAcceleration: MultiplyOp[Mass, Acceleration, Force] with {
    def multiply(a: Mass, b: Acceleration): Force = a * b
  }

  given accelerationTimesMass: MultiplyOp[Acceleration, Mass, Force] with {
    def multiply(a: Acceleration, b: Mass): Force = a * b
  }

  given forceTimesLength: MultiplyOp[Force, Length, Energy] with {
    def multiply(a: Force, b: Length): Energy = a * b
  }

  given lengthTimesForce: MultiplyOp[Length, Force, Energy] with {
    def multiply(a: Length, b: Force): Energy = a * b
  }

  // Momentum
  given massTimesVelocity: MultiplyOp[Mass, Velocity, Momentum] with {
    def multiply(a: Mass, b: Velocity): Momentum = a * b
  }

  given velocityTimesMass: MultiplyOp[Velocity, Mass, Momentum] with {
    def multiply(a: Velocity, b: Mass): Momentum = a * b
  }

  // Power and Energy
  given powerTimesTime: MultiplyOp[Power, Time, Energy] with {
    def multiply(a: Power, b: Time): Energy = a * b
  }

  given timesTimesPower: MultiplyOp[Time, Power, Energy] with {
    def multiply(a: Time, b: Power): Energy = a * b
  }

  // Electrical
  given currentTimesResistance: MultiplyOp[ElectricCurrent, ElectricalResistance, ElectricPotential] with {
    def multiply(a: ElectricCurrent, b: ElectricalResistance): ElectricPotential = a * b
  }

  given resistanceTimesCurrent: MultiplyOp[ElectricalResistance, ElectricCurrent, ElectricPotential] with {
    def multiply(a: ElectricalResistance, b: ElectricCurrent): ElectricPotential = a * b
  }

  given currentTimesPotential: MultiplyOp[ElectricCurrent, ElectricPotential, Power] with {
    def multiply(a: ElectricCurrent, b: ElectricPotential): Power = a * b
  }

  given potentialTimesCurrent: MultiplyOp[ElectricPotential, ElectricCurrent, Power] with {
    def multiply(a: ElectricPotential, b: ElectricCurrent): Power = a * b
  }

  // Density
  given densityTimesVolume: MultiplyOp[Density, Volume, Mass] with {
    def multiply(a: Density, b: Volume): Mass = a * b
  }

  given volumeTimesDensity: MultiplyOp[Volume, Density, Mass] with {
    def multiply(a: Volume, b: Density): Mass = a * b
  }

  // Pressure and Force
  given pressureTimesArea: MultiplyOp[Pressure, Area, Force] with {
    def multiply(a: Pressure, b: Area): Force = a * b
  }

  given areaTimesPressure: MultiplyOp[Area, Pressure, Force] with {
    def multiply(a: Area, b: Pressure): Force = a * b
  }
}

/** Typeclass for division operations that change dimensions. */
trait DivideOp[Q1 <: Quantity[Q1], Q2 <: Quantity[Q2], R <: Quantity[R]] {
  def divide(a: Q1, b: Q2): R
}

object DivideOp {
  // Kinematics
  given lengthDivTime: DivideOp[Length, Time, Velocity] with {
    def divide(a: Length, b: Time): Velocity = a / b
  }

  given velocityDivTime: DivideOp[Velocity, Time, Acceleration] with {
    def divide(a: Velocity, b: Time): Acceleration = a / b
  }

  given areaDivLength: DivideOp[Area, Length, Length] with {
    def divide(a: Area, b: Length): Length = a / b
  }

  given volumeDivLength: DivideOp[Volume, Length, Area] with {
    def divide(a: Volume, b: Length): Area = a / b
  }

  given volumeDivArea: DivideOp[Volume, Area, Length] with {
    def divide(a: Volume, b: Area): Length = a / b
  }

  // Dynamics
  given forceDivMass: DivideOp[Force, Mass, Acceleration] with {
    def divide(a: Force, b: Mass): Acceleration = a / b
  }

  given forceDivAcceleration: DivideOp[Force, Acceleration, Mass] with {
    def divide(a: Force, b: Acceleration): Mass = a / b
  }

  given energyDivLength: DivideOp[Energy, Length, Force] with {
    def divide(a: Energy, b: Length): Force = a / b
  }

  given energyDivForce: DivideOp[Energy, Force, Length] with {
    def divide(a: Energy, b: Force): Length = a / b
  }

  given momentumDivMass: DivideOp[Momentum, Mass, Velocity] with {
    def divide(a: Momentum, b: Mass): Velocity = a / b
  }

  given momentumDivVelocity: DivideOp[Momentum, Velocity, Mass] with {
    def divide(a: Momentum, b: Velocity): Mass = a / b
  }

  // Power and Energy
  given energyDivTime: DivideOp[Energy, Time, Power] with {
    def divide(a: Energy, b: Time): Power = a / b
  }

  given energyDivPower: DivideOp[Energy, Power, Time] with {
    def divide(a: Energy, b: Power): Time = a / b
  }

  given powerDivPotential: DivideOp[Power, ElectricPotential, ElectricCurrent] with {
    def divide(a: Power, b: ElectricPotential): ElectricCurrent = a / b
  }

  given powerDivCurrent: DivideOp[Power, ElectricCurrent, ElectricPotential] with {
    def divide(a: Power, b: ElectricCurrent): ElectricPotential = a / b
  }

  // Electrical
  given potentialDivCurrent: DivideOp[ElectricPotential, ElectricCurrent, ElectricalResistance] with {
    def divide(a: ElectricPotential, b: ElectricCurrent): ElectricalResistance = a / b
  }

  given potentialDivResistance: DivideOp[ElectricPotential, ElectricalResistance, ElectricCurrent] with {
    def divide(a: ElectricPotential, b: ElectricalResistance): ElectricCurrent = a / b
  }

  // Density
  given massDivVolume: DivideOp[Mass, Volume, Density] with {
    def divide(a: Mass, b: Volume): Density = a / b
  }

  given massDivDensity: DivideOp[Mass, Density, Volume] with {
    def divide(a: Mass, b: Density): Volume = a / b
  }

  // Pressure
  given forceDivArea: DivideOp[Force, Area, Pressure] with {
    def divide(a: Force, b: Area): Pressure = a / b
  }

  given forceDivPressure: DivideOp[Force, Pressure, Area] with {
    def divide(a: Force, b: Pressure): Area = a / b
  }
}
