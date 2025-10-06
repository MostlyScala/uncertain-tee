package mostly.uncertaintee.squants

import _root_.squants.*
import _root_.squants.electro.*
import _root_.squants.energy.*
import _root_.squants.information.*
import _root_.squants.mass.*
import _root_.squants.motion.*
import _root_.squants.photo.*
import _root_.squants.radio.*
import _root_.squants.space.*
import _root_.squants.thermal.*
import _root_.squants.time.*
import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.*

/** TODO - this might be better to build atop BigDecimal instead of double, or provide `...viaBigDouble` methods since 
  *
  * Typeclass for computing statistics on Quantity types.
  *
  * Similar to [[StatisticallyConvertible]] bidirectional and unit-aware, allowing statistical operations to preserve
  * quantity types and units.
  */
trait QuantityStatisticallyConvertible[Q <: Quantity[Q]] {

  /** The primary unit to use for internal statistical computations */
  def primaryUnit: UnitOfMeasure[Q]

  /** Convert quantity to double in primary unit for computation */
  def toDouble(q: Q): Double = q.to(primaryUnit)

  /** Create quantity from double in primary unit */
  def fromDouble(d: Double): Q

}

object QuantityStatisticallyConvertible {

  given dimensionlessStats: QuantityStatisticallyConvertible[Dimensionless] with {
    override def primaryUnit: UnitOfMeasure[Dimensionless] = Each
    override def fromDouble(d: Double): Dimensionless      = Each(d)
  }

  // squants.electro
  given capacitanceStats: QuantityStatisticallyConvertible[Capacitance] with {
    override def primaryUnit: UnitOfMeasure[Capacitance] = Farads
    override def fromDouble(d: Double): Capacitance      = Farads(d)
  }

  given conductivityStats: QuantityStatisticallyConvertible[Conductivity] with {
    override def primaryUnit: UnitOfMeasure[Conductivity] = SiemensPerMeter
    override def fromDouble(d: Double): Conductivity      = SiemensPerMeter(d)
  }

  given currentStats: QuantityStatisticallyConvertible[ElectricCurrent] with {
    override def primaryUnit: UnitOfMeasure[ElectricCurrent] = Amperes
    override def fromDouble(d: Double): ElectricCurrent      = Amperes(d)
  }

  given potentialStats: QuantityStatisticallyConvertible[ElectricPotential] with {
    override def primaryUnit: UnitOfMeasure[ElectricPotential] = Volts
    override def fromDouble(d: Double): ElectricPotential      = Volts(d)
  }

  given resistanceStats: QuantityStatisticallyConvertible[ElectricalResistance] with {
    override def primaryUnit: UnitOfMeasure[ElectricalResistance] = Ohms
    override def fromDouble(d: Double): ElectricalResistance      = Ohms(d)
  }

  given chargeStats: QuantityStatisticallyConvertible[ElectricCharge] with {
    override def primaryUnit: UnitOfMeasure[ElectricCharge] = Coulombs
    override def fromDouble(d: Double): ElectricCharge      = Coulombs(d)
  }

  given conductanceStats: QuantityStatisticallyConvertible[ElectricalConductance] with {
    override def primaryUnit: UnitOfMeasure[ElectricalConductance] = Siemens
    override def fromDouble(d: Double): ElectricalConductance      = Siemens(d)
  }

  given inductanceStats: QuantityStatisticallyConvertible[Inductance] with {
    override def primaryUnit: UnitOfMeasure[Inductance] = Henry
    override def fromDouble(d: Double): Inductance      = Henry(d)
  }

  given magneticFluxStats: QuantityStatisticallyConvertible[MagneticFlux] with {
    override def primaryUnit: UnitOfMeasure[MagneticFlux] = Webers
    override def fromDouble(d: Double): MagneticFlux      = Webers(d)
  }

  given magneticFluxDensityStats: QuantityStatisticallyConvertible[MagneticFluxDensity] with {
    override def primaryUnit: UnitOfMeasure[MagneticFluxDensity] = Teslas
    override def fromDouble(d: Double): MagneticFluxDensity      = Teslas(d)
  }

  given resistivityStats: QuantityStatisticallyConvertible[Resistivity] with {
    override def primaryUnit: UnitOfMeasure[Resistivity] = OhmMeters
    override def fromDouble(d: Double): Resistivity      = OhmMeters(d)
  }

  // squants.energy
  given energyStats: QuantityStatisticallyConvertible[Energy] with {
    override def primaryUnit: UnitOfMeasure[Energy] = Joules
    override def fromDouble(d: Double): Energy      = Joules(d)
  }

  given energyDensityStats: QuantityStatisticallyConvertible[EnergyDensity] with {
    override def primaryUnit: UnitOfMeasure[EnergyDensity] = JoulesPerCubicMeter
    override def fromDouble(d: Double): EnergyDensity      = JoulesPerCubicMeter(d)
  }

  given powerStats: QuantityStatisticallyConvertible[Power] with {
    override def primaryUnit: UnitOfMeasure[Power] = Watts
    override def fromDouble(d: Double): Power      = Watts(d)
  }

  given powerRampStats: QuantityStatisticallyConvertible[PowerRamp] with {
    override def primaryUnit: UnitOfMeasure[PowerRamp] = WattsPerHour
    override def fromDouble(d: Double): PowerRamp      = WattsPerHour(d)
  }

  given specificEnergyStats: QuantityStatisticallyConvertible[SpecificEnergy] with {
    override def primaryUnit: UnitOfMeasure[SpecificEnergy] = Grays
    override def fromDouble(d: Double): SpecificEnergy      = Grays(d)
  }

  // squants.information
  given informationStats: QuantityStatisticallyConvertible[Information] with {
    override def primaryUnit: UnitOfMeasure[Information] = Bytes
    override def fromDouble(d: Double): Information      = Bytes(d)
  }

  // squants.mass
  given areaDensityStats: QuantityStatisticallyConvertible[AreaDensity] with {
    override def primaryUnit: UnitOfMeasure[AreaDensity] = KilogramsPerSquareMeter
    override def fromDouble(d: Double): AreaDensity      = KilogramsPerSquareMeter(d)
  }

  given chemicalAmountStats: QuantityStatisticallyConvertible[ChemicalAmount] with {
    override def primaryUnit: UnitOfMeasure[ChemicalAmount] = Moles
    override def fromDouble(d: Double): ChemicalAmount      = Moles(d)
  }

  given densityStats: QuantityStatisticallyConvertible[Density] with {
    override def primaryUnit: UnitOfMeasure[Density] = KilogramsPerCubicMeter
    override def fromDouble(d: Double): Density      = KilogramsPerCubicMeter(d)
  }

  given massStats: QuantityStatisticallyConvertible[Mass] with {
    override def primaryUnit: UnitOfMeasure[Mass] = Kilograms
    override def fromDouble(d: Double): Mass      = Kilograms(d)
  }

  given massFlowRateStats: QuantityStatisticallyConvertible[MassFlow] with {
    override def primaryUnit: UnitOfMeasure[MassFlow] = KilogramsPerSecond
    override def fromDouble(d: Double): MassFlow      = KilogramsPerSecond(d)
  }

  given molarEnergyStats: QuantityStatisticallyConvertible[MolarEnergy] with {
    override def primaryUnit: UnitOfMeasure[MolarEnergy] = JoulesPerMole
    override def fromDouble(d: Double): MolarEnergy      = JoulesPerMole(d)
  }

  given volumeFlowStats: QuantityStatisticallyConvertible[VolumeFlow] with {
    override def primaryUnit: UnitOfMeasure[VolumeFlow] = CubicMetersPerSecond
    override def fromDouble(d: Double): VolumeFlow      = CubicMetersPerSecond(d)
  }

  // squants.motion
  given accelerationStats: QuantityStatisticallyConvertible[Acceleration] with {
    override def primaryUnit: UnitOfMeasure[Acceleration] = MetersPerSecondSquared
    override def fromDouble(d: Double): Acceleration      = MetersPerSecondSquared(d)
  }

  given forceStats: QuantityStatisticallyConvertible[Force] with {
    override def primaryUnit: UnitOfMeasure[Force] = Newtons
    override def fromDouble(d: Double): Force      = Newtons(d)
  }

  given jerkStats: QuantityStatisticallyConvertible[Jerk] with {
    override def primaryUnit: UnitOfMeasure[Jerk] = MetersPerSecondCubed
    override def fromDouble(d: Double): Jerk      = MetersPerSecondCubed(d)
  }

  given momentumStats: QuantityStatisticallyConvertible[Momentum] with {
    override def primaryUnit: UnitOfMeasure[Momentum] = NewtonSeconds
    override def fromDouble(d: Double): Momentum      = NewtonSeconds(d)
  }

  given pressureStats: QuantityStatisticallyConvertible[Pressure] with {
    override def primaryUnit: UnitOfMeasure[Pressure] = Pascals
    override def fromDouble(d: Double): Pressure      = Pascals(d)
  }

  given pressureChangeStats: QuantityStatisticallyConvertible[PressureChange] with {
    override def primaryUnit: UnitOfMeasure[PressureChange] = PascalsPerSecond
    override def fromDouble(d: Double): PressureChange      = PascalsPerSecond(d)
  }

  given velocityStats: QuantityStatisticallyConvertible[Velocity] with {
    override def primaryUnit: UnitOfMeasure[Velocity] = MetersPerSecond
    override def fromDouble(d: Double): Velocity      = MetersPerSecond(d)
  }

  given yankStats: QuantityStatisticallyConvertible[Yank] with {
    override def primaryUnit: UnitOfMeasure[Yank] = NewtonsPerSecond
    override def fromDouble(d: Double): Yank      = NewtonsPerSecond(d)
  }

  // squants.photo
  given illuminanceStats: QuantityStatisticallyConvertible[Illuminance] with {
    override def primaryUnit: UnitOfMeasure[Illuminance] = Lux
    override def fromDouble(d: Double): Illuminance      = Lux(d)
  }

  given luminanceStats: QuantityStatisticallyConvertible[Luminance] with {
    override def primaryUnit: UnitOfMeasure[Luminance] = CandelasPerSquareMeter
    override def fromDouble(d: Double): Luminance      = CandelasPerSquareMeter(d)
  }

  given luminousFluxStats: QuantityStatisticallyConvertible[LuminousFlux] with {
    override def primaryUnit: UnitOfMeasure[LuminousFlux] = Lumens
    override def fromDouble(d: Double): LuminousFlux      = Lumens(d)
  }

  given luminousIntensityStats: QuantityStatisticallyConvertible[LuminousIntensity] with {
    override def primaryUnit: UnitOfMeasure[LuminousIntensity] = Candelas
    override def fromDouble(d: Double): LuminousIntensity      = Candelas(d)
  }

  // squants.radio
  given irradianceStats: QuantityStatisticallyConvertible[Irradiance] with {
    override def primaryUnit: UnitOfMeasure[Irradiance] = WattsPerSquareMeter
    override def fromDouble(d: Double): Irradiance      = WattsPerSquareMeter(d)
  }

  given radianceStats: QuantityStatisticallyConvertible[Radiance] with {
    override def primaryUnit: UnitOfMeasure[Radiance] = WattsPerSteradianPerSquareMeter
    override def fromDouble(d: Double): Radiance      = WattsPerSteradianPerSquareMeter(d)
  }

  given radiantIntensity: QuantityStatisticallyConvertible[RadiantIntensity] with {
    override def primaryUnit: UnitOfMeasure[RadiantIntensity] = WattsPerSteradian
    override def fromDouble(d: Double): RadiantIntensity      = WattsPerSteradian(d)
  }

  // squants.space
  given angleStats: QuantityStatisticallyConvertible[Angle] with {
    override def primaryUnit: UnitOfMeasure[Angle] = Radians
    override def fromDouble(d: Double): Angle      = Radians(d)
  }

  given areaStats: QuantityStatisticallyConvertible[Area] with {
    override def primaryUnit: UnitOfMeasure[Area] = SquareMeters
    override def fromDouble(d: Double): Area      = SquareMeters(d)
  }

  given lengthStats: QuantityStatisticallyConvertible[Length] with {
    override def primaryUnit: UnitOfMeasure[Length] = Meters
    override def fromDouble(d: Double): Length      = Meters(d)
  }

  given solidAngleStats: QuantityStatisticallyConvertible[SolidAngle] with {
    override def primaryUnit: UnitOfMeasure[SolidAngle] = SquareRadians
    override def fromDouble(d: Double): SolidAngle      = SquareRadians(d)
  }

  given volumeStats: QuantityStatisticallyConvertible[Volume] with {
    override def primaryUnit: UnitOfMeasure[Volume] = CubicMeters
    override def fromDouble(d: Double): Volume      = CubicMeters(d)
  }

  // squants.thermal
  given temperatureStats: QuantityStatisticallyConvertible[Temperature] with {
    override def primaryUnit: UnitOfMeasure[Temperature] = Kelvin
    override def fromDouble(d: Double): Temperature      = Kelvin(d)
  }

  given thermalCapacityStats: QuantityStatisticallyConvertible[ThermalCapacity] with {
    override def primaryUnit: UnitOfMeasure[ThermalCapacity] = JoulesPerKelvin
    override def fromDouble(d: Double): ThermalCapacity      = JoulesPerKelvin(d)
  }

  // squants.time
  given frequencyStats: QuantityStatisticallyConvertible[Frequency] with {
    override def primaryUnit: UnitOfMeasure[Frequency] = Hertz
    override def fromDouble(d: Double): Frequency      = Hertz(d)
  }

  given timeStats: QuantityStatisticallyConvertible[Time] with {
    override def primaryUnit: UnitOfMeasure[Time] = Seconds
    override def fromDouble(d: Double): Time      = Seconds(d)
  }
}
