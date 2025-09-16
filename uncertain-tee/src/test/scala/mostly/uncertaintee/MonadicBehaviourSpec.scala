package mostly.uncertaintee

import mostly.uncertaintee.syntax.*

import scala.math.{abs, pow, sqrt}

class MonadicBehaviourSpec extends RngSuite {

  private val sampleCount = 100_000
  private val tolerance   = 0.05

  // --- Helper to get stats from a distribution ---
  // sampleMean, sampleStdDev
  private def getStats(u: Uncertain[Double]): (Double, Double) = {
    val samples  = u.take(sampleCount)
    val mean     = samples.sum / sampleCount
    val variance = samples.map(x => pow(x - mean, 2)).sum / (sampleCount - 1)
    (mean, sqrt(variance))
  }

  // =================================================================================================
  // Basic Monadic Operations
  // =================================================================================================

  rngTest("map should transform the value inside an Uncertain distribution") {
    val x = Uncertain.normal(10, 2)
    val y = x.map(_ * 3.0)

    val theoreticalMean   = 30.0
    val theoreticalStdDev = 6.0

    val (sampleMean, sampleStdDev) = getStats(y)
    assert(abs(sampleMean - theoreticalMean) < tolerance, s"Mean should be ~${theoreticalMean}, but was $sampleMean")
    assert(
      abs(sampleStdDev - theoreticalStdDev) < tolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("flatMap should chain dependent uncertain computations") {
    // A choice that determines which distribution to use next.
    val isHighOutcome = Uncertain.bernoulli(0.4) // 40% chance of being true

    val result = isHighOutcome.flatMap { choice =>
      if (choice) Uncertain.normal(100, 5) // High outcome
      else Uncertain.normal(20, 5) // Low outcome
    }

    // Theoretical mean of mixture = p*E[X1] + (1-p)*E[X2]
    // = 0.4 * 100 + 0.6 * 20 = 40 + 12 = 52
    val theoreticalMean = 52.0

    // Theoretical variance is more complex for a mixture.
    // Var(X) = (p*Var1 + (1-p)*Var2) + (p*(1-p)*(E1-E2)^2)
    // = (0.4*25 + 0.6*25) + (0.4*0.6*(100-20)^2) = 25 + 0.24*6400 = 1561
    val theoreticalStdDev = sqrt(1561.0) // ~39.5

    val (sampleMean, sampleStdDev) = getStats(result)
    assert(
      abs(sampleMean - theoreticalMean) < tolerance * 5,
      s"Mean of mixture should be ~${theoreticalMean}, but was $sampleMean"
    )
    assert(
      abs(sampleStdDev - theoreticalStdDev) < tolerance * 5,
      s"StdDev of mixture should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  rngTest("for-comprehension should correctly combine independent distributions") {
    val x = Uncertain.normal(mean = 10, standardDeviation = 2)
    val y = Uncertain.normal(mean = 5, standardDeviation = 3)

    // This is equivalent to `x + y`
    val z = for {
      a <- x
      b <- y
    } yield a + b

    val theoreticalMean   = 15.0
    val theoreticalStdDev = sqrt(13.0)

    val (sampleMean, sampleStdDev) = getStats(z)
    assert(abs(sampleMean - theoreticalMean) < tolerance, s"Mean should be ~${theoreticalMean}, but was $sampleMean")
    assert(
      abs(sampleStdDev - theoreticalStdDev) < tolerance,
      s"StdDev should be ~${theoreticalStdDev}, but was $sampleStdDev"
    )
  }

  // =================================================================================================
  // Real World Scenarios
  // =================================================================================================

  // --- Scenario 1: Mapping to a Domain Model ---

  sealed trait TemperatureStatus
  case object Cold extends TemperatureStatus
  case object Warm extends TemperatureStatus
  case object Hot  extends TemperatureStatus

  def classifyTemperature(celsius: Double): TemperatureStatus =
    if (celsius < 15.0) Cold
    else if (celsius < 25.0) Warm
    else Hot

  rngTest("map can be used to transform uncertain data into a domain model") {
    val uncertainCelsius: Uncertain[Double] = Uncertain.normal(mean = 22, standardDeviation = 5)

    // Transform the uncertain double into an uncertain status
    val uncertainStatus: Uncertain[TemperatureStatus] = uncertainCelsius.map(classifyTemperature)

    // Calculate the theoretical probabilities for each status
    val pCold = (uncertainCelsius < 15.0).expectedValue(sampleCount)
    val pHot  = (uncertainCelsius >= 25.0).expectedValue(sampleCount)
    val pWarm = 1.0 - pCold - pHot

    // Get the observed frequencies from our mapped distribution
    val samples     = uncertainStatus.take(sampleCount)
    val frequencies = samples.groupBy(identity).view.mapValues(_.length.toDouble / sampleCount).toMap

    assert(abs(frequencies.getOrElse(Cold, 0.0) - pCold) < tolerance, s"Cold frequency should be ~$pCold")
    assert(abs(frequencies.getOrElse(Warm, 0.0) - pWarm) < tolerance, s"Warm frequency should be ~$pWarm")
    assert(abs(frequencies.getOrElse(Hot, 0.0) - pHot) < tolerance, s"Hot frequency should be ~$pHot")
  }

  // --- Scenario 2: Combining Domain Models ---

  sealed trait WeatherForecast
  case object Sunny extends WeatherForecast
  case object Rainy extends WeatherForecast

  sealed trait DayType
  case object Weekday extends DayType
  case object Weekend extends DayType

  case class ParkAttendance(estimatedPeople: Double)

  def estimateAttendance(weather: WeatherForecast, day: DayType): ParkAttendance =
    (weather, day) match {
      case (Sunny, Weekend) => ParkAttendance(500)
      case (Sunny, Weekday) => ParkAttendance(200)
      case (Rainy, Weekend) => ParkAttendance(100)
      case (Rainy, Weekday) => ParkAttendance(50)
    }

  rngTest("for-comprehension can combine different uncertain domain models") {
    // 70% chance of sun
    val weather: Uncertain[WeatherForecast] = Uncertain.bernoulli(0.7).map(if (_) Sunny else Rainy)
    // 20% chance of it being a weekend
    val day: Uncertain[DayType]             = Uncertain.bernoulli(0.2).map(if (_) Weekend else Weekday)

    // Use a for-comprehension to combine these uncertain factors
    val attendanceUncertain: Uncertain[ParkAttendance] = for {
      w <- weather
      d <- day
    } yield estimateAttendance(w, d)

    // Calculate theoretical mean attendance based on probabilities
    // P(S,Wknd)=0.7*0.2=0.14; P(S,Wkdy)=0.7*0.8=0.56; P(R,Wknd)=0.3*0.2=0.06; P(R,Wkdy)=0.3*0.8=0.24
    // E[Att] = (0.14*500) + (0.56*200) + (0.06*100) + (0.24*50) = 70 + 112 + 6 + 12 = 200
    val theoreticalMean = 200.0

    val sampleMean = attendanceUncertain.map(_.estimatedPeople).expectedValue(sampleCount)
    assert(
      abs(sampleMean - theoreticalMean) < tolerance * 10,
      s"Mean attendance should be ~${theoreticalMean}, but was $sampleMean"
    )
  }
}
