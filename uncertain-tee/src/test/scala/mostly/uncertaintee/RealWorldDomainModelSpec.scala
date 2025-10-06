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

import scala.math.abs
import scala.util.Random

/** This spec showcases the library by contextualising it with "real-world" use-cases
  *
  * Each test acts as a small tutorial, demonstrating how to model complex, uncertain scenarios using custom domain
  * models (case classes, sealed traits).
  */
class RealWorldDomainModelSpec extends RngSuite {

  private val sampleCount = 10_000

  // =================================================================================================
  // 1. E-commerce: A/B Testing
  // =================================================================================================

  sealed trait WebVariant
  case object VariantA extends WebVariant
  case object VariantB extends WebVariant
  case class ABTestResult(winner: WebVariant, relativeImprovement: Double)

  /** Models an A/B test where the conversion rates for two website variants are uncertain. We use a for-comprehension
    * to sample from both uncertain rates simultaneously, determine the winner for that sample, and calculate the
    * improvement. The result is an `Uncertain[ABTestResult]` which can be analyzed to see how likely VariantB is to
    * win.
    */
  rngTest("1. E-commerce: A/B Testing for Website Conversion") {
    // Conversion rates are modeled as uncertain, perhaps from early data.
    val conversionA = Uncertain.normal(mean = 0.10, standardDeviation = 0.02) // 10% ± 2%
    val conversionB = Uncertain.normal(mean = 0.12, standardDeviation = 0.02) // 12% ± 2%

    val testResult = for {
      a     <- conversionA
      b     <- conversionB
      result = if (b > a)
                 ABTestResult(
                   winner = VariantB,
                   relativeImprovement = (b - a) / a
                 )
               else
                 ABTestResult(
                   winner = VariantA,
                   relativeImprovement = (a - b) / a
                 )
    } yield result

    val samples              = testResult.take(sampleCount)
    val bWins                = samples.count(_.winner == VariantB)
    val probabilityBIsBetter = bWins.toDouble / sampleCount

    // With B having a higher mean, it should win more than 50% of the time.
    assert(
      probabilityBIsBetter > 0.6,
      s"Expected Variant B to win with >60% probability, but it won only in ${probabilityBIsBetter * 100}% of simulations."
    )
  }

  // =================================================================================================
  // 2. Supply Chain: Inventory and Stockout Risk
  // =================================================================================================

  sealed trait InventoryStatus
  case class Stockout(unitsShort: Int) extends InventoryStatus
  case class InStock(unitsLeft: Int)   extends InventoryStatus

  /** Models inventory management where both product demand and shipment lead times are uncertain. We use `flatMap` to
    * create a dependency: first, we determine the lead time, and *then* we model the total demand *during that specific
    * time*. This allows us to find the probability of a stockout.
    */
  rngTest("2. Supply Chain: Modeling Stockout Risk") {
    val initialStock = 100
    // Lead time for the next shipment is uncertain (e.g., 5 to 10 days).
    val leadTimeDays = Uncertain.uniform(5, 11).map(_.toInt)
    // Daily demand is also uncertain.
    val dailyDemand  = Uncertain.poisson(15) // Average 15 units/day

    val inventoryStatus = leadTimeDays.flatMap { days =>
      // Total demand is the sum of `days` independent daily demands.
      val totalDemand = (1 to days).map(_ => dailyDemand).reduce(_ + _)
      totalDemand.map { demand =>
        if (demand > initialStock) Stockout(demand - initialStock)
        else InStock(initialStock - demand)
      }
    }

    val samples             = inventoryStatus.take(sampleCount)
    val stockoutProbability = samples.count(_.isInstanceOf[Stockout]).toDouble / sampleCount

    // Expected demand: E[leadTime]*E[dailyDemand] = 8 * 15 = 120.
    // Since expected demand (120) > stock (100), a stockout is the more likely outcome.
    assert(
      stockoutProbability > 0.5,
      s"Expected stockout probability to be >50%, but it was only ${stockoutProbability * 100}%."
    )
  }

  // =================================================================================================
  // 3. Finance: Profitability Projection
  // =================================================================================================

  /** Models a simple financial projection where both revenue and costs are uncertain. A for-comprehension combines
    * these two factors to produce an `Uncertain[Double]` for profit. We then `map` this uncertain profit into a domain
    * model (`Profitability`) to easily calculate the probability of the venture being profitable.
    */
  rngTest("3. Finance: Projecting Profitability") {
    sealed trait Profitability
    case object Profitable extends Profitability
    case object Loss       extends Profitability

    val revenue = Uncertain.normal(mean = 100000, standardDeviation = 15000)
    val costs   = Uncertain.normal(mean = 85000, standardDeviation = 10000)

    val profit = for {
      r <- revenue
      c <- costs
    } yield r - c

    val profitability = profit.map(p => if (p > 0) Profitable else Loss)

    val probabilityOfProfit = profitability.map(_ == Profitable).expectedValue(sampleCount)
    assert(
      probabilityOfProfit > 0.75,
      s"Expected probability of profit to be >75%, but it was ${probabilityOfProfit * 100}%."
    )
  }

  // =================================================================================================
  // 4. Healthcare: Medical Test Classification
  // =================================================================================================

  /** Models a medical device reading that has measurement error. The uncertain reading (a `Double`) is transformed
    * using `map` into a clear, understandable diagnostic category. This separates the raw data from its clinical
    * interpretation.
    */
  rngTest("4. Healthcare: Classifying Uncertain Blood Pressure Readings") {
    sealed trait BloodPressureCategory
    case object Normal       extends BloodPressureCategory
    case object Elevated     extends BloodPressureCategory
    case object Hypertension extends BloodPressureCategory

    // A systolic reading, where the true value is 125 but there's measurement error.
    val systolicReading = Uncertain.normal(mean = 125, standardDeviation = 8)

    val category = systolicReading.map { reading =>
      if (reading < 120) Normal
      else if (reading < 130) Elevated
      else Hypertension
    }

    val frequencies        = category.histogram(sampleCount)
    val mostLikelyCategory = frequencies.maxBy(_._2)._1

    assertEquals(
      mostLikelyCategory,
      Elevated,
      s"Expected most likely category to be Elevated, but got $mostLikelyCategory."
    )
  }

  // =================================================================================================
  // 5. Project Management: Sequential Task Completion
  // =================================================================================================

  /** Models a project with two *sequential* tasks, where each task's duration is uncertain. A for-comprehension is used
    * to sum the durations. The total duration is then `map`ped to a status to determine the probability of the project
    * being delayed past its deadline.
    */
  rngTest("5. Project Management: Estimating Project Timelines") {
    sealed trait ProjectTimeline
    case object OnTime  extends ProjectTimeline
    case object Delayed extends ProjectTimeline

    val task1Duration = Uncertain.uniform(5, 10) // 5-9 days
    val task2Duration = Uncertain.uniform(8, 15) // 8-14 days
    val deadline      = 22.0

    val totalDuration = for {
      t1 <- task1Duration
      t2 <- task2Duration
    } yield t1 + t2

    val timeline = totalDuration.map(d => if (d <= deadline) OnTime else Delayed)

    val probabilityDelayed = timeline.map(_ == Delayed).expectedValue(sampleCount)
    // E[Total] = E[T1] + E[T2] = 7.5 + 11.5 = 19.0.
    // Since the mean is below the deadline, the delay probability should be less than 50%.
    assert(
      probabilityDelayed < 0.5,
      s"Expected probability of delay to be <50%, but it was ${probabilityDelayed * 100}%."
    )
  }

  // =================================================================================================
  // 6. Insurance: Claim Cost Modeling
  // =================================================================================================

  /** Models an insurance scenario with two levels of uncertainty. First, is there an accident? (a `Boolean`). Second,
    * *if* there is an accident, what is the cost? (a `Double`). `flatMap` is perfect for this dependency, creating a
    * final distribution of claim costs that includes many samples of zero (for no accident).
    */
  rngTest("6. Insurance: Modeling Uncertain Claim Costs") {
    case class InsuranceClaim(cost: Double)

    // 5% chance of a claim being filed in a given period.
    val isClaimFiled  = Uncertain.bernoulli(0.05)
    // If a claim is filed, the cost follows an exponential distribution.
    val claimSeverity = Uncertain.exponential(rate = 0.0005) // Average cost of 1/rate = $2000

    val claimCost = isClaimFiled.flatMap { filed =>
      if (filed) claimSeverity.map(cost => InsuranceClaim(cost))
      else Uncertain.point(InsuranceClaim(0.0)) // No claim, zero cost
    }

    val expectedCost = claimCost.map(_.cost).mean(sampleCount)
    // Theoretical E[Cost] = P(claim) * E[severity] = 0.05 * 2000 = 100
    assert(
      abs(expectedCost - 100.0) < 10.0,
      s"Expected cost should be around 100.0 (within 10.0), but was $expectedCost."
    )
  }

  // =================================================================================================
  // 7. Agriculture: Crop Yield Prediction
  // =================================================================================================

  /** Models crop yield based on two uncertain environmental factors: rainfall and temperature. A for-comprehension
    * combines them into a single "growth score". This score is then `map`ped to a qualitative `YieldQuality`, allowing
    * us to assess the outlook.
    */
  rngTest("7. Agriculture: Predicting Crop Yield") {

    sealed trait YieldQuality
    case object Poor      extends YieldQuality
    case object Average   extends YieldQuality
    case object Bountiful extends YieldQuality

    // Normalized values, where 1.0 is ideal.
    val rainfallFactor    = Uncertain.normal(mean = 0.9, standardDeviation = 0.2)
    val temperatureFactor = Uncertain.normal(mean = 1.0, standardDeviation = 0.15)

    // A simple model where yield is proportional to the product of the factors.
    val growthScore = for {
      r <- rainfallFactor
      t <- temperatureFactor
    } yield r * t

    val yieldQuality = growthScore.map { score =>
      if (score < 0.7) Poor
      else if (score < 1.1) Average
      else Bountiful
    }

    val frequencies     = yieldQuality.histogram(sampleCount)
    val mostLikelyYield = frequencies.maxBy(_._2)._1
    // E[Score] = E[R]*E[T] = 0.9 * 1.0 = 0.9. This falls in the "Average" range.
    assertEquals(
      mostLikelyYield,
      Average,
      s"Expected most likely yield quality to be Average, but got $mostLikelyYield."
    )
  }

  // =================================================================================================
  // 8. Systems Engineering: Server Load Simulation
  // =================================================================================================

  /** Models the number of concurrent users on a server using a Poisson distribution, which is common for arrival rates.
    * The uncertain number of users (`Int`) is then `map`ped to a `ServerStatus` based on capacity thresholds.
    */
  rngTest("8. Systems Engineering: Simulating Server Load Status") {

    sealed trait ServerStatus
    case object Idle       extends ServerStatus
    case object Busy       extends ServerStatus
    case object Overloaded extends ServerStatus

    // Average of 80 concurrent users, but with random fluctuation.
    val concurrentUsers   = Uncertain.poisson(80)
    val busyThreshold     = 50
    val overloadThreshold = 100

    val serverStatus = concurrentUsers.map { users =>
      if (users > overloadThreshold) Overloaded
      else if (users > busyThreshold) Busy
      else Idle
    }

    val probabilityOverloaded = serverStatus.map(_ == Overloaded).expectedValue(sampleCount)
    // The mean is 80, so it's far from the overload threshold of 100.
    // The probability of overload should be small but non-zero.
    assert(
      probabilityOverloaded > 0.01 && probabilityOverloaded < 0.2,
      s"Expected overload probability to be between 1% and 20%, but it was ${probabilityOverloaded * 100}%."
    )
  }

  // =================================================================================================
  // 9. Game Development: AI Decision Making
  // =================================================================================================

  /** Models an AI agent making a decision based on an uncertain situation. The AI first assesses its advantage (a
    * `Double`). Based on this uncertain assessment, it `flatMap`s into a distribution of possible actions, with
    * probabilities changing based on the situation.
    */
  rngTest("9. Game Development: Probabilistic AI Decision Making") {
    sealed trait AIAction
    case object Attack extends AIAction
    case object Defend extends AIAction
    case object Flee   extends AIAction
    // AI's assessment of its advantage. Positive means it thinks it's winning.
    val advantageAssessment = Uncertain.normal(mean = 5, standardDeviation = 10)

    val aiAction = advantageAssessment.flatMap { advantage =>
      val attackProb = if (advantage > 10) 0.8 else if (advantage > -10) 0.5 else 0.1
      val fleeProb   = if (advantage < -15) 0.7 else 0.1
      val defendProb = (1.0 - attackProb - fleeProb).max(0) // Ensure non-negative

      Uncertain.categorical(
        Map(
          Attack -> attackProb,
          Defend -> defendProb,
          Flee   -> fleeProb
        )
      )
    }

    val frequencies      = aiAction.histogram(sampleCount)
    val mostCommonAction = frequencies.maxBy(_._2)._1
    // Mean advantage is 5, which is between -10 and 10, so Attack prob is 0.5. This should be the most common action.
    assertEquals(mostCommonAction, Attack, s"Expected most common AI action to be Attack, but got $mostCommonAction.")
  }

  // =================================================================================================
  // 10. Marketing: Customer Lifetime Value (CLV)
  // =================================================================================================

  case class Customer(clv: Double)

  /** Models Customer Lifetime Value (CLV), a key marketing metric. CLV depends on uncertain factors: how long a
    * customer stays (retention), and how much they spend per year. This example uses a recursive function with
    * `flatMap` to model the year-on-year retention process, which is a powerful pattern for processes that repeat an
    * uncertain number of times.
    */
  rngTest("10. Marketing: Estimating Customer Lifetime Value (CLV)") {
    val annualSpend          = Uncertain.normal(mean = 200, standardDeviation = 50)
    val retentionProbability = 0.7

    // A recursive function is a great way to model a process that repeats
    // an uncertain number of times. Each step in the recursion represents one year.
    def modelRetention(yearsSoFar: Int): Uncertain[Int] =
      Uncertain.bernoulli(retentionProbability).flatMap { isRetained =>
        if (isRetained) {
          // Customer stays, continue to the next year by recurring.
          modelRetention(yearsSoFar + 1)
        } else {
          // Customer churns, the process stops. Return the total years they stayed.
          Uncertain.point(yearsSoFar)
        }
      }

    // Start the simulation from year 0. The result is an uncertain number of years.
    val yearsRetained = modelRetention(0)

    val customerCLV = for {
      spend <- annualSpend
      years <- yearsRetained
    } yield Customer(spend * years)

    val expectedCLV = customerCLV.map(_.clv).mean(sampleCount)
    // E[Years] for geometric P(success)=p is p/(1-p) = 0.7/0.3 ≈ 2.33
    // E[CLV] ≈ E[Spend] * E[Years] = 200 * 2.33 ≈ 466
    // This is a rough approximation.
    assert(abs(expectedCLV - 466.0) < 50.0, s"Expected CLV to be around 466.0 (within 50.0), but was $expectedCLV.")
  }
}
