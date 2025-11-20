# Real-World Use Cases

Practical examples showing how to use `Uncertain[T]` to solve real problems.

## Project Planning

Estimate project completion with realistic uncertainty.

```scala mdoc
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*

// Task estimates using triangular distribution
val frontend = Uncertain.triangular(5, 10, 20)  // days
val backend = Uncertain.triangular(8, 15, 30)
val testing = Uncertain.triangular(3, 5, 10)

val totalDuration = frontend + backend + testing

// What's the probability we finish in 30 days?
val onTimeProb = (totalDuration < 30).probability(sampleCount = 100_000)
println(s"Probability of finishing in 30 days: ${(onTimeProb * 100).round}%")

// Get percentiles for planning
val percentiles = totalDuration.percentiles(sampleCount = 100_000)
println(s"50% chance: ${percentiles(50)} days")
println(s"90% chance: ${percentiles(90)} days")
println(s"95% chance: ${percentiles(95)} days")
```

**Key insight:** The 50th percentile gives you the median estimate, but planning to the 90th or 95th percentile gives you much more confidence.

## A/B Testing

Determine if a new feature actually improves conversion.

```scala mdoc
// Historical data suggests these conversion rates
val controlConversion = Uncertain.normal(0.08, 0.01)   // 8% ± 1%
val testConversion = Uncertain.normal(0.11, 0.015)     // 11% ± 1.5%

val improvement = testConversion - controlConversion

// Is the improvement real (95% confidence)?
val isSignificant = (improvement > 0.0).probabilityExceeds(
  exceeds = 0.95, 
  sampleCount = 10_000
)

if (isSignificant) {
  println("Test variant is significantly better!")
  println(s"Expected improvement: ${improvement.mean(sampleCount = 10_000) * 100}%")
} else {
  println("Not enough evidence for a significant difference")
}
```

## Measurement with Error

Handle sensor readings with known error bounds.

```scala mdoc
// Temperature sensor with ±0.8°C error
val sensorReading = Uncertain.normal(actualValue = 23.5, error = 0.8)

// Safe operating range: 20-25°C
val isInRange = (sensorReading > 20.0) && (sensorReading < 25.0)

if (isInRange.isProbable(sampleCount = 10_000)) {
  println("Sensor reading is probably in safe range")
} else {
  println("Warning: sensor reading may be outside safe range")
}

// Get confidence interval
val (low, high) = sensorReading.confidenceInterval(0.95, sampleCount = 10_000)
println(s"95% confident temperature is between $low and $high °C")
```

## Risk Assessment

Evaluate the risk of exceeding critical thresholds.

```scala mdoc
// Server load with variation
val serverLoad = Uncertain.normal(0.65, 0.15) // 65% ± 15%
val criticalThreshold = 0.9

val riskOfOverload = serverLoad > criticalThreshold
val riskProbability = riskOfOverload.probability(sampleCount = 10_000)

println(s"Risk of server overload: ${(riskProbability * 100).round}%")

if (riskProbability > 0.05) {  // More than 5% risk
  println("Warning: Consider adding capacity")
}
```

## Revenue Forecasting

Model business outcomes with realistic uncertainty.

```scala mdoc
// Daily customer arrivals (Poisson)
val customersPerDay = Uncertain.poisson(lambda = 50.0)

// Conversion rate (Beta distribution for probability)
val conversionRate = Uncertain.beta(alpha = 15, beta = 85) // ~15% rate

// Average purchase amount (triangular estimate)
val purchaseAmount = Uncertain.triangular(min = 20, peak = 45, max = 200)

// Daily revenue calculation
val dailyRevenue = for {
  customers <- customersPerDay
  rate <- conversionRate
  avgPurchase <- purchaseAmount
} yield (customers * rate * avgPurchase)

println(s"Expected daily revenue: $${dailyRevenue.mean(sampleCount = 10_000).round}")

val revPercentiles = dailyRevenue.percentiles(sampleCount = 100_000)
println(s"90% confident revenue will be at least: $${revPercentiles(10).round}")
println(s"90% confident revenue will be at most: $${revPercentiles(90).round}")
```

## Inventory Management

Decide how much stock to keep with uncertain demand.

```scala mdoc
// Weekly demand based on historical data
val historicalDemand = List(45, 52, 48, 51, 49, 53, 47, 50, 46, 54)
val weeklyDemand = Uncertain.empirical(historicalDemand)

// Holding cost per unit
val holdingCostPerUnit = 2.0
// Stockout cost per unit
val stockoutCostPerUnit = 15.0

def calculateCost(stockLevel: Int): Uncertain[Double] = 
  weeklyDemand.map { demand =>
    if (demand <= stockLevel) {
      // Holding cost for unsold units
      (stockLevel - demand) * holdingCostPerUnit
    } else {
      // Stockout cost for unmet demand
      (demand - stockLevel) * stockoutCostPerUnit
    }
  }

// Find optimal stock level (simplified example)
val stockLevels = 40 to 60
val costsAnalysis = stockLevels.map { level =>
  val cost = calculateCost(level).mean(sampleCount = 10_000)
  (level, cost)
}

val (optimalStock, minCost) = costsAnalysis.minBy(_._2)
println(s"Optimal stock level: $optimalStock units")
println(s"Expected weekly cost: $$$minCost")
```

## Quality Control

Monitor manufacturing process with statistical control.

```scala mdoc
// Part dimensions with manufacturing variation
val partDimension = Uncertain.normal(mean = 10.0, standardDeviation = 0.05) // mm

// Specification limits
val lowerLimit = 9.9
val upperLimit = 10.1

// Check if process is in control
val inSpec = (partDimension >= lowerLimit) && (partDimension <= upperLimit)
val yieldRate = inSpec.probability(sampleCount = 10_000)

println(s"Expected yield rate: ${(yieldRate * 100).round}%")

if (yieldRate < 0.997) {  // Six Sigma target
  println("Warning: Process capability below target")
}
```

## Portfolio Risk

Model investment returns with uncertainty.

```scala mdoc
// Expected returns with uncertainty
val stockReturn = Uncertain.normal(0.10, 0.15)   // 10% ± 15%
val bondReturn = Uncertain.normal(0.04, 0.05)    // 4% ± 5%

// Portfolio allocation
val stockAllocation = 0.6
val bondAllocation = 0.4

val portfolioReturn = for {
  stockRet <- stockReturn
  bondRet <- bondReturn
} yield (stockRet * stockAllocation + bondRet * bondAllocation)

println(s"Expected portfolio return: ${(portfolioReturn.mean(sampleCount = 10_000) * 100).round}%")

// Risk metrics
val (low, high) = portfolioReturn.confidenceInterval(0.95, sampleCount = 10_000)
println(s"95% confidence interval: ${(low * 100).round}% to ${(high * 100).round}%")

val downSideRisk = (portfolioReturn < 0).probability(sampleCount = 10_000)
println(s"Probability of loss: ${(downSideRisk * 100).round}%")
```

## Weather-Dependent Planning

Make decisions based on probabilistic weather forecasts.

```scala mdoc
// Weather forecast
val weatherIsGood = Uncertain.bernoulli(0.7) // 70% chance of good weather

// Event attendance depends on weather
val attendance = weatherIsGood.flatMap { isGood =>
  if (isGood) {
    Uncertain.normal(100, 10) // Good weather: 100 ± 10 people
  } else {
    Uncertain.normal(60, 15)  // Bad weather: 60 ± 15 people
  }
}

// Revenue per attendee
val revenuePerPerson = 25.0

val eventRevenue = attendance.map(_ * revenuePerPerson)

println(s"Expected revenue: $${eventRevenue.mean(sampleCount = 10_000).round}")

val percentiles = eventRevenue.percentiles(sampleCount = 50_000)
println(s"Pessimistic (10th percentile): $${percentiles(10).round}")
println(s"Optimistic (90th percentile): $${percentiles(90).round}")
```

## Machine Learning Predictions

Handle ML model uncertainty properly.

```scala mdoc
// Model predictions with confidence
val prediction1 = Uncertain.normal(mean = 0.75, standardDeviation = 0.05)
val prediction2 = Uncertain.normal(mean = 0.82, standardDeviation = 0.08)

// Is model 2 actually better?
val difference = prediction2 - prediction1
val isBetter = (difference > 0).probabilityExceeds(
  exceeds = 0.95,
  sampleCount = 10_000
)

if (isBetter) {
  println("Model 2 is significantly better")
} else {
  println("Difference is not statistically significant")
}
```

## Next Steps

- Learn about [Statistical Operations](statistics.md)
- Explore [Quantiles and Percentiles](quantiles.md)
- Check the [API Reference](api.md)
