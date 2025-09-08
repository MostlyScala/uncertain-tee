package mostly.uncertaintee

import munit.FunSuite

import scala.math.{abs, pow, sqrt}

class HistogramSpec extends RngSuite {

  private val sampleCount = 10_000

  // --- Foundational and Mathematical Tests ---

  rngTest("Histogram's total count must equal the sample count") {
    val coinFlip   = Uncertain.bernoulli(0.5)
    val histogram  = coinFlip.histogram(sampleCount)
    val totalCount = histogram.values.sum
    assertEquals(
      obtained = totalCount,
      expected = sampleCount,
      clue = s"The sum of histogram counts ($totalCount) must match the sample count ($sampleCount)."
    )
  }

  rngTest("Histogram of a deterministic value should have only one entry") {
    val deterministicValue = Uncertain.point("Fixed Outcome")
    val histogram          = deterministicValue.histogram(sampleCount)
    assert(histogram.size == 1, s"Histogram should have exactly one entry, but found ${histogram.size}.")
    assertEquals(
      obtained = histogram.get("Fixed Outcome"),
      expected = Some(sampleCount),
      clue = "The single entry's count must be the sample count."
    )
  }

  rngTest("Histogram of a uniform discrete distribution should have roughly equal counts") {
    val uniform       = Uncertain.empirical(List(1, 2, 3, 4))
    val histogram     = uniform.histogram(sampleCount)
    val expectedCount = sampleCount / 4.0
    val tolerance     = expectedCount * 0.15
    histogram.foreach { (outcome, count) =>
      val hint = s"Count for outcome $outcome ($count) should be close to the expected uniform count ($expectedCount)."
      assert(
        cond = abs(count - expectedCount) < tolerance,
        clue = hint
      )
    }
  }

  rngTest("Histogram of a categorical distribution should reflect its probabilities") {
    val biased    = Uncertain.categorical(Map("A" -> 0.7, "B" -> 0.2, "C" -> 0.1))
    val histogram = biased.histogram(sampleCount)
    val expectedA = sampleCount * 0.7
    val expectedB = sampleCount * 0.2
    val tolerance = sampleCount * 0.05
    val countA    = histogram.getOrElse("A", 0)
    val countB    = histogram.getOrElse("B", 0)
    assert(
      cond = abs(countA - expectedA) < tolerance,
      clue = s"Count for 'A' ($countA) should be close to the expected value ($expectedA)."
    )
    assert(
      cond = abs(countB - expectedB) < tolerance,
      clue = s"Count for 'B' ($countB) should be close to the expected value ($expectedB)."
    )
  }

  rngTest("Real World: Simulating a fair six-sided die roll") {
    val dieRoll              = Uncertain.empirical(List(1, 2, 3, 4, 5, 6))
    val totalRolls           = 60_000
    val rollHistogram        = dieRoll.histogram(totalRolls)
    val expectedCountPerFace = totalRolls / 6.0
    val tolerance            = expectedCountPerFace * 0.1
    assertEquals(rollHistogram.size, 6, "A standard die should have 6 possible outcomes.")
    rollHistogram.foreach { (face, count) =>
      val hint =
        s"For a fair die, the count for face $face ($count) should be near the expected value ($expectedCountPerFace)."
      assert(
        cond = abs(count - expectedCountPerFace) < tolerance,
        clue = hint
      )
    }
  }

  rngTest("Real World: Modeling website user actions") {
    enum UserAction { case ClickedAd, ScrolledPage, BouncedImmediately }
    val userModel       = Uncertain.categorical(
      Map(
        UserAction.ClickedAd          -> 0.05,
        UserAction.ScrolledPage       -> 0.65,
        UserAction.BouncedImmediately -> 0.30
      )
    )
    val simulatedUsers  = 5_000
    val userHistogram   = userModel.histogram(simulatedUsers)
    val expectedClicks  = simulatedUsers * 0.05
    val expectedScrolls = simulatedUsers * 0.65
    val tolerance       = simulatedUsers * 0.05
    val clickCount      = userHistogram.getOrElse(UserAction.ClickedAd, 0)
    val scrollCount     = userHistogram.getOrElse(UserAction.ScrolledPage, 0)
    assertEquals(
      obtained = userHistogram.values.sum,
      expected = simulatedUsers,
      clue = "The total actions must equal the number of simulated users."
    )
    val clickHint       = s"Click count ($clickCount) should approximate the expected ${expectedClicks} clicks."
    val scrollHint      = s"Scroll count ($scrollCount) should approximate the expected ${expectedScrolls} scrolls."
    assert(
      cond = abs(clickCount - expectedClicks) < tolerance,
      clue = clickHint
    )
    assert(
      cond = abs(scrollCount - expectedScrolls) < tolerance,
      clue = scrollHint
    )
  }

  test("Precondition: Histogram requires a positive sample count") {
    val dist = Uncertain.point(1)
    intercept[IllegalArgumentException](dist.histogram(0))
    intercept[IllegalArgumentException](dist.histogram(-100))
  }

  test("Precondition: Categorical distribution requires non-empty, positive-weight outcomes") {
    intercept[IllegalArgumentException](Uncertain.categorical(Map.empty[String, Double]))
    intercept[IllegalArgumentException](Uncertain.categorical(Map("A" -> 0.0, "B" -> 0.0)))
  }

  rngTest("Edge Case: Histogram with a single sample should contain one element with a count of 1") {
    val dist = Uncertain.empirical(List("a", "b", "c"))
    val hist = dist.histogram(1)
    assertEquals(hist.size, 1, "Histogram of a single sample must have exactly one entry.")
    assertEquals(hist.values.sum, 1, "The total count of a single-sample histogram must be 1.")
    assert(hist.values.head == 1, "The single entry's count must be 1.")
  }

  rngTest("Complex Key: Histogram should work correctly with case classes") {
    case class Product(id: Int, category: String)
    val p1             = Product(101, "Electronics")
    val p2             = Product(205, "Books")
    val inventoryPicks = Uncertain.empirical(List(p1, p1, p1, p2))
    val hist           = inventoryPicks.histogram(2000)
    val expectedP1     = 2000 * 0.75
    val tolerance      = 2000 * 0.1
    val countP1        = hist.getOrElse(p1, 0)
    assertEquals(hist.size, 2, "Histogram should contain two distinct products.")
    assert(
      cond = abs(countP1 - expectedP1) < tolerance,
      clue = s"Count for Product 1 ($countP1) should be close to expected ($expectedP1)."
    )
  }

  rngTest("Zero-Probability Outcome: Histogram should not contain keys for unsampled outcomes") {
    val dist = Uncertain.categorical(Map("Guaranteed" -> 1.0, "Impossible" -> 0.0))
    val hist = dist.histogram(1000)
    assertEquals(hist.size, 1, "The histogram should only contain the single, guaranteed outcome.")
    assertEquals(
      obtained = hist.get("Guaranteed"),
      expected = Some(1000),
      clue = "The guaranteed outcome must have a count equal to the sample size."
    )
    assertEquals(hist.get("Impossible"), None, "The impossible outcome must not be a key in the histogram map.")
  }

  rngTest("Statistical Property: Aggregated histograms should approximate hit rate within a principled tolerance") {
    val numTrials      = 10_000
    val histogramSize  = 100
    val winProbability = 0.01
    val lottery        = Uncertain.categorical(Map("Win" -> winProbability, "Lose" -> (1.0 - winProbability)))

    val hitCount = (0 until numTrials).count { _ =>
      val hist = lottery.histogram(histogramSize)
      hist.contains("Win")
    }

    // Use a principled tolerance based on the binomial distribution's standard deviation.
    // The number of "hits" follows a binomial distribution. We can use the normal
    // approximation to create a robust confidence interval.
    val probabilityOfNoWins = pow(1.0 - winProbability, histogramSize)
    val theoreticalHitRate  = 1.0 - probabilityOfNoWins
    val expectedHits        = numTrials * theoreticalHitRate

    // Calculate tolerance for a >99.9999% confidence interval (5 standard deviations)
    val stdDev    = sqrt(numTrials * theoreticalHitRate * (1.0 - theoreticalHitRate))
    val tolerance = 5 * stdDev

    val hint =
      s"The observed hit count ($hitCount) should be within 5 standard deviations of the theoretical expected count (${expectedHits.round})."
    assert(
      cond = abs(hitCount - expectedHits) < tolerance,
      clue = hint
    )
  }
}
