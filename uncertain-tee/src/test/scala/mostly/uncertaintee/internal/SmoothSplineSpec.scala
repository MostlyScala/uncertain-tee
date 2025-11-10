package mostly.uncertaintee.internal

import munit.FunSuite

class SmoothSplineSpec extends FunSuite {

  // pretty tight tolerance here as we're doing deterministic math; the tolerance (unlike in the `rng`-based tests for the rest of the library)
  // is to deal with floating point precision loss during arithmetic
  private val tolerance = 1e-9

  // --- Basic buildSpline Tests ---

  test("buildSpline should require at least 3 values") {
    intercept[IllegalArgumentException] {
      SmoothSpline(List())
    }

    intercept[IllegalArgumentException] {
      SmoothSpline(List(1.0))
    }

    intercept[IllegalArgumentException] {
      SmoothSpline(List(1.0, 2.0))
    }
  }

  test("buildSpline should work with minimum 3 values (linear increasing data)") {
    val spline = SmoothSpline(List(0.0, 1.0, 2.0))

    // Should have correct x-coordinates (evenly spaced from 0 to 1)
    assert(
      cond = spline.x.length == 3,
      clue = "Spline should have 3 x-coordinates"
    )
    assert(
      cond = math.abs(spline.x(0) - 0.0) < tolerance,
      clue = "First x-coordinate should be 0.0"
    )
    assert(
      cond = math.abs(spline.x(1) - 0.5) < tolerance,
      clue = "Second x-coordinate should be 0.5"
    )
    assert(
      cond = math.abs(spline.x(2) - 1.0) < tolerance,
      clue = "Third x-coordinate should be 1.0"
    )

    // Should have 2 segments for 3 points
    assert(
      cond = spline.segments.length == 2,
      clue = "Spline should have 2 segments for 3 points"
    )
  }

  test("buildSpline should interpolate given points exactly (non-monotonic data)") {
    val values = List(1.0, 3.0, 2.0, 4.0)
    val spline = SmoothSpline(values)

    // Test that spline passes through all original points
    for (i <- values.indices) {
      val x         = i.toDouble / (values.length - 1)
      val actualY   = spline(x)
      val expectedY = values(i)
      assert(
        math.abs(actualY - expectedY) < tolerance,
        s"Spline should pass through point ($x, $expectedY) but got $actualY"
      )
    }
  }

  test("evaluate should handle boundary cases correctly (linear increasing data)") {
    val values = List(1.0, 2.0, 3.0)
    val spline = SmoothSpline(values)

    // Test before first point
    assert(
      cond = math.abs(spline(-0.5) - 1.0) < tolerance,
      clue = "Evaluation before first point should return first value"
    )

    // Test after last point
    assert(
      cond = math.abs(spline(1.5) - 3.0) < tolerance,
      clue = "Evaluation after last point should return last value"
    )
  }

  test("evaluate should work for linear increasing data") {
    val values = List(0.0, 1.0, 2.0, 3.0)
    val spline = SmoothSpline(values)

    // Test evaluation at midpoint between first two points
    val midX = 1.0 / 6.0 // halfway between x=0 and x=1/3
    val midY = spline(midX)

    // Should be between 0.0 and 1.0
    assert(
      cond = midY > 0.0 && midY < 1.0,
      clue = s"Interpolated value $midY should be between 0.0 and 1.0"
    )

    // Test monotonicity - values should increase
    val x1 = 0.1
    val x2 = 0.2
    val x3 = 0.3
    assert(
      cond = spline(x1) <= spline(x2),
      clue = "Values should increase monotonically"
    )
    assert(
      cond = spline(x2) <= spline(x3),
      clue = "Values should increase monotonically"
    )
  }

  test("evaluate should work for linear decreasing data") {
    val values = List(4.0, 3.0, 2.0, 1.0)
    val spline = SmoothSpline(values)

    // Test monotonicity - values should decrease
    val x1 = 0.1
    val x2 = 0.5
    val x3 = 0.9
    assert(
      cond = spline(x1) >= spline(x2),
      clue = "Values should decrease monotonically"
    )
    assert(
      cond = spline(x2) >= spline(x3),
      clue = "Values should decrease monotonically"
    )
  }

  test("evaluate should handle non-monotonic data") {
    val values = List(1.0, 3.0, 2.0, 4.0, 1.5)
    val spline = SmoothSpline(values)

    // Should still interpolate exact points
    for (i <- values.indices) {
      val x         = i.toDouble / (values.length - 1)
      val actualY   = spline(x)
      val expectedY = values(i)
      assert(
        cond = math.abs(actualY - expectedY) < tolerance,
        clue = s"Spline should pass through point ($x, $expectedY) but got $actualY"
      )
    }
  }

  test("evaluate should handle flat/constant segments") {
    val values = List(1.0, 2.0, 2.0, 2.0, 3.0)
    val spline = SmoothSpline(values)

    // Should interpolate exact points
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Should interpolate point $i exactly: expected ${values(i)}, got $actualY"
      )
    }

    // Values in the flat region should be close to 2.0
    val midFlat = spline(0.5) // middle of flat region
    assert(
      cond = math.abs(midFlat - 2.0) < 0.1,
      clue = s"Value in flat region should be close to 2.0, got $midFlat"
    )
  }

  test("evaluate should preserve monotonicity in strictly linear increasing data") {
    val values = List(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
    val spline = SmoothSpline(values)

    // Sample many points and verify monotonicity
    val samplePoints  = (0 to 100).map(_ * 0.01) // x from 0.0 to 1.0 in steps of 0.01
    val sampledValues = samplePoints.map(spline.apply)

    // Check that each value is >= the previous (allowing for small numerical errors)
    for (i <- 1 until sampledValues.length)
      assert(
        cond = sampledValues(i) >= sampledValues(i - 1) - tolerance,
        clue = s"Monotonicity violated at index $i: ${sampledValues(i - 1)} -> ${sampledValues(i)}"
      )
  }

  test("evaluate should handle steep changes (step function data)") {
    val values = List(0.0, 0.1, 10.0, 10.1)
    val spline = SmoothSpline(values)

    // Should interpolate exact points even with steep changes
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Should interpolate point $i exactly with steep changes: expected ${values(i)}, got $actualY"
      )
    }

    // Check that interpolation is reasonable - no wild oscillations
    val midpoint = spline(0.5)
    assert(
      cond = midpoint >= 0.0 && midpoint <= 11.0,
      clue = s"Interpolation should be reasonable, got $midpoint"
    )
  }

  test("CubicSegment.evaluateSegment should work correctly") {
    // Test the cubic segment evaluation directly
    val segment = CubicSegment(
      valueAtStart = 1.0,
      linearCoefficient = 2.0,
      quadraticCoefficient = -1.0,
      cubicCoefficient = 0.5
    )

    // Test at t=0 (should be valueAtStart)
    assert(
      cond = math.abs(segment(0.0) - 1.0) < tolerance,
      clue = "Cubic segment at t=0 should equal valueAtStart"
    )

    // Test at t=1: 1.0 + 2.0*1 + (-1.0)*1^2 + 0.5*1^3 = 1 + 2 - 1 + 0.5 = 2.5
    assert(
      cond = math.abs(segment(1.0) - 2.5) < tolerance,
      clue = "Cubic segment at t=1 should equal calculated value 2.5"
    )

    // Test at t=0.5: 1.0 + 2.0*0.5 + (-1.0)*0.25 + 0.5*0.125 = 1 + 1 - 0.25 + 0.0625 = 1.8125
    assert(
      cond = math.abs(segment(0.5) - 1.8125) < tolerance,
      clue = "Cubic segment at t=0.5 should equal calculated value 1.8125"
    )
  }

  test("buildSpline should work with larger datasets (sinusoidal data)") {
    val values = (0 to 10).map(i => math.sin(i * 0.5)).toList
    val spline = SmoothSpline(values)

    // Should have correct number of segments
    assert(
      cond = spline.segments.length == values.length - 1,
      clue = "Spline should have correct number of segments"
    )

    // Should interpolate all points exactly
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Point $i: expected ${values(i)}, got $actualY"
      )
    }
  }

  test("should apply Fritsch-Carlson constraints to prevent overshooting") {
    // This data pattern forces the algorithm to apply monotonicity constraints
    // The sharp reversal between points 1-2 would cause overshooting without constraints
    val values = List(0.0, 1.0, 0.5, 2.0)
    val spline = SmoothSpline(values)

    // Should still interpolate all points exactly
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Should interpolate point $i exactly despite constraints: expected ${values(i)}, got $actualY"
      )
    }

    // Verify no wild overshooting occurs between points 1 and 2
    val midX = 1.5 / 3.0 // halfway between point 1 (x=1/3) and point 2 (x=2/3)
    val midY = spline(midX)
    // Should be between the two y-values without excessive overshoot
    assert(
      cond = midY >= 0.4 && midY <= 1.1,
      clue = s"Value between reversal points should be reasonable, got $midY"
    )
  }

  test("should handle very small x-intervals without numerical instability") {
    // Test with very close x-coordinates that might cause numerical issues
    val values = List(1.0, 1.0000001, 2.0)
    val spline = SmoothSpline(values)

    // Should not throw exceptions or produce NaN/Inf values
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        cond = !actualY.isNaN && !actualY.isInfinite,
        clue = s"Should produce finite values, got $actualY"
      )
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Should interpolate point $i exactly with small intervals: expected ${values(i)}, got $actualY"
      )
    }

    // Should produce reasonable intermediate values
    val midY = spline(0.5)
    assert(
      cond = midY >= 1.0 && midY <= 2.0,
      clue = s"Interpolated value should be reasonable, got $midY"
    )
  }

  test("should work correctly with negative y-values") {
    val values = List(-5.0, -2.0, -1.0, 3.0)
    val spline = SmoothSpline(values)

    // Should interpolate all points exactly
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        math.abs(actualY - values(i)) < tolerance,
        s"Should interpolate negative values correctly at point $i: expected ${values(i)}, got $actualY"
      )
    }

    // Should preserve monotonicity in the increasing part
    val x1 = 0.6 // between points 1 and 2
    val x2 = 0.8 // between points 2 and 3
    assert(
      cond = spline(x1) <= spline(x2),
      clue = "Should maintain monotonicity with negative values"
    )
  }

  test("should handle large dynamic ranges without precision loss") {
    val values = List(1e-6, 1e3, 1e-9, 1e6)
    val spline = SmoothSpline(values)

    // Should interpolate all points exactly despite large dynamic range
    for (i <- values.indices) {
      val x             = i.toDouble / (values.length - 1)
      val actualY       = spline(x)
      val relativeError = math.abs(actualY - values(i)) / math.max(math.abs(values(i)), 1e-12)
      assert(
        relativeError < 1e-4, // More realistic tolerance for extreme dynamic ranges
        s"Relative error too large at point $i: expected ${values(i)}, got $actualY, relative error $relativeError"
      )
    }

    // Should not produce NaN or infinite values
    val testPoints = List(0.1, 0.3, 0.7, 0.9)
    testPoints.foreach { x =>
      val y = spline(x)
      assert(
        cond = !y.isNaN && !y.isInfinite,
        clue = s"Should produce finite values at x=$x, got $y"
      )
    }
  }

  test("should handle zero slopes and flat transitions correctly") {
    // Data that creates zero slopes in some segments
    val values = List(1.0, 1.0, 2.0, 2.0, 3.0)
    val spline = SmoothSpline(values)

    // Should interpolate all points exactly
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline(x)
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Should interpolate point $i exactly with zero slopes: expected ${values(i)}, got $actualY"
      )
    }

    // Should handle the flat-to-increasing transition smoothly
    val transition1 = spline(0.125) // between first flat segment and ramp
    val transition2 = spline(0.625) // between ramp and second flat segment
    assert(
      cond = transition1 >= 1.0 && transition1 <= 2.0,
      clue = s"First transition should be smooth, got $transition1"
    )
    assert(
      cond = transition2 >= 2.0 && transition2 <= 3.0,
      clue = s"Second transition should be smooth, got $transition2"
    )
  }

  test("should produce reasonable tangent estimates with weighted harmonic mean") {
    // Test data that exercises the tangent calculation logic
    val values = List(0.0, 1.0, 4.0, 5.0) // varying slopes: 1, 3, 1
    val spline = SmoothSpline(values)

    // Should interpolate exact points
    for (i <- values.indices) {
      val x       = i.toDouble / (values.length - 1)
      val actualY = spline.apply(x)
      assert(
        cond = math.abs(actualY - values(i)) < tolerance,
        clue = s"Should interpolate point $i exactly with tangent estimation: expected ${values(i)}, got $actualY"
      )
    }

    // The middle segment should show reasonable curvature
    // (testing indirectly that tangent calculations are working)
    val samples           = (0 to 10).map(i => 1.0 / 3.0 + i * (1.0 / 3.0) / 10.0) // sample middle segment
    val values_in_segment = samples.map(spline.apply)

    // Should show smooth transition (no wild oscillations)
    for (i <- 1 until values_in_segment.length) {
      val slope = values_in_segment(i) - values_in_segment(i - 1)
      assert(
        cond = slope >= -0.5 && slope <= 5.0,
        clue = s"Local slope should be reasonable, got $slope"
      )
    }
  }

}
