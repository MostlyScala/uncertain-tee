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
package mostly.uncertaintee.internal

import scala.collection.Searching.{Found, InsertionPoint}

/** Represents a single cubic polynomial segment (a + b*t + c*t² + d*t³)
  *
  * @param valueAtStart
  *   constant term (value at t=0)
  * @param linearCoefficient
  *   linear coefficient
  * @param quadraticCoefficient
  *   quadratic coefficient
  * @param cubicCoefficient
  *   cubic coefficient
  */
final private[uncertaintee] case class CubicSegment(
  valueAtStart: Double,
  linearCoefficient: Double,
  quadraticCoefficient: Double,
  cubicCoefficient: Double
) {

  /** Evaluates the cubic polynomial at position t.
    *
    * Uses Horner's method for numerical stability: a + t(b + t(c + td))
    *
    * @param t
    *   parameter value in [0,1] (input < 0 will be treated as 0, input > 1 will be treated as 1)
    * @return
    *   polynomial value at t
    */
  def apply(t: Double): Double = {
    val clampedT = if (t <= 0) {
      0.0
    } else if (t <= 1.0) {
      t
    } else {
      1.0
    }
    valueAtStart + (clampedT * (linearCoefficient + (clampedT * (quadraticCoefficient + (clampedT * cubicCoefficient)))))
  }
}

/** A complete monotonic cubic spline with segments and evaluation method.
  *
  * @param x
  *   the x-coordinates (independent variable) in strictly increasing order
  * @param segments
  *   array of cubic polynomial segments, one for each interval [x(i), x(i+1)]
  */
final private[uncertaintee] case class SmoothSpline(
  x: Vector[Double],
  segments: Vector[CubicSegment]
) {

  /** Evaluates the spline at a given x-coordinate.
    *
    * Finds the appropriate segment and evaluates its cubic polynomial. Handles boundary cases by returning the endpoint values.
    *
    * @param xCoordinate
    *   the x-coordinate to evaluate at
    * @return
    *   the interpolated y-value
    */
  def apply(xCoordinate: Double): Double =
    if (xCoordinate <= x.head) {
      segments.head.valueAtStart
    } else if (xCoordinate >= x.last) {
      segments.last(1.0)
    } else {
      // we binary search for the xCoordinate in the x values, which returns
      x.search(xCoordinate) match {
        case Found(exactSegmentIndex)      =>
          // exact match at an *interior* knot point x(idx), as in, the value is simply y(idx), which is the starting value of the segment that begins at that point.
          segments(exactSegmentIndex).valueAtStart
        case InsertionPoint(upperBoundary) =>
          // we're between two boundaries:
          // x(upperBoundary-1) < xCoordinate < x(upperBoundary),
          // meaning that (upperBoundary-1): [x(upperBoundary-1), x(upperBoundary)]
          val i       = upperBoundary - 1
          val segment = segments(i)
          val t       = (xCoordinate - x(i)) / (x(i + 1) - x(i))
          segment(t)
      }
    }
}

/** Monotonic cubic Hermite spline interpolation (specifically using the Fritsch-Carlson method)
  *
  * The spline:
  *   - Interpolates all given points exactly
  *   - Has continuous first derivatives (C1 continuity)
  *   - Preserves monotonicity
  *
  * @see
  *   <a href="https://en.wikipedia.org/wiki/Monotone_cubic_interpolation">Monotone cubic interpolation</a>
  */
private[uncertaintee] object SmoothSpline {

  /** Builds a monotonic cubic spline from a list of Y-values.
    *
    * This method simplifies spline creation by assuming evenly spaced X-coordinates spanning the interval `[0.0, 1.0]`. For example, a list of 4 values will be mapped to
    * x-coordinates `[0.0, 0.33..., 0.66..., 1.0]`.
    *
    * @param values
    *   The list of `y`-coordinates (dependent variable) to interpolate. Must have at least 3 values.
    */
  def apply(values: List[Double]): SmoothSpline = {
    require(values.length >= 3, "Must have at least 3 values in order to calculate a smooth spline")
    // picture, if you will, an X/Y coordinate system with x from 0-1, evenly split into [values.length] segments.
    val x = values.indices.map(i => i.toDouble / (values.length - 1)).toVector
    val y = values.toVector
    build(x, y)
  }

  /** Builds a monotonic cubic spline through the given points.
    *
    * Constructs a piecewise cubic Hermite spline that preserves monotonicity using the Fritsch-Carlson algorithm. The resulting spline interpolates all points exactly and has
    * continuous first derivatives.
    *
    * @param x
    *   x-coordinates **in strictly increasing order** and within 0 to 1
    * @param y
    *   y-coordinates (must have same length as x)
    * @return
    *   a [[SmoothSpline]] object that can be evaluated at any x-coordinate
    */
  private def build(
    x: Vector[Double],
    y: Vector[Double]
  ): SmoothSpline = {
    require(x.length == y.length, s"x and y must be of equal length, was ${x.length} and ${y.length} respectively.")
    require(x.length >= 3, "Must have at least 3 values in order to calculate a smooth spline")

    val intervals: Vector[Double]           = x.sliding(2).map(previousAndNext => previousAndNext.last - previousAndNext.head).toVector
    val slopes: Vector[Double]              = y.zip(y.tail).zip(intervals).map { case ((yPrev, yNext), interval) => (yNext - yPrev) / interval }
    val rawTangents: Vector[Double]         = calculateRawTangents(x.length, slopes, intervals)
    val monotoneTangents: Vector[Double]    = calculateMonotoneTangents(x.length, slopes, rawTangents)
    val cubicSegments: Vector[CubicSegment] = calculateCubicSegments(x.length, y, intervals, monotoneTangents)

    SmoothSpline(
      x = x,
      segments = cubicSegments
    )
  }

  private def calculateCubicSegments(
    numPoints: Int,
    y: Vector[Double],
    intervals: Vector[Double],
    monotoneTangents: Vector[Double]
  ): Vector[CubicSegment] =
    (0 until numPoints - 1).map { segmentIndex =>
      val segmentWidth_dx  = intervals(segmentIndex)
      val segmentHeight_dy = y(segmentIndex + 1) - y(segmentIndex)
      CubicSegment(
        valueAtStart = y(segmentIndex),
        linearCoefficient = monotoneTangents(segmentIndex) * segmentWidth_dx,
        quadraticCoefficient = 3 * segmentHeight_dy - (2 * monotoneTangents(segmentIndex) + monotoneTangents(segmentIndex + 1)) * segmentWidth_dx,
        cubicCoefficient = -2 * segmentHeight_dy + (monotoneTangents(segmentIndex) + monotoneTangents(segmentIndex + 1)) * segmentWidth_dx
      )
    }.toVector

  private def calculateMonotoneTangents(
    numPoints: Int,
    segmentSlopes: Vector[Double],
    rawTangents: Vector[Double],
    epsilon: Double = 1e-12
  ): Vector[Double] = {

    val numSegments = numPoints - 1

    // ---- Pass 1: compute immutable scaling factors for each segment ----
    // Each segment produces a tuple: (scaleForStartTangent, scaleForEndTangent)
    val segmentScales: Vector[(Double, Double)] = (0 until numSegments).map { i =>
      val segmentSlope = segmentSlopes(i)
      val tangentStart = rawTangents(i)
      val tangentEnd   = rawTangents(i + 1)
      if (math.abs(segmentSlope) <= epsilon || tangentStart / segmentSlope < 0.0 || tangentEnd / segmentSlope < 0.0) {
        // Flat segment or tangent points away from slope → zero tangents
        (0.0, 0.0)
      } else {
        val startRatio = tangentStart / segmentSlope
        val endRatio   = tangentEnd / segmentSlope
        if (startRatio * startRatio + endRatio * endRatio > 9.0) {
          // Scale both tangents to fit inside the monotonicity circle
          val scale = 3.0 / math.sqrt(startRatio * startRatio + endRatio * endRatio)
          (scale, scale)
        } else {
          // No scaling needed
          (1.0, 1.0)
        }
      }
    }.toVector

    // ---- Pass 2: compute final tangents for each point ----
    (0 until numPoints).map { i =>
      val scaleFromLeftSegment  = if (i == 0) segmentScales.head._1 else segmentScales(i - 1)._2
      val scaleFromRightSegment = if (i == numPoints - 1) segmentScales.last._2 else segmentScales(i)._1
      val finalScale            = math.min(scaleFromLeftSegment, scaleFromRightSegment)
      rawTangents(i) * finalScale
    }.toVector
  }

  /** Calculates initial tangent estimates using weighted harmonic mean.
    *
    * For interior points, uses the Fritsch-Carlson weighted harmonic mean formula which accounts for non-uniform spacing between points.
    */
  private def calculateRawTangents(
    numPoints: Int,
    slopes: Vector[Double],
    intervals: Vector[Double]
  ): Vector[Double] =
    (0 until numPoints).map { pointIndex =>
      if (pointIndex == 0) {
        // First point: use first segment slope
        slopes.head
      } else if (pointIndex == numPoints - 1) {
        // Last point: use last segment slope
        slopes.last
      } else {
        // Interior point: weighted harmonic mean of adjacent slopes
        val slopePrev = slopes(pointIndex - 1)
        val slopeNext = slopes(pointIndex)

        // If slopes have opposite signs or either is zero, we're at a local extremum
        if (slopePrev * slopeNext <= 0) {
          0.0
        } else {
          val intervalPrev = intervals(pointIndex - 1)
          val intervalNext = intervals(pointIndex)
          val weightPrev   = 2 * intervalNext + intervalPrev
          val weightNext   = intervalNext + 2 * intervalPrev
          (weightPrev + weightNext) / (weightPrev / slopePrev + weightNext / slopeNext)
        }
      }
    }.toVector
}
