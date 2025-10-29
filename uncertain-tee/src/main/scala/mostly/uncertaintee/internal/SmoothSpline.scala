package mostly.uncertaintee.internal

import scala.annotation.tailrec
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
  @tailrec
  def apply(t: Double): Double =
    if (t <= 0) {
      valueAtStart
    } else if (t <= 1.0) {
      valueAtStart + (t * (linearCoefficient + (t * (quadraticCoefficient + (t * cubicCoefficient)))))
    } else { // if t > 1
      this.apply(1.0)
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
  def apply(
    values: List[Double],
    slopeEffectivelyZeroWhenLessThan: Double = 1e-15
  ): SmoothSpline = {
    require(values.length >= 3, "Must have at least 3 values in order to calculate a smooth spline")
    // picture, if you will, an X/Y coordinate system with x from 0-1, evenly split into [values.length] segments.
    val x = values.indices.map(i => i.toDouble / (values.length - 1)).toVector
    val y = values.toVector
    build(x, y)(slopeEffectivelyZeroWhenLessThan)
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
  )(slopeEpsilon: Double): SmoothSpline = {
    require(x.length == y.length, s"x and y must be of equal length, was ${x.length} and ${y.length} respectively.")
    require(x.length >= 3, "Must have at least 3 values in order to calculate a smooth spline")
    val intervals: Vector[Double]           = x.sliding(2).map(previousAndNext => previousAndNext.last - previousAndNext.head).toVector
    val slopes: Vector[Double]              = y.zip(y.tail).zip(intervals).map { case ((yPrev, yNext), interval) => (yNext - yPrev) / interval }
    val rawTangents: Vector[Double]         = calculateRawTangents(x.length, slopes, intervals)
    val monotoneTangents: Vector[Double]    = calculateMonotoneTangents(x.length, slopeEpsilon, slopes, rawTangents)
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
    slopeEpsilon: Double,
    slopes: Vector[Double],
    rawTangents: Vector[Double]
  ): Vector[Double] = {
    val monotoneTangents: Seq[Double] = rawTangents.indices.map { pointIndex =>
      if (pointIndex == numPoints - 1) rawTangents(pointIndex)
      else {
        val segmentSlope = slopes(pointIndex)
        val tangentStart = rawTangents(pointIndex)
        val tangentEnd   = rawTangents(pointIndex + 1)

        if (math.abs(segmentSlope) <= slopeEpsilon) {
          0.0
        } else {
          val ratioStart        = tangentStart / segmentSlope
          val ratioEnd          = tangentEnd / segmentSlope
          val maxSlopeFactor    = 3.0
          val ratioStartClamped = math.max(0.0, ratioStart)
          val ratioEndClamped   = math.max(0.0, ratioEnd)
          val scale             =
            if (ratioStartClamped * ratioStartClamped + ratioEndClamped * ratioEndClamped > 9.0)
              maxSlopeFactor / math.sqrt(ratioStartClamped * ratioStartClamped + ratioEndClamped * ratioEndClamped)
            else 1.0
          scale * ratioStartClamped * segmentSlope
        }
      }
    }
    monotoneTangents.toVector :+ rawTangents.last
  }

  private def calculateRawTangents(
    numPoints: Int,
    slopes: Vector[Double],
    intervals: Vector[Double]
  ): Vector[Double] =
    (0 until numPoints).map { pointIndex =>
      if (pointIndex == 0) slopes.head
      else if (pointIndex == numPoints - 1) slopes.last
      else {
        val slopePrev = slopes(pointIndex - 1)
        val slopeNext = slopes(pointIndex)
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
