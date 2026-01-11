package mostly.uncertaintee

import io.circe.*
import io.circe.syntax.*
import mostly.uncertaintee.quantiles.Quantiles

import scala.language.implicitConversions
import scala.util.Random

/** Provides circe encoders and decoders for [[Uncertain]] values via [[Quantiles]] */
package object circe {

  private object keys {
    val boundaryValues = "boundaryValues"
    val quantiles      = "quantiles"
  }

  /** {{{
    *   { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] }
    * }}}
    */
  implicit def encoderQuantiles[T](using Encoder[T]): Encoder[Quantiles[T]] =
    x => Json.obj(keys.boundaryValues := x.boundaryValues)

  /** @see
    *   [[Quantiles.fromBoundaryValues]]
    *
    * {{{
    *   { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] }
    * }}}
    */
  implicit def decoderQuantiles[T](using Decoder[T]): Decoder[Quantiles[T]] =
    _.downField(keys.boundaryValues).as[List[T]].map(Quantiles.fromBoundaryValues)

  /** {{{
    *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
    * }}}
    *
    * @see
    *   [[Quantiles.reconstructDiscrete]]
    */
  def decoderUncertainDiscrete[T](using dec: Decoder[T], random: Random = new Random()): Decoder[Uncertain[T]] =
    _.downField(keys.quantiles).as[Quantiles[T]].map(_.reconstructDiscrete)

  /** {{{
    *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
    * }}}
    *
    * @see
    *   [[Quantiles.reconstructStep]]
    */
  def decoderUncertainStep(using random: Random = new Random()): Decoder[Uncertain[Double]] =
    _.downField(keys.quantiles).as[Quantiles[Double]].map(_.reconstructStep)

  /** {{{
    *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
    * }}}
    *
    * @see
    *   [[Quantiles.reconstructLinear]]
    */
  def decoderUncertainLinear(using random: Random = new Random()): Decoder[Uncertain[Double]] =
    _.downField(keys.quantiles).as[Quantiles[Double]].map(_.reconstructLinear)

  /** {{{
    *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
    * }}}
    *
    * @see
    *   [[Quantiles.reconstructSmooth]]
    */
  def decoderUncertainSmooth(using random: Random = new Random()): Decoder[Uncertain[Double]] =
    _.downField(keys.quantiles).as[Quantiles[Double]].map(_.reconstructSmooth)

  /** Serializes an [[Uncertain]] to JSON via quantiles with configurable precision. As always with this library, you trade compute time for precision - higher sample counts yields
    * more accurate serialisations. It's strongly recommended to test your use case to determine the optimal sample count; this is generally where you will want the largest sample
    * count when using the uncertain-tee library, since you actively loose precision when serializing with too small a sample count.
    *
    * ==Example JSON Output==
    *
    * {{{
    *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
    * }}}
    *
    * ==Why Quantiles for Serialization?==
    *
    * Quantiles provide a compact, distribution-agnostic way to serialize uncertain values:
    *   - Works for any [[Uncertain]][T] regardless of how it was constructed (normal, mixture, flatMap chains, etc.)
    *   - Compact representation: only stores boundary values (e.g., 101 values for percentiles vs thousands of samples)
    *   - Can be reconstructed into an [[Uncertain]] using [[Quantiles.reconstructLinear]], [[Quantiles.reconstructStep]], [[Quantiles.reconstructSmooth]], or
    *     [[Quantiles.reconstructDiscrete]]
    *
    * ==Why T Needs Ordering==
    *
    * Computing quantiles requires sorting samples to find boundary values at specific percentiles. Without [[Ordering]][T], we cannot determine which values lie at the 25th, 50th,
    * 75th percentile positions, etc.
    *
    * ==Sample Count Recommendations==
    *
    * Use large sample counts (100,000+) for accurate quantile estimation:
    *   - Small sample counts (< 1,000) produce noisy quantile boundaries
    *   - Medium sample counts (1,000-10,000) are acceptable for coarse precision
    *   - Large sample counts (100,000+) provide stable, reproducible quantile values
    *   - The reconstruction quality depends entirely on quantile accuracy
    *
    * ==Precision vs Size Trade-off==
    *
    * Choose `quantilesToSerialize` based on your needs:
    *   - 4 intervals (quartiles, 5 boundary values will be serialized): Very compact but loses shape information
    *   - 10 intervals (deciles, 11 boundary values will be serialized): Balanced for most uses
    *   - 100 intervals (percentiles, 101 boundary values will be serialized): High fidelity, recommended **default**
    *   - 100+ intervals: Diminishing returns, usually unnecessary - but, as always, test.
    *
    * @param quantileIntervals
    *   number of quantile intervals to compute (e.g., 100 for percentiles)
    * @param sampleCount
    *   number of samples to draw for computing quantiles (recommend 100,000+)
    * @return
    *   JSON representation of the quantiles
    * @see
    *   [[toJsonViaPercentiles]] for a convenience method using 100 intervals
    * @see
    *   [[Quantiles.reconstructLinear]], [[Quantiles.reconstructStep]], [[Quantiles.reconstructLinear]], [[Quantiles.reconstructSmooth]] for reconstructing the uncertain value from
    *   quantiles
    */
  def encoderUncertain[T](sampleCount: Int, quantileIntervals: Int = 100)(using Encoder[T], Ordering[T]): Encoder[Uncertain[T]] = u =>
    Json.obj(
      keys.quantiles := Quantiles.ofSize(
        quantileIntervals = quantileIntervals,
        uncertain = u,
        sampleCount = sampleCount
      )
    )

  extension [T](q: Quantiles[T]) {
    def toJson(using Encoder[T]): Json = q.asJson
  }

  extension [T](u: Uncertain[T]) {

    /** Serializes an [[Uncertain]] to JSON via quantiles with configurable precision. As always with this library, you trade compute time for precision - higher sample counts
      * yields more accurate serialisations. It's strongly recommended to test your use case to determine the optimal sample count; this is generally where you will want the
      * largest sample count when using the uncertain-tee library, since you actively loose precision when serializing with too small a sample count.
      *
      * ==Example JSON Output==
      *
      * {{{
      *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
      * }}}
      * ==Why Quantiles for Serialization?==
      *
      * Quantiles provide a compact, distribution-agnostic way to serialize uncertain values:
      *   - Works for any [[Uncertain]][T] regardless of how it was constructed (normal, mixture, flatMap chains, etc.)
      *   - Compact representation: only stores boundary values (e.g., 101 values for percentiles vs thousands of samples)
      *   - Can be reconstructed into an [[Uncertain]] using [[Quantiles.reconstructLinear]], [[Quantiles.reconstructSmooth]], or [[Quantiles.reconstructDiscrete]]
      *
      * ==Why T Needs Ordering==
      *
      * Computing quantiles requires sorting samples to find boundary values at specific percentiles. Without [[Ordering]][T], we cannot determine which values lie at the 25th,
      * 50th, 75th percentile positions, etc.
      *
      * ==Sample Count Recommendations==
      *
      * Use large sample counts (100,000+) for accurate quantile estimation:
      *   - Small sample counts (< 1,000) produce noisy quantile boundaries
      *   - Medium sample counts (1,000-10,000) are acceptable for coarse precision
      *   - Large sample counts (100,000+) provide stable, reproducible quantile values
      *   - The reconstruction quality depends entirely on quantile accuracy
      *
      * ==Precision vs Size Trade-off==
      *
      * Choose `quantilesToSerialize` based on your needs:
      *   - 4 intervals (quartiles, 5 boundary values will be serialized): Very compact but loses shape information
      *   - 10 intervals (deciles, 11 boundary values will be serialized): Balanced for most uses
      *   - 100 intervals (percentiles, 101 boundary values will be serialized): High fidelity, recommended default
      *   - 100+ intervals: Diminishing returns, usually unnecessary - but, as always, test.
      *
      * @param quantilesToSerialize
      *   number of quantile intervals to compute (e.g., 100 for percentiles)
      * @param sampleCount
      *   number of samples to draw for computing quantiles (recommend 100,000+)
      * @return
      *   JSON representation of the quantiles
      * @see
      *   [[toJsonViaPercentiles]] for a convenience method using 100 intervals
      */
    def toJson(sampleCount: Int, quantilesToSerialize: Int = 100)(using Encoder[T], Ordering[T]): Json =
      encoderUncertain(
        sampleCount = sampleCount,
        quantileIntervals = quantilesToSerialize
      ).apply(u)
  }

  extension (q: Quantiles.type) {

    /** {{{
      *   { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] }
      * }}}
      */
    def fromJson[T](json: Json)(using Decoder[T]): Either[DecodingFailure, Quantiles[T]] =
      decoderQuantiles.decodeJson(json)
  }

  extension (u: Uncertain.type) {

    /** {{{
      *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
      * }}}
      * @see
      *   [[Quantiles.reconstructDiscrete]]
      */
    def fromJsonAsDiscreteDistribution[T](json: Json)(using dec: Decoder[T], random: Random = new Random()): Either[DecodingFailure, Uncertain[T]] =
      decoderUncertainDiscrete.decodeJson(json)

    /** {{{
      *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
      * }}}
      * @see
      *   [[Quantiles.reconstructStep]]
      */
    def fromJsonAsStepDistribution(json: Json)(using random: Random = new Random()): Either[DecodingFailure, Uncertain[Double]] =
      decoderUncertainStep.decodeJson(json)

    /** {{{
      *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
      * }}}
      * @see
      *   [[Quantiles.reconstructLinear]]
      */
    def fromJsonAsLinearDistribution(json: Json)(using random: Random = new Random()): Either[DecodingFailure, Uncertain[Double]] =
      decoderUncertainLinear.decodeJson(json)

    /** {{{
      *   { "quantiles": { "boundaryValues": [10.0, 20.0, 30.0, 40.0, 50.0] } }
      * }}}
      * @see
      *   [[Quantiles.reconstructSmooth]]
      */
    def fromJsonAsSmoothDistribution(json: Json)(using random: Random = new Random()): Either[DecodingFailure, Uncertain[Double]] =
      decoderUncertainSmooth.decodeJson(json)
  }

}
