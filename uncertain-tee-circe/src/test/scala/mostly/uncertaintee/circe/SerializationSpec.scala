package mostly.uncertaintee.circe

import io.circe.literal.*
import mostly.uncertaintee.quantiles.Quantiles
import mostly.uncertaintee.{RngSuite, Uncertain}

import scala.util.Random

class SerializationSpec extends RngSuite {

  test("Quantiles should roundtrip via Encoder/Decoder") {
    val q    = Quantiles.fromBoundaryValues(list = List(1.0, 2.0, 3.0))
    val json = json"""{ "boundaryValues": [1.0, 2.0, 3.0] }"""

    assertEquals(
      obtained = encoderQuantiles[Double].apply(a = q),
      expected = json,
      clue = "Encoding Quantiles via implicit Encoder should produce the expected JSON"
    )
    val decoded = decoderQuantiles[Double].decodeJson(j = json)
    assertEquals(
      obtained = decoded.isRight,
      expected = true,
      clue = "Decoding Quantiles via implicit Decoder should succeed"
    )
    assertEquals(
      obtained = decoded.toOption.get.boundaryValues,
      expected = q.boundaryValues,
      clue = "Decoded Quantiles boundary values should match original"
    )
  }

  test("Quantiles should roundtrip via extension methods") {
    val q    = Quantiles.fromBoundaryValues(list = List(1.0, 2.0, 3.0))
    val json = json"""{ "boundaryValues": [1.0, 2.0, 3.0] }"""

    assertEquals(
      obtained = q.toJson,
      expected = json,
      clue = "Encoding Quantiles via .toJson should produce the expected JSON"
    )
    val decoded = Quantiles.fromJson[Double](json = json)
    assertEquals(
      obtained = decoded.isRight,
      expected = true,
      clue = "Decoding Quantiles via Quantiles.fromJson should succeed"
    )
    assertEquals(
      obtained = decoded.toOption.get.boundaryValues,
      expected = q.boundaryValues,
      clue = "Decoded Quantiles boundary values from extension method should match original"
    )
  }

  rngTest("Uncertain should encode to JSON") {
    val u        = Uncertain.always(value = 10.0)
    val json     = u.toJson(sampleCount = 10, quantilesToSerialize = 4)
    val expected = json"""{ "quantiles": { "boundaryValues": [10.0, 10.0, 10.0, 10.0, 10.0] } }"""
    assertEquals(
      obtained = json,
      expected = expected,
      clue = "Encoding Uncertain.always(10.0) should produce the expected JSON with 5 boundary values"
    )
  }

  rngTest("Uncertain should decode via reconstructDiscrete") {
    val json   = json"""{ "quantiles": { "boundaryValues": [10.0, 20.0, 30.0] } }"""
    val result = Uncertain.fromJsonAsDiscreteDistribution[Double](json = json)

    assertEquals(
      obtained = result.isRight,
      expected = true,
      clue = "Decoding Uncertain via fromJsonAsDiscreteDistribution should succeed"
    )
    val u       = result.toOption.get
    val samples = u.take(n = 1000)
    assertEquals(
      obtained = samples.forall(s => s == 10.0 || s == 20.0 || s == 30.0),
      expected = true,
      clue = "Discrete reconstruction samples must only include boundary values"
    )
  }

  rngTest("Uncertain should decode via reconstructStep") {
    val json   = json"""{ "quantiles": { "boundaryValues": [10.0, 20.0, 30.0] } }"""
    val result = Uncertain.fromJsonAsStepDistribution(json = json)

    assertEquals(
      obtained = result.isRight,
      expected = true,
      clue = "Decoding Uncertain via fromJsonAsStepDistribution should succeed"
    )
    val u       = result.toOption.get
    val samples = u.take(n = 10)
    assertEquals(
      obtained = samples.forall(s => s >= 10.0 && s <= 30.0),
      expected = true,
      clue = "Step reconstruction samples must be within boundary range [10, 30]"
    )
  }

  rngTest("Uncertain should decode via reconstructLinear") {
    val json   = json"""{ "quantiles": { "boundaryValues": [10.0, 20.0, 30.0] } }"""
    val result = Uncertain.fromJsonAsLinearDistribution(json = json)

    assertEquals(
      obtained = result.isRight,
      expected = true,
      clue = "Decoding Uncertain via fromJsonAsLinearDistribution should succeed"
    )
    val u       = result.toOption.get
    val samples = u.take(n = 10)
    assertEquals(
      obtained = samples.forall(s => s >= 10.0 && s <= 30.0),
      expected = true,
      clue = "Linear reconstruction samples must be within boundary range [10, 30]"
    )
  }

  rngTest("Uncertain should decode via reconstructSmooth") {
    // Smooth reconstruction might need more points
    val json   = json"""{ "quantiles": { "boundaryValues": [10.0, 15.0, 20.0, 25.0, 30.0] } }"""
    val result = Uncertain.fromJsonAsSmoothDistribution(json = json)

    assertEquals(
      obtained = result.isRight,
      expected = true,
      clue = "Decoding Uncertain via fromJsonAsSmoothDistribution should succeed"
    )
    val u       = result.toOption.get
    val samples = u.take(n = 10)
    assertEquals(
      obtained = samples.forall(s => s >= 10.0 && s <= 30.0),
      expected = true,
      clue = "Smooth reconstruction samples must be within boundary range [10, 30]"
    )
  }

  test("Direct decoder usage for Uncertain") {
    val json = json"""{ "quantiles": { "boundaryValues": [10.0, 20.0, 30.0] } }"""

    assertEquals(
      obtained = decoderUncertainDiscrete[Double].decodeJson(j = json).isRight,
      expected = true,
      clue = "Discrete decoder should successfully decode valid JSON"
    )
    assertEquals(
      obtained = decoderUncertainStep.decodeJson(j = json).isRight,
      expected = true,
      clue = "Step decoder should successfully decode valid JSON"
    )
    assertEquals(
      obtained = decoderUncertainLinear.decodeJson(j = json).isRight,
      expected = true,
      clue = "Linear decoder should successfully decode valid JSON"
    )
  }

  test("Direct decoder usage for Uncertain Smooth") {
    val json = json"""{ "quantiles": { "boundaryValues": [10.0, 15.0, 20.0, 25.0, 30.0] } }"""
    assertEquals(
      obtained = decoderUncertainSmooth.decodeJson(j = json).isRight,
      expected = true,
      clue = "Smooth decoder should successfully decode valid JSON with enough points"
    )
  }
}
