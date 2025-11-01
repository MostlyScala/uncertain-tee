package mostly.uncertaintee.arbitrary

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryDistributions {

  // Use to avoid poor test performance by limiting distribution sizes
  private val SizeLimit = 10_000

  def genPoint[T: Arbitrary]: Gen[Uncertain[T]] =
    arbitrary[T].map(Uncertain.point)

  def genMixture[T: Arbitrary](distributionGen: => Gen[Uncertain[T]]): Gen[Uncertain[T]] =
    Gen
      .nonEmptyMap(
        for {
          key   <- distributionGen
          value <- Gen.posNum[Double]
        } yield key -> value
      )
      .map(Uncertain.mixture)

  def genEqualMixture[T: Arbitrary](distributionGen: => Gen[Uncertain[T]]): Gen[Uncertain[T]] =
    Gen
      .nonEmptyListOf(distributionGen)
      .map(Uncertain.equalMixture)

  def genEmpirical[T: Arbitrary]: Gen[Uncertain[T]] =
    Gen.nonEmptyListOf(arbitrary[T]).map(Uncertain.empirical)

  def genCategorical[T: Arbitrary]: Gen[Uncertain[T]] =
    Gen.nonEmptyMap(arbitrary[(T, Double)]).map(Uncertain.categorical)

  def genNormal: Gen[Uncertain[Double]] =
    for {
      mean              <- arbitrary[Double]
      standardDeviation <- Gen.posNum[Double]
    } yield Uncertain.normal(mean, standardDeviation)

  def genUniform: Gen[Uncertain[Double]] =
    for {
      min <- arbitrary[Double]
      max <- Gen.choose(min, Double.PositiveInfinity)
    } yield Uncertain.uniform(min, max)

  def genTriangular: Gen[Uncertain[Double]] =
    for {
      min  <- arbitrary[Double]
      max  <- Gen.choose(min, Double.PositiveInfinity)
      peak <- Gen.choose(min, max)
    } yield Uncertain.triangular(min, peak, max)

  def genExponential: Gen[Uncertain[Double]] =
    for {
      rate <- Gen.posNum[Double]
    } yield Uncertain.exponential(rate)

  def genBernoulli: Gen[Uncertain[Boolean]] =
    for {
      rate <- arbitrary[Double]
    } yield Uncertain.bernoulli(rate)

  def genKumaraswamy: Gen[Uncertain[Double]] =
    for {
      a <- Gen.posNum[Double]
      b <- Gen.posNum[Double]
    } yield Uncertain.kumaraswamy(a, b)

  def genRayleigh: Gen[Uncertain[Double]] =
    for {
      scale <- arbitrary[Double]
    } yield Uncertain.rayleigh(scale)

  def genBinomial: Gen[Uncertain[Int]] =
    for {
      trials      <- arbitrary[Int]
      probability <- arbitrary[Double]
    } yield Uncertain.binomial(trials, probability)

  def genPoisson: Gen[Uncertain[Int]] =
    for {
      lambda <- Gen.posNum[Double]
    } yield Uncertain.poisson(lambda)

  // fix type to `Double` so we can include all distributions
  def genDistribution: Gen[Uncertain[Double]] = {
    val singleDist = Gen.oneOf(
      genPoint[Double],
      genEmpirical[Double],
      genCategorical[Double],
      genNormal,
      genUniform,
      genTriangular,
      genExponential,
      genBernoulli.map(_.map(b => if b then 1.0 else 0.0)),
      genKumaraswamy,
      genRayleigh,
      genBinomial.map(_.map(_.toDouble)),
      genPoisson.map(_.map(_.toDouble))
    )

    // avoid outright recursive distributions because test performance suffers otherwise
    val mixedDist = Gen.oneOf(
      genMixture[Double](singleDist),
      genEqualMixture[Double](singleDist)
    )

    Gen.oneOf(singleDist, mixedDist)
  }

  given Arbitrary[Uncertain[Double]] = Arbitrary(genDistribution)

  def genXValues: Gen[Seq[Double]] =
    for {
      start <- Gen.choose(0, SizeLimit)
      end   <- Gen.choose(start, SizeLimit)
      step  <- Gen.choose(1, SizeLimit)

    } yield (start to end by step).map(_.toDouble)
}
