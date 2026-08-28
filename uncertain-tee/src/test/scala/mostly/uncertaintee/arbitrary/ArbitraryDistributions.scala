package mostly.uncertaintee.arbitrary

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryDistributions {

  def genPoint: Gen[Uncertain[Double]] =
    Gen.sized { size =>
      for {
        point <- Gen.choose(-size / 2, size / 2)
        _     <- Gen.label(s"point($point)")
      } yield Uncertain.point(point)
    }

  def genMixture[T](distributionGen: => Gen[Uncertain[T]]): Gen[Uncertain[T]] =
    Gen
      .nonEmptyMap(
        for {
          key   <- distributionGen
          value <- Gen.posNum[Double]
        } yield key -> value
      )
      .map(Uncertain.mixture)

  def genEqualMixture[T](distributionGen: => Gen[Uncertain[T]]): Gen[Uncertain[T]] =
    Gen
      .nonEmptyListOf(distributionGen)
      .map(Uncertain.equalMixture)

  def genEmpirical: Gen[Uncertain[Double]] =
    Gen.sized { size =>
      for {
        data <- genNumbersWithSum(size)
        _    <- Gen.label(s"empirical(data=$data)})")
      } yield Uncertain.empirical(data)
    }

  def genCategorical[T: Arbitrary]: Gen[Uncertain[T]] =
    for {
      values <- Gen.nonEmptyMap {
                  Gen.zip(arbitrary[T], Gen.posNum[Double])
                }
      _      <- Gen.label(s"categorical(values=$values)")
    } yield Uncertain.categorical(values)

  def genNormal: Gen[Uncertain[Double]] =
    Gen.sized { size =>
      for {
        mean              <- Gen.choose(-size / 2, size / 2)
        standardDeviation <- Gen.choose(0, size / 2)
        _                 <- Gen.label(s"normal(mean=$mean,stdDev=$standardDeviation)")
      } yield Uncertain.normal(mean, standardDeviation)
    }

  def genUniform: Gen[Uncertain[Double]] =
    Gen.sized { size =>
      for {
        min <- Gen.choose(-size / 2, size / 2)
        max <- Gen.choose(min, size / 2)
        _   <- Gen.label(s"uniform(min=$min, max=$max)")
      } yield Uncertain.uniform(min, max)
    }

  def genTriangular: Gen[Uncertain[Double]] =
    Gen.sized { size =>
      for {
        min  <- Gen.choose(-size / 2, size / 2)
        max  <- Gen.choose(min, size / 2)
        peak <- Gen.choose(min, max)
        _    <- Gen.label(s"triangular(min=$min,peak=$peak,max=$max)")
      } yield Uncertain.triangular(min, peak, max)
    }

  def genExponential: Gen[Uncertain[Double]] =
    for {
      rate <- Gen.double
      _    <- Gen.label(s"exponential(rate=$rate)")
    } yield Uncertain
      .exponential(rate)

  def genBernoulli: Gen[Uncertain[Boolean]] =
    for {
      probability <- Gen.choose(0.0, 1.0)
      _           <- Gen.label(s"bernoulli($probability)")
    } yield Uncertain.bernoulli(probability)

  def genKumaraswamy: Gen[Uncertain[Double]] =
    Gen.sized { size =>
      for {
        a <- Gen.choose(1, size.max(1))
        b <- Gen.choose(1, size.max(1))
        _ <- Gen.label(s"kumaraswamy($a, $b)")
      } yield Uncertain.kumaraswamy(a, b)
    }

  def genRayleigh: Gen[Uncertain[Double]] =
    for {
      size  <- Gen.size
      if size > 0
      scale <- Gen.choose[Double](1, size)
      _     <- Gen.label(s"rayleigh(scale=$scale)")
    } yield Uncertain.rayleigh(scale)

  def genBinomial: Gen[Uncertain[Int]] =
      for {
        size  <- Gen.size
        if size > 0
        trials      <- Gen.choose(1, size)
        probability <- Gen.chooseNum[Double](0, 1)
        _           <- Gen.label(s"binomial(trials= $trials, probability=$probability")
      } yield Uncertain.binomial(trials, probability)

  def genPoisson: Gen[Uncertain[Int]] =
      for {
        size  <- Gen.size
        lambda <- Gen.choose(0, size)
        _      <- Gen.label(s"poisson(lambda=$lambda)")
      } yield Uncertain.poisson(lambda)

  // fix type to `Double` so we can include all distributions
  def genDistribution: Gen[Uncertain[Double]] =
    Gen.oneOf(
      genPoint,
      genEmpirical,
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

  given Arbitrary[Uncertain[Double]] = Arbitrary(genDistribution)

  /** Generates a list of values with a given sum.
    *
    * @param sum
    *   the target value
    * @return
    *   a list of numbers that equal the target value
    */
  def genNumbersWithSum(sum: Double): Gen[List[Double]] = Gen.sized { size =>
    if size <= 1 then Gen.const(sum :: Nil)
    else
      Gen.tailRecM((sum, size, List.empty[Double])) {
        case (k, 1, acc) => Gen.const(Right(k +: acc))
        case (k, n, acc) =>
          val bound = math.abs(k)
          Gen.choose(-bound, bound).map { nextK =>
            Left(k - nextK, n - 1, nextK +: acc)
          }
      }
  }
}
