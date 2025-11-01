package mostly.uncertaintee.arbitrary

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.NumericRange.Inclusive

trait GenericDistributions[T: Arbitrary: Ordering: Choose] {

  // Use to avoid poor test performance by limiting distribution sizes
  val distributionSize: Inclusive[T]

  def genPoint: Gen[Uncertain[T]] =
    Gen.choose(distributionSize.min, distributionSize.max).map(Uncertain.point)

  def genMixture(distributionGen: => Gen[Uncertain[T]]): Gen[Uncertain[T]] =
    Gen
      .nonEmptyMap(
        for {
          key   <- distributionGen
          value <- Gen.posNum[Double]
        } yield key -> value
      )
      .map(Uncertain.mixture)

  def genEqualMixture(distributionGen: => Gen[Uncertain[T]]): Gen[Uncertain[T]] =
    Gen
      .nonEmptyListOf(distributionGen)
      .map(Uncertain.equalMixture)

  def genEmpirical: Gen[Uncertain[T]] =
    Gen.nonEmptyListOf(arbitrary[T]).map(Uncertain.empirical)

  def genCategorical: Gen[Uncertain[T]] =
    Gen.nonEmptyMap(arbitrary[(T, Double)]).map(Uncertain.categorical)

}
