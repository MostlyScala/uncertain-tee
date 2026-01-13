package mostly.uncertaintee.arbitrary

import mostly.uncertaintee.Uncertain
import munit.ScalaCheckSuite
import org.scalacheck.Prop.{exists, forAll}

class ArbitraryDistributionsSpec extends ScalaCheckSuite with ArbitraryDistributions {
  property("distributions only produce real numbers") {
    forAll { (distribution: Uncertain[Double]) =>
      val value = distribution.sample()
      !value.isNaN && !value.isInfinite
    }
  }

  property("distributions have real sums") {
    forAll { (distribution: Uncertain[Double], n: Int) =>
      val value = distribution.take(n).sum
      !value.isNaN && !value.isInfinite
    }
  }

  property("generating number lists with a given sum") {
    forAll { (sum: Double) =>
      exists(genNumbersWithSum(sum)) { result =>
        result.sum == sum
      }
    }
  }
}
