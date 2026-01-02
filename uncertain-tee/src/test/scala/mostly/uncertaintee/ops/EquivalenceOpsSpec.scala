package mostly.uncertaintee.ops

import mostly.uncertaintee.Proofs.ratioTest
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.arbitrary.ArbitraryDistributions
import mostly.uncertaintee.syntax.*
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, forAllNoShrink}

/** Some distributions require more samples before we identify a consistent, accurate equivalence score. To achieve that consistently, while executing tests in a reasonable time,
  * we evaluate properties using a [[https://en.wikipedia.org/wiki/Ratio_test Ratio test]].
  */
class EquivalenceOpsSpec extends ScalaCheckSuite with ArbitraryDistributions {

  property("kolmogorov-smirnov statistic of the same series approaches zero") {
    forAll { (distribution: Uncertain[Double]) =>
      val result =
        ratioTest(accuracy = 0.1) { n =>
          Uncertain.ksStatistic(
            f1 = distribution,
            n = n,
            f2 = distribution,
            m = n
          )
        }

      forAllNoShrink("result" |: result)(_ < 1)
    }
  }

  // we can't compare two arbitrary distributions, as sometimes distributions with different base formulas still produce similar values
//  property("kolmogorov-smirnov statistic of the same series exceeds one") {
//  }

//  property("kolmogorov-smirnov test identifies identical distributions") {
//  }

  //  property("kolmogorov-smirnov test identifies different distributions") {
  //  }
}
