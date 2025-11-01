package mostly.uncertaintee.ops

import mostly.uncertaintee.syntax.*
import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.arbitrary.ArbitraryDistributions
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Gen, Prop}

class EquivalenceOpsSpec extends ScalaCheckSuite with ArbitraryDistributions {

  property("kolmogorov-smirnov statistic of the same series") {
    forAll(
      "distribution" |: arbitrary[Uncertain[Double]],
      "n" |: Gen.posNum[Int],
      "m" |: Gen.posNum[Int]
    ) { (distribution, n, m) =>
      forAll("xValues" |: genXValues()) { xValues =>
        val statistic = Uncertain.ksStatistic(
          f1 = distribution,
          n,
          f2 = distribution,
          m,
          xValues
        )

        statistic ?= 0.0
      }
    }
  }

//  property("kolmogorov-smirnov statistic of different series") {
//    val statistic = KolmogorovSmirnov.statistic(
//      f1,
//      n = 1,
//      f2,
//      m = 1,
//      xValues = 0 until 10
//    )
//
//    assertEquals(statistic, 0.1)
//  }
//
//  property("kolmogorov-smirnov test of the same series") {
//    assert(
//      KolmogorovSmirnov.test(
//        f1 = Uncertain.always(1),
//        n = 1,
//        f2 = Uncertain.always(1),
//        m = 1,
//        xValues = Seq(1),
//        alpha = 0.0
//      )
//    )
//  }
//
//  property("kolmogorov-smirnov test of different series") {
//    assert(
//      !KolmogorovSmirnov.test(
//        f1 = Uncertain.always(1),
//        n = 1,
//        f2 = Uncertain.always(10),
//        m = 1,
//        xValues = Seq(1),
//        alpha = 0.0
//      )
//    )
//  }
}
