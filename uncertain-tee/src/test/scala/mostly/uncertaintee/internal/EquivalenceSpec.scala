package mostly.uncertaintee.internal

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.internal.Equivalence.KolmogorovSmirnov
import munit.FunSuite

class EquivalenceSpec extends FunSuite {
  test("kolmogorov-smirnov statistic of the same series") {
    val statistic = KolmogorovSmirnov.statistic(
      f1 = Uncertain.always(1),
      f2 = Uncertain.always(1),
      n = 1,
      m = 1,
      xValues = Seq(1)
    )

    assertEquals(statistic, 0.0)
  }

  test("kolmogorov-smirnov statistic of different series") {
    val statistic = KolmogorovSmirnov.statistic(
      f1 = Uncertain.always(1),
      n = 1,
      f2 = Uncertain.always(10),
      m = 1,
      xValues = 0 until 10
    )

    assertEquals(statistic, 0.1)
  }

  test("kolmogorov-smirnov test of the same series") {
    assert(
      KolmogorovSmirnov.test(
        f1 = Uncertain.always(1),
        n = 1,
        f2 = Uncertain.always(1),
        m = 1,
        xValues = Seq(1),
        alpha = 1.0
      )
    )
  }

  test("kolmogorov-smirnov test of different series") {
    assert(
      !KolmogorovSmirnov.test(
        f1 = Uncertain.always(1),
        n = 1,
        f2 = Uncertain.always(10),
        m = 1,
        xValues = Seq(1),
        alpha = 1.0
      )
    )
  }
}
