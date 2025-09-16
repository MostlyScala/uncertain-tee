package mostly.uncertaintee

import munit.FunSuite
import mostly.uncertaintee.syntax.*

class StackSafetyTest extends FunSuite {

  test("recursive samples should be stack safe") {
    def foo(value: Uncertain[Int]): Uncertain[Int] =
      value.flatMap(i =>
        if (i == 1_000_000) Uncertain.point(i)
        else foo(Uncertain.point(i + 1))
      )

    assertEquals(foo(Uncertain.point(0)).sample(), 1_000_000)
  }
}
