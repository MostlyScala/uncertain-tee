package mostly.uncertaintee

import munit.FunSuite

import scala.util.Random

/** A base trait for MUnit test suites that require a deterministic, seeded Random instance for each test */
trait RngSuite extends FunSuite {

  val seededRandomFixture = FunFixture[Random](
    setup = { _ => new Random(42L) },
    teardown = { _ => () }
  )

  def rngTest(name: String)(testLogic: Random ?=> Any): Unit =
    seededRandomFixture.test(name)(rng => testLogic(using rng))
}
