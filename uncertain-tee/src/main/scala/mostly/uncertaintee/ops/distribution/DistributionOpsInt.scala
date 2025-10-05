package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.point

import scala.math.*
import scala.util.Random

trait DistributionOpsInt {
  extension (u: Uncertain.type) {

    /** Creates a uniform distribution of Integers. */
    def uniformInt(minInclusive: Int, maxExclusive: Int)(using random: Random = new Random()): Uncertain[Int] = {
      require(maxExclusive >= minInclusive, s"max ($maxExclusive) must be >= min ($minInclusive).")
      if (minInclusive == maxExclusive) Uncertain.point(minInclusive)
      else Uncertain(() => random.between(minInclusive, maxExclusive))
    }

    /** Creates a binomial distribution. */
    def binomialInt(trials: Int, probability: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(trials >= 0, "Number of trials cannot be negative.")
      require(probability >= 0 && probability <= 1, s"Probability ($probability) must be between 0 and 1.")
      Uncertain(() => (0 until trials).count(_ => random.nextDouble() < probability))
    }

    /** Creates a Poisson distribution. */
    def poissonInt(lambda: Double)(using random: Random = new Random()): Uncertain[Int] = {
      require(lambda >= 0, "Lambda (average rate) cannot be negative.")
      if (lambda == Zero) {
        Uncertain.point(0)
      } else {
        Uncertain { () =>
          val L = exp(-lambda)
          var k = 0
          var p = One
          while (p > L) {
            k += 1
            p *= random.nextDouble()
          }
          k - 1
        }
      }
    }
  }
}
