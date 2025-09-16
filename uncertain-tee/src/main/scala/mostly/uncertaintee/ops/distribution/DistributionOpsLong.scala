package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.point

import scala.util.Random

trait DistributionOpsLong {
  extension (u: Uncertain.type) {

    /** Creates a uniform distribution of Longs. */
    def uniformLong(minInclusive: Long, maxExclusive: Long)(using random: Random = new Random()): Uncertain[Long] = {
      require(maxExclusive >= minInclusive, s"max ($maxExclusive) must be >= min ($minInclusive).")
      if (minInclusive == maxExclusive) Uncertain.point(minInclusive)
      else Uncertain(() => random.between(minInclusive, maxExclusive))
    }
  }
}
