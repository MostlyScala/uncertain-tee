package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.point

import scala.util.Random

trait DistributionOpsShort {
  extension (u: Uncertain.type) {

    def uniformShort(
      minInclusive: Short,
      maxExclusive: Short
    )(using random: Random = new Random()): Uncertain[Short] = {
      require(maxExclusive >= minInclusive, s"max ($maxExclusive) must be >= min ($minInclusive).")
      if (minInclusive == maxExclusive) Uncertain.point(minInclusive)
      else Uncertain(() => random.between(minInclusive.toInt, maxExclusive.toInt).toShort)
    }

  }

}
