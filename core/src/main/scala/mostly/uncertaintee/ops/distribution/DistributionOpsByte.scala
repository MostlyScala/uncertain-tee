package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.*
import mostly.uncertaintee.syntax.point

import scala.util.Random

trait DistributionOpsByte {
  extension (u: Uncertain.type) {

    /** Creates a uniform distribution of Bytes. */
    def uniformByte(minInclusive: Byte, maxExclusive: Byte)(using random: Random = new Random()): Uncertain[Byte] = {
      require(maxExclusive >= minInclusive, s"max ($maxExclusive) must be >= min ($minInclusive).")
      if (minInclusive == maxExclusive) Uncertain.point(minInclusive)
      else Uncertain(() => random.between(minInclusive.toInt, maxExclusive.toInt).toByte)
    }
  }
}
