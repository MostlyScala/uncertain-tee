package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee.Uncertain

import java.util.Random

trait DistributionOpsBoolean {

  extension (u: Uncertain.type) {

    /** Creates a uniform distribution of Boolean values */
    def uniformBoolean(using random: Random = new Random()): Uncertain[Boolean] =
      Uncertain(random.nextBoolean)
  }
}
