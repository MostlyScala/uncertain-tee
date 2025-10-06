package mostly.uncertaintee.squants

import _root_.squants.*
import mostly.uncertaintee.*
import mostly.uncertaintee.squants.ops.QuantityDistributionOps
import mostly.uncertaintee.syntax.*

import scala.util.Random

/** Syntax extensions for working with uncertain quantities */
object syntax extends QuantityDistributionOps {
  val distribution: QuantityDistributionOps = this
}
