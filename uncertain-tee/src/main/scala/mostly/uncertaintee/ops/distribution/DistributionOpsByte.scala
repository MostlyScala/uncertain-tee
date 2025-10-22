/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee.ops.distribution

import mostly.uncertaintee._
import mostly.uncertaintee.syntax.*

import scala.util.Random

trait DistributionOpsByte {
  extension (u: Uncertain.type) {

    /** Creates a uniform distribution of Bytes. */
    def uniformByte(minInclusive: Byte, maxExclusive: Byte)(using random: Random = new Random()): Uncertain[Byte] = {
      require(maxExclusive >= minInclusive, s"max ($maxExclusive) must be >= min ($minInclusive).")
      if (minInclusive == maxExclusive) Uncertain.always(minInclusive)
      else Uncertain(() => random.between(minInclusive.toInt, maxExclusive.toInt).toByte)
    }
  }
}
