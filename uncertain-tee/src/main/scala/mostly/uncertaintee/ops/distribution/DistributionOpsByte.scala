/*
 * Copyright (c) 2025 Mostly Codes
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
