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
package mostly.uncertaintee

import mostly.uncertaintee.quantiles.Percentiles
import mostly.uncertaintee.syntax.*

import scala.math.abs

class PercentilesSpec extends RngSuite {

  val tolerance = 0.05

  rngTest("A point distribution should result in all percentiles being that point") {
    val p: Percentiles[Int] = Uncertain.always(42).percentiles(1000)
    (0 to 100).foreach { n =>
      assert(
        p.percentile(n) == 42,
        s"Percentile $n should be exactly 42 when underlying distribution always returns 42"
      )
    }
  }

  rngTest("Uniform distribution should should have correct percentiles (uniform 0 to 10_000)") {
    val p: Percentiles[Int]  = Uncertain.fromRange(0 to 10_000).percentiles(5_000_000)
    val percentileBucketSize = 100
    val toleranceForTest     = tolerance * percentileBucketSize
    (0 to 100).foreach { n =>
      val theoretical = n * percentileBucketSize
      val actual      = p.percentile(n)
      assert(
        abs(theoretical - actual) <= toleranceForTest,
        s"Percentile $n should be within a $toleranceForTest tolerance of $theoretical (actual: $actual)"
      )
    }
  }

  rngTest("Uniform distribution should should have correct percentiles (uniform 0 to 100_000)") {
    val p: Percentiles[Int]  = Uncertain.fromRange(0 to 100_000).percentiles(5_000_000)
    val percentileBucketSize = 1000
    val toleranceForTest     = tolerance * percentileBucketSize
    (0 to 100).foreach { n =>
      val theoretical = n * percentileBucketSize
      val actual      = p.percentile(n)
      assert(
        abs(theoretical - actual) <= toleranceForTest,
        s"Percentile $n should be within a $toleranceForTest tolerance of $theoretical (actual: $actual)"
      )
    }
  }

  rngTest("Uniform distribution should should have correct percentiles (uniform 0 to 1_000_000_000)") {
    val p: Percentiles[Int]  = Uncertain.fromRange(0 to 100_000_000).percentiles(5_000_000)
    val percentileBucketSize = 1_000_000
    val toleranceForTest     = tolerance * percentileBucketSize
    (0 to 100).foreach { n =>
      val theoretical = n * percentileBucketSize
      val actual      = p.percentile(n)
      assert(
        abs(theoretical - actual) <= toleranceForTest,
        s"Percentile $n should be within a $toleranceForTest tolerance of $theoretical (actual: $actual)"
      )
    }
  }

}
