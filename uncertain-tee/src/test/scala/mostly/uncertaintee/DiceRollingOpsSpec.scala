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

import mostly.uncertaintee.syntax.*

class DiceRollingOpsSpec extends RngSuite {

  rngTest("exploding success counts should allow thresholds above die sides") {
    val numberOfDice = 5000
    val successes    = Uncertain
      .rollExplodingCountSuccessesAtThreshold(
        numberOfDice = numberOfDice,
        sides = 6,
        successThreshold = 8,
        explodeThreshold = 6,
        maxExplosions = 2
      )
      .sample()

    assert(successes >= 0, s"Successes should be non-negative, got $successes")
    assert(successes <= numberOfDice, s"Successes should be <= $numberOfDice, got $successes")
    assert(successes > 0, "Exploding d6 should produce totals >= 8 sometimes")
  }

  rngTest("exploding failure counts should allow thresholds above die sides") {
    val numberOfDice = 5000
    val failures     = Uncertain
      .rollExplodingCountFailuresAtThreshold(
        numberOfDice = numberOfDice,
        sides = 6,
        successThreshold = 8,
        explodeThreshold = 6,
        maxExplosions = 2
      )
      .sample()

    assert(failures >= 0, s"Failures should be non-negative, got $failures")
    assert(failures <= numberOfDice, s"Failures should be <= $numberOfDice, got $failures")
    assert(failures < numberOfDice, "Exploding d6 should not fail all rolls at threshold 8")
  }
}
