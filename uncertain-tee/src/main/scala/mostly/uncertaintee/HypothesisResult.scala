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

/** Result from a statistical hypothesis test.
  *
  * @param decision
  *   true if we accept H₁ (effect exists), false if we accept H₀ (no effect)
  * @param probability
  *   Estimated probability based on samples
  * @param confidenceLevel
  *   Confidence level used (e.g., 0.95 for 95%)
  * @param samplesUsed
  *   Number of samples needed to reach decision
  */
final case class HypothesisResult(
  decision: Boolean,
  probability: Double,
  confidenceLevel: Double,
  samplesUsed: Int
)
