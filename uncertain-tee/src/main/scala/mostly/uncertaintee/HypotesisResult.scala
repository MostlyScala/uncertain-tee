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
