package edu.rit.cs.mmior.jsonoid.discovery

final case class JsonoidParams(
    val additionalProperties: Boolean = false,
    val er: EquivalenceRelation = EquivalenceRelations.KindEquivalenceRelation,
    val formatThreshold: Float = 1.0f,
    val maxExamples: Int = 100
) {
  def withAdditionalProperties(
      newAdditionalProperties: Boolean
  ): JsonoidParams = {
    JsonoidParams(newAdditionalProperties, er, formatThreshold, maxExamples)
  }

  def withER(newER: EquivalenceRelation): JsonoidParams = {
    JsonoidParams(additionalProperties, newER, formatThreshold, maxExamples)
  }

  def withFormatThreshold(newFormatThreshold: Float): JsonoidParams = {
    JsonoidParams(additionalProperties, er, newFormatThreshold, maxExamples)
  }

  def withMaxExamples(newMaxExamples: Int): JsonoidParams = {
    JsonoidParams(additionalProperties, er, formatThreshold, newMaxExamples)
  }
}

object JsonoidParams {
  implicit val defaultJsonoidParams: JsonoidParams = JsonoidParams()
}
