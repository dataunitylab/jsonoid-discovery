package edu.rit.cs.mmior.jsonoid.discovery

final case class JsonoidParams(
    val additionalProperties: Boolean = false,
    val er: EquivalenceRelation = EquivalenceRelations.KindEquivalenceRelation,
    val maxExamples: Int = 100
) {
  def withAdditionalProperties(
      newAdditionalProperties: Boolean
  ): JsonoidParams = {
    JsonoidParams(newAdditionalProperties, er, maxExamples)
  }

  def withER(newER: EquivalenceRelation): JsonoidParams = {
    JsonoidParams(additionalProperties, newER, maxExamples)
  }

  def withMaxExamples(newMaxExamples: Int): JsonoidParams = {
    JsonoidParams(additionalProperties, er, newMaxExamples)
  }
}

object JsonoidParams {
  implicit val defaultJsonoidParams: JsonoidParams = JsonoidParams()
}
