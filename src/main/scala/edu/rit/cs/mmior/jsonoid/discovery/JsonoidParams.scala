package edu.rit.cs.mmior.jsonoid.discovery

/** Parameters used during the discovery process.
  *
  * @param additionalProperties whether `additionalProperties` is set to true in generated schemas
  * @param er the equivalence relation to use during the discovery process
  * @param formatThreshold the fraction of values that must match a given format for [[schemas.FormatProperty]] to consider the format valid
  * @param maxExamples the maximum number of examples to be kept for any examples property
  * @param resetFormatLength whether to reset max/min length of strings with [[schemas.FormatProperty]]
  */
final case class JsonoidParams(
    val additionalProperties: Boolean = false,
    val er: EquivalenceRelation = EquivalenceRelations.KindEquivalenceRelation,
    val formatThreshold: Float = 1.0f,
    val maxExamples: Int = 100,
    val resetFormatLength: Boolean = false
) {
  def withAdditionalProperties(
      newAdditionalProperties: Boolean
  ): JsonoidParams = {
    JsonoidParams(
      newAdditionalProperties,
      er,
      formatThreshold,
      maxExamples,
      resetFormatLength
    )
  }

  def withER(newER: EquivalenceRelation): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      newER,
      formatThreshold,
      maxExamples,
      resetFormatLength
    )
  }

  def withFormatThreshold(newFormatThreshold: Float): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      newFormatThreshold,
      maxExamples,
      resetFormatLength
    )
  }

  def withMaxExamples(newMaxExamples: Int): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      formatThreshold,
      newMaxExamples,
      resetFormatLength
    )
  }

  def withResetFormatLength(newResetFormatLength: Boolean): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      formatThreshold,
      maxExamples,
      newResetFormatLength
    )
  }
}

object JsonoidParams {
  implicit val defaultJsonoidParams: JsonoidParams = JsonoidParams()
}
