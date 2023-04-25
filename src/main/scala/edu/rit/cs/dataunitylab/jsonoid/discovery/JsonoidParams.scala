package edu.rit.cs.dataunitylab.jsonoid.discovery

import schemas.{PropertySet, PropertySets}

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
    val propSet: PropertySet = PropertySets.AllProperties,
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
      propSet,
      resetFormatLength
    )
  }

  def withER(newER: EquivalenceRelation): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      newER,
      formatThreshold,
      maxExamples,
      propSet,
      resetFormatLength
    )
  }

  def withFormatThreshold(newFormatThreshold: Float): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      newFormatThreshold,
      maxExamples,
      propSet,
      resetFormatLength
    )
  }

  def withMaxExamples(newMaxExamples: Int): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      formatThreshold,
      newMaxExamples,
      propSet,
      resetFormatLength
    )
  }

  def withPropertySet(newPropSet: PropertySet): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      formatThreshold,
      maxExamples,
      newPropSet,
      resetFormatLength
    )
  }

  def withResetFormatLength(newResetFormatLength: Boolean): JsonoidParams = {
    JsonoidParams(
      additionalProperties,
      er,
      formatThreshold,
      maxExamples,
      propSet,
      newResetFormatLength
    )
  }
}

object JsonoidParams {
  implicit val defaultJsonoidParams: JsonoidParams = JsonoidParams()
}
