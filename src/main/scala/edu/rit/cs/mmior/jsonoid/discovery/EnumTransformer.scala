package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s._

import schemas._

object EnumTransformer {
  val EnumRatio: Int = 10

  def transformSchema(schema: JsonSchema[_]): JsonSchema[_] = {
    schema.transformProperties {
      case i @ IntegerSchema(properties) =>
        val examples = properties.get[IntExamplesProperty].examples
        maybeEnum(examples, { case v: BigInt => JInt(v) }).getOrElse(i)
      case n @ NumberSchema(properties) =>
        val examples = properties.get[NumExamplesProperty].examples
        maybeEnum(examples, { case v: BigDecimal => JDecimal(v) }).getOrElse(n)
      case s @ StringSchema(properties) =>
        val examples = properties.get[StringExamplesProperty].examples
        maybeEnum(examples, { case v: String => JString(v) }).getOrElse(s)
    }
  }

  private def maybeEnum(
      examples: ExamplesProperty[_],
      exampleTransformer: PartialFunction[Any, JValue]
  ): Option[JsonSchema[_]] = {
    val distinctExamples = examples.examples.distinct
    if (
      examples.totalExamples < ExamplesProperty.MaxExamples &&
      examples.totalExamples > (distinctExamples.length *
        EnumTransformer.EnumRatio)
    ) {
      Some(EnumSchema(distinctExamples.map(exampleTransformer)))
    } else {
      None
    }
  }
}
