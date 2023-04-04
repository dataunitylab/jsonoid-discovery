package edu.rit.cs.dataunitylab.jsonoid.discovery

import org.json4s._

import schemas._

object EnumTransformer {
  val EnumRatio: Int = 10
  val MaxValues: Int = 50

  def getFromOther(
      schema: JsonSchema[_],
      maybeOtherSchema: Option[JsonSchema[_]],
      path: String
  ): Seq[JsonSchema[_]] = {
    val pointer = Helpers.pathToInexactPointer(path)
    maybeOtherSchema.map(_.findByInexactPointer(pointer)).getOrElse(Seq.empty)
  }

  def transformSchema(
      schema: JsonSchema[_],
      otherSchema: Option[JsonSchema[_]] = None
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    schema.transformPropertiesWithInexactPath(
      {
        case (path, i: IntegerSchema)
            if i.properties.has[IntExamplesProperty] =>
          val examples = i.properties.get[IntExamplesProperty].examples
          val otherSchemas = getFromOther(i, otherSchema, path)
          val otherExamples = if (otherSchema.isDefined) {
            otherSchemas match {
              case Seq(otherInt: IntegerSchema) =>
                Some(otherInt.properties.get[IntExamplesProperty].examples)
              case _ => Some(ExamplesProperty[BigInt]())
            }
          } else {
            None
          }
          maybeEnum(examples, otherExamples, { case v: BigInt => JInt(v) })
            .getOrElse(i)
        case (path, n: NumberSchema) if n.properties.has[NumExamplesProperty] =>
          val examples = n.properties.get[NumExamplesProperty].examples
          val otherSchemas = getFromOther(n, otherSchema, path)
          val otherExamples = if (otherSchema.isDefined) {
            otherSchemas match {
              case Seq(otherInt: NumberSchema) =>
                Some(otherInt.properties.get[NumExamplesProperty].examples)
              case _ => Some(ExamplesProperty[BigDecimal]())
            }
          } else {
            None
          }
          maybeEnum(
            examples,
            otherExamples,
            { case v: BigDecimal => JDecimal(v) }
          ).getOrElse(n)
        case (path, s: StringSchema)
            if s.properties.has[StringExamplesProperty] =>
          val examples = s.properties.get[StringExamplesProperty].examples
          val otherSchemas = getFromOther(s, otherSchema, path)
          val otherExamples = if (otherSchema.isDefined) {
            otherSchemas match {
              case Seq(otherInt: StringSchema) =>
                Some(otherInt.properties.get[StringExamplesProperty].examples)
              case _ => Some(ExamplesProperty[String]())
            }
          } else {
            None
          }
          maybeEnum(examples, otherExamples, { case v: String => JString(v) })
            .getOrElse(s)
      },
      true
    )
  }

  private def maybeEnum[S](
      examples: ExamplesProperty[S],
      otherExamples: Option[ExamplesProperty[S]],
      exampleTransformer: PartialFunction[Any, JValue]
  ): Option[JsonSchema[_]] = {
    val distinctExamples = examples.examples.toSet
    if (
      examples.totalExamples > (distinctExamples.size *
        EnumTransformer.EnumRatio) && distinctExamples.size < EnumTransformer.MaxValues
    ) {
      val otherExampleSet =
        otherExamples.map(_.examples.toSet).getOrElse(Set.empty)
      if (otherExampleSet.subsetOf(distinctExamples)) {
        Some(EnumSchema(distinctExamples.map(exampleTransformer)))
      } else {
        None
      }
    } else {
      None
    }
  }
}
