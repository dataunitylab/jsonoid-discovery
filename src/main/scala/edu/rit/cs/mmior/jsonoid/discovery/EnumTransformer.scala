package edu.rit.cs.mmior.jsonoid.discovery

import scala.reflect.{ClassTag, classTag}

import org.json4s._

import schemas._

object EnumTransformer {
  val EnumRatio: Int = 10
  val MaxValues: Int = 50

  def getFromOther[S <: JsonSchema[_]: ClassTag](
      schema: S,
      maybeOtherSchema: Option[JsonSchema[_]],
      path: String
  ): Option[S] = {
    val pointer = Helpers.pathToPointer(path)
    maybeOtherSchema match {
      case Some(otherSchema) =>
        otherSchema.findByPointer(pointer) match {
          case Some(other: S) if classTag[S].runtimeClass.isInstance(other) =>
            Some(other.asInstanceOf[S])
          case _ =>
            throw new IllegalStateException(
              "Could not find matching schema when transforming to enum"
            )
        }
      case None => None
    }
  }

  def transformSchema(
      schema: JsonSchema[_],
      otherSchema: Option[JsonSchema[_]] = None
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    schema.transformPropertiesWithPath(
      {
        case (path, i: IntegerSchema)
            if i.properties.has[IntExamplesProperty] =>
          val examples = i.properties.get[IntExamplesProperty].examples
          val otherExamples = getFromOther(i, otherSchema, path).map(
            _.properties.get[IntExamplesProperty].examples
          )
          maybeEnum(examples, otherExamples, { case v: BigInt => JInt(v) })
            .getOrElse(i)
        case (path, n: NumberSchema) if n.properties.has[NumExamplesProperty] =>
          val examples = n.properties.get[NumExamplesProperty].examples
          val otherExamples = getFromOther(n, otherSchema, path).map(
            _.properties.get[NumExamplesProperty].examples
          )
          maybeEnum(
            examples,
            otherExamples,
            { case v: BigDecimal => JDecimal(v) }
          ).getOrElse(n)
        case (path, s: StringSchema)
            if s.properties.has[StringExamplesProperty] =>
          val examples = s.properties.get[StringExamplesProperty].examples
          val otherExamples = getFromOther(s, otherSchema, path).map(
            _.properties.get[StringExamplesProperty].examples
          )
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
