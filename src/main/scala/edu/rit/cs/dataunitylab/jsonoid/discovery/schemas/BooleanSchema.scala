package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect._

import org.json4s.JsonDSL._
import org.json4s._

object BooleanSchema {
  def apply(
      value: Boolean
  )(implicit propSet: PropertySet, p: JsonoidParams): BooleanSchema = {
    BooleanSchema(
      propSet.booleanProperties.mergeValue(value)(p)
    )
  }

  val MinProperties: SchemaProperties[Boolean] =
    SchemaProperties.empty[Boolean]

  val SimpleProperties: SchemaProperties[Boolean] =
    SchemaProperties.empty[Boolean]

  val AllProperties: SchemaProperties[Boolean] = {
    val props = SchemaProperties.empty[Boolean]
    props.add(BooleanPercentProperty())

    props
  }
}

/** Represents Boolean values in JSON Schema.
  */
final case class BooleanSchema(
    override val properties: SchemaProperties[Boolean] =
      BooleanSchema.AllProperties
) extends JsonSchema[Boolean] {
  override val schemaType = "boolean"

  override val validTypes: Set[Class[_]] = Set(classOf[JBool])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ BooleanSchema(otherProperties) =>
      BooleanSchema(properties.merge(otherProperties, mergeType))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[Boolean]): BooleanSchema = {
    val newSchema = BooleanSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  override def collectAnomalies[S <: JValue](
      value: S,
      path: String
  )(implicit p: JsonoidParams, t: ClassTag[S]): Seq[Anomaly] = {
    value match {
      case JBool(_) => Seq.empty
      case _        => Seq(Anomaly(path, "expected boolean type", AnomalyLevel.Fatal))
    }
  }
}

/** Tracks the percentage of true values for a boolean.
  *
  * @constructor Create a new Boolean percent property
  * @param totalTrue the number of true values observed
  * @param totalFalse the number of false values observed
  */
final case class BooleanPercentProperty(
    totalTrue: BigInt = 0,
    totalFalse: BigInt = 0
) extends SchemaProperty[Boolean] {
  override type S = BooleanPercentProperty

  override def newDefault()(implicit p: JsonoidParams): BooleanPercentProperty =
    BooleanPercentProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = if (
    totalTrue > 0 || totalFalse > 0
  ) {
    ("pctTrue" -> totalTrue.toDouble / (totalTrue + totalFalse).toDouble)
  } else {
    Nil
  }

  override def unionMerge(
      otherProp: BooleanPercentProperty
  )(implicit p: JsonoidParams): BooleanPercentProperty = {
    BooleanPercentProperty(
      totalTrue + otherProp.totalTrue,
      totalFalse + otherProp.totalFalse
    )
  }

  override def mergeValue(
      value: Boolean
  )(implicit p: JsonoidParams): BooleanPercentProperty = {
    BooleanPercentProperty(
      totalTrue + (if (value) 1 else 0),
      totalFalse + (if (!value) 1 else 0)
    )
  }
}
