package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

object EnumSchema {
  def apply(value: Set[JValue])(implicit p: JsonoidParams): EnumSchema = {
    EnumSchema(
      EnumSchema.AllProperties.mergeValue(value)(p)
    )
  }

  val MinProperties: SchemaProperties[Set[JValue]] = {
    val props = SchemaProperties.empty[Set[JValue]]
    props.add(EnumValuesProperty())

    props
  }

  val AllProperties: SchemaProperties[Set[JValue]] = {
    val props = SchemaProperties.empty[Set[JValue]]
    props.add(EnumValuesProperty())

    props
  }
}

/** Represents both `enum` and `const` in JSON Schema.`
  */
final case class EnumSchema(
    override val properties: SchemaProperties[Set[JValue]] =
      EnumSchema.AllProperties
) extends JsonSchema[Set[JValue]] {
  override def hasType: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  override val validTypes: Set[Class[_]] = Set.empty

  override def isValidType[S <: JValue](
      value: S
  ): Boolean = {
    // Any type is potentially valid
    true
  }

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ EnumSchema(otherProperties) =>
      EnumSchema(properties.merge(otherProperties, mergeType))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[Set[JValue]]): EnumSchema = {
    val newSchema = EnumSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }
}

/** Tracks all possible values of the enum.
  *
  * @constructor Create a new enum values property
  * @param values the values of the enum
  */
final case class EnumValuesProperty(values: Set[JValue] = Set.empty)
    extends SchemaProperty[Set[JValue]] {
  override type S = EnumValuesProperty

  override def newDefault()(implicit p: JsonoidParams): EnumValuesProperty =
    EnumValuesProperty()

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.TraversableOps"
    )
  )
  override def toJson()(implicit p: JsonoidParams): JObject = if (
    values.size == 1
  ) {
    ("const" -> values.head)
  } else {
    val sortedValues = if (values.head.isInstanceOf[JString]) {
      values.asInstanceOf[Set[JString]].toList.sortBy(_.s)
    } else {
      values
    }
    ("enum" -> sortedValues)
  }

  override def intersectMerge(
      otherProp: EnumValuesProperty
  )(implicit p: JsonoidParams): EnumValuesProperty = {
    EnumValuesProperty(values & otherProp.values)
  }

  override def unionMerge(
      otherProp: EnumValuesProperty
  )(implicit p: JsonoidParams): EnumValuesProperty = {
    EnumValuesProperty(values ++ otherProp.values)
  }

  override def mergeValue(
      value: Set[JValue]
  )(implicit p: JsonoidParams): EnumValuesProperty = {
    EnumValuesProperty(value ++ values)
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    if (values.contains(value)) {
      Seq.empty
    } else {
      Seq(Anomaly(path, "enum value not found", Fatal))
    }
  }

  override def isCompatibleWith(
      other: EnumValuesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    other.values.subsetOf(values)
  }
}
