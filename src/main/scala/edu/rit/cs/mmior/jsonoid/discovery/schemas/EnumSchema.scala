package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

object EnumSchema {
  def apply(value: List[JValue]): EnumSchema = {
    EnumSchema(EnumSchema.initialProperties.mergeValue(value))
  }

  def initialProperties: SchemaProperties[List[JValue]] =
    SchemaProperties
      .empty[List[JValue]]
      .add(EnumValuesProperty())
}

final case class EnumSchema(
    override val properties: SchemaProperties[List[JValue]] =
      SchemaProperties.empty[List[JValue]]
) extends JsonSchema[List[JValue]] {
  override def hasType: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ EnumSchema(otherProperties) =>
      EnumSchema(properties.merge(otherProperties))
  }

  override def copy(properties: SchemaProperties[List[JValue]]): EnumSchema =
    EnumSchema(properties)
}

final case class EnumValuesProperty(values: List[JValue] = List.empty)
    extends SchemaProperty[List[JValue], EnumValuesProperty] {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def toJson: JObject = if (values.length == 1) {
    ("const" -> values(0))
  } else {
    ("enum" -> values)
  }

  override def merge(otherProp: EnumValuesProperty): EnumValuesProperty = {
    EnumValuesProperty(values ++ otherProp.values)
  }

  override def mergeValue(value: List[JValue]): EnumValuesProperty = {
    EnumValuesProperty(value ++ values)
  }
}
