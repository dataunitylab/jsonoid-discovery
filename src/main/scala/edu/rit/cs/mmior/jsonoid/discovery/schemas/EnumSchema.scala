package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

object EnumSchema {
  def apply(value: Set[JValue]): EnumSchema = {
    EnumSchema(
      EnumSchema.AllProperties.mergeValue(value)(
        EquivalenceRelations.KindEquivalenceRelation
      )
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

final case class EnumSchema(
    override val properties: SchemaProperties[Set[JValue]] =
      EnumSchema.AllProperties
) extends JsonSchema[Set[JValue]] {
  override def hasType: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ EnumSchema(otherProperties) =>
      EnumSchema(properties.merge(otherProperties))
  }

  override def copy(properties: SchemaProperties[Set[JValue]]): EnumSchema =
    EnumSchema(properties)
}

final case class EnumValuesProperty(values: Set[JValue] = Set.empty)
    extends SchemaProperty[Set[JValue], EnumValuesProperty] {
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.TraversableOps"
    )
  )
  override def toJson: JObject = if (values.size == 1) {
    ("const" -> values.head)
  } else {
    val sortedValues = if (values.head.isInstanceOf[JString]) {
      values.asInstanceOf[Set[JString]].toList.sortBy(_.s)
    } else {
      values
    }
    ("enum" -> sortedValues)
  }

  override def merge(
      otherProp: EnumValuesProperty
  )(implicit er: EquivalenceRelation): EnumValuesProperty = {
    EnumValuesProperty(values ++ otherProp.values)
  }

  override def mergeValue(
      value: Set[JValue]
  )(implicit er: EquivalenceRelation): EnumValuesProperty = {
    EnumValuesProperty(value ++ values)
  }
}
