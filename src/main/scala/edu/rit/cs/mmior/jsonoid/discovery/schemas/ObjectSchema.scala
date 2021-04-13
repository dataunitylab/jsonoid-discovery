package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object ObjectSchema {
  def apply(value: Map[String, JsonSchema[_]]): ObjectSchema = {
    ObjectSchema(ObjectSchema.initialProperties.mergeValue(value))
  }

  def initialProperties: SchemaProperties[Map[String, JsonSchema[_]]] =
    SchemaProperties
      .empty[Map[String, JsonSchema[_]]]
      .add(ObjectTypesProperty())
      .add(RequiredProperty())
}

final case class ObjectSchema(
    override val properties: SchemaProperties[Map[String, JsonSchema[_]]] =
      ObjectSchema.initialProperties
) extends JsonSchema[Map[String, JsonSchema[_]]] {
  override val schemaType = "object"

  override val staticProperties: JObject = ("additionalProperties" -> false)

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ObjectSchema(otherProperties) =>
      ObjectSchema(properties.merge(otherProperties))
  }
}

final case class ObjectTypesProperty(
    objectTypes: Map[String, JsonSchema[_]] = Map.empty[String, JsonSchema[_]]
) extends SchemaProperty[Map[String, JsonSchema[_]], ObjectTypesProperty] {
  override def toJson: JObject = ("properties" -> objectTypes.map {
    case (propType, schema) => (propType -> schema.toJson)
  }) ~ ("additionalProperties" -> false)

  override def merge(
      otherProp: ObjectTypesProperty
  ): ObjectTypesProperty = {
    val other = otherProp.objectTypes
    this.mergeValue(other)
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  ): ObjectTypesProperty = {
    val merged = objectTypes.toSeq ++ value.toSeq
    val grouped = merged.groupBy(_._1)
    ObjectTypesProperty(
      // .map(identity) below is necessary to
      // produce a map which is serializable
      grouped
        .mapValues(_.map(_._2).fold(ZeroSchema())(_.merge(_)))
        .map(identity)
        .toMap
    )
  }
}

final case class RequiredProperty(
    required: Option[Set[String]] = None
) extends SchemaProperty[Map[String, JsonSchema[_]], RequiredProperty] {
  override def toJson: JObject = ("required" -> required)

  override def merge(
      otherProp: RequiredProperty
  ): RequiredProperty = {
    val other = otherProp.required
    RequiredProperty(intersectOrNone(other, required))
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  ): RequiredProperty = {
    RequiredProperty(intersectOrNone(Some(value.keySet), required))
  }
}
