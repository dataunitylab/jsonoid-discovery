package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._

import Helpers._

object ObjectSchema {
  def apply(value: Map[String, JsonSchema[_]]): ObjectSchema = {
    ObjectSchema(ObjectSchema.initialProperties.merge(value))
  }

  def initialProperties: SchemaProperties[Map[String, JsonSchema[_]]] =
    SchemaProperties(
      ObjectTypesProperty(),
      RequiredProperty(),
    )
}

final case class ObjectSchema(
    override val properties: SchemaProperties[Map[String, JsonSchema[_]]] =
      ObjectSchema.initialProperties
) extends JsonSchema[Map[String, JsonSchema[_]]] {
  override val schemaType = "object"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ObjectSchema(otherProperties) =>
      ObjectSchema(properties.merge(otherProperties))
  }
}

final case class ObjectTypesProperty(
    objectTypes: Map[String, JsonSchema[_]] = Map.empty[String, JsonSchema[_]]
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override val toJson = ("properties" -> objectTypes.map {
    case (propType, schema) => (propType -> schema.toJson)
  }) ~ ("additionalProperties" -> false)

  override def merge(otherProp: SchemaProperty[Map[String, JsonSchema[_]]]) = {
    val other = otherProp.asInstanceOf[ObjectTypesProperty].objectTypes
    this.merge(other)
  }

  override def merge(value: Map[String, JsonSchema[_]]) = {
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
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override val toJson = ("required" -> required)

  override def merge(otherProp: SchemaProperty[Map[String, JsonSchema[_]]]) = {
    val other = otherProp.asInstanceOf[RequiredProperty].required
    RequiredProperty(intersectOrNone(other, required))
  }

  override def merge(value: Map[String, JsonSchema[_]]) = {
    RequiredProperty(intersectOrNone(Some(value.keySet), required))
  }
}
