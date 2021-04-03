package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials

import org.json4s.JsonDSL._

final case class ProductSchema(
    override val properties: SchemaProperties[JsonSchema[_]] =
      SchemaProperties.empty
) extends JsonSchema[JsonSchema[_]] {
  override val schemaType = "anyOf"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      ProductSchema(properties.merge(otherProperties))
  }
}

final case class ProductSchemaTypesProperty(
    val schemaTypes: Map[Class[_ <: JsonSchema[_]], JsonSchema[_]] = Map
      .empty[Class[_ <: JsonSchema[_]], JsonSchema[_]]
      .withDefaultValue(ZeroSchema())
) extends SchemaProperty[JsonSchema[_]] {
  override val toJson = ("TODO" -> 0)

  override def merge(otherProp: SchemaProperty[JsonSchema[_]]) = {
    val merged = schemaTypes.toSeq ++ otherProp
      .asInstanceOf[ProductSchemaTypesProperty]
      .schemaTypes
      .toSeq
    val grouped = merged.groupBy(_._1)
    ProductSchemaTypesProperty(
      grouped.view.mapValues(_.map(_._2).fold(ZeroSchema())(_.merge(_))).toMap
    )
  }

  override def merge(value: JsonSchema[_]) = {
    val newType = (value.getClass -> schemaTypes(value.getClass).merge(value))
    ProductSchemaTypesProperty(schemaTypes + newType)
  }
}
