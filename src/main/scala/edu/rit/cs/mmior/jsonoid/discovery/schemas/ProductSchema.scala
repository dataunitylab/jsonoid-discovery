package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials

import org.json4s.JsonDSL._

object ProductSchema {
  def apply(value: JsonSchema[_]): ProductSchema = {
    ProductSchema(ProductSchema.initialProperties.merge(value))
  }

  def initialProperties: SchemaProperties[JsonSchema[_]] = SchemaProperties(
    ProductSchemaTypesProperty(),
  )
}

final case class ProductSchema(
    override val properties: SchemaProperties[JsonSchema[_]] =
      ProductSchema.initialProperties
) extends JsonSchema[JsonSchema[_]] {
  override def hasType: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType = null

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      ProductSchema(properties.merge(otherProperties))
  }

  override def merge(other: JsonSchema[_]): JsonSchema[_] = {
    other match {
      case prod: ProductSchema => this.mergeSameType(prod)
      case zero: ZeroSchema    => this
      case _                   => ProductSchema(this.properties.merge(other))
    }
  }
}

final case class ProductSchemaTypesProperty(
    val schemaTypes: Map[Class[_ <: JsonSchema[_]], JsonSchema[_]] = Map
      .empty[Class[_ <: JsonSchema[_]], JsonSchema[_]]
      .withDefaultValue(ZeroSchema())
) extends SchemaProperty[JsonSchema[_]] {
  override val toJson = ("anyOf" -> schemaTypes.values.map(_.toJson))

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
