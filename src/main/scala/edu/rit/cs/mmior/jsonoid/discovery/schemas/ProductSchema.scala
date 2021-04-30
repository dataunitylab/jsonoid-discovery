package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials

import org.json4s.JsonDSL._
import org.json4s._

object ProductSchema {
  def apply(value: JsonSchema[_]): ProductSchema = {
    ProductSchema(ProductSchema.initialProperties.mergeValue(value))
  }

  def initialProperties: SchemaProperties[JsonSchema[_]] =
    SchemaProperties.empty[JsonSchema[_]].add(ProductSchemaTypesProperty())
}

final case class ProductSchema(
    override val properties: SchemaProperties[JsonSchema[_]] =
      ProductSchema.initialProperties
) extends JsonSchema[JsonSchema[_]] {
  override def hasType: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      ProductSchema(properties.merge(otherProperties))
  }

  override def merge(other: JsonSchema[_]): JsonSchema[_] = {
    other match {
      case prod: ProductSchema => this.mergeSameType(prod)
      case zero: ZeroSchema    => this
      case _                   => ProductSchema(this.properties.mergeValue(other))
    }
  }

  override def copy(
      properties: SchemaProperties[JsonSchema[_]]
  ): ProductSchema =
    ProductSchema(properties)
}

final case class ProductSchemaTypesProperty(
    val schemaTypes: Map[Class[_ <: JsonSchema[_]], JsonSchema[_]] = Map
      .empty[Class[_ <: JsonSchema[_]], JsonSchema[_]]
      .withDefaultValue(ZeroSchema())
) extends SchemaProperty[JsonSchema[_], ProductSchemaTypesProperty] {
  override def toJson: JObject = ("anyOf" -> schemaTypes.values.map(_.toJson))

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ProductSchemaTypesProperty = {
    ProductSchemaTypesProperty(
      schemaTypes
        .map { case (cls, schema) => (cls, transformer(schema)) }
        .groupBy(_._1)
        .mapValues(_.map(_._2).fold(ZeroSchema())(_.merge(_)))
        .map(identity)
        .toMap
    )
  }

  override def merge(
      otherProp: ProductSchemaTypesProperty
  ): ProductSchemaTypesProperty = {
    val merged = schemaTypes.toSeq ++ otherProp.schemaTypes.toSeq
    val grouped = merged.groupBy(_._1)
    ProductSchemaTypesProperty(
      // .map(identity) below is necessary to
      // produce a map which is serializable
      grouped
        .mapValues(_.map(_._2).fold(ZeroSchema())(_.merge(_)))
        .map(identity)
        .toMap
    )
  }

  override def mergeValue(value: JsonSchema[_]): ProductSchemaTypesProperty = {
    val newType = (value.getClass -> schemaTypes(value.getClass).merge(value))
    ProductSchemaTypesProperty(schemaTypes + newType)
  }
}
