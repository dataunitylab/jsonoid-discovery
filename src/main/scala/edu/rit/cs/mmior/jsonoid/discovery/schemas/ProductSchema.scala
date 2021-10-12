package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object ProductSchema {
  def apply(
      value: JsonSchema[_]
  )(implicit er: EquivalenceRelation): ProductSchema = {
    ProductSchema(
      SchemaProperties
        .empty[JsonSchema[_]]
        .replaceProperty(ProductSchemaTypesProperty(List(value))(er))
    )
  }
}

final case class ProductSchema(
    override val properties: SchemaProperties[JsonSchema[_]]
)(implicit er: EquivalenceRelation)
    extends JsonSchema[JsonSchema[_]] {
  override def hasType: Boolean = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      ProductSchema(properties.merge(otherProperties))
  }

  override def merge(
      other: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    other match {
      case prod: ProductSchema => this.mergeSameType()(er)(prod)
      case zero: ZeroSchema    => this
      case _                   => ProductSchema(this.properties.mergeValue(other))
    }
  }

  override def copy(
      properties: SchemaProperties[JsonSchema[_]]
  ): ProductSchema =
    ProductSchema(properties)

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: String): Option[JsonSchema[_]] = {
    val schemas = properties.get[ProductSchemaTypesProperty].schemaTypes
    pointer.split("/", 3) match {
      case Array(_)        => None
      case Array(_, "")    => Some(this)
      case Array(_, first) => Some(schemas(first.toInt))
      case Array(_, first, rest) =>
        schemas(first.toInt).findByPointer("/" + rest)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def replaceWithReference(
      pointer: String,
      reference: String
  ): JsonSchema[_] = {
    val schemas = properties.get[ProductSchemaTypesProperty].schemaTypes
    // Build a new type list that replaces the required type
    val newSchemas = pointer.split("/", 3) match {
      case Array(_) =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, "") =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, first) =>
        schemas.updated(first.toInt, ReferenceSchema(reference))
      case Array(_, first, rest) =>
        schemas.updated(
          first.toInt,
          schemas(first.toInt).replaceWithReference("/" + rest, reference)
        )
    }

    val typeProp = ProductSchemaTypesProperty(newSchemas)
    ProductSchema(this.properties.replaceProperty(typeProp))
  }
}

final case class ProductSchemaTypesProperty(
    val schemaTypes: List[JsonSchema[_]] = List.empty[JsonSchema[_]]
)(implicit er: EquivalenceRelation)
    extends SchemaProperty[JsonSchema[_], ProductSchemaTypesProperty] {
  override def toJson: JObject = ("anyOf" -> schemaTypes.map(_.toJson))

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ProductSchemaTypesProperty = {
    ProductSchemaTypesProperty(schemaTypes.map { s =>
      if (transformer.isDefinedAt(s)) {
        transformer(s)
      } else {
        s
      }
    })
  }

  override def merge(
      otherProp: ProductSchemaTypesProperty
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    otherProp.schemaTypes.foldLeft(this)(_.mergeValue(_))
  }

  override def mergeValue(
      value: JsonSchema[_]
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    val newTypes = schemaTypes.zipWithIndex.find { case (s, i) =>
      s.schemaType === value.schemaType
    } match {
      case Some((s, i)) if er.fuse(s, value) =>
        schemaTypes.updated(i, s.merge(value))
      case _ => schemaTypes :+ value
    }
    ProductSchemaTypesProperty(newTypes)
  }
}
