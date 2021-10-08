package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

trait JsonSchema[T] {
  def toJson: JObject = {
    val propertyJson =
      properties.map(_.toJson).foldLeft(staticProperties)(_.merge(_))
    if (hasType) {
      ("type" -> schemaType) ~ propertyJson
    } else {
      propertyJson
    }
  }

  def staticProperties: JObject = Nil

  def properties: SchemaProperties[T]

  def schemaType: String

  def hasType: Boolean = true

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]]

  def createProduct()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case other =>
    ProductSchema(this)(er).merge(other)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def merge(
      other: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    val sameType = mergeSameType()(er)
    if (sameType.isDefinedAt(other) && er.fuse(this, other)) {
      sameType(other)
    } else {
      createProduct()(er)(other)
    }
  }

  def copy(properties: SchemaProperties[T]): JsonSchema[_]

  def transformProperties(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): JsonSchema[_] = {
    copy(properties.transform(transformer))
  }

  def findByPointer(pointer: String): Option[JsonSchema[_]] = None

  def replaceWithReference(pointer: String, reference: String): JsonSchema[_] =
    this
}
