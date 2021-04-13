package edu.rit.cs.mmior.jsonoid.discovery.schemas

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

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]]

  def createProduct: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other => ProductSchema().merge(this).merge(other)
  }

  def merge(other: JsonSchema[_]): JsonSchema[_] = {
    mergeSameType.orElse(createProduct).apply(other)
  }
}
