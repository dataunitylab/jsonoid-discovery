package edu.rit.cs.mmior.jsonoid.discovery.schemas

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._


trait JsonSchema[T] {
  def toJson: JObject =
    ("type" -> schemaType) ~
    properties.map(_.toJson).foldLeft(JObject(Nil))(_.merge(_))

  def properties: SchemaProperties[T]

  def schemaType: String

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]]

  def createProduct: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other => ProductSchema().merge(this).merge(other)
  }

  def merge(other: JsonSchema[_]): JsonSchema[_] = {
    mergeSameType.orElse(createProduct).apply(other)
  }
}
