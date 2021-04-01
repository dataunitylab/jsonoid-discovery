package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import Scalaz._

import Helpers._

object StringSchema {
  def apply(value: String): StringSchema = {
    StringSchema(
      SchemaProperties(
        MinLengthProperty(),
        MaxLengthProperty()
      ).merge(value)
    )
  }
}

case class StringSchema(
    override val properties: SchemaProperties[String] = SchemaProperties.empty
) extends JsonSchema[String] {
  override val schemaType = "string"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ StringSchema(otherProperties) =>
      StringSchema(properties.merge(otherProperties))
  }
}

case class MinLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override val toJson = ("minLength" -> minLength)

  override def merge(otherProp: SchemaProperty[String]) = {
    MinLengthProperty(
      minOrNone(minLength, otherProp.asInstanceOf[MinLengthProperty].minLength)
    )
  }

  override def merge(value: String) = {
    MinLengthProperty(minOrNone(Some(value.length), minLength))
  }
}

case class MaxLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override val toJson = ("maxLength" -> maxLength)

  override def merge(otherProp: SchemaProperty[String]) = {
    MaxLengthProperty(
      maxOrNone(maxLength, otherProp.asInstanceOf[MaxLengthProperty].maxLength)
    )
  }

  override def merge(value: String) = {
    MaxLengthProperty(maxOrNone(Some(value.length), maxLength))
  }
}
