package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._


case class StringSchema(override val properties: SchemaProperties[String] = SchemaProperties.empty) extends JsonSchema[String] {
  override val schemaType = "string"

  def this(value: String) = {
    this(SchemaProperties(
      MinLengthProperty().merge(value),
      MaxLengthProperty().merge(value)
    ))
  }
}

case class MinLengthProperty(minLength: Option[Int] = None) extends SchemaProperty[String] {
  override val toJson = ("minLength" -> minLength)

  override def merge(value: String) = {
    MinLengthProperty(minOrNone(Some(value.length), minLength))
  }

  def merge(prop: MinLengthProperty) = {
    MinLengthProperty(minOrNone(prop.minLength, minLength))
  }
}

case class MaxLengthProperty(maxLength: Option[Int] = None) extends SchemaProperty[String] {
  override val toJson = ("maxLength" -> maxLength)

  override def merge(value: String) = {
    MaxLengthProperty(maxOrNone(Some(value.length), maxLength))
  }

  def merge(prop: MaxLengthProperty) = {
    MaxLengthProperty(maxOrNone(prop.maxLength, maxLength))
  }
}
