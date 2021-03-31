package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._


object IntegerSchema {
  def apply(value: BigInt): IntegerSchema = {
    IntegerSchema(SchemaProperties(
      MinIntValueProperty(),
      MaxIntValueProperty()
    ).merge(value))
  }
}

case class IntegerSchema(override val properties: SchemaProperties[BigInt] = SchemaProperties.empty) extends JsonSchema[BigInt] {
  override val schemaType = "string"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ IntegerSchema(otherProperties) =>
      IntegerSchema(properties.merge(otherProperties))
    case other: NumberSchema => other.mergeSameType(this)
  }
}

case class MinIntValueProperty(minIntValue: Option[BigInt] = None) extends SchemaProperty[BigInt] {
  override val toJson = ("minimum" -> minIntValue)

  override def merge(otherProp: SchemaProperty[BigInt]) = {
    MinIntValueProperty(minOrNone(minIntValue, otherProp.asInstanceOf[MinIntValueProperty].minIntValue))
  }

  override def merge(value: BigInt) = {
    MinIntValueProperty(minOrNone(Some(value), minIntValue))
  }
}

case class MaxIntValueProperty(maxIntValue: Option[BigInt] = None) extends SchemaProperty[BigInt] {
  override val toJson = ("maximum" -> maxIntValue)

  override def merge(otherProp: SchemaProperty[BigInt]) = {
    MaxIntValueProperty(maxOrNone(maxIntValue, otherProp.asInstanceOf[MaxIntValueProperty].maxIntValue))
  }

  override def merge(value: BigInt) = {
    MaxIntValueProperty(maxOrNone(Some(value), maxIntValue))
  }
}
