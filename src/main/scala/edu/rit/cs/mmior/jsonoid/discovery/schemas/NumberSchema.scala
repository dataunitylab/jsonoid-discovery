package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._


case class NumberSchema(override val properties: SchemaProperties[Double] = SchemaProperties.empty) extends JsonSchema[Double] {
  override val schemaType = "string"

  def this(value: Double) = {
    this(SchemaProperties(
      MinNumValueProperty(),
      MaxNumValueProperty()
    ).merge(value))
  }

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NumberSchema(otherProperties) =>
      NumberSchema(properties.merge(otherProperties))

    case other @ IntegerSchema(otherProperties) => {
      val newProperties = otherProperties.collect {
        case MinIntValueProperty(minValue) => MinNumValueProperty(minValue.map(_.toDouble))
        case MaxIntValueProperty(maxValue) => MaxNumValueProperty(maxValue.map(_.toDouble))
      }.toSeq

      NumberSchema(properties.merge(SchemaProperties(newProperties)))
    }
  }
}

case class MinNumValueProperty(minNumValue: Option[Double] = None) extends SchemaProperty[Double] {
  override val toJson = ("minimum" -> minNumValue)

  override def merge(otherProp: SchemaProperty[Double]) = {
    MinNumValueProperty(minOrNone(minNumValue, otherProp.asInstanceOf[MinNumValueProperty].minNumValue))
  }

  override def merge(value: Double) = {
    MinNumValueProperty(minOrNone(Some(value), minNumValue))
  }
}

case class MaxNumValueProperty(maxNumValue: Option[Double] = None) extends SchemaProperty[Double] {
  override val toJson = ("maximum" -> maxNumValue)

  override def merge(otherProp: SchemaProperty[Double]) = {
    MaxNumValueProperty(maxOrNone(maxNumValue, otherProp.asInstanceOf[MaxNumValueProperty].maxNumValue))
  }

  override def merge(value: Double) = {
    MaxNumValueProperty(maxOrNone(Some(value), maxNumValue))
  }
}
