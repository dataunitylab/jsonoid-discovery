package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import Scalaz._

import Helpers._
import utils.HyperLogLog

object StringSchema {
  def apply(value: String): StringSchema = {
    StringSchema(StringSchema.initialProperties.merge(value))
  }

  def initialProperties: SchemaProperties[String] = SchemaProperties(
    MinLengthProperty(),
    MaxLengthProperty(),
    HyperLogLogProperty(),
  )
}

final case class StringSchema(
    override val properties: SchemaProperties[String] =
      StringSchema.initialProperties
) extends JsonSchema[String] {
  override val schemaType = "string"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ StringSchema(otherProperties) =>
      StringSchema(properties.merge(otherProperties))
  }
}

final case class MinLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override def toJson = ("minLength" -> minLength)

  override def merge(otherProp: SchemaProperty[String]) = {
    MinLengthProperty(
      minOrNone(minLength, otherProp.asInstanceOf[MinLengthProperty].minLength)
    )
  }

  override def merge(value: String) = {
    MinLengthProperty(minOrNone(Some(value.length), minLength))
  }
}

final case class MaxLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override def toJson = ("maxLength" -> maxLength)

  override def merge(otherProp: SchemaProperty[String]) = {
    MaxLengthProperty(
      maxOrNone(maxLength, otherProp.asInstanceOf[MaxLengthProperty].maxLength)
    )
  }

  override def merge(value: String) = {
    MaxLengthProperty(maxOrNone(Some(value.length), maxLength))
  }
}

final case class HyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[String] {
  override def toJson = ("distinctValues" -> hll.count())

  override def merge(otherProp: SchemaProperty[String]) = {
    val prop = HyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.asInstanceOf[HyperLogLogProperty].hll)

    prop
  }

  override def merge(value: String) = {
    val prop = HyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.addString(value)

    prop
  }
}
