package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
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
    StringHyperLogLogProperty()
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
  override def toJson: JObject = ("minLength" -> minLength)

  override def merge(otherProp: SchemaProperty[String]): MinLengthProperty = {
    MinLengthProperty(
      minOrNone(minLength, otherProp.asInstanceOf[MinLengthProperty].minLength)
    )
  }

  override def merge(value: String): MinLengthProperty = {
    MinLengthProperty(minOrNone(Some(value.length), minLength))
  }
}

final case class MaxLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override def toJson: JObject = ("maxLength" -> maxLength)

  override def merge(otherProp: SchemaProperty[String]): MaxLengthProperty = {
    MaxLengthProperty(
      maxOrNone(maxLength, otherProp.asInstanceOf[MaxLengthProperty].maxLength)
    )
  }

  override def merge(value: String): MaxLengthProperty = {
    MaxLengthProperty(maxOrNone(Some(value.length), maxLength))
  }
}

final case class StringHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[String] {
  override def toJson: JObject = ("distinctValues" -> hll.count())

  override def merge(
      otherProp: SchemaProperty[String]
  ): StringHyperLogLogProperty = {
    val prop = StringHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.asInstanceOf[StringHyperLogLogProperty].hll)

    prop
  }

  override def merge(value: String): StringHyperLogLogProperty = {
    val prop = StringHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.addString(value)

    prop
  }
}
