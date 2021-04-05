package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import Scalaz._

import Helpers._

object ArraySchema {
  def apply(value: List[JsonSchema[_]]): ArraySchema = {
    ArraySchema(ArraySchema.initialProperties.merge(value))
  }

  def initialProperties: SchemaProperties[List[JsonSchema[_]]] = SchemaProperties(
    ItemTypeProperty(),
    MinArrayLengthProperty(),
    MaxArrayLengthProperty(),
  )
}

final case class ArraySchema(
    override val properties: SchemaProperties[List[JsonSchema[_]]] =
      ArraySchema.initialProperties
) extends JsonSchema[List[JsonSchema[_]]] {
  override val schemaType = "array"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ArraySchema(otherProperties) =>
      ArraySchema(properties.merge(otherProperties))
  }
}

final case class ItemTypeProperty(itemType: JsonSchema[_] = ZeroSchema())
    extends SchemaProperty[List[JsonSchema[_]]] {
  override val toJson = ("items" -> itemType.toJson)

  override def merge(otherProp: SchemaProperty[List[JsonSchema[_]]]) = {
    ItemTypeProperty(
      itemType.merge(otherProp.asInstanceOf[ItemTypeProperty].itemType)
    )
  }

  override def merge(value: List[JsonSchema[_]]) = {
    ItemTypeProperty(value.fold(itemType)(_.merge(_)))
  }
}

final case class MinArrayLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]]] {
  override val toJson = ("minLength" -> minLength)

  override def merge(otherProp: SchemaProperty[List[JsonSchema[_]]]) = {
    MinArrayLengthProperty(
      minOrNone(
        minLength,
        otherProp.asInstanceOf[MinArrayLengthProperty].minLength
      )
    )
  }

  override def merge(value: List[JsonSchema[_]]) = {
    MinArrayLengthProperty(minOrNone(Some(value.length), minLength))
  }
}

final case class MaxArrayLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]]] {
  override val toJson = ("maxLength" -> maxLength)

  override def merge(otherProp: SchemaProperty[List[JsonSchema[_]]]) = {
    MaxArrayLengthProperty(
      maxOrNone(
        maxLength,
        otherProp.asInstanceOf[MaxArrayLengthProperty].maxLength
      )
    )
  }

  override def merge(value: List[JsonSchema[_]]) = {
    MaxArrayLengthProperty(maxOrNone(Some(value.length), maxLength))
  }
}
