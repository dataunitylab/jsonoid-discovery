package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._

object ArraySchema {
  def apply(value: List[JsonSchema[_]]): ArraySchema = {
    ArraySchema(ArraySchema.initialProperties.mergeValue(value))
  }

  def initialProperties: SchemaProperties[List[JsonSchema[_]]] =
    SchemaProperties
      .empty[List[JsonSchema[_]]]
      .add(ItemTypeProperty())
      .add(MinArrayLengthProperty())
      .add(MaxArrayLengthProperty())
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

final case class ItemTypeProperty(
    itemType: Either[JsonSchema[_], List[JsonSchema[_]]] = Left(ZeroSchema())
) extends SchemaProperty[List[JsonSchema[_]], ItemTypeProperty] {
  override def toJson: JObject = ("items" -> (itemType match {
    case Left(schema)   => schema.toJson
    case Right(schemas) => JArray(schemas.map(_.toJson))
  }))

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def merge(
      otherProp: ItemTypeProperty
  ): ItemTypeProperty = {
    val newType = (itemType, otherProp.itemType) match {
      case (Right(schema1), Right(schema2)) =>
        if (schema1.length == schema2.length) {
          // Merge tuple schemas that are the same length
          Right((schema1 zip schema2).map(_.fold(_.merge(_))))
        } else {
          // Tuple schemas are different length, so convert to list
          Left((schema1 ++ schema2).fold(ZeroSchema())(_.merge(_)))
        }

      // Merge two list schemas
      case (Left(schema1), Left(schema2)) => Left(schema1.merge(schema2))

      // When merging with ZeroSchema, stay as a tuple
      case (Left(_: ZeroSchema), Right(schema2)) => Right(schema2)
      case (Right(schema1), Left(_: ZeroSchema)) => Right(schema1)

      // Otherwise, when merging a list and tuple schema, convert to list
      case (Left(schema1), Right(schema2)) =>
        Left((schema1 :: schema2).fold(ZeroSchema())(_.merge(_)))
      case (Right(schema1), Left(schema2)) =>
        Left((schema2 :: schema1).fold(ZeroSchema())(_.merge(_)))
    }
    ItemTypeProperty(newType)
  }

  override def mergeValue(value: List[JsonSchema[_]]): ItemTypeProperty = {
    merge(ItemTypeProperty(Right(value)))
  }
}

final case class MinArrayLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]], MinArrayLengthProperty] {
  override def toJson: JObject = ("minLength" -> minLength)

  override def merge(
      otherProp: MinArrayLengthProperty
  ): MinArrayLengthProperty = {
    MinArrayLengthProperty(
      minOrNone(
        minLength,
        otherProp.minLength
      )
    )
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  ): MinArrayLengthProperty = {
    MinArrayLengthProperty(minOrNone(Some(value.length), minLength))
  }
}

final case class MaxArrayLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]], MaxArrayLengthProperty] {
  override def toJson: JObject = ("maxLength" -> maxLength)

  override def merge(
      otherProp: MaxArrayLengthProperty
  ): MaxArrayLengthProperty = {
    MaxArrayLengthProperty(
      maxOrNone(
        maxLength,
        otherProp.maxLength
      )
    )
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  ): MaxArrayLengthProperty = {
    MaxArrayLengthProperty(maxOrNone(Some(value.length), maxLength))
  }
}
