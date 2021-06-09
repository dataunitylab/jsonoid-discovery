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

  def initialProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())
    props.add(MinArrayLengthProperty())
    props.add(MaxArrayLengthProperty())
    props.add(UniqueProperty())

    props
  }
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

  override def copy(
      properties: SchemaProperties[List[JsonSchema[_]]]
  ): ArraySchema =
    ArraySchema(properties)

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: String): Option[JsonSchema[_]] = {
    properties.get[ItemTypeProperty].itemType match {
      // We can only follow pointers for tuple schemas, not real array schemas
      case Left(_)        => None
      case Right(schemas) =>
        pointer.split("/", 3) match {
          case Array(_)              => None
          case Array(_, "")          => Some(this)
          case Array(_, first)       => Some(schemas(first.toInt))
          case Array(_, first, rest) =>
            schemas(first.toInt).findByPointer("/" + rest)
        }
    }
  }
}

final case class ItemTypeProperty(
    itemType: Either[JsonSchema[_], List[JsonSchema[_]]] = Left(ZeroSchema())
) extends SchemaProperty[List[JsonSchema[_]], ItemTypeProperty] {
  override def toJson: JObject = ("items" -> (itemType match {
    case Left(schema)   => schema.toJson
    case Right(schemas) => JArray(schemas.map(_.toJson))
  }))

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ItemTypeProperty = {
    ItemTypeProperty(itemType match {
      case Left(singleType) => Left(transformer(singleType))
      case Right(typeList)  => Right(typeList.map(transformer(_)))
    })
  }

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

final case class UniqueProperty(unique: Boolean = true, unary: Boolean = true)
    extends SchemaProperty[List[JsonSchema[_]], UniqueProperty] {
  override def toJson: JObject = if (unique && !unary) {
    ("uniqueItems" -> true)
  } else {
    // Since we only check uniqueness for primitive types, we may
    // have some false negatives, so we omit the property here
    Nil
  }

  override def merge(
      otherProp: UniqueProperty
  ): UniqueProperty = {
    UniqueProperty(unique && otherProp.unique, unary && otherProp.unary)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def mergeValue(
      value: List[JsonSchema[_]]
  ): UniqueProperty = {
    // Use the examples property to check uniqueness
    val examples = value.fold(ZeroSchema())(_.merge(_)) match {
      case IntegerSchema(props) =>
        props.get[IntExamplesProperty].examples.examples
      case NumberSchema(props) =>
        props.get[NumExamplesProperty].examples.examples
      case StringSchema(props) =>
        props.get[StringExamplesProperty].examples.examples
      case _ => List()
    }

    merge(UniqueProperty(examples.length == value.length, value.length <= 1))
  }
}
