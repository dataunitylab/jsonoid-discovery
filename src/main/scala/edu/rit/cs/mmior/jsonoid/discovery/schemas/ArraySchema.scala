package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials
import scala.reflect._

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.Histogram

object ArraySchema {
  def apply(
      value: List[JsonSchema[_]]
  )(implicit propSet: PropertySet): ArraySchema = {
    ArraySchema(
      propSet.arrayProperties.mergeValue(value)(
        EquivalenceRelations.KindEquivalenceRelation
      )
    )
  }

  val AllProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())
    props.add(MinItemsProperty())
    props.add(MaxItemsProperty())
    props.add(UniqueProperty())
    props.add(ArrayLengthHistogramProperty())

    props
  }

  val MinProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())

    props
  }

  val SimpleProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())
    props.add(MinItemsProperty())
    props.add(MaxItemsProperty())

    props
  }
}

final case class ArraySchema(
    override val properties: SchemaProperties[List[JsonSchema[_]]] =
      ArraySchema.AllProperties
) extends JsonSchema[List[JsonSchema[_]]] {
  override val schemaType = "array"

  override val validTypes: Set[ClassTag[_ <: JValue]] = Set(classTag[JArray])

  override def mergeSameType(mergeType: MergeType)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ArraySchema(otherProperties) =>
      ArraySchema(properties.merge(otherProperties, mergeType))
  }

  override def copy(
      properties: SchemaProperties[List[JsonSchema[_]]]
  ): ArraySchema =
    ArraySchema(properties)

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: String): Option[JsonSchema[_]] = {
    properties.get[ItemTypeProperty].itemType match {
      // We can only follow pointers for tuple schemas, not real array schemas
      case Left(schema) =>
        // XXX The * is not real JSON Pointer syntax
        //     but allows us to work with array schemas
        pointer.split("/", 3) match {
          case Array(_, "")        => Some(this)
          case Array(_, "*")       => Some(schema)
          case Array(_, "*", rest) => schema.findByPointer("/" + rest)
          case _                   => None
        }
      case Right(schemas) =>
        pointer.split("/", 3) match {
          case Array(_)        => None
          case Array(_, "")    => Some(this)
          case Array(_, first) => Some(schemas(first.toInt))
          case Array(_, first, rest) =>
            schemas(first.toInt).findByPointer("/" + rest)
        }
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  override def replaceWithReference(
      pointer: String,
      reference: String,
      obj: Option[JsonSchema[_]]
  ): JsonSchema[_] = {
    val itemTypes = properties.get[ItemTypeProperty].itemType
    itemTypes match {
      case Left(schema) =>
        // XXX The * is not real JSON Pointer syntax
        //     but allows us to work with array schemas
        // Build a new type property that replaces the required type
        val typeProp = pointer.split("/", 3) match {
          case Array(_, "*") =>
            ItemTypeProperty(Left(ReferenceSchema(reference, obj)))
          case Array(_, "*", rest) =>
            ItemTypeProperty(
              Left(schema.replaceWithReference("/" + rest, reference, obj))
            )
          case _ =>
            throw new IllegalArgumentException("Invalid path for reference")
        }

        ArraySchema(this.properties.replaceProperty(typeProp))
      case Right(schemas) =>
        // Build a new type list that replaces the required type
        val newSchemas = pointer.split("/", 3) match {
          case Array(_) =>
            throw new IllegalArgumentException("Invalid path for reference")
          case Array(_, "") =>
            throw new IllegalArgumentException("Invalid path for reference")
          case Array(_, first) =>
            schemas.updated(first.toInt, ReferenceSchema(reference, obj))
          case Array(_, first, rest) =>
            schemas.updated(
              first.toInt,
              schemas(first.toInt)
                .replaceWithReference("/" + rest, reference, obj)
            )
        }

        val typeProp = ItemTypeProperty(Right(newSchemas))
        val newSchema = ArraySchema(this.properties.replaceProperty(typeProp))
        newSchema.definitions ++= this.definitions
        newSchema
    }
  }
}

final case class ItemTypeProperty(
    itemType: Either[JsonSchema[_], List[JsonSchema[_]]] = Left(ZeroSchema())
) extends SchemaProperty[List[JsonSchema[_]], ItemTypeProperty] {
  override def toJson: JObject = itemType match {
    case Left(schema)   => ("items" -> schema.toJson)
    case Right(schemas) => ("items" -> JArray(schemas.map(_.toJson)))
  }

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ItemTypeProperty = {
    ItemTypeProperty(itemType match {
      case Left(singleType) => Left(transformer(singleType))
      case Right(typeList)  => Right(typeList.map(transformer(_)))
    })
  }

  override def intersectMerge(
      otherProp: ItemTypeProperty
  )(implicit er: EquivalenceRelation): ItemTypeProperty =
    merge(otherProp, Intersect)(er)

  override def unionMerge(
      otherProp: ItemTypeProperty
  )(implicit er: EquivalenceRelation): ItemTypeProperty =
    merge(otherProp, Union)(er)

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def merge(
      otherProp: ItemTypeProperty,
      mergeType: MergeType
  )(implicit er: EquivalenceRelation): ItemTypeProperty = {
    val newType = (itemType, otherProp.itemType) match {
      case (Right(schema1), Right(schema2)) =>
        if (schema1.length == schema2.length) {
          // Merge tuple schemas that are the same length
          Right((schema1 zip schema2).map(_.fold(_.merge(_, mergeType))))
        } else {
          // Tuple schemas are different length, so convert to list
          Left((schema1 ++ schema2).fold(ZeroSchema())(_.merge(_, mergeType)))
        }

      // Merge two list schemas
      case (Left(schema1), Left(schema2)) =>
        Left(schema1.merge(schema2, mergeType))

      // When merging with ZeroSchema, stay as a tuple
      case (Left(_: ZeroSchema), Right(schema2)) => Right(schema2)
      case (Right(schema1), Left(_: ZeroSchema)) => Right(schema1)

      // Otherwise, when merging a list and tuple schema, convert to list
      case (Left(schema1), Right(schema2)) =>
        Left((schema1 :: schema2).fold(ZeroSchema())(_.merge(_, mergeType)))
      case (Right(schema1), Left(schema2)) =>
        Left((schema2 :: schema1).fold(ZeroSchema())(_.merge(_, mergeType)))
    }
    ItemTypeProperty(newType)
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit er: EquivalenceRelation): ItemTypeProperty = {
    unionMerge(ItemTypeProperty(Right(value)))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def collectAnomalies(value: JValue, path: String) = {
    value match {
      case JArray(arr) =>
        itemType match {
          case Left(singleType) =>
            arr.zipWithIndex.flatMap { case (schema, index) =>
              singleType.collectAnomalies(schema, f"${path}[$index]")
            }

          case Right(typeList) =>
            if (arr.length != typeList.length) {
              Seq(
                Anomaly(
                  path,
                  "wrong length for tuple schema",
                  Fatal
                )
              )
            } else {
              typeList.zip(arr).zipWithIndex.flatMap {
                case ((schema, arrayValue), index) =>
                  schema.collectAnomalies(arrayValue, f"${path}[$index]")
              }
            }
        }
      case _ => Seq.empty
    }
  }
}

final case class MinItemsProperty(minItems: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]], MinItemsProperty] {
  override def toJson: JObject = ("minItems" -> minItems)

  override def intersectMerge(
      otherProp: MinItemsProperty
  )(implicit er: EquivalenceRelation): MinItemsProperty = {
    MinItemsProperty(
      maxOrNone(
        minItems,
        otherProp.minItems
      )
    )
  }

  override def unionMerge(
      otherProp: MinItemsProperty
  )(implicit er: EquivalenceRelation): MinItemsProperty = {
    MinItemsProperty(
      minOrNone(
        minItems,
        otherProp.minItems
      )
    )
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit er: EquivalenceRelation): MinItemsProperty = {
    MinItemsProperty(minOrNone(Some(value.length), minItems))
  }

  override def collectAnomalies(value: JValue, path: String) = {
    value match {
      case JArray(arr) =>
        minItems match {
          case Some(items) =>
            if (arr.length < items) {
              Seq(
                Anomaly(path, "array smaller than minimum length", Warning)
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

final case class MaxItemsProperty(maxItems: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]], MaxItemsProperty] {
  override def toJson: JObject = ("maxItems" -> maxItems)

  override def intersectMerge(
      otherProp: MaxItemsProperty
  )(implicit er: EquivalenceRelation): MaxItemsProperty = {
    MaxItemsProperty(
      minOrNone(
        maxItems,
        otherProp.maxItems
      )
    )
  }

  override def unionMerge(
      otherProp: MaxItemsProperty
  )(implicit er: EquivalenceRelation): MaxItemsProperty = {
    MaxItemsProperty(
      maxOrNone(
        maxItems,
        otherProp.maxItems
      )
    )
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit er: EquivalenceRelation): MaxItemsProperty = {
    MaxItemsProperty(maxOrNone(Some(value.length), maxItems))
  }

  override def collectAnomalies(value: JValue, path: String) = {
    value match {
      case JArray(arr) =>
        maxItems match {
          case Some(items) =>
            if (arr.length > items) {
              Seq(
                Anomaly(path, "array larger than maximum length", Warning)
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
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

  override def intersectMerge(
      otherProp: UniqueProperty
  )(implicit er: EquivalenceRelation): UniqueProperty = {
    val unique = this.unique || otherProp.unique
    UniqueProperty(
      unique,
      (this.unique && this.unary) ||
        (otherProp.unique && otherProp.unary) ||
        (!unique && (this.unary || otherProp.unary))
    )
  }

  override def unionMerge(
      otherProp: UniqueProperty
  )(implicit er: EquivalenceRelation): UniqueProperty = {
    UniqueProperty(unique && otherProp.unique, unary && otherProp.unary)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit er: EquivalenceRelation): UniqueProperty = {
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

    unionMerge(
      UniqueProperty(examples.length == value.length, value.length <= 1)
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def collectAnomalies(value: JValue, path: String) = {
    value match {
      case JArray(arr) =>
        if (
          unique && !unary && (arr.toSet.size !=
            arr.length)
        ) {
          Seq(Anomaly(path, "array items are not unique", Fatal))
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

final case class ArrayLengthHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[List[JsonSchema[_]], ArrayLengthHistogramProperty] {
  override def toJson: JObject = {
    ("lengthHistogram" -> histogram.bins.map { case (value, count) =>
      List(value.doubleValue, count.longValue)
    })
  }

  override def unionMerge(
      otherProp: ArrayLengthHistogramProperty
  )(implicit er: EquivalenceRelation): ArrayLengthHistogramProperty = {
    ArrayLengthHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit er: EquivalenceRelation): ArrayLengthHistogramProperty = {
    ArrayLengthHistogramProperty(
      histogram.merge(Histogram(List((value.length, 1))))
    )
  }

  override def collectAnomalies(value: JValue, path: String) = {
    value match {
      case JArray(arr) =>
        if (histogram.isAnomalous(arr.length)) {
          Seq(
            Anomaly(path, "array length outside histogram bounds", Warning)
          )
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}
