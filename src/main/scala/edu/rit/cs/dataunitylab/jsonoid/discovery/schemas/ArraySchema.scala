package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

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
  )(implicit p: JsonoidParams): ArraySchema = {
    ArraySchema(
      p.propSet.arrayProperties.mergeValue(value)(p)
    )
  }

  def array(
      value: JsonSchema[_]
  )(implicit p: JsonoidParams): ArraySchema = {
    val newProps = apply(List(value)).properties
      .replaceProperty(ItemTypeProperty(Left(value), 1))
    ArraySchema(newProps)
  }

  def tuple(
      value: List[JsonSchema[_]]
  )(implicit p: JsonoidParams): ArraySchema = {
    val newProps =
      apply(value).properties.replaceProperty(ItemTypeProperty(Right(value), 1))
    ArraySchema(newProps)
  }

  /** Convert a serialized JSON value to an array schema object. */
  def fromJson(arr: JObject): ArraySchema = {
    implicit val formats: Formats = DefaultFormats
    val props = SchemaProperties.empty[List[JsonSchema[_]]]

    if ((arr \ "contains") =/= JNothing) {
      throw new UnsupportedOperationException("contains not supported")
    }

    if ((arr \ "minItems") =/= JNothing) {
      try {
        props.add(MinItemsProperty(Some((arr \ "minItems").extract[Int])))
      } catch {
        case e: org.json4s.MappingException =>
      }
    }

    if ((arr \ "maxItems") =/= JNothing) {
      try {
        props.add(MaxItemsProperty(Some((arr \ "maxItems").extract[Int])))
      } catch {
        case e: org.json4s.MappingException =>
      }
    }

    if ((arr \ "uniqueItems") =/= JNothing) {
      try {
        props.add(UniqueProperty((arr \ "uniqueItems").extract[Boolean], false))
      } catch {
        case e: org.json4s.MappingException =>
      }
    }

    val itemType: Either[JsonSchema[_], List[JsonSchema[_]]] =
      if ((arr \ "prefixItems") =/= JNothing) {
        if ((arr \ "items") =/= JNothing) {
          throw new UnsupportedOperationException(
            "Both items and prefixItems cannot be specified"
          )
        }

        val schemas =
          try {
            (arr \ "prefixItems").extract[List[JValue]]
          } catch {
            case e: org.json4s.MappingException => List.empty[JValue]
          }

        Right(schemas.map(s => JsonSchema.fromJson(s)))
      } else if ((arr \ "items") =/= JNothing) {
        (arr \ "items") match {
          case a: JArray =>
            Right(a.extract[List[JValue]].map(s => JsonSchema.fromJson(s)))
          case _ =>
            Left(JsonSchema.fromJson((arr \ "items").extract[JValue]))
        }
      } else if ((arr \ "additionalItems") =/= JNothing) {
        Left(JsonSchema.fromJson((arr \ "additionalItems").extract[JValue]))
      } else {
        // items and additionalItems not specified, accept anything
        Left(AnySchema())
      }

    props.add(ItemTypeProperty(itemType))

    ArraySchema(props)
  }

  lazy val AllProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())
    props.add(MinItemsProperty())
    props.add(MaxItemsProperty())
    props.add(UniqueProperty())
    props.add(ArrayLengthHistogramProperty())

    props
  }

  lazy val MinProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())

    props
  }

  lazy val SimpleProperties: SchemaProperties[List[JsonSchema[_]]] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]
    props.add(ItemTypeProperty())
    props.add(MinItemsProperty())
    props.add(MaxItemsProperty())
    props.add(UniqueProperty())

    props
  }
}

/** Represents both tuples and arrays in JSON Schema.
  */
final case class ArraySchema(
    override val properties: SchemaProperties[List[JsonSchema[_]]] =
      ArraySchema.AllProperties
) extends JsonSchema[List[JsonSchema[_]]] {
  override val schemaType = "array"

  override val validTypes: Set[Class[_]] = Set(classOf[JArray])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ArraySchema(otherProperties) =>
      ArraySchema(properties.merge(otherProperties, mergeType))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(
      properties: SchemaProperties[List[JsonSchema[_]]]
  ): ArraySchema = {
    val newSchema = ArraySchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

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

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByInexactPointer(pointer: String): Seq[JsonSchema[_]] = {
    properties.get[ItemTypeProperty].itemType match {
      // We can only follow pointers for tuple schemas, not real array schemas
      case Left(schema) =>
        schema.findByInexactPointer(pointer)
      case Right(schemas) =>
        schemas.foldLeft(Seq.empty[JsonSchema[_]]) {
          _ ++ _.findByInexactPointer(pointer)
        }
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  override def replaceWithSchema(
      pointer: String,
      replaceSchema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    val itemTypeProp = properties.get[ItemTypeProperty]
    itemTypeProp.itemType match {
      case Left(schema) =>
        // XXX The * is not real JSON Pointer syntax
        //     but allows us to work with array schemas
        // Build a new type property that replaces the required type
        val typeProp = pointer.split("/", 3) match {
          case Array(_, "*") =>
            ItemTypeProperty(Left(replaceSchema))
          case Array(_, "*", rest) =>
            ItemTypeProperty(
              Left(schema.replaceWithSchema("/" + rest, replaceSchema)),
              itemTypeProp.count
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
            schemas.updated(first.toInt, replaceSchema)
          case Array(_, first, rest) =>
            schemas.updated(
              first.toInt,
              schemas(first.toInt).replaceWithSchema("/" + rest, replaceSchema)
            )
        }

        val typeProp = ItemTypeProperty(Right(newSchemas), itemTypeProp.count)
        val newSchema = ArraySchema(this.properties.replaceProperty(typeProp))
        newSchema.definitions ++= this.definitions
        newSchema
    }
  }
}

/** The type of item stored in this array schema.
  *
  * @constructor Create a new item type property.
  * @param itemType either `Left` for a single item type or `Right` for a tuple schema with multiple types
  */
final case class ItemTypeProperty(
    itemType: Either[JsonSchema[_], List[JsonSchema[_]]] = Left(ZeroSchema()),
    count: Int = 0
) extends SchemaProperty[List[JsonSchema[_]]] {
  override type S = ItemTypeProperty

  override def newDefault()(implicit p: JsonoidParams): ItemTypeProperty =
    ItemTypeProperty()

  override def toJson()(implicit p: JsonoidParams): JObject = itemType match {
    case Left(schema) => ("items" -> schema.toJson())
    case Right(schemas) =>
      if (schemas.nonEmpty) {
        if (count > 0) {
          ("items" -> JArray(schemas.map(_.toJson()(p))))
        } else {
          val combinedSchema = schemas.fold(ZeroSchema())(_.merge(_, Union))
          ("items" -> combinedSchema.toJson())
        }
      } else {
        Nil
      }
  }

  override def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): ItemTypeProperty = {
    ItemTypeProperty(itemType match {
      case Left(singleType) =>
        Left(
          singleType.transformPropertiesWithInexactPath(transformer, true, path)
        )
      case Right(typeList) =>
        Right(typeList.zipWithIndex.map { case (schema, index) =>
          schema.transformPropertiesWithInexactPath(
            transformer,
            true,
            path
          )
        })
    })
  }

  override def intersectMerge(
      otherProp: ItemTypeProperty
  )(implicit p: JsonoidParams): ItemTypeProperty =
    merge(otherProp, Intersect)(p)

  override def unionMerge(
      otherProp: ItemTypeProperty
  )(implicit p: JsonoidParams): ItemTypeProperty =
    merge(otherProp, Union)(p)

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def merge(
      otherProp: ItemTypeProperty,
      mergeType: MergeType
  )(implicit p: JsonoidParams): ItemTypeProperty = {
    val newType: Either[JsonSchema[_], List[JsonSchema[_]]] =
      (itemType, otherProp.itemType) match {
        case (Right(schema1), Right(schema2)) =>
          if (schema1.length == schema2.length) {
            // Merge tuple schemas that are the same length
            val newSchemas: List[JsonSchema[_]] =
              (schema1 zip schema2).map(_.fold(_.merge(_, mergeType)))

            // Check that the new schemas are compatible with the old
            assert(
              (schema1 zip newSchemas).forall(
                (s: Tuple2[JsonSchema[_], JsonSchema[_]]) =>
                  s._1.isSubsetOf(s._2)
              )
            )
            assert(
              (schema2 zip newSchemas).forall(
                (s: Tuple2[JsonSchema[_], JsonSchema[_]]) =>
                  s._1.isSubsetOf(s._2)
              )
            )

            Right(newSchemas)
          } else {
            // Tuple schemas are different length, so convert to list
            Left((schema1 ++ schema2).fold(ZeroSchema())(_.merge(_, mergeType)))
          }

        // Merge two list schemas
        case (Left(schema1), Left(schema2)) =>
          val newSchema = schema1.merge(schema2, mergeType)

          // The new schema must be compatible with the originals
          assert(schema1.isSubsetOf(newSchema))
          assert(schema2.isSubsetOf(newSchema))

          Left(schema1.merge(schema2, mergeType))

        // When merging with ZeroSchema, stay as a tuple
        case (Left(_: ZeroSchema), Right(schema2)) => Right(schema2)
        case (Right(schema1), Left(_: ZeroSchema)) => Right(schema1)

        // Otherwise, when merging a list and tuple schema, convert to list
        case (Left(schema1), Right(schema2)) =>
          val newSchema =
            (schema1 :: schema2).fold(ZeroSchema())(_.merge(_, mergeType))

          // The new schema must be compatible with the originals
          assert(schema1.isSubsetOf(newSchema))
          assert(schema2.forall(_.isSubsetOf(newSchema)))

          Left(newSchema)
        case (Right(schema1), Left(schema2)) =>
          val newSchema =
            (schema2 :: schema1).fold(ZeroSchema())(_.merge(_, mergeType))

          // The new schema must be compatible with the originals
          assert(schema1.forall(_.isSubsetOf(newSchema)))
          assert(schema2.isSubsetOf(newSchema))

          Left(newSchema)
      }
    ItemTypeProperty(newType, count + otherProp.count)
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit p: JsonoidParams): ItemTypeProperty = {
    unionMerge(ItemTypeProperty(Right(value), 1))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
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
                  AnomalyLevel.Fatal
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

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def isSubsetOf(
      other: ItemTypeProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    (itemType, other.itemType) match {
      // Single types must match
      case (Left(schema1), Left(schema2)) =>
        !recursive || schema1.isSubsetOf(schema2)

      // Tuple schemas cannot be compatible with item schemas
      case (Left(_), Right(_)) => false

      case (Right(schemas), Left(schema)) => {
        val oneSchema = schemas.fold(ZeroSchema())(_.merge(_, Union))
        !recursive || oneSchema.isSubsetOf(schema)
      }

      // Corresponding types must match
      case (Right(schemas1), Right(schemas2)) =>
        schemas1.length == schemas2.length &&
          (!recursive || schemas1
            .zip(schemas2)
            .forall({ case (s1, s2) => s1.isSubsetOf(s2) }))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def expandTo(other: Option[ItemTypeProperty]): ItemTypeProperty = {
    (itemType, other.map(_.itemType)) match {
      case (Left(schema1), Some(Left(schema2))) =>
        // Expand the single item schema to match
        ItemTypeProperty(Left(schema1.expandTo(Some(schema2))))

      case (Left(schema1), None) =>
        ItemTypeProperty(Left(schema1.expandTo(None)))

      case (Right(schemas), Some(Left(schema))) =>
        // Collapse down to a single schema and expand
        val oneSchema = schemas.fold(ZeroSchema())(_.merge(_, Union))
        ItemTypeProperty(Left(oneSchema.expandTo(Some(schema))))

      case (Right(schemas), None) =>
        // Collapse down to a single schema and expand
        val oneSchema = schemas.fold(ZeroSchema())(_.merge(_, Union))
        ItemTypeProperty(Left(oneSchema.expandTo(None)))

      case (Right(schemas1), Some(Right(schemas2))) =>
        if (schemas1.length == schemas2.length) {
          // Combine corresponding tuple schemas
          val schemas =
            schemas1.zip(schemas2).map { case (s1, s2) =>
              s1.expandTo(Some(s2))
            }
          ItemTypeProperty(Right(schemas))
        } else {
          // Collapse both to a single schema and expand
          val oneSchema1 = schemas1.fold(ZeroSchema())(_.merge(_, Union))
          val oneSchema2 = schemas2.fold(ZeroSchema())(_.merge(_, Union))
          ItemTypeProperty(Left(oneSchema1.expandTo(Some(oneSchema2))))
        }

      case (Left(schema), Some(Right(schemas))) =>
        // Collapse the other side to a single schema and expand
        val oneSchema = schemas.fold(ZeroSchema())(_.merge(_, Union))
        ItemTypeProperty(Left(schema.expandTo(Some(oneSchema))))
    }
  }
}

/** Tracks the minimum number of items in the array.
  *
  * @constructor Create a new minimum items property
  * @param minItems the minimum number of items in the array
  */
final case class MinItemsProperty(minItems: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]]] {
  override type S = MinItemsProperty

  override def newDefault()(implicit p: JsonoidParams): MinItemsProperty =
    MinItemsProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("minItems" -> minItems)

  override def intersectMerge(
      otherProp: MinItemsProperty
  )(implicit p: JsonoidParams): MinItemsProperty = {
    MinItemsProperty(
      maxOrNone(
        minItems,
        otherProp.minItems
      )
    )
  }

  override def unionMerge(
      otherProp: MinItemsProperty
  )(implicit p: JsonoidParams): MinItemsProperty = {
    MinItemsProperty(
      minOrNone(
        minItems,
        otherProp.minItems
      )
    )
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit p: JsonoidParams): MinItemsProperty = {
    MinItemsProperty(minOrNone(Some(value.length), minItems))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JArray(arr) =>
        minItems match {
          case Some(items) =>
            if (arr.length < items) {
              Seq(
                Anomaly(
                  path,
                  "array smaller than minimum length",
                  AnomalyLevel.Warning
                )
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: MinItemsProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMinCoveredBy(minItems, false, other.minItems, false)
  }

  override def expandTo(other: Option[MinItemsProperty]): MinItemsProperty = {
    val newMin = maybeContractInt(
      minItems,
      other.map(_.minItems).getOrElse(None),
      false,
      other.isEmpty
    )._1
    MinItemsProperty(newMin)
  }
}

/*
 * Tracks the maximum number of items in the array.
 *
 * @constructor Create a new maximum items property
 * @param maxItems the maximum number of items in the array
 */
final case class MaxItemsProperty(maxItems: Option[Int] = None)
    extends SchemaProperty[List[JsonSchema[_]]] {
  override type S = MaxItemsProperty

  override def newDefault()(implicit p: JsonoidParams): MaxItemsProperty =
    MaxItemsProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("maxItems" -> maxItems)

  override def intersectMerge(
      otherProp: MaxItemsProperty
  )(implicit p: JsonoidParams): MaxItemsProperty = {
    MaxItemsProperty(
      minOrNone(
        maxItems,
        otherProp.maxItems
      )
    )
  }

  override def unionMerge(
      otherProp: MaxItemsProperty
  )(implicit p: JsonoidParams): MaxItemsProperty = {
    MaxItemsProperty(
      maxOrNone(
        maxItems,
        otherProp.maxItems
      )
    )
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit p: JsonoidParams): MaxItemsProperty = {
    MaxItemsProperty(maxOrNone(Some(value.length), maxItems))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JArray(arr) =>
        maxItems match {
          case Some(items) =>
            if (arr.length > items) {
              Seq(
                Anomaly(
                  path,
                  "array larger than maximum length",
                  AnomalyLevel.Warning
                )
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: MaxItemsProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMaxCoveredBy(maxItems, false, other.maxItems, false)
  }

  override def expandTo(other: Option[MaxItemsProperty]): MaxItemsProperty = {
    val newMax = maybeExpandInt(
      maxItems.map(BigInt(_)),
      other.flatMap(_.maxItems.map(BigInt(_))),
      false,
      other.isEmpty
    )._1
    MaxItemsProperty(newMax.map(_.toInt))
  }
}

/*
 * Tracks whether array items are unique.
 *
 * @constructor Create a new unique items property
 * @param unique whether all observed items are unique
 * @param unary whether only one item has been observed
 */
final case class UniqueProperty(unique: Boolean = true, unary: Boolean = true)
    extends SchemaProperty[List[JsonSchema[_]]] {
  override type S = UniqueProperty

  override def newDefault()(implicit p: JsonoidParams): UniqueProperty =
    UniqueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject = if (
    unique && !unary
  ) {
    ("uniqueItems" -> true)
  } else {
    // Since we only check uniqueness for primitive types, we may
    // have some false negatives, so we omit the property here
    Nil
  }

  override def intersectMerge(
      otherProp: UniqueProperty
  )(implicit p: JsonoidParams): UniqueProperty = {
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
  )(implicit p: JsonoidParams): UniqueProperty = {
    UniqueProperty(unique && otherProp.unique, unary && otherProp.unary)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit p: JsonoidParams): UniqueProperty = {
    // Use the examples property to check uniqueness
    val examples: List[_] = value.fold(ZeroSchema())(_.merge(_)) match {
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
  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JArray(arr) =>
        if (unique && !unary && (arr.toSet.size != arr.length))
          Seq(Anomaly(path, "array items are not unique", AnomalyLevel.Fatal))
        else
          Seq.empty
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: UniqueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // It's ok if the other schema is unique as long as we are not
    (other.unique && !other.unary) >= (unique && !unary)
  }

  override def expandTo(other: Option[UniqueProperty]): UniqueProperty = {
    // If no other property is specified, assume uniqueness
    (
      unique,
      unary,
      other.map(_.unique).getOrElse(false),
      other.map(_.unary).getOrElse(false)
    ) match {
      // If we are unary, stay that way, since we haven't confirmed uniqueness
      case (_, true, _, _) => this

      // If not unique, no need to expand
      case (false, _, _, _) => this

      // If the other schema is unique, whether we are unique does not matter
      case (_, _, true, _) => this

      // If we are unique and the other is not, expand
      case _ => UniqueProperty(false, unary)
    }
  }
}

/*
 * Tracks a histogram of array lengths.
 *
 * @constructor Create a new length histogram property
 * @param histogram the initial histogram of lengths
 */
final case class ArrayLengthHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[List[JsonSchema[_]]] {
  override type S = ArrayLengthHistogramProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): ArrayLengthHistogramProperty =
    ArrayLengthHistogramProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("lengthHistogram" -> histogram.toJson)
  }

  override def unionMerge(
      otherProp: ArrayLengthHistogramProperty
  )(implicit p: JsonoidParams): ArrayLengthHistogramProperty = {
    ArrayLengthHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: List[JsonSchema[_]]
  )(implicit p: JsonoidParams): ArrayLengthHistogramProperty = {
    ArrayLengthHistogramProperty(
      histogram.merge(value.length)
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JArray(arr) =>
        if (histogram.isAnomalous(arr.length)) {
          Seq(
            Anomaly(
              path,
              "array length outside histogram bounds",
              AnomalyLevel.Info
            )
          )
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}
