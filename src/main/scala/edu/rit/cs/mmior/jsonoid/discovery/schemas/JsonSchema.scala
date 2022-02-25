package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.collection.mutable
import scala.reflect._
import scala.util.matching.Regex

import org.json4s.JsonDSL._
import org.json4s._

object JsonSchema {
  implicit val formats: Formats = DefaultFormats

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fromJson(schema: JValue): JsonSchema[_] = {
    schema match {
      case JBool(true)  => AnySchema()
      case JBool(false) => ZeroSchema()
      case o: JObject   => fromJsonObjectValue(o)
      case _ =>
        throw new UnsupportedOperationException("invalid schema element")
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Recursion"
    )
  )
  def fromJsonObjectValue(schema: JObject): JsonSchema[_] = {
    val baseSchema = if (schema.obj.isEmpty) {
      AnySchema()
    } else if ((schema \ "$ref") != JNothing) {
      ReferenceSchema((schema \ "$ref").extract[String])
    } else if ((schema \ "enum") != JNothing) {
      val values = (schema \ "enum").extract[Set[JValue]]
      EnumSchema(values)
    } else {
      val schemaTypes = if ((schema \ "type") != JNothing) {
        (schema \ "type") match {
          case s: JString => List(s.extract[String])
          case a: JArray  => a.extract[List[String]]
          case _ =>
            throw new UnsupportedOperationException("invalid type")
        }
      } else {
        TypeDetector.detectAllTypes(schema.obj.toMap)
      }

      val schemas = schemaTypes.map { schemaType =>
        schemaType match {
          case "array"   => fromJsonArray(schema)
          case "boolean" => BooleanSchema()
          case "integer" => fromJsonInteger(schema)
          case "number"  => fromJsonNumber(schema)
          case "null"    => NullSchema()
          case "object"  => fromJsonObject(schema)
          case "string"  => fromJsonString(schema)
          case _ =>
            throw new UnsupportedOperationException("type not supported")
        }
      }

      schemas.length match {
        case 0 => AnySchema()
        case 1 => schemas(0)
        case _ => buildProductSchema(AnySchema(), schemas, AnyOf)
      }
    }

    val convertedSchema = if ((schema \ "allOf") != JNothing) {
      val schemas = (schema \ "allOf").extract[List[JObject]]
      schemas.length match {
        case 1 =>
          fromJson(schemas(0)).merge(baseSchema, Intersect)(
            EquivalenceRelations.AlwaysEquivalenceRelation
          )
        case _ =>
          buildProductSchema(baseSchema, schemas.map(fromJson(_)), AllOf)
      }
    } else if ((schema \ "oneOf") != JNothing) {
      productFromJsons(
        baseSchema,
        (schema \ "oneOf").extract[List[JObject]],
        OneOf
      )
    } else if ((schema \ "anyOf") != JNothing) {
      // XXX This technically isn't correct since we change anyOf to oneOf
      productFromJsons(
        baseSchema,
        (schema \ "anyOf").extract[List[JObject]],
        AnyOf
      )
    } else {
      baseSchema
    }

    val definitionsKey = if ((schema \ "definitions") != JNothing) {
      Some("definitions")
    } else if ((schema \ "$defs") != JNothing) {
      Some("$defs")
    } else {
      None
    }
    if (!definitionsKey.isEmpty) {
      val defs = (schema \ definitionsKey.get)
        .extract[Map[String, JObject]]
        .foreach { case (key, value) =>
          convertedSchema.definitions += (key -> fromJson(value))
        }
    }

    convertedSchema
  }

  private def buildProductSchema(
      baseSchema: JsonSchema[_],
      schemas: List[JsonSchema[_]],
      productType: ProductType
  ): ProductSchema = {
    val er: EquivalenceRelation =
      EquivalenceRelations.NonEquivalenceRelation
    val typesProp = ProductSchemaTypesProperty(
      baseSchema,
      schemas,
      List.fill(schemas.length)(1),
      productType
    )(er)
    val properties =
      SchemaProperties.empty[JsonSchema[_]].replaceProperty(typesProp)
    ProductSchema(properties)(er)
  }

  private def productFromJsons(
      baseSchema: JsonSchema[_],
      schemas: List[JObject],
      productType: ProductType
  ): JsonSchema[_] = {
    schemas.length match {
      case 1 =>
        val schema = baseSchema.merge(fromJson(schemas(0)), Intersect)(
          EquivalenceRelations.AlwaysEquivalenceRelation
        )
        schema
      case _ =>
        buildProductSchema(baseSchema, schemas.map(fromJson(_)), productType)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def fromJsonArray(arr: JObject): JsonSchema[_] = {
    val props = SchemaProperties.empty[List[JsonSchema[_]]]

    if ((arr \ "contains") != JNothing) {
      throw new UnsupportedOperationException("contains not supported")
    }

    if ((arr \ "minItems") != JNothing) {
      props.add(MinItemsProperty(Some((arr \ "minItems").extract[Int])))
    }

    if ((arr \ "maxItems") != JNothing) {
      props.add(MaxItemsProperty(Some((arr \ "maxItems").extract[Int])))
    }

    if ((arr \ "uniqueItems") != JNothing) {
      props.add(UniqueProperty((arr \ "uniqueItems").extract[Boolean], false))
    }

    val itemType: Either[JsonSchema[_], List[JsonSchema[_]]] =
      if ((arr \ "prefixItems") != JNothing) {
        if ((arr \ "items") != JNothing) {
          throw new UnsupportedOperationException(
            "Both items and prefixItems cannot be specified"
          )
        }

        Right((arr \ "prefixItems").extract[List[JValue]].map(s => fromJson(s)))
      } else if ((arr \ "items") != JNothing) {
        (arr \ "items") match {
          case a: JArray =>
            Right(a.extract[List[JValue]].map(s => fromJson(s)))
          case _ =>
            Left(fromJson((arr \ "items").extract[JValue]))
        }
      } else if ((arr \ "additionalItems") != JNothing) {
        Left(fromJson((arr \ "additionalItems").extract[JValue]))
      } else {
        // items and additionalItems not specified, accept anything
        Left(AnySchema())
      }

    props.add(ItemTypeProperty(itemType))

    ArraySchema(props)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def fromJsonInteger(int: JObject): JsonSchema[_] = {
    val props = SchemaProperties.empty[BigInt]

    if (int.values.contains("multipleOf")) {
      props.add(
        IntMultipleOfProperty(Some((int \ "multipleOf").extract[BigInt]))
      )
    }

    if (int.values.contains("minimum")) {
      props.add(MinIntValueProperty(Some((int \ "minimum").extract[BigInt])))
    }

    if (int.values.contains("exclusiveMinimum")) {
      props.add(
        MinIntValueProperty(
          Some((int \ "exclusiveMinimum").extract[BigInt]),
          true
        )
      )
    }

    if (int.values.contains("maximum")) {
      props.add(MaxIntValueProperty(Some((int \ "maximum").extract[BigInt])))
    }

    if (int.values.contains("exclusiveMaximum")) {
      props.add(
        MaxIntValueProperty(
          Some((int \ "exclusiveMaximum").extract[BigInt]),
          true
        )
      )
    }

    IntegerSchema(props)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def fromJsonNumber(num: JObject): JsonSchema[_] = {
    val props = SchemaProperties.empty[BigDecimal]

    if ((num \ "multipleOf") != JNothing) {
      props.add(
        NumMultipleOfProperty(Some((num \ "multipleOf").extract[BigDecimal]))
      )
    }

    if ((num \ "minimum") != JNothing) {
      props.add(
        MinNumValueProperty(Some((num \ "minimum").extract[BigDecimal]))
      )
    }

    if ((num \ "exclusiveMinimum") != JNothing) {
      props.add(
        MinNumValueProperty(
          Some((num \ "exclusiveMinimum").extract[BigDecimal]),
          true
        )
      )
    }

    if ((num \ "maximum") != JNothing) {
      props.add(
        MaxNumValueProperty(Some((num \ "maximum").extract[BigDecimal]))
      )
    }

    if ((num \ "exclusiveMaximum") != JNothing) {
      props.add(
        MaxNumValueProperty(
          Some((num \ "exclusiveMaximum").extract[BigDecimal]),
          true
        )
      )
    }

    NumberSchema(props)
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Recursion"
    )
  )
  private def fromJsonObject(obj: JObject): JsonSchema[_] = {
    if ((obj \ "not") != JNothing) {
      throw new UnsupportedOperationException("not isn't supported")
    }

    // TODO Add support for dependencies
    if ((obj \ "dependencies") != JNothing) {
      throw new UnsupportedOperationException("dependencies not supported")
    }
    if ((obj \ "dependentRequired") != JNothing) {
      throw new UnsupportedOperationException("dependentRequired not supported")
    }
    if ((obj \ "dependentSchemas") != JNothing) {
      throw new UnsupportedOperationException("dependentSchemas not supported")
    }

    val objProps = if ((obj \ "properties") != JNothing) {
      (obj \ "properties").extract[Map[String, JObject]]
    } else {
      Map.empty
    }
    val objTypes: Map[String, JsonSchema[_]] = objProps.map {
      case (prop, value) =>
        (prop -> fromJson(value))
    }.toMap

    val patternProps = if ((obj \ "patternProperties") != JNothing) {
      (obj \ "patternProperties").extract[Map[String, JObject]]
    } else {
      Map.empty
    }
    val patternTypes: Map[Regex, JsonSchema[_]] = patternProps.map {
      case (pattern, value) =>
        (pattern.r -> fromJson(value))
    }.toMap

    val required = (obj \ "required").extract[Set[String]]
    val reqProp = RequiredProperty(Some(required))

    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(ObjectTypesProperty(objTypes))
    props.add(PatternTypesProperty(patternTypes))
    props.add(RequiredProperty(Some(required)))

    ObjectSchema(props)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def fromJsonString(str: JObject): JsonSchema[_] = {
    val props = SchemaProperties.empty[String]

    if ((str \ "format") != JNothing) {
      val format = (str \ "format").extract[String]
      props.add(FormatProperty(Map(format -> 1)))
    }

    if ((str \ "pattern") != JNothing) {
      props.add(StaticPatternProperty((str \ "pattern").extract[String].r))
    }

    if ((str \ "minLength") != JNothing) {
      props.add(MinLengthProperty(Some((str \ "minLength").extract[Int])))
    }

    if ((str \ "maxLength") != JNothing) {
      props.add(MaxLengthProperty(Some((str \ "maxLength").extract[Int])))
    }

    StringSchema(props)
  }
}

trait JsonSchema[T] {
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def toJson: JObject = {
    val propertyJson =
      properties.map(_.toJson).foldLeft(staticProperties)(_.merge(_))
    val typedPropertyJson = if (hasType) {
      ("type" -> schemaType) ~ propertyJson
    } else {
      propertyJson
    }

    val definitionJson: JObject = if (definitions.isEmpty) {
      Nil
    } else {
      "$defs" -> definitions.map { case (defn, schema) =>
        (defn -> schema.toJson)
      }.toMap
    }

    typedPropertyJson.merge(definitionJson)
  }

  def toJsonSchema: JObject = {
    val schemaObj: JObject =
      ("$schema" -> "https://json-schema.org/draft/2019-09/schema") ~
        ("description" ->
          (s"""Generated by JSONoid ${BuildInfo.version}. Not to be""" +
            " used for validation purposes. See" +
            " https://github.com/michaelmior/jsonoid-discovery/."))

    toJson.merge(schemaObj)
  }

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  val definitions: mutable.Map[String, JsonSchema[_]] = mutable.Map.empty

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def addDefinition(definition: JsonSchema[_], name: String): Unit =
    definitions += (name -> definition)

  def staticProperties: JObject = Nil

  def properties: SchemaProperties[T]

  def schemaType: String

  def hasType: Boolean = true

  def validTypes: Set[ClassTag[_ <: JValue]]

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def isValidType[S <: JValue](value: S)(implicit tag: ClassTag[S]): Boolean = {
    validTypes.contains(tag)
  }

  def mergeSameType(mergeType: MergeType = Union)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]]

  def createProduct()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case other =>
    ProductSchema(this)(er).merge(other)
  }

  def isMaxMin: Boolean = {
    this.isInstanceOf[AnySchema] || this.isInstanceOf[ZeroSchema]
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  def merge(
      other: JsonSchema[_],
      mergeType: MergeType = Union
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    val otherIsProduct =
      other.isInstanceOf[ProductSchema] && !this.isInstanceOf[ProductSchema]
    if ((other.isMaxMin && !this.isMaxMin) || otherIsProduct) {
      other.merge(this, mergeType)
    } else {
      val sameType = mergeSameType(mergeType)(er)
      val newSchema = if (sameType.isDefinedAt(other) && er.fuse(this, other)) {
        sameType(other)
      } else {
        createProduct()(er)(other)
      }

      newSchema.definitions ++= this.definitions
      newSchema.definitions ++= other.definitions

      newSchema
    }
  }

  def copy(properties: SchemaProperties[T]): JsonSchema[_]

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def transformProperties(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]],
      transformBase: Boolean = false
  ): JsonSchema[_] = {
    if (transformer.isDefinedAt(this) && transformBase) {
      transformer(this).transformProperties(transformer, false)
    } else {
      copy(properties.transform(transformer))
    }
  }

  def findByPointer(pointer: String): Option[JsonSchema[_]] = None

  def replaceWithReference(
      pointer: String,
      reference: String,
      obj: Option[JsonSchema[_]] = None
  ): JsonSchema[_] =
    this

  def isAnomalous[S <: JValue](value: S, path: String = "$")(implicit
      tag: ClassTag[S]
  ): Boolean = {
    !collectAnomalies(value, path)(tag).isEmpty
  }

  def collectAnomalies[S <: JValue](
      value: S,
      path: String = "$"
  )(implicit tag: ClassTag[S]): Seq[Anomaly] = {
    if (isValidType(value)(tag)) {
      properties.flatMap(_.collectAnomalies(value, path)(tag)).toSeq
    } else {
      Seq(Anomaly(path, f"${value} has wrong type", Fatal))
    }
  }
}
