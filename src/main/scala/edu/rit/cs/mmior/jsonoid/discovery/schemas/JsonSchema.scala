package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect._
import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

object JsonSchema {
  implicit val formats: Formats = DefaultFormats

  def fromJson(schema: JValue): JsonSchema[_] = {
    schema match {
      case JBool(true)  => AnySchema()
      case JBool(false) => ZeroSchema()
      case o: JObject   => fromJson(o)
      case _ =>
        throw new UnsupportedOperationException("invalid schema element")
    }
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.Equals", "org.wartremover.warts.Recursion")
  )
  def fromJson(schema: JObject): JsonSchema[_] = {
    if (schema.obj.isEmpty) {
      AnySchema()
    } else if ((schema \ "$ref") != JNothing) {
      throw new UnsupportedOperationException("$ref not supported")
    } else if ((schema \ "allOf") != JNothing) {
      val schemas = (schema \ "allOf").extract[List[JObject]]
      schemas.length match {
        case 1 => fromJson(schemas(0))
        case _ =>
          throw new UnsupportedOperationException("allOf not supported")
      }
    } else if ((schema \ "oneOf") != JNothing) {
      productFromJsons((schema \ "oneOf").extract[List[JObject]])
    } else if ((schema \ "anyOf") != JNothing) {
      // XXX This technically isn't correct since we change anyOf to oneOf
      productFromJsons((schema \ "anyOf").extract[List[JObject]])
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
      } else if ((schema \ "properties") != JNothing) {
        // If this has properties, assumed it is an object
        List("object")
      } else {
        throw new UnsupportedOperationException("missing type encountered")
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

      if (schemas.length == 1) {
        schemas(0)
      } else {
        buildProductSchema(schemas)
      }
    }
  }

  private def buildProductSchema(
      schemas: List[JsonSchema[_]]
  ): ProductSchema = {
    val er: EquivalenceRelation =
      EquivalenceRelations.NonEquivalenceRelation
    val typesProp = ProductSchemaTypesProperty(
      schemas,
      List.fill(schemas.length)(1)
    )(er)
    val properties =
      SchemaProperties.empty[JsonSchema[_]].replaceProperty(typesProp)
    ProductSchema(properties)(er)
  }

  private def productFromJsons(schemas: List[JObject]): JsonSchema[_] = {
    schemas.length match {
      case 1 => fromJson(schemas(0))
      case _ => buildProductSchema(schemas.map(fromJson(_)))
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

    if ((arr \ "additionalItems") != JNothing) {
      throw new UnsupportedOperationException("additionalItems not supported")
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
        Left(fromJson((arr \ "items").extract[JValue]))
      } else {
        throw new UnsupportedOperationException(
          "items or prefixItems must be specified"
        )
      }

    props.add(ItemTypeProperty(itemType))

    ArraySchema(props)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def fromJsonInteger(int: JObject): JsonSchema[_] = {
    val props = SchemaProperties.empty[BigInt]

    if (int.values.contains("exclusiveMinimum")) {
      throw new UnsupportedOperationException("exclusiveMinimum not supported")
    }

    if (int.values.contains("exclusiveMaximum")) {
      throw new UnsupportedOperationException("exclusiveMaximum not supported")
    }

    if (int.values.contains("multipleOf")) {
      props.add(MultipleOfProperty(Some((int \ "multipleOf").extract[BigInt])))
    }

    if (int.values.contains("minimum")) {
      props.add(MinIntValueProperty(Some((int \ "minimum").extract[BigInt])))
    }

    if (int.values.contains("maximum")) {
      props.add(MaxIntValueProperty(Some((int \ "maximum").extract[BigInt])))
    }

    IntegerSchema(props)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def fromJsonNumber(num: JObject): JsonSchema[_] = {
    val props = SchemaProperties.empty[BigDecimal]

    if ((num \ "exclusiveMinimum") != JNothing) {
      throw new UnsupportedOperationException("exclusiveMinimum not supported")
    }

    if ((num \ "exclusiveMaximum") != JNothing) {
      throw new UnsupportedOperationException("exclusiveMaximum not supported")
    }

    if ((num \ "multipleOf") != JNothing) {
      throw new UnsupportedOperationException("multipleOf not supported")
    }

    if ((num \ "minimum") != JNothing) {
      props.add(
        MinNumValueProperty(Some((num \ "minimum").extract[BigDecimal]))
      )
    }

    if ((num \ "maximum") != JNothing) {
      props.add(
        MaxNumValueProperty(Some((num \ "maximum").extract[BigDecimal]))
      )
    }

    NumberSchema(props)
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.Equals", "org.wartremover.warts.Recursion")
  )
  private def fromJsonObject(obj: JObject): JsonSchema[_] = {
    // TODO Add support for dependencies
    if ((obj \ "dependencies") != JNothing) {
      throw new UnsupportedOperationException("dependencies not supported")
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

    val required = (obj \ "required").extract[Set[String]]
    val reqProp = RequiredProperty(Some(required))

    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(ObjectTypesProperty(objTypes))
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
  def toJson: JObject = {
    val propertyJson =
      properties.map(_.toJson).foldLeft(staticProperties)(_.merge(_))
    if (hasType) {
      ("type" -> schemaType) ~ propertyJson
    } else {
      propertyJson
    }
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

  def staticProperties: JObject = Nil

  def properties: SchemaProperties[T]

  def schemaType: String

  def hasType: Boolean = true

  def validTypes: Set[ClassTag[_ <: JValue]]

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def isValidType[S <: JValue](value: S)(implicit tag: ClassTag[S]): Boolean = {
    validTypes.contains(tag)
  }

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]]

  def createProduct()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case other =>
    ProductSchema(this)(er).merge(other)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def merge(
      other: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    val sameType = mergeSameType()(er)
    if (sameType.isDefinedAt(other) && er.fuse(this, other)) {
      sameType(other)
    } else {
      createProduct()(er)(other)
    }
  }

  def copy(properties: SchemaProperties[T]): JsonSchema[_]

  def transformProperties(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): JsonSchema[_] = {
    copy(properties.transform(transformer))
  }

  def findByPointer(pointer: String): Option[JsonSchema[_]] = None

  def replaceWithReference(
      pointer: String,
      reference: String,
      obj: Option[JsonSchema[_]] = None
  ): JsonSchema[_] =
    this

  def isAnomalous(value: JValue, path: String = "$"): Boolean =
    !collectAnomalies(value, path).isEmpty

  def collectAnomalies[S <: JValue](
      value: S,
      path: String = "$"
  )(implicit tag: ClassTag[S]): Seq[Anomaly] = {
    if (isValidType(value)(tag)) {
      properties.flatMap(_.collectAnomalies(value, path)).toSeq
    } else {
      Seq(Anomaly(path, f"${value} has wrong type", Fatal))
    }
  }
}
