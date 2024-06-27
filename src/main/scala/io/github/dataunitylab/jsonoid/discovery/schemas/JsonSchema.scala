package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.collection.mutable
import scala.language.existentials
import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import Helpers._
import utils.JsonPointer

object JsonSchema {
  implicit val formats: Formats = DefaultFormats

  /** Construct a JSON Schema object from a serialized schema. */
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

  /** Produce an object schema from a serialized JSON Schema object. */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Recursion"
    )
  )
  private def fromJsonObjectValue(schema: JObject): JsonSchema[_] = {
    if ((schema \ "if") =/= JNothing || (schema \ "then") =/= JNothing) {
      throw new UnsupportedOperationException("if/then not supported")
    }

    val baseSchema: JsonSchema[_] = if (schema.obj.isEmpty) {
      AnySchema()
    } else if ((schema \ "$ref") =/= JNothing) {
      try {
        val ref = (schema \ "$ref").extract[String]
        ReferenceSchema((if (ref.isEmpty) "#" else ref))
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException => ReferenceSchema("#")
        // $COVERAGE-ON$
      }
    } else if ((schema \ "enum") =/= JNothing) {
      val values =
        try {
          (schema \ "enum").extract[Set[JValue]]
        } catch {
          // $COVERAGE-OFF$
          case e: org.json4s.MappingException => Set.empty[JValue]
          // $COVERAGE-ON$
        }
      EnumSchema(values)
    } else if ((schema \ "const") =/= JNothing) {
      val values =
        try {
          Set((schema \ "const").extract[JValue])
        } catch {
          // $COVERAGE-OFF$
          case e: org.json4s.MappingException => Set.empty[JValue]
          // $COVERAGE-ON$
        }

      if (values.size === 1 && values.head.isInstanceOf[JBool]) {
        // Convert Boolean constants to BooleanSchema
        BooleanSchema(values.head.asInstanceOf[JBool].value)
      } else {
        EnumSchema(values)
      }
    } else {
      val schemaTypes: List[String] = if ((schema \ "type") =/= JNothing) {
        try {
          (schema \ "type") match {
            case s: JString => List(s.extract[String])
            case a: JArray  => a.extract[List[String]]
            case _ =>
              throw new UnsupportedOperationException("invalid type")
          }
        } catch {
          case e: org.json4s.MappingException =>
            throw new UnsupportedOperationException("invalid type")
        }
      } else {
        TypeDetector.detectAllTypes(schema.obj.toMap)
      }

      val schemas = schemaTypes.map { schemaType =>
        val convertedSchema: JsonSchema[_] = schemaType match {
          case "array"   => ArraySchema.fromJson(schema)
          case "boolean" => BooleanSchema()
          case "integer" => IntegerSchema.fromJson(schema)
          case "number"  => NumberSchema.fromJson(schema)
          case "null"    => NullSchema()
          case "object"  => ObjectSchema.fromJson(schema)
          case "string"  => StringSchema.fromJson(schema)
          case _ =>
            throw new UnsupportedOperationException("type not supported")
        }

        convertedSchema
      }

      schemas.length match {
        case 0 => AnySchema()
        case 1 => schemas(0)
        case _ => buildProductSchema(AnySchema(), schemas, AnyOf)
      }
    }

    val convertedSchema = if ((schema \ "allOf") =/= JNothing) {
      val schemas =
        try {
          (schema \ "allOf").extract[List[JObject]]
        } catch {
          case e: org.json4s.MappingException =>
            List.empty
        }
      schemas.length match {
        case 1 =>
          fromJson(schemas(0)).merge(baseSchema, Intersect)(
            JsonoidParams()
              .withER(EquivalenceRelations.AlwaysEquivalenceRelation)
          )
        case _ =>
          buildProductSchema(baseSchema, schemas.map(fromJson(_)), AllOf)
      }
    } else if ((schema \ "oneOf") =/= JNothing) {
      val schemas =
        try {
          (schema \ "oneOf").extract[List[JObject]]
        } catch {
          // $COVERAGE-OFF$
          case e: org.json4s.MappingException => List.empty
          // $COVERAGE-ON$
        }
      productFromJsons(baseSchema, schemas, OneOf)
    } else if ((schema \ "anyOf") =/= JNothing) {
      // XXX This technically isn't correct since we change anyOf to oneOf
      val schemas =
        try {
          (schema \ "anyOf").extract[List[JObject]]
        } catch {
          // $COVERAGE-OFF$
          case e: org.json4s.MappingException => List.empty
          // $COVERAGE-ON$
        }
      productFromJsons(baseSchema, schemas, AnyOf)
    } else {
      baseSchema
    }

    val definitionsKey =
      if ((schema \ "definitions") =/= JNothing)
        Some("definitions")
      else if ((schema \ "$defs") =/= JNothing)
        Some("$defs")
      else
        None

    if (definitionsKey.isDefined) {
      try {
        val defs = (schema \ definitionsKey.get)
          .extract[Map[String, JObject]]
          .foreach { case (key, value) =>
            convertedSchema.definitions += (key -> fromJson(value))
          }
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    convertedSchema
  }

  /** Helper function to produce a [[ProductSchema]] from a list of schemas. */
  private[schemas] def buildProductSchema(
      baseSchema: JsonSchema[_],
      schemas: List[JsonSchema[_]],
      productType: ProductType
  ): ProductSchema = {
    val er: EquivalenceRelation =
      EquivalenceRelations.NonEquivalenceRelation
    val p = JsonoidParams().withER(er)
    val typesProp = ProductSchemaTypesProperty(
      baseSchema,
      schemas,
      List.fill(schemas.length)(1),
      productType
    )(p)
    val properties =
      SchemaProperties.empty[JsonSchema[_]].replaceProperty(typesProp)
    ProductSchema(properties)(p)
  }

  /** Helper function for [[fromJsonObjectValue]]i to build a product schema
    *  from a list of serialized schema objects.
    */
  private def productFromJsons(
      baseSchema: JsonSchema[_],
      schemas: List[JObject],
      productType: ProductType
  ): JsonSchema[_] = {
    schemas.length match {
      case 1 =>
        val schema = baseSchema.merge(fromJson(schemas(0)), Intersect)(
          JsonoidParams()
            .withER(EquivalenceRelations.AlwaysEquivalenceRelation)
        )
        schema
      case _ =>
        buildProductSchema(baseSchema, schemas.map(fromJson(_)), productType)
    }
  }
}

/** Base trait for all JSON Schema types.
  */
trait JsonSchema[T] {

  /** Convert the schema to JSON Schema.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def toJson()(implicit p: JsonoidParams): JObject = {
    val propertyJson =
      properties.map(_.toJson()(p)).foldLeft(JObject())(_.merge(_))
    val typedPropertyJson =
      if (hasType)
        ("type" -> schemaType) ~ propertyJson
      else
        propertyJson

    val definitionJson: JObject = if (definitions.isEmpty) {
      Nil
    } else {
      "$defs" -> definitions.map { case (defn, schema) =>
        (defn -> schema.toJson()(p))
      }.toMap
    }

    typedPropertyJson.merge(definitionJson)
  }

  /** Convert the schema to JSON Schema with the schema version specified.
    */
  def toJsonSchema()(implicit p: JsonoidParams): JObject = {
    val schemaObj: JObject =
      ("$schema" -> "https://json-schema.org/draft/2020-12/schema") ~
        ("description" ->
          (s"""Generated by JSONoid ${io.github.dataunitylab.jsonoid.discovery.BuildInfo.version}. Not to be""" +
            " used for validation purposes."))

    toJson()(p).merge(schemaObj)
  }

  /** A set of definitions used to express repeated structures. */
  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  val definitions: mutable.Map[String, JsonSchema[_]] = mutable.Map.empty

  /** Add a new definition to the set of [[definitions]].
    *
    * @param definition the new definition to add
    * @param name the name of the definition
    */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def addDefinition(definition: JsonSchema[_], name: String): Unit =
    definitions += (name -> definition)

  /** A set of properties wh */
  def properties: SchemaProperties[T]

  /** A string representing the type of this schema. */
  def schemaType: String

  /** Whether this schema is numeric. */
  def isNumeric: Boolean = false

  /** Whether [[schemaType]] has any meaning for this schema class. */
  def hasType: Boolean = true

  /** The set of valid types that can be contained in this schema. */
  def validTypes: Set[Class[_]]

  /** Whether a given value is a valid type for this schema. */
  def isValidType[S <: JValue](value: S): Boolean = {
    validTypes.contains(value.getClass)
  }

  /** Merge two schemas which are of the same basic type. */
  def mergeSameType(mergeType: MergeType = Union)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]]

  /** A function which creates a new product schema. */
  def createProduct()(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case other =>
    ProductSchema(this)(p).merge(other)
  }

  /** Whether this schema either accepts all or no values. */
  def isMaxMin: Boolean = {
    this.isInstanceOf[AnySchema] || this.isInstanceOf[ZeroSchema]
  }

  /** Merge multiple schemas together.
    *
    * @param other the schema to merge with
    * @param mergeType the type of merge to perform
    *
    * @return the merged schema
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  def merge(
      other: JsonSchema[_],
      mergeType: MergeType = Union
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    val otherIsProduct =
      other.isInstanceOf[ProductSchema] && !this.isInstanceOf[ProductSchema]
    if ((other.isMaxMin && !this.isMaxMin) || otherIsProduct) {
      other.merge(this, mergeType)
    } else {
      val sameType = mergeSameType(mergeType)(p)
      val newSchema =
        if (sameType.isDefinedAt(other) && p.er.fuse(this, other))
          sameType(other)
        else
          createProduct()(p)(other)

      newSchema.definitions ++= this.definitions
      newSchema.definitions ++= other.definitions

      // We must be one of the original types or a ProductSchema
      assert(
        List(schemaType, other.schemaType).contains(
          newSchema.schemaType
        ) || newSchema.isInstanceOf[ProductSchema]
      )

      newSchema
    }
  }

  /** Create a copy of this schema with a new set of properties.
    */
  def copy(properties: SchemaProperties[T]): JsonSchema[T]

  /** Create a copy of this schema with the same set of properties, but with each
    * property set to their default value.
    */
  def copyWithReset()(implicit p: JsonoidParams): JsonSchema[T] = {
    copy(properties.copyWithReset()(p))
  }

  /** Transform all the properties in this schema and any nested schemas
    * according to a specified function.
    *
    * @param transformer the function to transform the properties
    * @param transformBase whether to also transform this schema
    *
    * @return a new schema with transformed properties
    */
  def transformProperties(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]],
      transformBase: Boolean = false
  ): JsonSchema[_] = {
    val newTransformer =
      new PartialFunction[(String, JsonSchema[_]), JsonSchema[_]] {
        def apply(x: (String, JsonSchema[_])) = typedApply(x._2)
        def typedApply[S](s: JsonSchema[S]): JsonSchema[S] =
          transformer(s).asInstanceOf[JsonSchema[S]]
        def isDefinedAt(x: (String, JsonSchema[_])) =
          transformer.isDefinedAt(x._2)
      }

    transformPropertiesWithInexactPath(newTransformer, transformBase)
  }

  /** A variant of [[transformProperties]], which also provides the path to the
    * transformer function. This path is *inexact* in that it does not include
    * which array element is being used or which schema within a
    * [[ProductSchema]] is being referenced.
    *
    * @param transformer the function to transform the properties
    * @param transformBase whether to also transform this schema
    * @param path the base of the inexact path
    */
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def transformPropertiesWithInexactPath(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      transformBase: Boolean = false,
      path: String = "$"
  ): JsonSchema[_] = {
    if (transformer.isDefinedAt((path, this)) && transformBase) {
      transformer((path, this))
        .transformPropertiesWithInexactPath(transformer, false, path)
    } else {
      copy(properties.transform(transformer, path))
    }
  }

  /** Finda nested schema based on a JSON Pointer. */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def findByPointer(pointer: JsonPointer): Option[JsonSchema[_]] = if (
    pointer.isEmpty
  ) {
    Some(this)
  } else {
    None
  }

  /** Find a nested schema based on an *inexact* JSON pointer. Inexact pointers
    * do not include which array element is being used or which schema within a
    * [[ProductSchema]] is being referenced.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def findByInexactPointer(pointer: JsonPointer): Seq[JsonSchema[_]] = if (
    pointer.isEmpty
  ) {
    Seq(this)
  } else {
    Seq.empty
  }

  /** Replace a schema at a particular pointer with a new schema.
    *
    * @param pointer the JSON pointer to the schema to replace
    * @param replaceSchema the schema to replace with
    *
    * @return a new schema with the referenced schema replaced
    */
  def replaceWithSchema(
      pointer: JsonPointer,
      replaceSchema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] =
    this

  /** Replace a schema at a particular pointer with a reference to another schema.
    *
    * @param pointer the JSON pointer to the schema to replace
    * @param reference the reference to use in the new schema
    * @param obj an optional object which represents the referenced schema
    *
    * @return a new schema with a reference
    */
  def replaceWithReference(
      pointer: JsonPointer,
      reference: String,
      obj: Option[JsonSchema[_]] = None
  )(implicit p: JsonoidParams): JsonSchema[_] =
    replaceWithSchema(pointer, ReferenceSchema(reference, obj))

  /** Whether a value at a particular path is anomalous.
    *
    * @param value the value to check for anomalies
    * @param path the path where this anomaly is being checked
    *
    * @return true if the value is anomalous, false otherwise
    */
  def isAnomalous[S <: JValue](
      value: S,
      path: String = "$",
      level: AnomalyLevel = AnomalyLevel.Info
  )(implicit tag: ClassTag[S], p: JsonoidParams): Boolean = {
    maxAnomalyLevel(value, path)(tag, p)
      .map(_.order)
      .getOrElse(-1) >= level.order
  }

  /** Whether a value at a particular path is anomalous.
    *
    * @param value the value to check for anomalies
    * @param path the path where this anomaly is being checked
    *
    * @return true if the value is anomalous, false otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def maxAnomalyLevel[S <: JValue](
      value: S,
      path: String = "$"
  )(implicit tag: ClassTag[S], p: JsonoidParams): Option[AnomalyLevel] = {
    val anomalyLevels =
      collectAnomalies(value, path)(p, tag).map(_.anomalyLevel)
    if (anomalyLevels.isEmpty)
      None
    else
      Some(anomalyLevels.max)
  }

  /** Produce a list of anomalies when validating a given value.
    *
    * @param value the value to check for anomalies
    * @param path the path where this anomaly is being checked
    *
    * @return a sequence of anomalies observed for this value
    */
  def collectAnomalies[S <: JValue](
      value: S,
      path: String = "$"
  )(implicit p: JsonoidParams, tag: ClassTag[S]): Seq[Anomaly] = {
    if (isValidType(value))
      properties.flatMap(_.collectAnomalies(value, path)(p, tag)).toSeq
    else
      Seq(
        Anomaly(
          path,
          f"${compact(render(value))} has wrong type",
          AnomalyLevel.Fatal
        )
      )
  }

  /** Update a schema to only include a specific set of properties.
    *
    * @param props the properties to include
    *
    * @return a new schema with only the specified properties
    */
  def onlyProperties(props: Seq[Class[_]]): JsonSchema[T] = {
    val copyProps =
      new PartialFunction[JsonSchema[_], JsonSchema[_]] {
        def apply(x: JsonSchema[_]) = typedApply(x)
        def typedApply[S](s: JsonSchema[S]): JsonSchema[S] = {
          val newProps =
            s.properties.only(props).asInstanceOf[SchemaProperties[S]]
          s.copy(newProps)
        }
        def isDefinedAt(x: JsonSchema[_]) = true
      }
    transformProperties(copyProps, true).asInstanceOf[JsonSchema[T]]
  }

  /** Update a schema to only include a specific named set of properties.
    *
    * @param props the names properties to include
    *
    * @return a new schema with only the specified properties
    */
  def onlyPropertiesNamed(props: Seq[String]): JsonSchema[T] = {
    onlyProperties(
      props.map(c =>
        Class.forName("io.github.dataunitylab.jsonoid.discovery.schemas." + c)
      )
    )
  }

  /** Find incompatibilities with this schema and another schema.
    *
    * @param other the other schema to compare with
    * @param recursive whether to recursively compare the schemas
    *
    * @return a sequence of properties which are incompatible
    */
  def findIncompatibilities(
      other: JsonSchema[_],
      recursive: Boolean
  ): Seq[ClassTag[_]] =
    properties.findIncompatibilities(other.properties, recursive)

  /** Check whether this schema is compatible with antoher schema.
    *
    * @param other the other schema to compare with
    * @param recursive whether to recursively compare the schemas
    *
    * @return true if this schema is compatible with the other, false otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def isSubsetOf(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    val (schema1, schema2) = (this, other) match {
      case (s1: NumberSchema, s2: IntegerSchema) => (s1, s2.asNumberSchema)
      case (s1: IntegerSchema, s2: NumberSchema) => (s1.asNumberSchema, s2)
      case (s1, s2)                              => (s1, s2)
    }

    if (schema2.isInstanceOf[AnySchema]) {
      true
    } else if (other.isInstanceOf[ProductSchema]) {
      other.asInstanceOf[ProductSchema].isSupersetOf(this, recursive)
    } else {
      (schema1.schemaType == schema2.schemaType &&
      schema1.properties.isSubsetOf(schema2.properties, recursive)(p))
    }
  }

  /** Expand this schema to be compatible with another schema if possible.
    *
    * @param other the other schema to expand to
    *
    * @return a possibly expanded schema to be compatible with the other
    */
  @SuppressWarnings(
    Array("org.wartremover.warts.Equals", "org.wartremover.warts.Recursion")
  )
  def expandTo[S](other: Option[JsonSchema[S]]): JsonSchema[_] = {
    if (other.isEmpty || schemaType == other.map(_.schemaType).getOrElse("")) {
      copy(
        properties.expandTo(other.map(_.asInstanceOf[JsonSchema[T]].properties))
      )
    } else {
      // Convert to a product schema if we need to add a new type
      JsonSchema
        .buildProductSchema(AnySchema(), List(this), OneOf)
        .expandTo(other)
    }
  }

  /** The number of possible types accepted by this schema.
    * It must be overridden by subclasses to do anything useful.
    *
    * @return the number of types or None if entropy cannot be calculated
    */
  def entropy(implicit p: JsonoidParams): Option[Long] = Some(1)
}
