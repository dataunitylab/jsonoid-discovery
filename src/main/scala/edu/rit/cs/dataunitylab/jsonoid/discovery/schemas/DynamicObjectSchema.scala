package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect._

import org.json4s.JsonDSL._
import org.json4s._

object DynamicObjectSchema {
  val AllProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(DynamicObjectTypeProperty())

    props
  }
}

/** Represents objects in JSON Schema with dynamic keys.
  */
final case class DynamicObjectSchema(
    override val properties: SchemaProperties[Map[String, JsonSchema[_]]] =
      DynamicObjectSchema.AllProperties
)(implicit p: JsonoidParams)
    extends JsonSchema[Map[String, JsonSchema[_]]] {
  override val schemaType = "object"

  override val validTypes: Set[Class[_]] = Set(classOf[JObject])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ DynamicObjectSchema(otherProperties) =>
      DynamicObjectSchema(properties.merge(otherProperties, mergeType))(p)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(
      properties: SchemaProperties[Map[String, JsonSchema[_]]]
  ): DynamicObjectSchema = {
    val newSchema = DynamicObjectSchema(properties)(p)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: String): Option[JsonSchema[_]] = {
    val typeProp = properties.get[DynamicObjectTypeProperty]
    pointer.split("/", 3) match {
      case Array(_)        => None
      case Array(_, "")    => Some(this)
      case Array(_, first) => Some(typeProp.valueType)
      case Array(_, first, rest) =>
        typeProp.valueType.findByPointer("/" + rest)
    }
  }

  override def findByInexactPointer(pointer: String): Seq[JsonSchema[_]] = {
    val typeProp = properties.get[DynamicObjectTypeProperty]
    pointer.split("/", 3) match {
      case Array(_)        => Seq()
      case Array(_, "")    => Seq(this)
      case Array(_, first) => Seq(typeProp.valueType)
      case Array(_, first, rest) =>
        typeProp.valueType.findByInexactPointer("/" + rest)
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
    // Build a new type map that replaces the required type
    val valueType = properties.get[DynamicObjectTypeProperty].valueType
    val newType = pointer.split("/", 3) match {
      case Array(_) | Array(_, "") =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, first) =>
        replaceSchema

      case Array(_, first, rest) =>
        valueType.replaceWithSchema("/" + rest, replaceSchema)
    }

    val typeProp = DynamicObjectTypeProperty(newType)
    val newSchema =
      DynamicObjectSchema(this.properties.replaceProperty(typeProp))(p)
    newSchema.definitions ++= this.definitions
    newSchema
  }
}

/** The type of the values in a dynamic object schema.
  *
  * @constructor Create a new dynamic object type property.
  * @param valueType the type of the object value
  */
final case class DynamicObjectTypeProperty(
    valueType: JsonSchema[_] = ZeroSchema()
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = DynamicObjectTypeProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): DynamicObjectTypeProperty = DynamicObjectTypeProperty()

  override def toJson()(implicit p: JsonoidParams): JObject = Nil

  override def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): DynamicObjectTypeProperty = {
    DynamicObjectTypeProperty(
      valueType.transformPropertiesWithInexactPath(transformer, true, path)
    )
  }

  override def intersectMerge(
      otherProp: DynamicObjectTypeProperty
  )(implicit p: JsonoidParams): DynamicObjectTypeProperty = {
    val other = otherProp.valueType
    this.mergeValue(other, Intersect)
  }

  override def unionMerge(
      otherProp: DynamicObjectTypeProperty
  )(implicit p: JsonoidParams): DynamicObjectTypeProperty = {
    val other = otherProp.valueType
    this.mergeValue(other, Union)(p)
  }

  override def mergeValue(value: Map[String, JsonSchema[_]])(implicit
      p: JsonoidParams
  ): DynamicObjectTypeProperty = {
    DynamicObjectTypeProperty(
      value.values.fold(valueType)((a, b) => a.merge(b, Union))
    )
  }

  def mergeValue(
      value: JsonSchema[_],
      mergeType: MergeType
  )(implicit p: JsonoidParams): DynamicObjectTypeProperty = {
    DynamicObjectTypeProperty(valueType.merge(value, mergeType))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JObject(fields) =>
        fields.flatMap((f: JField) => valueType.collectAnomalies(f._2, path))
      case _ => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: DynamicObjectTypeProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    valueType.isCompatibleWith(other.valueType, recursive)
  }

  override def expandTo(
      other: Option[DynamicObjectTypeProperty]
  ): DynamicObjectTypeProperty = {
    other match {
      case Some(prop) =>
        DynamicObjectTypeProperty(valueType.expandTo(Some(prop.valueType)))
      case None => this
    }
  }
}
