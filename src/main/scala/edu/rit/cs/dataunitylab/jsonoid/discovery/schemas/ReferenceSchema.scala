package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

object ReferenceSchema {
  def apply(
      reference: String,
      obj: Option[JsonSchema[_]] = None
  ): ReferenceSchema = {
    val props = SchemaProperties.empty[String]
    props.add(ReferencePathProperty(reference))

    obj match {
      case Some(schemaObj) => props.add(ReferenceObjectProperty(schemaObj))
      case None            =>
    }

    ReferenceSchema(props)
  }
}

/** Represents a reference (`\$ref`) in JSON Schema.
  */
final case class ReferenceSchema(
    override val properties: SchemaProperties[String]
) extends JsonSchema[String] {
  override val hasType = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  override val validTypes: Set[Class[_]] = Set.empty

  override def isValidType[S <: JValue](value: S): Boolean = {
    throw new UnsupportedOperationException("$ref cannot be type checked")
  }

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = Map.empty

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[String]): ReferenceSchema = {
    val newSchema = ReferenceSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  override def collectAnomalies[S <: JValue](
      value: S,
      path: String
  )(implicit p: JsonoidParams, t: ClassTag[S]): Seq[Anomaly] = {
    Seq(
      Anomaly(path, "$ref cannot be checked for anomalies", AnomalyLevel.Fatal)
    )
  }
}

/** Represents a reference to a particular path.
  *
  * @constructor Create a path reference property
  * @param path the referenced path
  */
final case class ReferencePathProperty(path: String)
    extends SchemaProperty[String] {
  override type S = ReferencePathProperty

  override def newDefault()(implicit p: JsonoidParams): ReferencePathProperty =
    ReferencePathProperty("")

  override def toJson()(implicit p: JsonoidParams): JObject = ("$ref" -> path)

  override def unionMerge(
      otherProp: ReferencePathProperty
  )(implicit p: JsonoidParams): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }

  override def mergeValue(
      otherPath: String
  )(implicit p: JsonoidParams): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }
}

/** Represents a reference to a particular schema object.
  *
  * @constructor Create an object reference property
  * @param schema the referenced object
  */
final case class ReferenceObjectProperty(schema: JsonSchema[_])
    extends SchemaProperty[String] {
  override type S = ReferenceObjectProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): ReferenceObjectProperty = ReferenceObjectProperty(
    ZeroSchema()
  )

  override def toJson()(implicit p: JsonoidParams): JObject = Nil

  override def toString: String = "ReferenceObjectProperty(…)"

  override def unionMerge(
      otherProp: ReferenceObjectProperty
  )(implicit p: JsonoidParams): ReferenceObjectProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }

  override def mergeValue(
      otherObject: String
  )(implicit p: JsonoidParams): ReferenceObjectProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }
}
