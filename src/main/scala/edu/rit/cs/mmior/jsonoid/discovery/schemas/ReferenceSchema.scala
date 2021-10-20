package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

object ReferenceSchema {
  def apply(reference: String): ReferenceSchema = {
    val props = SchemaProperties.empty[String]
    props.add(ReferencePathProperty(reference))

    ReferenceSchema(props)
  }
}

final case class ReferenceSchema(
    override val properties: SchemaProperties[String]
) extends JsonSchema[String] {
  override val hasType = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  override val validTypes: Set[ClassTag[_ <: JValue]] = Set.empty

  override def isValidType[S <: JValue](value: S)(implicit
      tag: ClassTag[S]
  ): Boolean = {
    throw new UnsupportedOperationException("$ref cannot be type checked")
  }

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = Map.empty

  override def copy(properties: SchemaProperties[String]): ReferenceSchema =
    ReferenceSchema(properties)

  override def collectAnomalies[S <: JValue](
      value: S,
      path: String
  )(implicit tag: ClassTag[S]) = {
    Seq(
      Anomaly(path, "$ref cannot be checked for anomalies", Fatal)
    )
  }
}

final case class ReferencePathProperty(path: String)
    extends SchemaProperty[String, ReferencePathProperty] {
  override def toJson: JObject = ("$ref" -> path)

  override def merge(
      otherProp: ReferencePathProperty
  )(implicit er: EquivalenceRelation): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }

  override def mergeValue(
      otherPath: String
  )(implicit er: EquivalenceRelation): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }
}
