package edu.rit.cs.mmior.jsonoid.discovery
package schemas

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

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = Map.empty

  override def copy(properties: SchemaProperties[String]): ReferenceSchema =
    ReferenceSchema(properties)
}

final case class ReferencePathProperty(path: String)
    extends SchemaProperty[String, ReferencePathProperty] {
  override def toJson: JObject = ("$ref" -> path)

  override def merge(
      otherProp: ReferencePathProperty
  ): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }

  override def mergeValue(otherPath: String): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }
}
