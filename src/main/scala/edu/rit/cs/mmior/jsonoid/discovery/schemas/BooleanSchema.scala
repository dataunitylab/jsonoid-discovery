package edu.rit.cs.mmior.jsonoid.discovery
package schemas

object BooleanSchema {
  def apply(value: Boolean): BooleanSchema = {
    BooleanSchema(BooleanSchema.initialProperties.mergeValue(value))
  }

  def initialProperties: SchemaProperties[Boolean] =
    SchemaProperties.empty[Boolean]
}

final case class BooleanSchema(
    override val properties: SchemaProperties[Boolean] =
      BooleanSchema.initialProperties
) extends JsonSchema[Boolean] {
  override val schemaType = "boolean"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ BooleanSchema(otherProperties) =>
      BooleanSchema(properties.merge(otherProperties))
  }

  override def copy(properties: SchemaProperties[Boolean]): BooleanSchema =
    BooleanSchema(properties)
}
