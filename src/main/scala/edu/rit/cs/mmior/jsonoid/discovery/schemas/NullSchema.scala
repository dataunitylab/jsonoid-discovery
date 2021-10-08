package edu.rit.cs.mmior.jsonoid.discovery
package schemas

final case class NullSchema(
    override val properties: SchemaProperties[Nothing] =
      SchemaProperties.empty[Nothing]
) extends JsonSchema[Nothing] {
  override val schemaType = "null"

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NullSchema(otherProperties) =>
      NullSchema(properties.merge(otherProperties))
  }

  override def copy(properties: SchemaProperties[Nothing]): NullSchema =
    NullSchema(properties)
}
