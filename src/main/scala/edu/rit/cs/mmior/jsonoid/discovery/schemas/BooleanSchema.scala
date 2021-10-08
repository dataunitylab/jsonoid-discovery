package edu.rit.cs.mmior.jsonoid.discovery
package schemas

object BooleanSchema {
  def apply(value: Boolean): BooleanSchema = {
    BooleanSchema(
      BooleanSchema.AllProperties.mergeValue(value)(
        EquivalenceRelations.KindEquivalenceRelation
      )
    )
  }

  val MinProperties: SchemaProperties[Boolean] =
    SchemaProperties.empty[Boolean]

  val AllProperties: SchemaProperties[Boolean] =
    SchemaProperties.empty[Boolean]
}

final case class BooleanSchema(
    override val properties: SchemaProperties[Boolean] =
      BooleanSchema.AllProperties
) extends JsonSchema[Boolean] {
  override val schemaType = "boolean"

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ BooleanSchema(otherProperties) =>
      BooleanSchema(properties.merge(otherProperties))
  }

  override def copy(properties: SchemaProperties[Boolean]): BooleanSchema =
    BooleanSchema(properties)
}
