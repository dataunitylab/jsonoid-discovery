package edu.rit.cs.mmior.jsonoid.discovery
package schemas

final case class ZeroSchema(
    override val properties: SchemaProperties[Nothing] = SchemaProperties.empty
) extends JsonSchema[Nothing] {
  override val schemaType = "zero"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case _ @other => other
  }

  override def copy(properties: SchemaProperties[Nothing]): ZeroSchema =
    ZeroSchema(properties)
}
