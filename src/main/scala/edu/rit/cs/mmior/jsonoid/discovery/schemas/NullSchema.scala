package edu.rit.cs.mmior.jsonoid.discovery
package schemas


case class NullSchema(override val properties: SchemaProperties[Nothing] = SchemaProperties.empty) extends JsonSchema[Nothing] {
  override val schemaType = "null"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NullSchema(otherProperties) =>
      NullSchema(properties.merge(otherProperties))
  }
}
