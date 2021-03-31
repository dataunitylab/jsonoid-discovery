package edu.rit.cs.mmior.jsonoid.discovery
package schemas


case class ZeroSchema(override val properties: SchemaProperties[Nothing] = SchemaProperties.empty) extends JsonSchema[Nothing] {
  override val schemaType = "zero"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    other => other
  }
}
