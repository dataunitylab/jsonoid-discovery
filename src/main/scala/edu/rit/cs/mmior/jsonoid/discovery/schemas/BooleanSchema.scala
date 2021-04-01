package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._

object BooleanSchema {
  def apply(value: Boolean): BooleanSchema = {
    BooleanSchema(SchemaProperties().merge(value))
  }
}

case class BooleanSchema(
    override val properties: SchemaProperties[Boolean] = SchemaProperties.empty
) extends JsonSchema[Boolean] {
  override val schemaType = "string"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ BooleanSchema(otherProperties) =>
      BooleanSchema(properties.merge(otherProperties))
  }
}
