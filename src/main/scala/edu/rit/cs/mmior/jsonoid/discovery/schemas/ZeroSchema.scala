package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect._

import org.json4s.JsonDSL._
import org.json4s._

final case class ZeroSchema(
    override val properties: SchemaProperties[Nothing] = SchemaProperties.empty
) extends JsonSchema[Nothing] {
  override val schemaType = "zero"

  override def toJson: JObject = ("not" -> Nil)

  override val validTypes: Set[ClassTag[_ <: JValue]] = Set.empty

  override def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case _ @other =>
    other
  }

  override def copy(properties: SchemaProperties[Nothing]): ZeroSchema =
    ZeroSchema(properties)
}
