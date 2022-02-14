package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect._

import org.json4s.JsonDSL._
import org.json4s._

final case class AnySchema(
    override val properties: SchemaProperties[Nothing] = SchemaProperties.empty
) extends JsonSchema[Nothing] {
  override val schemaType = "any"

  override def toJson: JObject = Nil

  // XXX Actually all types are valid, but we still have to specify this set
  override val validTypes: Set[ClassTag[_ <: JValue]] = Set.empty

  override def isValidType[S <: JValue](value: S)(implicit
      tag: ClassTag[S]
  ): Boolean = true

  override def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case _ @other =>
    this
  }

  override def copy(properties: SchemaProperties[Nothing]): AnySchema =
    AnySchema(properties)
}
