package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

final case class AnySchema(
    override val properties: SchemaProperties[Nothing] = SchemaProperties.empty
) extends JsonSchema[Nothing] {
  override val schemaType = "any"

  override def toJson: JObject = Nil

  // XXX Actually all types are valid, but we still have to specify this set
  override val validTypes: Set[Class[_]] = Set.empty

  override def isValidType[S <: JValue](value: S): Boolean = true

  override def mergeSameType(mergeType: MergeType)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case _ @other =>
    mergeType match {
      case Intersect => other
      case Union     => this
    }
  }

  override def copy(properties: SchemaProperties[Nothing]): AnySchema =
    AnySchema(properties)
}
