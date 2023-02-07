package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

final case class AnySchema(
    override val properties: SchemaProperties[Nothing] = SchemaProperties.empty
) extends JsonSchema[Nothing] {
  override val schemaType = "any"

  override def toJson()(implicit p: JsonoidParams): JObject = Nil

  // XXX Actually all types are valid, but we still have to specify this set
  override val validTypes: Set[Class[_]] = Set.empty

  override def isValidType[S <: JValue](value: S): Boolean = true

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case _ @other =>
    mergeType match {
      case Intersect => other
      case Union     => this
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[Nothing]): AnySchema = {
    val newSchema = AnySchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  override def isCompatibleWith(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = true
}
