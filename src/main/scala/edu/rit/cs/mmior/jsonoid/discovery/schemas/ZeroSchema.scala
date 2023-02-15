package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

final case class ZeroSchema(
    override val properties: SchemaProperties[Nothing] = SchemaProperties.empty
) extends JsonSchema[Nothing] {
  override val schemaType = "zero"

  override def toJson()(implicit p: JsonoidParams): JObject = ("not" -> Nil)

  override val validTypes: Set[Class[_]] = Set.empty

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = { case _ @other =>
    mergeType match {
      case Intersect => this
      case Union     => other
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[Nothing]): ZeroSchema = {
    val newSchema = ZeroSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }
}
