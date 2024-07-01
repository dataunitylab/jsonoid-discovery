package io.github.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

/** Represents `{"not": {}}` in JSON Schema or the schema which admits no
  * values.
  */
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

  // Compatibility doesn't really even make sense here, so go with false
  override def isSubsetOf(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = false

  // The only thing
  override def expandTo[S](other: Option[JsonSchema[S]]): JsonSchema[_] = {
    other match {
      // No need to expand if this is also a ZeroSchema
      case Some(_: ZeroSchema) => this

      // Expand a new instance of the type of the other schema
      case Some(other) =>
        other.copy(other.properties).copyWithReset().expandTo(Some(other))

      // The only thing we can do is go to AnySchema
      case None => AnySchema()
    }
  }
}
