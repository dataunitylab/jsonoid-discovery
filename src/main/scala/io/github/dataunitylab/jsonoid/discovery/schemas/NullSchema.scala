package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

import org.json4s._
import org.json4s.jackson.JsonMethods._

import Helpers._

/** Represents `null` in JSON Schema. */
final case class NullSchema(
    override val properties: SchemaProperties[Nothing] =
      SchemaProperties.empty[Nothing]
) extends JsonSchema[Nothing] {
  override val schemaType = "null"

  override val validTypes: Set[Class[_]] = Set.empty

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NullSchema(otherProperties) =>
      NullSchema(properties.merge(otherProperties, mergeType))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[Nothing]): NullSchema = {
    val newSchema = NullSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def isValidType[S <: JValue](value: S): Boolean = value == JNull

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def isSubsetOf(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    other match {
      case _: NullSchema => true
      case ps: ProductSchema =>
        val schemaTypes =
          ps.properties.get[ProductSchemaTypesProperty].schemaTypes
        schemaTypes.map(_.schemaType).exists(_ === "null")
      case _ => false
    }
  }

  override def collectAnomalies[S <: JValue](
      value: S,
      path: String
  )(implicit p: JsonoidParams, t: ClassTag[S]): Seq[Anomaly] = {
    if (value.isInstanceOf[JNull.type])
      Seq.empty
    else
      // We consider anomlies to be at the `Info` level since it's unlikely
      // to be a problem if a value that we expected to be null is not
      //
      // Note that we override this behaviour in [[ProductSchema]] since
      // there we are explicitly expecting some non-null value
      Seq(
        Anomaly(
          path,
          f"${compact(render(value))} is not null",
          AnomalyLevel.Info
        )
      )
  }
}
