package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials
import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

object ReferenceSchema {
  def apply(
      reference: String,
      obj: Option[JsonSchema[_]] = None
  ): ReferenceSchema = {
    val props = SchemaProperties.empty[String]
    props.add(ReferencePathProperty(reference))

    obj match {
      case Some(schemaObj) => props.add(ReferenceObjectProperty(schemaObj))
      case None            =>
    }

    ReferenceSchema(props)
  }
}

final case class ReferenceSchema(
    override val properties: SchemaProperties[String]
) extends JsonSchema[String] {
  override val hasType = false

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  override val validTypes: Set[ClassTag[_ <: JValue]] = Set.empty

  override def isValidType[S <: JValue](value: S)(implicit
      tag: ClassTag[S]
  ): Boolean = {
    throw new UnsupportedOperationException("$ref cannot be type checked")
  }

  override def mergeSameType(mergeType: MergeType)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = Map.empty

  override def copy(properties: SchemaProperties[String]): ReferenceSchema =
    ReferenceSchema(properties)

  override def collectAnomalies[S <: JValue](
      value: S,
      path: String
  )(implicit tag: ClassTag[S]) = {
    Seq(
      Anomaly(path, "$ref cannot be checked for anomalies", Fatal)
    )
  }
}

final case class ReferencePathProperty(path: String)
    extends SchemaProperty[String, ReferencePathProperty] {
  override def toJson: JObject = ("$ref" -> path)

  override def unionMerge(
      otherProp: ReferencePathProperty
  )(implicit er: EquivalenceRelation): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }

  override def mergeValue(
      otherPath: String
  )(implicit er: EquivalenceRelation): ReferencePathProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }
}

final case class ReferenceObjectProperty(schema: JsonSchema[_])
    extends SchemaProperty[String, ReferenceObjectProperty] {
  override def toJson: JObject = Nil

  override def toString: String = "ReferenceObjectProperty(…)"

  override def unionMerge(
      otherProp: ReferenceObjectProperty
  )(implicit er: EquivalenceRelation): ReferenceObjectProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }

  override def mergeValue(
      otherObject: String
  )(implicit er: EquivalenceRelation): ReferenceObjectProperty = {
    throw new UnsupportedOperationException("$ref cannot be merged")
  }
}
