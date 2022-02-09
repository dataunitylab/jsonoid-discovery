package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object ProductSchema {
  def apply(
      value: JsonSchema[_]
  )(implicit er: EquivalenceRelation): ProductSchema = {
    ProductSchema(
      SchemaProperties
        .empty[JsonSchema[_]]
        .replaceProperty(ProductSchemaTypesProperty(List(value), List(1)))
    )(er)
  }
}

final case class ProductSchema(
    override val properties: SchemaProperties[JsonSchema[_]]
)(implicit er: EquivalenceRelation)
    extends JsonSchema[JsonSchema[_]] {
  override def hasType: Boolean = false

  override val validTypes: Set[ClassTag[_ <: JValue]] = Set.empty

  override def isValidType[S <: JValue](
      value: S
  )(implicit tag: ClassTag[S]): Boolean = {
    // Any type is potentially valid
    true
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      val allThis = this.properties.get[ProductSchemaTypesProperty].all
      val allOther = other.properties.get[ProductSchemaTypesProperty].all
      (allThis, allOther) match {
        case (true, true) =>
          ProductSchema(
            properties.merge(otherProperties)(
              EquivalenceRelations.NonEquivalenceRelation
            )
          )(er)
        case (true, false) =>
          val newProps = SchemaProperties.empty[JsonSchema[_]]
          newProps.add(ProductSchemaTypesProperty(List(other), List(1), false))
          ProductSchema(
            properties.merge(newProps)(
              EquivalenceRelations.NonEquivalenceRelation
            )
          )(er)
        case (false, true) =>
          val newProps = SchemaProperties.empty[JsonSchema[_]]
          newProps.add(ProductSchemaTypesProperty(List(this), List(1), false))
          ProductSchema(
            properties.merge(newProps)(
              EquivalenceRelations.NonEquivalenceRelation
            )
          )(er)
        case (false, false) =>
          ProductSchema(properties.merge(otherProperties)(er))(er)
      }
  }

  override def merge(
      other: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    other match {
      case prod: ProductSchema => this.mergeSameType()(er)(prod)
      case zero: ZeroSchema    => this
      case _                   => ProductSchema(this.properties.mergeValue(other))(er)
    }
  }

  override def copy(
      properties: SchemaProperties[JsonSchema[_]]
  ): ProductSchema =
    ProductSchema(properties)

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: String): Option[JsonSchema[_]] = {
    val schemas = properties.get[ProductSchemaTypesProperty].schemaTypes
    pointer.split("/", 3) match {
      case Array(_)        => None
      case Array(_, "")    => Some(this)
      case Array(_, first) => Some(schemas(first.toInt))
      case Array(_, first, rest) =>
        schemas(first.toInt).findByPointer("/" + rest)
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  override def replaceWithReference(
      pointer: String,
      reference: String,
      obj: Option[JsonSchema[_]]
  ): JsonSchema[_] = {
    val typesProp = properties.get[ProductSchemaTypesProperty]
    // Build a new type list that replaces the required type
    val newSchemas = pointer.split("/", 3) match {
      case Array(_) =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, "") =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, first) =>
        typesProp.schemaTypes.updated(
          first.toInt,
          ReferenceSchema(reference, obj)
        )
      case Array(_, first, rest) =>
        val schema = typesProp.schemaTypes(first.toInt)
        typesProp.schemaTypes.updated(
          first.toInt,
          schema.replaceWithReference("/" + rest, reference, obj)
        )
    }

    val typeProp =
      ProductSchemaTypesProperty(newSchemas, typesProp.schemaCounts)
    val newSchema = ProductSchema(this.properties.replaceProperty(typeProp))
    newSchema.definitions ++= this.definitions
    newSchema
  }
}

final case class ProductSchemaTypesProperty(
    val schemaTypes: List[JsonSchema[_]] = List.empty[JsonSchema[_]],
    val schemaCounts: List[BigInt] = List.empty[BigInt],
    val all: Boolean = false
)(implicit er: EquivalenceRelation)
    extends SchemaProperty[JsonSchema[_], ProductSchemaTypesProperty] {
  override def toJson: JObject =
    ((if (all) { "allOf" }
      else { "oneOf" }) -> schemaTypes.map(_.toJson))

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ProductSchemaTypesProperty = {
    ProductSchemaTypesProperty(
      schemaTypes.map { s =>
        transformer.applyOrElse(s, (x: JsonSchema[_]) => x)
      },
      schemaCounts,
      all
    )
  }

  override def merge(
      otherProp: ProductSchemaTypesProperty
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    otherProp.schemaTypes.zipWithIndex.foldLeft(this) { case (p, (s, i)) =>
      p.mergeWithCount(otherProp.schemaCounts(i), s)(er)
    }
  }

  def mergeWithCount(
      count: BigInt,
      schema: JsonSchema[_]
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    val newTypes = schemaTypes.zipWithIndex.find { case (s, i) =>
      s.schemaType === schema.schemaType
    } match {
      case Some((s, i)) if er.fuse(s, schema) =>
        (
          schemaTypes.updated(i, s.merge(schema)(er)),
          schemaCounts.updated(i, count + schemaCounts(i)),
          all
        )
      case _ => (schemaTypes :+ schema, schemaCounts :+ count, all)
    }
    (ProductSchemaTypesProperty.apply _).tupled(newTypes)
  }

  override def mergeValue(
      value: JsonSchema[_]
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    mergeWithCount(1, value)
  }

  override def collectAnomalies(value: JValue, path: String) = {
    // Check that there is some type that matches this value
    // TODO: Check frequency for outliers
    if (schemaTypes.exists(!_.isAnomalous(value, path))) {
      Seq.empty
    } else {
      Seq(Anomaly(path, f"no alternative found for ${value}", Fatal))
    }
  }
}
