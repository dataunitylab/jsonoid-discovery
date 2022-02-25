package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.language.existentials
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
        .replaceProperty(
          ProductSchemaTypesProperty(AnySchema(), List(value), List(1))
        )
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

  override def mergeSameType(mergeType: MergeType)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      val thisTypes = this.properties.get[ProductSchemaTypesProperty]
      val otherTypes = other.properties.get[ProductSchemaTypesProperty]
      val newBase = thisTypes.baseSchema.merge(otherTypes.baseSchema)(
        EquivalenceRelations.KindEquivalenceRelation
      )
      val thisType = thisTypes.productType
      val otherType = otherTypes.productType
      // TODO: Reconcile productType with mergeType
      (thisType, otherType) match {
        case (AllOf, AllOf) =>
          ProductSchema(
            properties.merge(otherProperties, mergeType)(
              EquivalenceRelations.NonEquivalenceRelation
            )
          )(er)
        case (AllOf, t) =>
          val newType = mergeType match {
            case Union     => t
            case Intersect => AllOf
          }
          val newProps = SchemaProperties.empty[JsonSchema[_]]
          newProps.add(
            ProductSchemaTypesProperty(newBase, List(other), List(1), newType)
          )
          ProductSchema(
            properties.merge(newProps, mergeType)(
              EquivalenceRelations.NonEquivalenceRelation
            )
          )(er)
        case (t, AllOf) =>
          val newType = mergeType match {
            case Union     => t
            case Intersect => AllOf
          }
          val newProps = SchemaProperties.empty[JsonSchema[_]]
          newProps.add(
            ProductSchemaTypesProperty(newBase, List(this), List(1), newType)
          )
          ProductSchema(
            properties.merge(newProps, mergeType)(
              EquivalenceRelations.NonEquivalenceRelation
            )
          )(er)
        case (_, _) =>
          ProductSchema(properties.merge(otherProperties, mergeType)(er))(er)
      }
  }

  override def merge(
      other: JsonSchema[_],
      mergeType: MergeType
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    other match {
      case prod: ProductSchema => this.mergeSameType(mergeType)(er)(prod)
      case zero: ZeroSchema    => this
      case _ if mergeType === Union =>
        ProductSchema(this.properties.mergeValue(other))(er)
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
      ProductSchemaTypesProperty(
        typesProp.baseSchema,
        newSchemas,
        typesProp.schemaCounts
      )
    val newSchema = ProductSchema(this.properties.replaceProperty(typeProp))
    newSchema.definitions ++= this.definitions
    newSchema
  }
}

sealed trait ProductType {
  def toJson: String
}
case object AnyOf extends ProductType {
  override val toJson = "anyOf"
}
case object AllOf extends ProductType {
  override val toJson = "allOf"
}
case object OneOf extends ProductType {
  override val toJson = "oneOf"
}

final case class ProductSchemaTypesProperty(
    val baseSchema: JsonSchema[_] = AnySchema(),
    val schemaTypes: List[JsonSchema[_]] = List.empty[JsonSchema[_]],
    val schemaCounts: List[BigInt] = List.empty[BigInt],
    val productType: ProductType = OneOf
)(implicit er: EquivalenceRelation)
    extends SchemaProperty[JsonSchema[_], ProductSchemaTypesProperty] {
  override def toJson: JObject = (productType.toJson -> schemas.map(_.toJson))

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ProductSchemaTypesProperty = {
    ProductSchemaTypesProperty(
      transformer.applyOrElse(baseSchema, (x: JsonSchema[_]) => x),
      schemaTypes.map { s =>
        transformer.applyOrElse(s, (x: JsonSchema[_]) => x)
      },
      schemaCounts,
      productType
    )
  }

  override def intersectMerge(
      otherProp: ProductSchemaTypesProperty
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    otherProp.schemaTypes.zipWithIndex.foldLeft(this) { case (p, (s, i)) =>
      p.mergeWithCount(otherProp.schemaCounts(i), s, Intersect)(er)
    }
  }

  override def unionMerge(
      otherProp: ProductSchemaTypesProperty
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    otherProp.schemaTypes.zipWithIndex.foldLeft(this) { case (p, (s, i)) =>
      p.mergeWithCount(
        otherProp.schemaCounts(i),
        s,
        Union,
        otherProp.baseSchema
      )(er)
    }
  }

  def mergeWithCount(
      count: BigInt,
      schema: JsonSchema[_],
      mergeType: MergeType,
      otherBase: JsonSchema[_] = AnySchema()
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    val newBase =
      baseSchema.merge(otherBase)(EquivalenceRelations.KindEquivalenceRelation)
    val newTypes = schemaTypes.zipWithIndex.find { case (s, i) =>
      s.schemaType === schema.schemaType
    } match {
      case Some((s, i)) if er.fuse(s, schema) =>
        (
          newBase,
          schemaTypes.updated(i, s.merge(schema, mergeType)(er)),
          schemaCounts.updated(
            i,
            (mergeType match {
              case Union     => count + schemaCounts(i)
              case Intersect => count.min(schemaCounts(i))
            })
          ),
          productType
        )
      case _ =>
        mergeType match {
          case Union =>
            (newBase, schemaTypes :+ schema, schemaCounts :+ count, productType)
          case Intersect => (newBase, schemaTypes, schemaCounts, productType)
        }
    }
    (ProductSchemaTypesProperty.apply _).tupled(newTypes)
  }

  override def mergeValue(
      value: JsonSchema[_]
  )(implicit er: EquivalenceRelation): ProductSchemaTypesProperty = {
    mergeWithCount(1, value, Union)
  }

  def schemas: Seq[JsonSchema[_]] =
    schemaTypes
      .map(
        baseSchema.merge(_, Intersect)(
          EquivalenceRelations.KindEquivalenceRelation
        )
      )
      .toSeq

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    // Check that there is some type that matches this value
    // TODO: Check frequency for outliers
    val notAnomalous = schemas.map(!_.isAnomalous(value, path)(tag))
    val isValid = productType match {
      case AllOf => notAnomalous.forall(identity)
      case AnyOf => notAnomalous.exists(identity)
      case OneOf => notAnomalous.count(identity) === 1
    }
    if (isValid) {
      Seq.empty
    } else {
      Seq(Anomaly(path, f"no alternative found for ${value}", Fatal))
    }
  }
}
