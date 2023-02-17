package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object ProductSchema {
  def apply(
      value: JsonSchema[_]
  )(implicit p: JsonoidParams): ProductSchema = {
    ProductSchema(
      SchemaProperties
        .empty[JsonSchema[_]]
        .replaceProperty(
          ProductSchemaTypesProperty(AnySchema(), List(value), List(1))
        )
    )(p)
  }
}

final case class ProductSchema(
    override val properties: SchemaProperties[JsonSchema[_]]
)(implicit p: JsonoidParams)
    extends JsonSchema[JsonSchema[_]] {
  override def hasType: Boolean = false

  override val validTypes: Set[Class[_]] = Set.empty

  override def isValidType[S <: JValue](
      value: S
  ): Boolean = {
    // Any type is potentially valid
    true
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override val schemaType: String = null

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ProductSchema(otherProperties) =>
      val thisTypes = this.properties.get[ProductSchemaTypesProperty]
      val otherTypes = other.properties.get[ProductSchemaTypesProperty]
      val newBase = thisTypes.baseSchema.merge(otherTypes.baseSchema)(
        p.withER(EquivalenceRelations.KindEquivalenceRelation)
      )
      val thisType = thisTypes.productType
      val otherType = otherTypes.productType
      // TODO: Reconcile productType with mergeType
      (thisType, otherType) match {
        case (AllOf, AllOf) =>
          ProductSchema(
            properties.merge(otherProperties, mergeType)(
              p.withER(EquivalenceRelations.NonEquivalenceRelation)
            )
          )(p)
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
              p.withER(EquivalenceRelations.NonEquivalenceRelation)
            )
          )(p)
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
              p.withER(EquivalenceRelations.NonEquivalenceRelation)
            )
          )(p)
        case (_, _) =>
          ProductSchema(properties.merge(otherProperties, mergeType)(p))(p)
      }
  }

  override def merge(
      other: JsonSchema[_],
      mergeType: MergeType
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    other match {
      case prod: ProductSchema => this.mergeSameType(mergeType)(p)(prod)
      case zero: ZeroSchema    => this
      case _ if mergeType === Union =>
        ProductSchema(this.properties.mergeValue(other))(p)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(
      properties: SchemaProperties[JsonSchema[_]]
  ): ProductSchema = {
    val newSchema = ProductSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

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
  override def replaceWithSchema(
      pointer: String,
      replaceSchema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    val typesProp = properties.get[ProductSchemaTypesProperty]
    // Build a new type list that replaces the required type
    val newSchemas = pointer.split("/", 3) match {
      case Array(_) =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, "") =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, first) =>
        typesProp.schemaTypes.updated(first.toInt, replaceSchema)
      case Array(_, first, rest) =>
        val schema = typesProp.schemaTypes(first.toInt)
        typesProp.schemaTypes.updated(
          first.toInt,
          schema.replaceWithSchema("/" + rest, replaceSchema)
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

  override def isCompatibleWith(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Check if this type is compatible with anything in the product schema
    val types = properties.get[ProductSchemaTypesProperty].schemaTypes
    other match {
      // For two product schemas, find any compatible pair
      case ProductSchema(ps) =>
        val otherTypes = ps.get[ProductSchemaTypesProperty].schemaTypes
        otherTypes.forall(s =>
          types.exists(_.isCompatibleWith(s, recursive)(p))
        )

      // Otherwise check if the single type is compatible
      case _ => types.exists(_.isCompatibleWith(other, recursive)(p))
    }
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.Equals", "org.wartremover.warts.Recursion")
  )
  override def expandTo[S](other: JsonSchema[S]): JsonSchema[_] = {
    if (other.isInstanceOf[ProductSchema]) {
      copy(
        properties.expandTo(
          other.properties.asInstanceOf[SchemaProperties[JsonSchema[_]]]
        )
      )
    } else {
      // If the other schema is not a product schema, wrap it in one first
      expandTo(JsonSchema.buildProductSchema(AnySchema(), List(other), OneOf))
    }
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
)(implicit p: JsonoidParams)
    extends SchemaProperty[JsonSchema[_]] {
  override type S = ProductSchemaTypesProperty

  override def newDefault: ProductSchemaTypesProperty =
    ProductSchemaTypesProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    (productType.toJson -> schemas.map(_.toJson()(p)))

  override def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): ProductSchemaTypesProperty = {
    ProductSchemaTypesProperty(
      baseSchema.transformPropertiesWithPath(transformer, true, path),
      schemaTypes.zipWithIndex.map { case (schema, index) =>
        schema
          .transformPropertiesWithPath(transformer, true, s"${path}[$index]")
      },
      schemaCounts,
      productType
    )
  }

  override def intersectMerge(
      otherProp: ProductSchemaTypesProperty
  )(implicit p: JsonoidParams): ProductSchemaTypesProperty = {
    otherProp.schemaTypes.zipWithIndex.foldLeft(this) { case (s1, (s2, i)) =>
      s1.mergeWithCount(otherProp.schemaCounts(i), s2, Intersect)(p)
    }
  }

  override def unionMerge(
      otherProp: ProductSchemaTypesProperty
  )(implicit p: JsonoidParams): ProductSchemaTypesProperty = {
    otherProp.schemaTypes.zipWithIndex.foldLeft(this) { case (s1, (s2, i)) =>
      s1.mergeWithCount(
        otherProp.schemaCounts(i),
        s2,
        Union,
        otherProp.baseSchema
      )(p)
    }
  }

  def mergeWithCount(
      count: BigInt,
      schema: JsonSchema[_],
      mergeType: MergeType,
      otherBase: JsonSchema[_] = AnySchema()
  )(implicit p: JsonoidParams): ProductSchemaTypesProperty = {
    val newBase =
      baseSchema.merge(otherBase)(
        p.withER(EquivalenceRelations.KindEquivalenceRelation)
      )
    val newTypes = schemaTypes.zipWithIndex.find { case (s, i) =>
      s.schemaType === schema.schemaType
    } match {
      case Some((s, i)) if p.er.fuse(s, schema) =>
        (
          newBase,
          schemaTypes.updated(i, s.merge(schema, mergeType)(p)),
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
  )(implicit p: JsonoidParams): ProductSchemaTypesProperty = {
    mergeWithCount(1, value, Union)
  }

  def schemas: Seq[JsonSchema[_]] =
    schemaTypes
      .map(
        baseSchema.merge(_, Intersect)(
          p.withER(EquivalenceRelations.KindEquivalenceRelation)
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

  override def isCompatibleWith(
      other: ProductSchemaTypesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // The base schema and type of product must match
    val baseMatches = baseSchema.schemaType === other.baseSchema.schemaType
    val typeMatches = productType === other.productType

    // And there must be a compatible type for each alternative
    val allTypesCompatible = other.schemaTypes.forall(schema =>
      schemaTypes.exists(_.isCompatibleWith(schema, recursive))
    )

    baseMatches && typeMatches && allTypesCompatible
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.TraversableOps"
    )
  )
  override def expandTo(
      other: ProductSchemaTypesProperty
  ): ProductSchemaTypesProperty = {
    // Build a map from schema type to a list of (schema, index) pairs
    val types = schemaTypes.zipWithIndex.groupBy(_._1.schemaType)

    // Expand the base schema if needed
    val newBase = if (baseSchema.isCompatibleWith(other.baseSchema)) {
      baseSchema
    } else {
      baseSchema.expandTo(other.baseSchema)
    }

    // Build a mutable copy of the new schema types
    val newTypes = schemaTypes.to[ListBuffer]
    val hasAny = types.contains("any")

    if (types.contains("any")) {
      // We have any AnySchema here, so definitely compatible
      this
    } else {
      other.schemaTypes.foreach { s =>
        val matchingTypes = types.getOrElse(s.schemaType, List())
        if (matchingTypes.isEmpty && !hasAny) {
          // We have no matching type, so add a new one
          newTypes += s.copyWithReset.expandTo(s)
        } else {
          // Of the matching types, find the closest
          val (closestType, index) = matchingTypes.minBy(
            _._1.properties.findIncompatibilities(s.properties).length
          )

          // Expand the closest type to match
          newTypes(index) = closestType.expandTo(s)
        }
      }

      ProductSchemaTypesProperty(newBase, newTypes.toList)
    }
  }
}
