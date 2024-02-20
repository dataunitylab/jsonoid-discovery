package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.reflect.ClassTag

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._
import utils.JsonPointer

object ProductSchema {
  def apply(
      value: JsonSchema[_]
  )(implicit p: JsonoidParams): ProductSchema = product(List(value))(p)

  def product(
      schemas: List[JsonSchema[_]],
      productType: ProductType = OneOf
  )(implicit p: JsonoidParams): ProductSchema = {
    ProductSchema(
      SchemaProperties
        .empty[JsonSchema[_]]
        .replaceProperty(
          ProductSchemaTypesProperty(
            AnySchema(),
            schemas,
            List.fill(schemas.size)(1),
            productType
          )
        )
    )
  }
}

/** Represents `allOf`, `anyOf`, and `oneOf` in JSON Schema.
  */
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
      case zero: ZeroSchema    => if (mergeType === Union) this else zero
      case any: AnySchema      => if (mergeType === Union) any else this
      case _ if mergeType === Union =>
        ProductSchema(this.properties.mergeValue(other))(p)
      case _ =>
        throw new UnsupportedOperationException(
          "Merge with product schema not supported"
        )
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
  override def findByPointer(pointer: JsonPointer): Option[JsonSchema[_]] = {
    val schemas = properties.get[ProductSchemaTypesProperty].schemaTypes
    pointer.parts match {
      case Nil         => None
      case List("")    => Some(this)
      case List(first) => Some(schemas(first.toInt))
      case (first :: rest) =>
        schemas(first.toInt).findByPointer(JsonPointer(rest))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByInexactPointer(
      pointer: JsonPointer
  ): Seq[JsonSchema[_]] = {
    val schemas = properties.get[ProductSchemaTypesProperty].schemaTypes
    schemas.foldLeft(Seq.empty[JsonSchema[_]])(
      _ ++ _.findByInexactPointer(pointer)
    )
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  override def replaceWithSchema(
      pointer: JsonPointer,
      replaceSchema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    val typesProp = properties.get[ProductSchemaTypesProperty]
    // Build a new type list that replaces the required type
    val newSchemas = pointer.parts match {
      case Nil | List("") =>
        throw new IllegalArgumentException("Invalid path for reference")
      case List(first) =>
        typesProp.schemaTypes.updated(first.toInt, replaceSchema)
      case (first :: rest) =>
        val schema = typesProp.schemaTypes(first.toInt)
        typesProp.schemaTypes.updated(
          first.toInt,
          schema.replaceWithSchema(JsonPointer(rest), replaceSchema)
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

  override def isSubsetOf(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    other match {
      case AnySchema(_) => true

      // For two product schemas, make sure there are compatible pairs
      case ProductSchema(ps) =>
        val types = properties.get[ProductSchemaTypesProperty].schemaTypes
        val otherTypes = ps.get[ProductSchemaTypesProperty].schemaTypes
        types.forall(s => otherTypes.exists(s.isSubsetOf(_, recursive)(p)))

      // Otherwise, we can't be compatible
      case _ => false
    }
  }

  def isSupersetOf(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    val types = properties.get[ProductSchemaTypesProperty].schemaTypes

    other match {
      // XXX Technically this could be true if this schem is a
      //     product of all other basic types with no restrictions,
      //     but we consider this a very unlikely case.
      case AnySchema(_) => false

      // For two product schemas, find any compatible pair
      case ProductSchema(ps) =>
        val otherTypes = ps.get[ProductSchemaTypesProperty].schemaTypes
        otherTypes.forall(s => types.exists(s.isSubsetOf(_, recursive)(p)))

      // Otherwise check if the single type is compatible
      case _ => types.exists(other.isSubsetOf(_, recursive)(p))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def expandTo[S](other: Option[JsonSchema[S]]): JsonSchema[_] = {
    other match {
      case Some(ps: ProductSchema) =>
        copy(properties.expandTo(Some(ps.properties)))
      case Some(otherSchema) =>
        // If the other schema is not a product schema, wrap it in one first
        expandTo(
          Some(
            JsonSchema.buildProductSchema(AnySchema(), List(otherSchema), OneOf)
          )
        )
      case None => this
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def entropy(implicit p: JsonoidParams): Option[Long] = {
    val schemaTypes = properties.get[ProductSchemaTypesProperty]
    val baseEntropy = schemaTypes.baseSchema.entropy
    val schemaEntropies = schemaTypes.schemaTypes.map(_.entropy)
    if (
      baseEntropy.isDefined && schemaTypes.productType === OneOf && schemaEntropies
        .forall(_.isDefined)
    ) {
      Some(baseEntropy.get * schemaEntropies.map(_.get).sum)
    } else {
      None
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

/** The types of all values in a product schema.
  *
  * @constructor Create a new product schema types property.
  * @param baseSchema a schema which all values in the product schema are compatible with
  * @param schemaTypes the types of each value in the product schema
  * @param schemaCounts the number of values in each type in the product schema
  * @param productType the type of the product schema
  */
final case class ProductSchemaTypesProperty(
    val baseSchema: JsonSchema[_] = AnySchema(),
    val schemaTypes: List[JsonSchema[_]] = List.empty[JsonSchema[_]],
    val schemaCounts: List[BigInt] = List.empty[BigInt],
    val productType: ProductType = OneOf
)(implicit p: JsonoidParams)
    extends SchemaProperty[JsonSchema[_]] {
  override type S = ProductSchemaTypesProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): ProductSchemaTypesProperty =
    ProductSchemaTypesProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    (productType.toJson -> schemas.map(_.toJson()(p)))

  override def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): ProductSchemaTypesProperty = {
    ProductSchemaTypesProperty(
      baseSchema.transformPropertiesWithInexactPath(transformer, true, path),
      schemaTypes.zipWithIndex.map { case (schema, index) =>
        schema
          .transformPropertiesWithInexactPath(transformer, true, path)
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
      (s.schemaType === schema.schemaType) || (s.isNumeric && schema.isNumeric)
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

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Option2Iterable",
      "org.wartremover.warts.TraversableOps"
    )
  )
  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    // Check that there is some type that matches this value
    // TODO: Check frequency for outliers
    val maxAnomalyLevels = schemas.map { s =>
      // Override the null anomaly handling behavior to consider any
      // value as anomalous for an null schema since here we require
      // that there is a compatible type somewhere
      if (s.isInstanceOf[NullSchema] && !value.isInstanceOf[JNull.type])
        Some(AnomalyLevel.Fatal)
      else
        s.maxAnomalyLevel(value, path)
    }
    productType match {
      // All schemas must have no anomalies or only info level
      case AllOf =>
        if (
          maxAnomalyLevels.forall(
            _.map(_.order).getOrElse(-1) <= AnomalyLevel.Info.order
          )
        ) {
          Seq.empty
        } else {
          val maxLevel = maxAnomalyLevels.flatten.max
          Seq(Anomaly(path, f"failed match for ${value} in schema", maxLevel))
        }

      // At least one schema must have no anomalies or only info level
      case AnyOf =>
        if (
          maxAnomalyLevels.exists(
            _.map(_.order).getOrElse(-1) <= AnomalyLevel.Info.order
          )
        ) {
          Seq.empty
        } else {
          val maxLevel = maxAnomalyLevels.flatten.max
          Seq(Anomaly(path, f"failed match for ${value} in schema", maxLevel))
        }

      // Info anomalies are fine in multiple schemas
      case OneOf =>
        val matchingCount =
          maxAnomalyLevels.count(_.map(_ <= AnomalyLevel.Info).getOrElse(true))
        if (matchingCount === 1) {
          Seq.empty
        } else if (matchingCount === 0) {
          // We take the lowest anomaly level from each schema
          // since this is the schema with the closest match
          val minAnomalyLevel = maxAnomalyLevels.flatten.min
          Seq(Anomaly(path, f"no matches found for ${value}", minAnomalyLevel))
        } else {
          Seq(
            Anomaly(
              path,
              f"multiple matches found for ${value}",
              AnomalyLevel.Fatal
            )
          )
        }
    }
  }

  override def isSubsetOf(
      other: ProductSchemaTypesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // The base schema and type of product must match
    val baseMatches = baseSchema.isSubsetOf(other.baseSchema, recursive)
    val typeMatches = productType === other.productType

    // And there must be a compatible type for each alternative
    val allTypesCompatible = schemaTypes.forall(schema =>
      other.schemaTypes.exists(schema.isSubsetOf(_, recursive))
    )

    baseMatches && typeMatches && allTypesCompatible
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.TraversableOps"
    )
  )
  override def expandTo(
      other: Option[ProductSchemaTypesProperty]
  ): ProductSchemaTypesProperty = {
    // Build a map from schema type to a list of (schema, index) pairs
    val types = schemaTypes.zipWithIndex.groupBy(_._1.schemaType)

    // Expand the base schema if needed
    val newBase =
      if (other.isDefined && baseSchema.isSubsetOf(other.get.baseSchema))
        baseSchema
      else if (other.isDefined)
        baseSchema.expandTo(Some(other.get.baseSchema))
      else
        baseSchema.expandTo(None)

    // Build a mutable copy of the new schema types
    val newTypes = schemaTypes.to(ListBuffer)
    val hasAny = types.contains("any")

    if (types.contains("any")) {
      // We have any AnySchema here, so definitely compatible
      this
    } else {
      other.map(_.schemaTypes).getOrElse(List.empty).foreach { s =>
        val matchingTypes = types.getOrElse(s.schemaType, List())
        if (matchingTypes.isEmpty && !hasAny) {
          // We have no matching type, so add a new one
          newTypes += s.copyWithReset().expandTo(Some(s))
        } else {
          // Of the matching types, find the closest
          val (closestType, index) = matchingTypes.minBy(
            _._1.properties.findIncompatibilities(s.properties).length
          )

          // Expand the closest type to match
          newTypes(index) = closestType.expandTo(Some(s))
        }
      }

      ProductSchemaTypesProperty(newBase, newTypes.toList)
    }
  }
}
