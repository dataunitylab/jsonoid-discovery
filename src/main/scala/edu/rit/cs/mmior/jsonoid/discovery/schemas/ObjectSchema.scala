package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect._
import scala.util.matching.Regex

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object ObjectSchema {
  def apply(
      value: Map[String, JsonSchema[_]]
  )(implicit propSet: PropertySet): ObjectSchema = {
    ObjectSchema(
      propSet.objectProperties.mergeValue(value)(
        EquivalenceRelations.KindEquivalenceRelation
      )
    )
  }

  val AllProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(ObjectTypesProperty())
    props.add(FieldPresenceProperty())
    props.add(RequiredProperty())
    props.add(DependenciesProperty())

    props
  }

  val MinProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(ObjectTypesProperty())

    props
  }

  val SimpleProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(ObjectTypesProperty())
    props.add(RequiredProperty())
    props.add(DependenciesProperty())

    props
  }
}

final case class ObjectSchema(
    override val properties: SchemaProperties[Map[String, JsonSchema[_]]] =
      ObjectSchema.AllProperties
) extends JsonSchema[Map[String, JsonSchema[_]]] {
  override val schemaType = "object"

  override val validTypes: Set[ClassTag[_ <: JValue]] = Set(classTag[JObject])

  override def isValidType[S <: JValue](
      value: S
  )(implicit tag: ClassTag[S]): Boolean = {
    // We can't check the ClassTag here
    value.isInstanceOf[JObject]
  }

  override val staticProperties: JObject = ("additionalProperties" -> false)

  override def mergeSameType(mergeType: MergeType)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ObjectSchema(otherProperties) =>
      ObjectSchema(properties.merge(otherProperties, mergeType))
  }

  override def copy(
      properties: SchemaProperties[Map[String, JsonSchema[_]]]
  ): ObjectSchema =
    ObjectSchema(properties)

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: String): Option[JsonSchema[_]] = {
    val objectTypes = properties.get[ObjectTypesProperty].objectTypes
    pointer.split("/", 3) match {
      case Array(_)        => None
      case Array(_, "")    => Some(this)
      case Array(_, first) => objectTypes.get(first)
      case Array(_, first, rest) =>
        objectTypes.get(first) match {
          case Some(schema: JsonSchema[_]) => schema.findByPointer("/" + rest)
          case Some(_)                     => None
          case None                        => None
        }
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
    // Build a new type map that replaces the required type
    val objectTypes = properties.get[ObjectTypesProperty].objectTypes
    val newTypes = pointer.split("/", 3) match {
      case Array(_) | Array(_, "") =>
        throw new IllegalArgumentException("Invalid path for reference")
      case Array(_, first) =>
        objectTypes + (first -> ReferenceSchema(reference, obj))

      case Array(_, first, rest) =>
        objectTypes.get(first) match {
          case Some(schema: JsonSchema[_]) =>
            // Replace the type along the path with
            // one which has the replaced reference
            objectTypes + (first -> schema.replaceWithReference(
              "/" + rest,
              reference,
              obj
            ))
          case _ =>
            throw new IllegalArgumentException("Invalid path for reference")
        }
    }

    val typeProp = ObjectTypesProperty(newTypes)
    val newSchema = ObjectSchema(this.properties.replaceProperty(typeProp))
    newSchema.definitions ++= this.definitions
    newSchema
  }
}

final case class ObjectTypesProperty(
    objectTypes: Map[String, JsonSchema[_]] = Map.empty[String, JsonSchema[_]]
) extends SchemaProperty[Map[String, JsonSchema[_]], ObjectTypesProperty] {
  override def toJson: JObject = ("properties" -> objectTypes.map {
    case (propType, schema) => (propType -> schema.toJson)
  }) ~ ("additionalProperties" -> false)

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): ObjectTypesProperty = {
    ObjectTypesProperty(
      objectTypes.mapValues(transformer(_)).map(identity).toMap
    )
  }

  override def intersectMerge(
      otherProp: ObjectTypesProperty
  )(implicit er: EquivalenceRelation): ObjectTypesProperty = {
    val other = otherProp.objectTypes
    this.mergeValue(other, Intersect)
  }

  override def unionMerge(
      otherProp: ObjectTypesProperty
  )(implicit er: EquivalenceRelation): ObjectTypesProperty = {
    val other = otherProp.objectTypes
    this.mergeValue(other)
  }

  override def mergeValue(value: Map[String, JsonSchema[_]])(implicit
      er: EquivalenceRelation
  ) = mergeValue(value, Union)

  def mergeValue(
      value: Map[String, JsonSchema[_]],
      mergeType: MergeType
  )(implicit er: EquivalenceRelation): ObjectTypesProperty = {
    val mergedTypes = mergeType match {
      case Union     => objectTypes.keySet ++ value.keySet
      case Intersect => objectTypes.keySet & value.keySet
    }
    val merged =
      (objectTypes.toSeq ++ value.toSeq).filter(t => mergedTypes.contains(t._1))
    val grouped = merged.groupBy(_._1)
    val baseSchema = mergeType match {
      case Intersect => AnySchema()
      case Union     => ZeroSchema()
    }
    ObjectTypesProperty(
      // .map(identity) below is necessary to
      // produce a map which is serializable
      grouped
        .mapValues(
          _.map(_._2).fold(baseSchema)((a, b) => a.merge(b, mergeType))
        )
        .map(identity)
        .toMap
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JObject(fields) =>
        val fieldMap = fields.toMap
        val unknownFields = fieldMap.keySet -- objectTypes.keySet
        if (unknownFields.size > 0) {
          unknownFields
            .map(f => Anomaly(f"$path.$f", "found unknown field", Fatal))
            .toSeq
        } else {
          fieldMap.keySet
            .flatMap(key =>
              objectTypes(key).collectAnomalies(fieldMap(key), f"$path.$key")
            )
            .toSeq
        }
      case _ => Seq.empty
    }
  }
}

final case class PatternTypesProperty(
    patternTypes: Map[Regex, JsonSchema[_]] = Map.empty[Regex, JsonSchema[_]]
) extends SchemaProperty[Map[String, JsonSchema[_]], PatternTypesProperty] {
  override def toJson: JObject = ("patternProperties" -> patternTypes.map {
    case (pattern, schema) => (pattern.toString -> schema.toJson)
  })

  override def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): PatternTypesProperty = {
    PatternTypesProperty(
      patternTypes.mapValues(transformer(_)).map(identity).toMap
    )
  }

  override def intersectMerge(
      otherProp: PatternTypesProperty
  )(implicit er: EquivalenceRelation): PatternTypesProperty = {
    val other = otherProp.patternTypes
    this.mergeValueRegex(other, Intersect)
  }

  override def unionMerge(
      otherProp: PatternTypesProperty
  )(implicit er: EquivalenceRelation): PatternTypesProperty = {
    val other = otherProp.patternTypes
    this.mergeValueRegex(other, Union)
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit er: EquivalenceRelation): PatternTypesProperty = {
    val regexMap: Map[Regex, JsonSchema[_]] = value.map { case (k, v) =>
      (k.r, v)
    }.toMap
    mergeValueRegex(regexMap, Union)
  }

  def mergeValueRegex(
      value: Map[Regex, JsonSchema[_]],
      mergeType: MergeType
  )(implicit er: EquivalenceRelation): PatternTypesProperty = {
    val mergedTypes = mergeType match {
      case Union     => patternTypes.keySet ++ value.keySet
      case Intersect => patternTypes.keySet & value.keySet
    }
    val merged = (patternTypes.toSeq ++ value.toSeq).filter(t =>
      mergedTypes.contains(t._1)
    )
    val grouped = merged.groupBy(_._1)
    PatternTypesProperty(
      // .map(identity) below is necessary to
      // produce a map which is serializable
      grouped
        .mapValues(
          _.map(_._2).fold(ZeroSchema())((a, b) => a.merge(b, mergeType))
        )
        .map(identity)
        .toMap
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    throw new UnsupportedOperationException(
      "anomaly collection not supported for patternProperties"
    )
  }
}

final case class FieldPresenceProperty(
    fieldPresence: Map[String, BigInt] = Map.empty[String, BigInt],
    totalCount: BigInt = 0
) extends SchemaProperty[Map[String, JsonSchema[_]], FieldPresenceProperty] {
  override def toJson: JObject = ("fieldPresence" -> fieldPresence.map {
    case (key, count) => (key -> BigDecimal(count) / BigDecimal(totalCount))
  })

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def intersectMerge(
      otherProp: FieldPresenceProperty
  )(implicit er: EquivalenceRelation): FieldPresenceProperty = {
    val mergedTypes = fieldPresence.keySet & otherProp.fieldPresence.keySet
    val merged =
      (fieldPresence.toSeq ++ otherProp.fieldPresence.toSeq).filter(t =>
        mergedTypes.contains(t._1)
      )
    val grouped = merged.groupBy(_._1)
    FieldPresenceProperty(
      grouped.mapValues(_.map(_._2).min).map(identity).toMap,
      totalCount.min(otherProp.totalCount)
    )
  }

  override def unionMerge(
      otherProp: FieldPresenceProperty
  )(implicit er: EquivalenceRelation): FieldPresenceProperty = {
    val merged = fieldPresence.toSeq ++ otherProp.fieldPresence.toSeq
    val grouped = merged.groupBy(_._1)
    FieldPresenceProperty(
      grouped.mapValues(_.map(_._2).sum).map(identity).toMap,
      totalCount + otherProp.totalCount
    )
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit er: EquivalenceRelation): FieldPresenceProperty = {
    unionMerge(FieldPresenceProperty(value.mapValues(s => 1), 1))
  }
}

final case class RequiredProperty(
    required: Option[Set[String]] = None
) extends SchemaProperty[Map[String, JsonSchema[_]], RequiredProperty] {
  override def toJson: JObject = ("required" -> required)

  override def unionMerge(
      otherProp: RequiredProperty
  )(implicit er: EquivalenceRelation): RequiredProperty = {
    val other = otherProp.required
    RequiredProperty(intersectOrNone(other, required))
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit er: EquivalenceRelation): RequiredProperty = {
    RequiredProperty(intersectOrNone(Some(value.keySet), required))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JObject(fields) =>
        required match {
          case Some(requiredFields) =>
            (requiredFields -- fields.map(_._1).toSet)
              .map(f =>
                Anomaly(
                  f"$path.$f",
                  "missing required field",
                  Fatal
                )
              )
              .toSeq
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

object DependenciesProperty {
  val MaxProperties: Int = 50
}

final case class DependenciesProperty(
    totalCount: BigInt = 0,
    counts: Map[String, BigInt] = Map.empty,
    cooccurrence: Map[(String, String), BigInt] = Map.empty,
    overloaded: Boolean = false
) extends SchemaProperty[Map[String, JsonSchema[_]], DependenciesProperty] {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def toJson: JObject = {
    // Use cooccurrence count to check dependencies in both directions,
    // excluding cases where properties are required (count is totalCount)
    val dependencies = cooccurrence.toSeq
      .flatMap { case ((key1, key2), count) =>
        (if (
           counts(key1) == count && count != totalCount && counts(
             key2
           ) != totalCount
         ) {
           List((key1, key2))
         } else {
           List()
         }) ++ (if (
                  counts(key2) == count && count != totalCount && counts(
                    key1
                  ) != totalCount
                ) {
                  List((key2, key1))
                } else {
                  List()
                })
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .map(identity)

    if (dependencies.isEmpty) {
      Nil
    } else {
      ("dependentRequired" -> dependencies)
    }
  }

  override def unionMerge(
      otherProp: DependenciesProperty
  )(implicit er: EquivalenceRelation): DependenciesProperty = {
    if (overloaded || otherProp.overloaded) {
      DependenciesProperty(overloaded = true)
    } else {
      val mergedCounts = (counts.toSeq ++ otherProp.counts.toSeq)
        .groupBy(_._1)
        .mapValues(_.map(_._2).sum)
        .map(identity)
        .toMap
      val mergedCooccurrence =
        (cooccurrence.toSeq ++ otherProp.cooccurrence.toSeq)
          .groupBy(_._1)
          .mapValues(_.map(_._2).sum)
          .map(identity)
          .toMap
      DependenciesProperty(
        totalCount + otherProp.totalCount,
        mergedCounts,
        mergedCooccurrence
      )
    }
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit er: EquivalenceRelation): DependenciesProperty = {
    if (overloaded || value.size > DependenciesProperty.MaxProperties) {
      // If we have too many properties on any object, give up
      DependenciesProperty(overloaded = true)
    } else {
      val counts = value.keySet.map(_ -> BigInt(1)).toMap
      val cooccurrence = value.keySet.toSeq
        .combinations(2)
        .map(_.sorted match {
          case Seq(a, b) => (a, b) -> BigInt(1)
        })
        .toMap
      unionMerge(DependenciesProperty(1, counts, cooccurrence))
    }
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JObject(fields) =>
        // TODO: Check dependencies are satisfied
        Seq.empty
      case _ => Seq.empty
    }
  }
}
