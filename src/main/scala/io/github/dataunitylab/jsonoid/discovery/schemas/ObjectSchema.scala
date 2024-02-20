package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.collection.mutable.HashMap
import scala.reflect._
import scala.util.matching.Regex

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._
import utils.JsonPointer

object ObjectSchema {
  def apply(
      value: Map[String, JsonSchema[_]]
  )(implicit p: JsonoidParams): ObjectSchema = {
    ObjectSchema(
      p.propSet.objectProperties.mergeValue(value)(p)
    )(p)
  }

  /** Convert a serialized JSON value to an object schema object. */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Recursion"
    )
  )
  def fromJson(
      obj: JObject
  )(implicit p: JsonoidParams): ObjectSchema = {
    implicit val formats: Formats = DefaultFormats
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]

    (obj \ "additionalProperties") match {
      case JNothing =>
      case JBool(additionalProperties) =>
        props.add(AdditionalPropertiesProperty(Some(additionalProperties)))
      case _ =>
        throw new UnsupportedOperationException(
          "additionalProperties with non-Boolean schema not supported"
        )
    }

    if ((obj \ "not") =/= JNothing) {
      throw new UnsupportedOperationException("not isn't supported")
    }

    if ((obj \ "unevaluatedProperties") =/= JNothing) {
      throw new UnsupportedOperationException(
        "unevaluatedProperties isn't supported"
      )
    }

    // TODO Add support for dependencies
    if ((obj \ "dependencies") =/= JNothing) {
      throw new UnsupportedOperationException("dependencies not supported")
    }
    if ((obj \ "dependentRequired") =/= JNothing) {
      try {
        val deps = (obj \ "dependentRequired").extract[Map[String, Set[String]]]
        props.add(StaticDependenciesProperty(deps))
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }
    if ((obj \ "dependentSchemas") =/= JNothing) {
      throw new UnsupportedOperationException("dependentSchemas not supported")
    }

    val objProps = if ((obj \ "properties") =/= JNothing) {
      try {
        (obj \ "properties").extract[Map[String, JObject]]
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException => Map.empty
        // $COVERAGE-ON$
      }
    } else {
      Map.empty
    }
    val objTypes: Map[String, JsonSchema[_]] = objProps.map {
      op: Tuple2[String, JValue] =>
        (op._1 -> JsonSchema.fromJson(op._2))
    }.toMap

    val patternProps = if ((obj \ "patternProperties") =/= JNothing) {
      try {
        (obj \ "patternProperties").extract[Map[String, JObject]]
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException => Map.empty
        // $COVERAGE-ON$
      }
    } else {
      Map.empty
    }
    val patternTypes: Map[Regex, JsonSchema[_]] = patternProps.flatMap {
      pp: Tuple2[String, JValue] =>
        try {
          Some(
            (if (pp._1.isEmpty) ".*" else pp._1).r -> JsonSchema.fromJson(pp._2)
          )
        } catch {
          case e: java.util.regex.PatternSyntaxException => None
        }
    }.toMap

    val required =
      try {
        (obj \ "required").extract[Set[String]]
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException => Set.empty[String]
        // $COVERAGE-ON$
      }
    val reqProp = RequiredProperty(Some(required))

    props.add(ObjectTypesProperty(objTypes))
    if (!patternTypes.isEmpty) {
      props.add(PatternTypesProperty(patternTypes))
    }
    props.add(RequiredProperty(Some(required)))

    ObjectSchema(props)(p)
  }

  lazy val AllProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(AdditionalPropertiesProperty())
    props.add(ObjectTypesProperty())
    props.add(FieldPresenceProperty())
    props.add(RequiredProperty())
    props.add(DependenciesProperty())

    props
  }

  lazy val MinProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(AdditionalPropertiesProperty())
    props.add(ObjectTypesProperty())

    props
  }

  lazy val SimpleProperties: SchemaProperties[Map[String, JsonSchema[_]]] = {
    val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
    props.add(AdditionalPropertiesProperty())
    props.add(ObjectTypesProperty())
    props.add(RequiredProperty())
    props.add(DependenciesProperty())

    props
  }
}

/** Represents objects in JSON Schema.
  */
final case class ObjectSchema(
    override val properties: SchemaProperties[Map[String, JsonSchema[_]]] =
      ObjectSchema.AllProperties
)(implicit p: JsonoidParams)
    extends JsonSchema[Map[String, JsonSchema[_]]] {
  override val schemaType = "object"

  override val validTypes: Set[Class[_]] = Set(classOf[JObject])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ObjectSchema(otherProperties) =>
      ObjectSchema(properties.merge(otherProperties, mergeType))(p)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(
      properties: SchemaProperties[Map[String, JsonSchema[_]]]
  ): ObjectSchema = {
    val newSchema = ObjectSchema(properties)(p)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  override def findByPointer(pointer: JsonPointer): Option[JsonSchema[_]] = {
    val objectTypes = properties.get[ObjectTypesProperty].objectTypes
    pointer.parts match {
      case Nil         => None
      case List("")    => Some(this)
      case List(first) => objectTypes.get(first)
      case (first :: rest) =>
        objectTypes.get(first) match {
          case Some(schema: JsonSchema[_]) =>
            schema.findByPointer(JsonPointer(rest))
          case Some(_) => None
          case None    => None
        }
    }
  }

  override def findByInexactPointer(
      pointer: JsonPointer
  ): Seq[JsonSchema[_]] = {
    val objectTypes = properties.get[ObjectTypesProperty].objectTypes
    val pointerStr = pointer.toString
    pointerStr.split("/", 3) match {
      case Array(_)        => Seq()
      case Array(_, "")    => Seq(this)
      case Array(_, first) => objectTypes.get(first).toList
      case Array(_, first, rest) =>
        objectTypes.get(first) match {
          case Some(schema: JsonSchema[_]) =>
            schema.findByInexactPointer(JsonPointer(List(rest)))
          case _ => Seq()
        }
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion",
      "org.wartremover.warts.Return"
    )
  )
  override def replaceWithSchema(
      pointer: JsonPointer,
      replaceSchema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    // Build a new type map that replaces the required type
    val objectTypes = properties.get[ObjectTypesProperty].objectTypes
    val newTypes = pointer.parts match {
      case Nil | List("") =>
        return replaceSchema
      case List(first) =>
        objectTypes + (first -> replaceSchema)

      case (first :: rest) =>
        objectTypes.get(first) match {
          case Some(schema: JsonSchema[_]) =>
            // Replace the type along the path with
            // one which has the replaced schema
            objectTypes + (first -> schema.replaceWithSchema(
              JsonPointer(rest),
              replaceSchema
            ))
          case _ =>
            throw new IllegalArgumentException("Invalid path for replacement")
        }
    }

    val typeProp = ObjectTypesProperty(newTypes)
    val newSchema = ObjectSchema(this.properties.replaceProperty(typeProp))(p)
    newSchema.definitions ++= this.definitions
    newSchema
  }

  /** Returns a new schema with only the specified keys.
    *
    * @param keys a set of keys to include
    */
  def onlyKeys(keys: Set[String]): ObjectSchema = {
    val newProps = SchemaProperties(
      properties.properties.view
        .mapValues({ prop =>
          prop match {
            case p: DependenciesProperty       => p.onlyKeys(keys)
            case p: FieldPresenceProperty      => p.onlyKeys(keys)
            case p: ObjectTypesProperty        => p.onlyKeys(keys)
            case p: RequiredProperty           => p.onlyKeys(keys)
            case p: StaticDependenciesProperty => p.onlyKeys(keys)
            case _                             => prop
          }
        })
        .toMap
        .asInstanceOf[SchemaProperties.PropertyMap[Map[String, JsonSchema[_]]]]
    )

    ObjectSchema(newProps)(p)
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def entropy(implicit p: JsonoidParams): Option[Long] = {
    val additionalProperties = properties
      .getOrNone[AdditionalPropertiesProperty]
      .flatMap(_.overriddenAdditionalProperties)
      .getOrElse(p.additionalProperties)

    if (additionalProperties) {
      // This schema is open so it has infinite entropy
      None
    } else {
      val objectTypes = properties.get[ObjectTypesProperty].objectTypes
      val requiredProperties =
        properties
          .getOrNone[RequiredProperty]
          .flatMap(_.required)
          .getOrElse(Set.empty)
      val propertyEntropy = objectTypes.values.map(_.entropy)

      // TODO Entropy is reduced by dependencies

      // We can only calculate entropy if it is defined for all keys
      if (propertyEntropy.forall(_.isDefined)) {
        Some(objectTypes.map { case (key, schema) =>
          schema.entropy.get + (if (requiredProperties.contains(key)) 0 else 1)
        }.product)
      } else {
        None
      }
    }
  }
}

/** The types of all keys in an object schema.
  *
  * @constructor Create a new object types property.
  * @param objectTypes a map from object keys to schemas for the values
  */
final case class ObjectTypesProperty(
    objectTypes: Map[String, JsonSchema[_]] = Map.empty[String, JsonSchema[_]]
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = ObjectTypesProperty

  override def newDefault()(implicit p: JsonoidParams): ObjectTypesProperty =
    ObjectTypesProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("properties" -> objectTypes.map { case (propType, schema) =>
      (propType -> schema.toJson()(p))
    })

  def onlyKeys(keys: Set[String]): ObjectTypesProperty = {
    ObjectTypesProperty(objectTypes.view.filterKeys(keys.contains(_)).toMap)
  }

  override def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): ObjectTypesProperty = {
    ObjectTypesProperty(
      objectTypes
        .map { case (key, schema) =>
          key -> schema
            .transformPropertiesWithInexactPath(
              transformer,
              true,
              s"${path}.${key}"
            )
        }
        .map(identity)
        .toMap
    )
  }

  override def intersectMerge(
      otherProp: ObjectTypesProperty
  )(implicit p: JsonoidParams): ObjectTypesProperty = {
    val other = otherProp.objectTypes
    this.mergeValue(other, Intersect)
  }

  override def unionMerge(
      otherProp: ObjectTypesProperty
  )(implicit p: JsonoidParams): ObjectTypesProperty = {
    val other = otherProp.objectTypes
    this.mergeValue(other)(p)
  }

  override def mergeValue(value: Map[String, JsonSchema[_]])(implicit
      p: JsonoidParams
  ): ObjectTypesProperty = mergeValue(value, Union)(p)

  def mergeValue(
      value: Map[String, JsonSchema[_]],
      mergeType: MergeType
  )(implicit p: JsonoidParams): ObjectTypesProperty = {
    val mergedTypes = mergeType match {
      case Union     => objectTypes.keySet ++ value.keySet
      case Intersect => objectTypes.keySet & value.keySet
    }
    val merged =
      (objectTypes.toSeq ++ value.toSeq).filter(t => mergedTypes.contains(t._1))
    val grouped = merged.groupBy(_._1)
    val baseSchema: JsonSchema[_] = mergeType match {
      case Intersect => AnySchema()
      case Union     => ZeroSchema()
    }
    ObjectTypesProperty(
      // .map(identity) below is necessary to
      // produce a map which is serializable
      grouped.view
        .mapValues(
          _.map(_._2).fold(baseSchema)((a, b) => a.merge(b, mergeType))
        )
        .map(identity)
        .toMap
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JObject(fields) =>
        val fieldMap = fields.toMap
        val unknownFields = fieldMap.keySet -- objectTypes.keySet
        if (!p.additionalProperties && unknownFields.size > 0) {
          unknownFields
            .map(f =>
              Anomaly(f"$path.$f", "found unknown field", AnomalyLevel.Fatal)
            )
            .toSeq
        } else {
          fieldMap.keySet
            .intersect(objectTypes.keySet)
            .flatMap(key =>
              objectTypes(key).collectAnomalies(fieldMap(key), f"$path.$key")
            )
            .toSeq
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: ObjectTypesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    val overlapCompatible = objectTypes.keySet.forall(key =>
      other.objectTypes.get(key) match {
        case Some(schema) =>
          !recursive || objectTypes(key).isSubsetOf(schema)
        case None => true
      }
    )
    val newPropsCompatible = p.additionalProperties || objectTypes.keySet
      .subsetOf(other.objectTypes.keySet)

    overlapCompatible && newPropsCompatible
  }

  override def expandTo(
      other: Option[ObjectTypesProperty]
  ): ObjectTypesProperty = {
    val newTypes =
      objectTypes.keySet
        .union(other.map(_.objectTypes.keySet).getOrElse(Set.empty))
        .map { key =>
          (
            objectTypes.get(key),
            other.map(_.objectTypes.get(key)).getOrElse(None)
          ) match {
            case (Some(schema), Some(otherSchema)) =>
              key -> schema.expandTo(Some(otherSchema))
            case (Some(schema), None)      => key -> schema
            case (None, Some(otherSchema)) => key -> otherSchema
            case (None, None)              => key -> AnySchema()
          }
        }

    ObjectTypesProperty(newTypes.toMap)
  }
}

/** The types of all keys in an object schema which match given patterns.
  *
  * @constructor Create a new pattern types property.
  * @param objectTypes a map from patterns keys must match to schemas for the values
  */
final case class PatternTypesProperty(
    patternTypes: Map[Regex, JsonSchema[_]] = Map.empty[Regex, JsonSchema[_]]
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = PatternTypesProperty

  override def newDefault()(implicit p: JsonoidParams): PatternTypesProperty =
    PatternTypesProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("patternProperties" -> patternTypes.map { case (pattern, schema) =>
      (pattern.toString -> schema.toJson()(p))
    })

  override def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): PatternTypesProperty = {
    PatternTypesProperty(
      patternTypes
        .map { case (regex, schema) =>
          regex -> schema.transformPropertiesWithInexactPath(
            transformer,
            false,
            path + "." + regex.toString
          )
        }
        .map(identity)
        .toMap
    )
  }

  override def intersectMerge(
      otherProp: PatternTypesProperty
  )(implicit p: JsonoidParams): PatternTypesProperty = {
    val other = otherProp.patternTypes
    this.mergeValueRegex(other, Intersect)
  }

  override def unionMerge(
      otherProp: PatternTypesProperty
  )(implicit p: JsonoidParams): PatternTypesProperty = {
    val other = otherProp.patternTypes
    this.mergeValueRegex(other, Union)
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit p: JsonoidParams): PatternTypesProperty = {
    val regexMap: Map[Regex, JsonSchema[_]] = value.map { case (k, v) =>
      (k.r, v)
    }.toMap
    mergeValueRegex(regexMap, Union)
  }

  def mergeValueRegex(
      value: Map[Regex, JsonSchema[_]],
      mergeType: MergeType
  )(implicit p: JsonoidParams): PatternTypesProperty = {
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
      grouped.view
        .mapValues(
          _.map(_._2).fold(ZeroSchema())((a, b) => a.merge(b, mergeType))
        )
        .map(identity)
        .toMap
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JObject(fields) =>
        fields.flatMap { case (key, value) =>
          // Find a matching pattern schema if it exists
          val patternSchema = patternTypes
            .find { case (pattern, _) =>
              pattern.matches(key)
            }
            .map(_._2)

          // Either collect anomalies from the matching pattern
          // or report that there is no pattern that matches
          patternSchema match {
            case Some(schema) => schema.collectAnomalies(value, f"$path.$key")
            case None =>
              Seq(
                Anomaly(
                  f"$path.$key",
                  "found field not matching pattern",
                  AnomalyLevel.Fatal
                )
              )
          }
        }
      case _ => Seq.empty
    }
  }
}

/** Tracks what percentage of the time keys in an object schema are present.
  *
  * @constructor Create a new field presence property.
  * @param fieldPresence a map from keys to the number of times the key is present
  * @param totalCount the total number of observed objects
  */
final case class FieldPresenceProperty(
    fieldPresence: Map[String, BigInt] = Map.empty[String, BigInt],
    totalCount: BigInt = 0
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = FieldPresenceProperty

  override def newDefault()(implicit p: JsonoidParams): FieldPresenceProperty =
    FieldPresenceProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("fieldPresence" -> fieldPresence.map { case (key, count) =>
      (key -> BigDecimal(count) / BigDecimal(totalCount))
    })

  def onlyKeys(keys: Set[String]): FieldPresenceProperty = {
    FieldPresenceProperty(
      fieldPresence.view.filterKeys(keys.contains(_)).toMap,
      totalCount
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def intersectMerge(
      otherProp: FieldPresenceProperty
  )(implicit p: JsonoidParams): FieldPresenceProperty = {
    val mergedTypes = fieldPresence.keySet & otherProp.fieldPresence.keySet
    val merged =
      (fieldPresence.toSeq ++ otherProp.fieldPresence.toSeq).filter(t =>
        mergedTypes.contains(t._1)
      )
    val grouped = merged.groupBy(_._1)
    FieldPresenceProperty(
      grouped.view.mapValues(_.map(_._2).min).map(identity).toMap,
      totalCount.min(otherProp.totalCount)
    )
  }

  override def unionMerge(
      otherProp: FieldPresenceProperty
  )(implicit p: JsonoidParams): FieldPresenceProperty = {
    val merged = fieldPresence.toSeq ++ otherProp.fieldPresence.toSeq
    val grouped = merged.groupBy(_._1)
    FieldPresenceProperty(
      grouped.view.mapValues(_.map(_._2).sum).map(identity).toMap,
      totalCount + otherProp.totalCount
    )
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit p: JsonoidParams): FieldPresenceProperty = {
    unionMerge(
      FieldPresenceProperty(value.view.mapValues(s => BigInt(1)).toMap, 1)
    )
  }
}

/** Tracks what keys are required in an object schema.
  *
  * @constructor Create a new required property.
  * @param required a possible set of keys which may be required
  */
final case class RequiredProperty(
    required: Option[Set[String]] = None
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = RequiredProperty

  override def newDefault()(implicit p: JsonoidParams): RequiredProperty =
    RequiredProperty()

  override def toJson()(implicit p: JsonoidParams): JObject = required match {
    case Some(requiredKeys) => ("required" -> requiredKeys)
    case None               => Nil
  }

  def onlyKeys(keys: Set[String]): RequiredProperty = {
    RequiredProperty(required.map(_.intersect(keys)))
  }

  override def intersectMerge(
      otherProp: RequiredProperty
  )(implicit p: JsonoidParams): RequiredProperty = {
    val other = otherProp.required
    RequiredProperty(unionOrNone(other, required))
  }

  override def unionMerge(
      otherProp: RequiredProperty
  )(implicit p: JsonoidParams): RequiredProperty = {
    val other = otherProp.required
    RequiredProperty(intersectOrNone(other, required))
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit p: JsonoidParams): RequiredProperty = {
    RequiredProperty(intersectOrNone(Some(value.keySet), required))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JObject(fields) =>
        required match {
          case Some(requiredFields) =>
            (requiredFields -- fields.map(_._1).toSet)
              .map(f =>
                Anomaly(
                  f"$path.$f",
                  "missing required field",
                  AnomalyLevel.Fatal
                )
              )
              .toSeq
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: RequiredProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Compatible if the other has the same or fewer required properties
    other.required.getOrElse(Set()).subsetOf(required.getOrElse(Set()))
  }

  override def expandTo(other: Option[RequiredProperty]): RequiredProperty = {
    other match {
      // TODO This depends specifically on the other set of required properties
      case Some(otherRequired) => intersectMerge(otherRequired)
      case None                => RequiredProperty()
    }
  }
}

object DependenciesProperty {

  /** The maximimum number of properties where dependencies should be tracked. */
  val MaxProperties: Int = 50
}

/** Tracks dependencies between keys in an object schema.
  *
  * @constructor Create a new dependencies property.
  * @param totalCount the total number of observed objects
  * @param counts a map from keys to the number of times the key is present
  * @param cooccurrence a map from pairs of keys to the number of times they cooccur
  * @param overloaded whether this property is overloaded and stopped tracking
  */
final case class DependenciesProperty(
    totalCount: BigInt = 0,
    counts: Map[String, BigInt] = Map.empty,
    cooccurrence: Map[(String, String), BigInt] = Map.empty,
    overloaded: Boolean = false
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = DependenciesProperty

  override def newDefault()(implicit p: JsonoidParams): DependenciesProperty =
    DependenciesProperty()

  def onlyKeys(keys: Set[String]): DependenciesProperty = {
    DependenciesProperty(
      totalCount,
      counts.view.filterKeys(keys.contains(_)).toMap,
      cooccurrence.view
        .filterKeys(ks => keys.contains(ks._1) && keys.contains(ks._2))
        .toMap,
      overloaded
    )
  }

  override def toJson()(implicit p: JsonoidParams): JObject = {
    // Use cooccurrence count to check dependencies in both directions,
    // excluding cases where properties are required (count is totalCount)
    val dependencies = dependencyMap()
    if (dependencies.isEmpty)
      Nil
    else
      ("dependentRequired" -> dependencies)
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.Var"
    )
  )
  def disjointSets: Seq[Set[String]] = {
    val setMap = HashMap.empty[String, Int]
    var totalSets = 0

    // Add any keys which occur together to the group
    // Note that this works because key pairs are sorted on insertion
    cooccurrence.keySet.foreach { case (key1, key2) =>
      if (!setMap.contains(key1)) {
        // Track a new group of keys
        totalSets += 1
        setMap += (key1 -> totalSets)
      }

      setMap(key2) = setMap(key1)
    }

    // Group all keys based on the set index
    setMap.keySet.groupBy(setMap(_)).map(_._2.toSet).toSeq
  }

  def dependencyMap(
      includeEverywhere: Boolean = false
  ): Map[String, Set[String]] = {
    cooccurrence.toSeq
      .flatMap { case ((key1, key2), count) =>
        (if (
           counts(key1) === count &&
           (includeEverywhere ||
             (count =/= totalCount &&
               counts(key2) =/= totalCount))
         ) {
           List((key1, key2))
         } else {
           List()
         }) ++ (if (
                  counts(key2) === count &&
                  (includeEverywhere ||
                    (count =/= totalCount &&
                      counts(key1) =/= totalCount))
                ) {
                  List((key2, key1))
                } else {
                  List()
                })
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .map(identity)
      .toMap
  }

  override def unionMerge(
      otherProp: DependenciesProperty
  )(implicit p: JsonoidParams): DependenciesProperty = {
    if (overloaded || otherProp.overloaded) {
      DependenciesProperty(overloaded = true)
    } else {
      val mergedCounts = (counts.toSeq ++ otherProp.counts.toSeq)
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).sum)
        .map(identity)
        .toMap
      val mergedCooccurrence =
        (cooccurrence.toSeq ++ otherProp.cooccurrence.toSeq)
          .groupBy(_._1)
          .view
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
  )(implicit p: JsonoidParams): DependenciesProperty = {
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
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JObject(fields) =>
        val fieldMap = fields.toMap
        fieldMap.keySet.toSeq.flatMap(f =>
          dependencyMap()
            .getOrElse(f, List())
            .filter(!fieldMap.contains(_))
            .map(d =>
              Anomaly(
                path,
                f"dependency $path.$d not found for $path.$f",
                AnomalyLevel.Fatal
              )
            )
        )
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: DependenciesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // We must have a subset of dependencies to be compatible
    val dependencies = dependencyMap(true)
    val otherDependencies = other.dependencyMap(true)
    dependencies.keySet.forall { key =>
      if (other.counts.contains(key)) {
        // Only consider dependent properties which exist in the other schema
        val containedDeps = dependencies(key).filter(other.counts.contains(_))

        // If this key does exist in the other schema, make
        // sure we do not have any additional dependencies
        otherDependencies.getOrElse(key, Set()).subsetOf(containedDeps)
      } else {
        // If this key does not exist in the other schema,
        // then we don't need to consider dependencies
        true
      }
    }
  }

  override def expandTo(
      other: Option[DependenciesProperty]
  ): DependenciesProperty = {
    other match {
      // TODO This depends specifically on the other set of dependencies
      case Some(otherDeps) => intersectMerge(otherDeps)
      case None            => DependenciesProperty()
    }
  }
}

/** A set of dependencies between keys in an object schema extracted from a
  * serialized JSON Schema.
  *
  * @constructor Create a new static dependencies property
  * @param dependencies a map from keys to dependent keys
  */
final case class StaticDependenciesProperty(
    dependencies: Map[String, Set[String]] = Map.empty
) extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = StaticDependenciesProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): StaticDependenciesProperty =
    StaticDependenciesProperty()

  def onlyKeys(keys: Set[String]): StaticDependenciesProperty = {
    StaticDependenciesProperty(
      dependencies.view
        .filterKeys(keys.contains(_))
        .mapValues(_.intersect(keys))
        .toMap
    )
  }

  override def mergeable: Boolean = false

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("dependentRequired" -> dependencies)

  override def unionMerge(
      otherProp: StaticDependenciesProperty
  )(implicit p: JsonoidParams): StaticDependenciesProperty = {
    throw new UnsupportedOperationException(
      "StaticDependenciesProperty cannot be merged"
    )
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  )(implicit p: JsonoidParams): StaticDependenciesProperty = {
    throw new UnsupportedOperationException(
      "StaticDependenciesProperty cannot be merged"
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JObject(fields) =>
        val fieldMap = fields.toMap
        fieldMap.keySet.toSeq.flatMap(f =>
          dependencies
            .getOrElse(f, List())
            .filter(!fieldMap.contains(_))
            .map(d =>
              Anomaly(
                path,
                f"dependency $path.$d not found for $path.$f",
                AnomalyLevel.Fatal
              )
            )
        )
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: StaticDependenciesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Further compatibility checking is possible, but we really need to compare
    // with DependenciesProperty too which the current API does not permit

    // We must have a subset of dependencies to be compatible
    dependencies.keySet.forall { key =>
      // XXX We really should check if the other schema contains
      //     the relevant property as we do for DependenciesProperty
      other.dependencies.getOrElse(key, Set()).subsetOf(dependencies(key))
    }
  }
}

final case class AdditionalPropertiesProperty(
    overriddenAdditionalProperties: Option[Boolean] = None
)(implicit p: JsonoidParams)
    extends SchemaProperty[Map[String, JsonSchema[_]]] {
  override type S = AdditionalPropertiesProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): AdditionalPropertiesProperty = AdditionalPropertiesProperty()(p)

  def allowAdditional: Boolean =
    overriddenAdditionalProperties.getOrElse(p.additionalProperties)

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("additionalProperties" ->
      overriddenAdditionalProperties.getOrElse(p.additionalProperties))

  override def intersectMerge(
      otherProp: AdditionalPropertiesProperty
  )(implicit p: JsonoidParams): AdditionalPropertiesProperty = {
    (
      overriddenAdditionalProperties,
      otherProp.overriddenAdditionalProperties
    ) match {
      case (None, None) =>
        AdditionalPropertiesProperty(Some(p.additionalProperties))
      case (None, Some(o))      => AdditionalPropertiesProperty(Some(o))
      case (Some(o), None)      => AdditionalPropertiesProperty(Some(o))
      case (Some(o1), Some(o2)) => AdditionalPropertiesProperty(Some(o1 && o2))
    }
  }

  override def unionMerge(
      otherProp: AdditionalPropertiesProperty
  )(implicit p: JsonoidParams): AdditionalPropertiesProperty = {
    (
      overriddenAdditionalProperties,
      otherProp.overriddenAdditionalProperties
    ) match {
      case (None, None) =>
        AdditionalPropertiesProperty(Some(p.additionalProperties))
      case (None, Some(o))      => AdditionalPropertiesProperty(Some(o))
      case (Some(o), None)      => AdditionalPropertiesProperty(Some(o))
      case (Some(o1), Some(o2)) => AdditionalPropertiesProperty(Some(o1 || o2))
    }
  }

  override def mergeValue(value: Map[String, JsonSchema[_]])(implicit
      p: JsonoidParams
  ): AdditionalPropertiesProperty =
    AdditionalPropertiesProperty(
      Some(overriddenAdditionalProperties.getOrElse(p.additionalProperties))
    )

  override def isSubsetOf(
      other: AdditionalPropertiesProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    other.allowAdditional || !allowAdditional
  }

  override def expandTo(
      other: Option[AdditionalPropertiesProperty]
  ): AdditionalPropertiesProperty = {
    this
    // other match {
    //   case None => AdditionalPropertiesProperty(true)
    //   case _    => this
    // }
  }
}
