package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object ObjectSchema {
  def apply(
      value: Map[String, JsonSchema[_]]
  )(implicit propSet: PropertySet): ObjectSchema = {
    ObjectSchema(propSet.objectProperties.mergeValue(value))
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

  override val staticProperties: JObject = ("additionalProperties" -> false)

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ ObjectSchema(otherProperties) =>
      ObjectSchema(properties.merge(otherProperties))
  }

  override def copy(
      properties: SchemaProperties[Map[String, JsonSchema[_]]]
  ): ObjectSchema =
    ObjectSchema(properties)

  def toJsonSchema: JObject = {
    val schemaObj: JObject =
      ("$schema" -> "https://json-schema.org/draft/2020-12/schema") ~
        ("$vocabulary" ->
          ("https://json-schema.org/draft/2020-12/meta/core" -> true) ~
          ("https://json-schema.org/draft/2020-12/meta/validation" -> true) ~
          ("https://json-schema.org/draft/2020-12/meta/format" -> true))

    toJson.merge(schemaObj)
  }

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

  override def merge(
      otherProp: ObjectTypesProperty
  ): ObjectTypesProperty = {
    val other = otherProp.objectTypes
    this.mergeValue(other)
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  ): ObjectTypesProperty = {
    val merged = objectTypes.toSeq ++ value.toSeq
    val grouped = merged.groupBy(_._1)
    ObjectTypesProperty(
      // .map(identity) below is necessary to
      // produce a map which is serializable
      grouped
        .mapValues(_.map(_._2).fold(ZeroSchema())(_.merge(_)))
        .map(identity)
        .toMap
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

  override def merge(
      otherProp: FieldPresenceProperty
  ): FieldPresenceProperty = {
    val merged = fieldPresence.toSeq ++ otherProp.fieldPresence.toSeq
    val grouped = merged.groupBy(_._1)
    FieldPresenceProperty(
      grouped.mapValues(_.map(_._2).sum).map(identity).toMap,
      totalCount + otherProp.totalCount
    )
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  ): FieldPresenceProperty = {
    merge(FieldPresenceProperty(value.mapValues(s => 1), 1))
  }
}

final case class RequiredProperty(
    required: Option[Set[String]] = None
) extends SchemaProperty[Map[String, JsonSchema[_]], RequiredProperty] {
  override def toJson: JObject = ("required" -> required)

  override def merge(
      otherProp: RequiredProperty
  ): RequiredProperty = {
    val other = otherProp.required
    RequiredProperty(intersectOrNone(other, required))
  }

  override def mergeValue(
      value: Map[String, JsonSchema[_]]
  ): RequiredProperty = {
    RequiredProperty(intersectOrNone(Some(value.keySet), required))
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
      ("dependencies" -> dependencies)
    }
  }

  override def merge(
      otherProp: DependenciesProperty
  ): DependenciesProperty = {
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
  ): DependenciesProperty = {
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
      merge(DependenciesProperty(1, counts, cooccurrence))
    }
  }
}
