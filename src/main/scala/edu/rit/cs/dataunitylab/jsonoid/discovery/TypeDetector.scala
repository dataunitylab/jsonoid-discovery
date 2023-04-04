package edu.rit.cs.dataunitylab.jsonoid.discovery

import scala.util.Try

import org.json4s._

/** Provides the ability to detect the schema type of a serialized JSON Schema
  *  object without `type` based on other available properties.
  */
object TypeDetector {

  /** A map from JSON Schema types to their commonly observed properties. */
  val propertyTypes: Map[String, Set[String]] = Map(
    "array" -> Set(
      "additionalItems",
      "contains",
      "items",
      "maxContains",
      "maxItems",
      "minContains",
      "minItems",
      "prefixItems",
      "unevaluatedItems",
      "uniqueItems"
    ),
    "number" -> Set(
      "exclusiveMaximum",
      "exclusiveMinimum",
      "maximum",
      "minimum",
      "multipleOf"
    ),
    "object" -> Set(
      "additionalProperties",
      "maxProperties",
      "minProperties",
      "patternProperties",
      "properties",
      "propertyNames",
      "required",
      "unevaluatedProperties"
    ),
    "string" -> Set(
      "format",
      "maxLength",
      "minLength",
      "pattern"
    )
  )

  /** Detect the possible type of a serialized JSON Schema object. */
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def detectType(properties: Map[String, JValue]): Option[String] = {
    Try(
      propertyTypes
        .transform((key, value) => value.intersect(properties.keySet).size)
        .maxBy(_._2)
        ._1
    ).toOption
  }

  /** Detect all possible types of a serialized JSON Schema object. */
  def detectAllTypes(properties: Map[String, JValue]): List[String] = {
    propertyTypes
      .transform { (key, value) =>
        value.intersect(properties.keySet).size
      }
      .filter(_._2 > 0)
      .toList
      .sortBy(_._2)
      .map(_._1)
  }
}
