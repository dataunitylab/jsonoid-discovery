package edu.rit.cs.mmior.jsonoid.discovery

import scala.util.Try

import org.json4s._

object TypeDetector {
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

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def detectType(properties: Map[String, JValue]): Option[String] = {
    Try(
      propertyTypes
        .transform((key, value) => value.intersect(properties.keySet).size)
        .maxBy(_._2)
        ._1
    ).toOption
  }

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
