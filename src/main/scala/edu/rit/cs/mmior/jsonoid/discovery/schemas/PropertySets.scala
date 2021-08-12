package edu.rit.cs.mmior.jsonoid.discovery
package schemas

final case class PropertySet(
    val arrayProperties: SchemaProperties[List[JsonSchema[_]]],
    val integerProperties: SchemaProperties[BigInt],
    val numberProperties: SchemaProperties[BigDecimal],
    val objectProperties: SchemaProperties[Map[String, JsonSchema[_]]],
    val stringProperties: SchemaProperties[String]
) {}

object PropertySets {
  implicit val AllProperties: PropertySet = PropertySet(
    ArraySchema.AllProperties,
    IntegerSchema.AllProperties,
    NumberSchema.AllProperties,
    ObjectSchema.AllProperties,
    StringSchema.AllProperties
  )

  val MinProperties: PropertySet = PropertySet(
    ArraySchema.MinProperties,
    IntegerSchema.MinProperties,
    NumberSchema.MinProperties,
    ObjectSchema.MinProperties,
    StringSchema.MinProperties
  )
}
