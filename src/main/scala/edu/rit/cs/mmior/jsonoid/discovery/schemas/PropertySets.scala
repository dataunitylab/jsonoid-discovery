package edu.rit.cs.mmior.jsonoid.discovery
package schemas

final case class PropertySet(
    val arrayProperties: SchemaProperties[List[JsonSchema[_]]],
    val integerProperties: SchemaProperties[BigInt],
    val numberProperties: SchemaProperties[BigDecimal],
    val objectProperties: SchemaProperties[Map[String, JsonSchema[_]]],
    val stringProperties: SchemaProperties[String]
) {
  def onlyNamed(propNames: Seq[String]): PropertySet = {
    val props = propNames.map(c =>
      Class.forName("edu.rit.cs.mmior.jsonoid.discovery.schemas." + c)
    )
    PropertySet(
      arrayProperties.only(props),
      integerProperties.only(props),
      numberProperties.only(props),
      objectProperties.only(props),
      stringProperties.only(props)
    )
  }
}

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

  val SimpleProperties: PropertySet = PropertySet(
    ArraySchema.SimpleProperties,
    IntegerSchema.SimpleProperties,
    NumberSchema.SimpleProperties,
    ObjectSchema.SimpleProperties,
    StringSchema.SimpleProperties
  )
}
