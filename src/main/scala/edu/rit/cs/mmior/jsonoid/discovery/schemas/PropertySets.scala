package edu.rit.cs.mmior.jsonoid.discovery
package schemas

/** Sets of properties which can be used during schema discovery.
  *
  * @constructor Create a new set of properties
  * @param arrayProperties properties related to [[ArraySchema]]
  * @param booleanProperties properties related to [[BooleanSchema]]
  * @param integerProperties properties related to [[IntegerSchema]]
  * @param numberProperties properties related to [[NumberSchema]]
  * @param objectPropperties properties related to [[ObjectSchema]]
  * @param stringProperties properties related to [[StringSchema]]
  */
final case class PropertySet(
    val arrayProperties: SchemaProperties[List[JsonSchema[_]]],
    val booleanProperties: SchemaProperties[Boolean],
    val integerProperties: SchemaProperties[BigInt],
    val numberProperties: SchemaProperties[BigDecimal],
    val objectProperties: SchemaProperties[Map[String, JsonSchema[_]]],
    val stringProperties: SchemaProperties[String]
) {

  /** Create a new property set with only the specific sequence of named
    * properties.
    *
    * @param propNames the property names to include
    *
    * @return a new property set with only the named properties
    */
  def onlyNamed(propNames: Seq[String]): PropertySet = {
    val props = propNames.map(c =>
      Class.forName("edu.rit.cs.mmior.jsonoid.discovery.schemas." + c)
    )
    PropertySet(
      arrayProperties.only(props),
      booleanProperties.only(props),
      integerProperties.only(props),
      numberProperties.only(props),
      objectProperties.only(props),
      stringProperties.only(props)
    )
  }
}

/** Common collections of [[PropertySet]] values for discover. */
object PropertySets {

  /** A property set with all supported properties. */
  implicit val AllProperties: PropertySet = PropertySet(
    ArraySchema.AllProperties,
    BooleanSchema.AllProperties,
    IntegerSchema.AllProperties,
    NumberSchema.AllProperties,
    ObjectSchema.AllProperties,
    StringSchema.AllProperties
  )

  /** A property set with the minimum set of properties to discover structural
    *  information..
    */
  def MinProperties(implicit p: JsonoidParams): PropertySet = PropertySet(
    ArraySchema.MinProperties,
    BooleanSchema.MinProperties,
    IntegerSchema.MinProperties,
    NumberSchema.MinProperties,
    ObjectSchema.MinProperties(p),
    StringSchema.MinProperties
  )

  /** A property set with all properties supported by JSON Schema.. */
  val SimpleProperties: PropertySet = PropertySet(
    ArraySchema.SimpleProperties,
    BooleanSchema.SimpleProperties,
    IntegerSchema.SimpleProperties,
    NumberSchema.SimpleProperties,
    ObjectSchema.SimpleProperties,
    StringSchema.SimpleProperties
  )
}
