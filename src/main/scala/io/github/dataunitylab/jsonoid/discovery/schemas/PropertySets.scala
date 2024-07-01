package io.github.dataunitylab.jsonoid.discovery
package schemas

/** Sets of properties which can be used during schema discovery.
  *
  * @constructor
  *   Create a new set of properties
  * @param arrayProperties
  *   properties related to [[ArraySchema]]
  * @param booleanProperties
  *   properties related to [[BooleanSchema]]
  * @param integerProperties
  *   properties related to [[IntegerSchema]]
  * @param numberProperties
  *   properties related to [[NumberSchema]]
  * @param objectPropperties
  *   properties related to [[ObjectSchema]]
  * @param stringProperties
  *   properties related to [[StringSchema]]
  */
final case class PropertySet(
    val arrayProperties: SchemaProperties[List[JsonSchema[_]]],
    val booleanProperties: SchemaProperties[Boolean],
    val integerProperties: SchemaProperties[BigInt],
    val numberProperties: SchemaProperties[BigDecimal],
    val objectProperties: SchemaProperties[Map[String, JsonSchema[_]]],
    val stringProperties: SchemaProperties[String]
) {

  /** Create a new property set without the specific sequence of properties.
    *
    * @param propClasses
    *   the property classes to exclude
    *
    * @return
    *   a new property set without the given properties
    */
  def without(propClasses: Seq[Class[_]]): PropertySet = {
    PropertySet(
      arrayProperties.without(propClasses),
      booleanProperties.without(propClasses),
      integerProperties.without(propClasses),
      numberProperties.without(propClasses),
      objectProperties.without(propClasses),
      stringProperties.without(propClasses)
    )
  }

  /** Create a new property set without the specific sequence of named
    * properties.
    *
    * @param propNames
    *   the property names to exclude
    *
    * @return
    *   a new property set without the given properties
    */
  def withoutNamed(propNames: Seq[String]): PropertySet = {
    val propClasses = propNames.map(c =>
      Class.forName("io.github.dataunitylab.jsonoid.discovery.schemas." + c)
    )
    without(propClasses)
  }

  /** Create a new property set with only the specific sequence of properties.
    *
    * @param propClasses
    *   the property classes to include
    *
    * @return
    *   a new property set with only the given properties
    */
  def only(propClasses: Seq[Class[_]]): PropertySet = {
    PropertySet(
      arrayProperties.only(propClasses),
      booleanProperties.only(propClasses),
      integerProperties.only(propClasses),
      numberProperties.only(propClasses),
      objectProperties.only(propClasses),
      stringProperties.only(propClasses)
    )
  }

  /** Create a new property set with only the specific sequence of named
    * properties.
    *
    * @param propNames
    *   the property names to include
    *
    * @return
    *   a new property set with only the named properties
    */
  def onlyNamed(propNames: Seq[String]): PropertySet = {
    val propClasses = propNames.map(c =>
      Class.forName("io.github.dataunitylab.jsonoid.discovery.schemas." + c)
    )
    only(propClasses)
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
    * information..
    */
  val MinProperties: PropertySet = PropertySet(
    ArraySchema.MinProperties,
    BooleanSchema.MinProperties,
    IntegerSchema.MinProperties,
    NumberSchema.MinProperties,
    ObjectSchema.MinProperties,
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
