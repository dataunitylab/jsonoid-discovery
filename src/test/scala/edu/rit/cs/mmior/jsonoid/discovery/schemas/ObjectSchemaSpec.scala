package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class ObjectSchemaSpec extends UnitSpec {
  behavior of "ObjectSchema"

  private val objectTypes = Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema())
  private val objectSchema = ObjectSchema(Map("foo" -> BooleanSchema())).properties.mergeValue(objectTypes)

  it should "track required properties" in {
    objectSchema should contain (RequiredProperty(Some(Set("foo"))))
  }

  it should "track property schemas" in {
    objectSchema should contain (ObjectTypesProperty(objectTypes))
  }
}
