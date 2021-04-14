package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

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

  it should "track the percentage of objects with each field" in {
    val fieldPresenceProp = objectSchema.find(_.isInstanceOf[FieldPresenceProperty]).fold(FieldPresenceProperty())(_.asInstanceOf[FieldPresenceProperty])
    val expectedJson: JObject = ("fieldPresence" -> (("foo" -> JDouble(1)) ~ ("bar" -> JDouble(0.5))))
    fieldPresenceProp.toJson shouldEqual expectedJson
  }
}
