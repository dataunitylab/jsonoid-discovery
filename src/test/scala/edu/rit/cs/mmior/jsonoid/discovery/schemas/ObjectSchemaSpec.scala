package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
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
    val fieldPresenceProp = objectSchema.get[FieldPresenceProperty]
    val expectedJson: JObject = ("fieldPresence" -> (("foo" -> JDouble(1)) ~ ("bar" -> JDouble(0.5))))
    fieldPresenceProp.toJson shouldEqual expectedJson
  }

  it should "track dependencies in field occurrence" in {
    val dependentSchema = ObjectSchema(Map("foo" -> BooleanSchema(), "bar" ->
      BooleanSchema())).properties.mergeValue(Map("baz" ->
      BooleanSchema())).mergeValue(Map("foo" -> BooleanSchema()))
    val dependenciesProp = dependentSchema.get[DependenciesProperty]
    implicit val formats: Formats = DefaultFormats
    (dependenciesProp.toJson \ "dependencies").extract[Map[String, List[String]]] shouldEqual Map("bar" -> List("foo"))
  }
}
