package edu.rit.cs.dataunitylab.jsonoid.discovery

import org.json4s._
import org.json4s.jackson.JsonMethods._

import schemas._

import UnitSpec._

class DefinitionTransformerSpec extends UnitSpec {
  behavior of "DefinitionTransformer"

  implicit val formats: Formats = DefaultFormats

  it should "replace a common schema with a definition" in {
    val json: JValue = parse("""{
      "foo": {
        "bar": 1,
        "baz": 2
      },
      "quux": {
        "bar": 3,
        "baz": 4
      }
    }""")

    val schema =
      DiscoverSchema.discoverFromValue(json).asInstanceOf[ObjectSchema]
    val transformedSchema = DefinitionTransformer.transformSchema(schema)

    // Check that the definition exists
    val defs = transformedSchema.toJson() \ "$defs"
    val defNames =
      (defs \ "defn0" \ "properties").asInstanceOf[JObject].obj.map(_._1).toSet
    defNames shouldBe Set("bar", "baz")

    // Check that a reference was replaced
    val props = transformedSchema.toJson() \ "properties"
    val ref = Map("$ref" -> "#/$defs/defn0")
    (props \ "foo").extract[Map[String, String]] shouldBe ref
    (props \ "quux").extract[Map[String, String]] shouldBe ref

    // Check that a reference object was defined
    val referencedSchema = transformedSchema
      .findByPointer("/foo")
      .get
      .asInstanceOf[ReferenceSchema]
      .properties
      .get[ReferenceObjectProperty]
      .schema
      .asInstanceOf[ObjectSchema]
    referencedSchema.properties
      .get[ObjectTypesProperty]
      .objectTypes
      .keys should contain theSameElementsAs Set("bar", "baz")
  }

  it should "not fail on objects with single keys" in {
    val json: JValue = parse("""{
      "corge": 1
    }""")

    val schema =
      DiscoverSchema.discoverFromValue(json).asInstanceOf[ObjectSchema]
    val transformedSchema = DefinitionTransformer.transformSchema(schema)

    schema.shouldEqual(transformedSchema)
  }
}
