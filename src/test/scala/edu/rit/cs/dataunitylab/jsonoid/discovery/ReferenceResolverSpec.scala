package edu.rit.cs.dataunitylab.jsonoid.discovery

import org.json4s._
import org.json4s.jackson.JsonMethods._

import schemas._

import UnitSpec._

class ReferenceResolverSpec extends UnitSpec {
  behavior of "ReferenceResolver"

  implicit val formats: Formats = DefaultFormats

  val json: JValue = parse("""{
    "type": "object",
    "properties": {
      "foo": {
        "type": "boolean"
      },
      "quux": {
        "$ref": "#/foo"
      },
      "bar": {
        "type": "object",
        "properties": {
          "baz": {"$ref": "#/bar"}
        }
      }
    },
    "patternProperties": {
      "^abc": {
        "$ref": "#/foo"
      }
    }
  }""")

  it should "replace a common schema with a definition" in {
    val schema = JsonSchema.fromJson(json).asInstanceOf[ObjectSchema]
    val transformedSchema = ReferenceResolver.transformSchema(schema)
    val quux =
      transformedSchema.findByPointer("/quux").get.asInstanceOf[ReferenceSchema]
    quux.properties
      .get[ReferenceObjectProperty]
      .schema shouldBe a[BooleanSchema]

    val patternTypes = schema.properties.get[PatternTypesProperty].patternTypes
    val abc = patternTypes.toSeq(0)._2.asInstanceOf[ReferenceSchema]
    abc.properties
      .get[ReferenceObjectProperty]
      .schema shouldBe a[BooleanSchema]

    // Validate that the circular reference was identified
    val bar = transformedSchema.findByPointer("/bar").get
    val baz = transformedSchema
      .findByPointer("/bar/baz")
      .get
      .asInstanceOf[ReferenceSchema]
    baz.properties.get[ReferenceObjectProperty].schema shouldEqual bar
  }
}
