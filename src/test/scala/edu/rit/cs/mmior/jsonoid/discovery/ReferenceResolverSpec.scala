package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s._
import org.json4s.jackson.JsonMethods._

import schemas._

class ReferenceResolverSpec extends UnitSpec {
  behavior of "ReferenceResolver"

  implicit val formats: Formats = DefaultFormats
  implicit val er: EquivalenceRelation =
    EquivalenceRelations.KindEquivalenceRelation

  val json: JValue = parse("""{
    "type": "object",
    "properties": {
      "foo": {
        "type": "boolean"
      },
      "quux": {
        "$ref": "#/foo"
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
  }
}
