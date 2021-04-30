package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s.{DefaultFormats, Formats}

import schemas._

class EnumTransformerSpec extends UnitSpec {
  behavior of "EnumTransformer"

  implicit val formats: Formats = DefaultFormats

  it should "convert single examples to constants" in {
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    var stringSchema: JsonSchema[_] = StringSchema("foo")
    for (_ <- 1 to 4) { stringSchema = stringSchema.merge(stringSchema) }

    val transformedSchema = EnumTransformer.transformSchema(ObjectSchema(Map(("foo", stringSchema))))

    (transformedSchema.toJson \ "properties" \ "foo").extract[Map[String, String]] shouldBe Map(("const", "foo"))
  }

  it should "convert multiple examples to enums" in {
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    var stringSchema: JsonSchema[_] = StringSchema("foo").merge(StringSchema("bar"))
    for (_ <- 1 to 4) { stringSchema = stringSchema.merge(stringSchema) }

    val transformedSchema = EnumTransformer.transformSchema(ObjectSchema(Map(("foo", stringSchema))))

    (transformedSchema.toJson \ "properties" \ "foo").extract[Map[String, Set[String]]] shouldBe Map(("enum", Set("bar", "foo")))
  }
}
