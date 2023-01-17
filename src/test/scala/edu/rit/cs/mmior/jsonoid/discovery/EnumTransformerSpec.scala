package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s.{DefaultFormats, Formats}

import schemas._
import PropertySets._

import UnitSpec._

class EnumTransformerSpec extends UnitSpec {
  behavior of "EnumTransformer"

  implicit val formats: Formats = DefaultFormats

  def schemaWithOneValue[T](schema: JsonSchema[T], value: T): Unit = {
    it should s"convert single examples to constants for ${schema.getClass.getSimpleName}" in {
      var valueSchema: JsonSchema[_] = schema
      for (_ <- 1 to 4) { valueSchema = valueSchema.merge(valueSchema) }

      val transformedSchema =
        EnumTransformer.transformSchema(ObjectSchema(Map(("foo", valueSchema))))

      (transformedSchema.toJson \ "properties" \ "foo")
        .extract[Map[String, String]] shouldBe Map(("const", value.toString))
    }
  }

  schemaWithOneValue(IntegerSchema(1337), BigInt(1337))
  schemaWithOneValue(NumberSchema(3.14), BigDecimal(3.14))
  schemaWithOneValue(StringSchema("foo"), "foo")

  def schemaWithMultipleValues[T](
      schema: JsonSchema[T],
      values: Set[T]
  ): Unit = {
    it should s"convert multiple examples to enums for ${schema.getClass.getSimpleName}" in {
      var valueSchema: JsonSchema[_] = schema
      for (_ <- 1 to 4) { valueSchema = valueSchema.merge(valueSchema) }

      val transformedSchema = EnumTransformer.transformSchema(valueSchema)

      transformedSchema.toJson.extract[Map[String, Set[String]]] shouldBe Map(
        ("enum", values.map(_.toString))
      )
    }
  }

  schemaWithMultipleValues(
    IntegerSchema(1).merge(IntegerSchema(2)).asInstanceOf[IntegerSchema],
    Set(BigInt(1), BigInt(2))
  )
  schemaWithMultipleValues(
    NumberSchema(2.4).merge(NumberSchema(3.6)).asInstanceOf[NumberSchema],
    Set(BigDecimal(2.4), BigDecimal(3.6))
  )
  schemaWithMultipleValues(
    StringSchema("foo").merge(StringSchema("bar")).asInstanceOf[StringSchema],
    Set("foo", "bar")
  )

  it should "not convert to enum with insufficient evidence" in {
    val schema =
      IntegerSchema(1).merge(IntegerSchema(2)).asInstanceOf[IntegerSchema]

    val transformedSchema = EnumTransformer.transformSchema(schema)

    transformedSchema shouldBe a[IntegerSchema]
  }
}
