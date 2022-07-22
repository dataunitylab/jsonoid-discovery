package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import PropertySets._
import UnitSpec._

class JsonSchemaSpec extends UnitSpec {
  behavior of "JsonSchema"

  it should "convert false" in {
    JsonSchema.fromJson(JBool(false)) should equal(ZeroSchema())
  }

  it should "convert true" in {
    JsonSchema.fromJson(JBool(true)) should equal(AnySchema())
  }

  it should "convert a boolean schema" in {
    val boolSchema: JObject = ("type" -> "boolean")
    JsonSchema.fromJson(boolSchema) should equal(BooleanSchema())
  }

  it should "convert a null schema" in {
    val nullSchema: JObject = ("type" -> "null")
    JsonSchema.fromJson(nullSchema) should equal(NullSchema())
  }

  it should "convert an integer schema" in {
    val intSchema: JObject = ("type" -> "integer") ~
      ("minimum" -> 0) ~
      ("multipleOf" -> 2) ~
      ("maximum" -> 10)

    val convertedSchema = JsonSchema.fromJson(intSchema)
    convertedSchema.properties should contain(MinIntValueProperty(Some(0)))
    convertedSchema.properties should contain(IntMultipleOfProperty(Some(2)))
    convertedSchema.properties should contain(MaxIntValueProperty(Some(10)))
  }

  it should "convert an integer schema with exclusive max/min" in {
    val intSchema: JObject = ("type" -> "integer") ~
      ("exclusiveMinimum" -> 0) ~
      ("exclusiveMaximum" -> 10)

    val convertedSchema = JsonSchema.fromJson(intSchema)
    convertedSchema.properties should contain(
      MinIntValueProperty(Some(0), true)
    )
    convertedSchema.properties should contain(
      MaxIntValueProperty(Some(10), true)
    )
  }

  it should "convert a number schema" in {
    val intSchema: JObject = ("type" -> "number") ~
      ("minimum" -> 0.5) ~
      ("multipleOf" -> 0.5) ~
      ("maximum" -> 10.3)

    val convertedSchema = JsonSchema.fromJson(intSchema)
    convertedSchema.properties should contain(MinNumValueProperty(Some(0.5)))
    convertedSchema.properties should contain(NumMultipleOfProperty(Some(0.5)))
    convertedSchema.properties should contain(MaxNumValueProperty(Some(10.3)))
  }

  it should "convert a number schema with exclusive max/min" in {
    val intSchema: JObject = ("type" -> "number") ~
      ("exclusiveMinimum" -> 0.5) ~
      ("exclusiveMaximum" -> 10.3)

    val convertedSchema = JsonSchema.fromJson(intSchema)
    convertedSchema.properties should contain(
      MinNumValueProperty(Some(0.5), true)
    )
    convertedSchema.properties should contain(
      MaxNumValueProperty(Some(10.3), true)
    )
  }

  it should "convert a string schema" in {
    val intSchema: JObject = ("type" -> "string") ~
      ("minLength" -> 1) ~
      ("maxLength" -> 10) ~
      ("format" -> "date")

    val convertedSchema = JsonSchema.fromJson(intSchema)
    convertedSchema.properties should contain(MinLengthProperty(Some(1)))
    convertedSchema.properties should contain(MaxLengthProperty(Some(10)))
    convertedSchema.properties should contain(FormatProperty(Map("date" -> 1)))
  }

  it should "convert an object schema" in {
    val objSchema: JObject = ("type" -> "object") ~
      ("properties" -> ("foo" -> ("type" -> "boolean"))) ~
      ("patternProperties" -> ("^abc" -> ("type" -> "boolean"))) ~
      ("required" -> List("foo"))

    val convertedSchema =
      JsonSchema.fromJson(objSchema).asInstanceOf[ObjectSchema]

    convertedSchema.properties should contain(
      ObjectTypesProperty(Map("foo" -> BooleanSchema()))
    )

    val regexMap: Map[String, JsonSchema[_]] = convertedSchema.properties
      .get[PatternTypesProperty]
      .patternTypes
      .map { case (k, v) => (k.toString, v) }
      .toMap
    regexMap shouldBe Map("^abc" -> BooleanSchema())

    convertedSchema.properties should contain(
      RequiredProperty(Some(Set("foo")))
    )
  }

  it should "convert an object schema with dependencies" in {
    val objSchema: JObject = ("type" -> "object") ~
      ("dependentRequired" -> ("foo" -> List("bar")))

    val convertedSchema =
      JsonSchema.fromJson(objSchema).asInstanceOf[ObjectSchema]

    convertedSchema.properties should contain(
      StaticDependenciesProperty(Map("foo" -> List("bar")))
    )
  }

  it should "convert an object schema without properties" in {
    val objSchema: JObject = ("type" -> "object")

    val convertedSchema = JsonSchema.fromJson(objSchema)

    convertedSchema.properties should contain(ObjectTypesProperty(Map()))
  }

  it should "convert an object schema with missing type" in {
    val objSchema: JObject = ("properties" -> Nil)

    val convertedSchema = JsonSchema.fromJson(objSchema)

    convertedSchema shouldBe a[ObjectSchema]
  }

  it should "convert an array schema" in {
    val arraySchema: JObject = ("type" -> "array") ~
      ("items" -> ("type" -> "boolean")) ~
      ("minItems" -> 1) ~
      ("maxItems" -> 10)

    val convertedSchema = JsonSchema.fromJson(arraySchema)

    convertedSchema.properties should contain(
      ItemTypeProperty(Left(BooleanSchema()))
    )
    convertedSchema.properties should contain(MinItemsProperty(Some(1)))
    convertedSchema.properties should contain(MaxItemsProperty(Some(10)))
  }

  it should "convert an array schema where items is an array" in {
    val arraySchema: JObject = ("type" -> "array") ~
      ("items" -> List(("type" -> "boolean"), ("type" -> "null")))

    val convertedSchema = JsonSchema.fromJson(arraySchema)

    convertedSchema.properties should contain(
      ItemTypeProperty(Right(List(BooleanSchema(), NullSchema())))
    )
  }

  it should "convert an array schema with no item type" in {
    val arraySchema: JObject = ("type" -> "array")

    val convertedSchema = JsonSchema.fromJson(arraySchema)

    convertedSchema.properties should contain(
      ItemTypeProperty(Left(AnySchema()))
    )
  }

  it should "convert a tuple schema" in {
    val tupleSchema: JObject = ("type" -> "array") ~
      ("prefixItems" -> List(("type" -> "boolean"), ("type" -> "null")))

    val convertedSchema = JsonSchema.fromJson(tupleSchema)

    convertedSchema.properties should contain(
      ItemTypeProperty(Right(List(BooleanSchema(), NullSchema())))
    )
  }

  it should "convert anyOf to a ProductSchema" in {
    val anyOfSchema: JObject = ("anyOf" -> List(
      ("type" -> "boolean"),
      ("type" ->
        "integer")
    ))

    val convertedSchema =
      JsonSchema.fromJson(anyOfSchema).asInstanceOf[ProductSchema]
    val types =
      convertedSchema.properties.get[ProductSchemaTypesProperty].schemaTypes

    types(0) shouldBe a[BooleanSchema]
    types(1) shouldBe a[IntegerSchema]
  }

  it should "convert oneOf to a ProductSchema" in {
    val oneOfSchema: JObject = ("oneOf" -> List(
      ("type" -> "boolean"),
      ("type" ->
        "integer")
    ))

    val convertedSchema =
      JsonSchema.fromJson(oneOfSchema).asInstanceOf[ProductSchema]
    val types =
      convertedSchema.properties.get[ProductSchemaTypesProperty].schemaTypes

    types(0) shouldBe a[BooleanSchema]
    types(1) shouldBe a[IntegerSchema]
  }

  it should "strip allOf with a single element" in {
    val allOfSchema: JObject = ("allOf" -> List(("type" -> "boolean")))

    val convertedSchema = JsonSchema.fromJson(allOfSchema)
    convertedSchema shouldBe a[BooleanSchema]
  }

  it should "strip anyOf with a single element" in {
    val anyOfSchema: JObject = ("anyOf" -> List(("type" -> "boolean")))

    val convertedSchema = JsonSchema.fromJson(anyOfSchema)
    convertedSchema shouldBe a[BooleanSchema]
  }

  it should "strip oneOf with a single element" in {
    val oneOfSchema: JObject = ("oneOf" -> List(("type" -> "boolean")))

    val convertedSchema = JsonSchema.fromJson(oneOfSchema)
    convertedSchema shouldBe a[BooleanSchema]
  }

  it should "convert an array of types to a ProductSchema" in {
    val productSchema: JObject = ("type" -> List("boolean", "integer"))

    val convertedSchema =
      JsonSchema.fromJson(productSchema).asInstanceOf[ProductSchema]
    val types =
      convertedSchema.properties.get[ProductSchemaTypesProperty].schemaTypes

    types(0) shouldBe a[BooleanSchema]
    types(1) shouldBe a[IntegerSchema]
  }

  it should "convert an enum" in {
    val enumSchema: JObject = ("enum" -> JArray(List(3, "foo")))

    val convertedSchema =
      JsonSchema.fromJson(enumSchema).asInstanceOf[EnumSchema]
    val values =
      convertedSchema.properties.get[EnumValuesProperty].values

    values shouldBe Set(JInt(3), JString("foo"))
  }

  it should "convert a const" in {
    val enumSchema: JObject = ("const" -> "foo")

    val convertedSchema =
      JsonSchema.fromJson(enumSchema).asInstanceOf[EnumSchema]
    val values =
      convertedSchema.properties.get[EnumValuesProperty].values

    values shouldBe Set(JString("foo"))
  }

  it should "convert definitions" in {
    val defnSchema: JObject =
      ("type" -> "object") ~ ("definitions" -> ("foo" -> ("type" -> "boolean")))

    val convertedSchema =
      JsonSchema.fromJson(defnSchema).asInstanceOf[ObjectSchema]

    convertedSchema.definitions shouldBe Map("foo" -> BooleanSchema())
  }

  it should "convert values without an explicit type" in {
    val unknownSchema: JObject = ("minLength" -> JInt(3))

    val convertedSchema = JsonSchema.fromJson(unknownSchema)

    convertedSchema shouldBe a[StringSchema]
  }

  it should "convert values with multiple implicit types" in {
    val unknownSchema: JObject = ("minLength" -> JInt(3)) ~
      ("minimum" -> JInt(2))

    val convertedSchema = JsonSchema.fromJson(unknownSchema)

    convertedSchema shouldBe a[ProductSchema]

    val schemaTypes = convertedSchema
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
      .schemaTypes
      .map(_.schemaType)
    schemaTypes should contain theSameElementsAs List("string", "number")
  }

  it should "be able to limit available properties" in {
    val schema = IntegerSchema(5).onlyPropertiesNamed(
      List("MinIntValueProperty", "MinLengthProperty")
    )
    schema.properties.properties should have size 1
    schema.properties.has[MinIntValueProperty] shouldBe true
  }

  it should "not change schema type when limiting properties" in {
    val schema = ObjectSchema(Map("foo" -> BooleanSchema()))
      .onlyPropertiesNamed(List("ObjectTypesProperty"))
    schema.properties.properties should have size 1
    schema.properties.has[ObjectTypesProperty] shouldBe true
    schema.properties
      .get[ObjectTypesProperty]
      .objectTypes("foo") shouldBe a[BooleanSchema]
  }

  it should "should limit properties inside ProductSchemas" in {
    val schema = ProductSchema(StringSchema("foo"))
      .onlyPropertiesNamed(List("ProductSchemaTypesProperty"))

    val stringSchema =
      schema.properties.get[ProductSchemaTypesProperty].schemaTypes(0)
    stringSchema.properties.properties shouldBe empty
  }

  it should "limit properties in objects" in {
    val schema = ObjectSchema(Map("foo" -> StringSchema("foo")))
      .onlyPropertiesNamed(List("ObjectTypesProperty"))
    val fooType = schema.properties.get[ObjectTypesProperty].objectTypes("foo")
    fooType.properties.properties shouldBe empty
  }

  it should "limit properties in nested objects" in {
    val schema = ObjectSchema(
      Map("foo" -> ObjectSchema(Map("bar" -> StringSchema("foo"))))
    )
      .onlyPropertiesNamed(List("ObjectTypesProperty"))
    val fooType = schema.properties
      .get[ObjectTypesProperty]
      .objectTypes("foo")
      .asInstanceOf[ObjectSchema]
    val barType = fooType.properties.get[ObjectTypesProperty].objectTypes("bar")
    barType.properties.properties shouldBe empty
  }
}
